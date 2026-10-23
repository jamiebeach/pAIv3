(in-package #:pai-v3.api)

;;; Reading (bead v3-clp.21; operator request 2026-10-22). The operator puts a
;;; book in the workspace through Files and asks; the agent reads it a passage
;;; at a time in the background, within a daily budget. Each passage goes
;;; through graph formation with the book as its world, so what happens in a
;;; story is never retrieved as real, and the passage is the evidence. After
;;; each passage the agent keeps a few lines of its own about it, as a memory,
;;; so it can talk about the book. It says when it has finished.
;;;
;;; The text is kept in the agent's encrypted blob store, never in the ledger.
;;; EPUB, plain text, Markdown and HTML are read.

(define-setting "reading.daily_usd"
  :type :number :min 0 :max 20 :default 0.5d0 :group "Reading"
  :description "Most reading books may spend in a UTC day.")
(define-setting "reading.passage_characters"
  :type :integer :min 1500 :max 30000 :default 6000 :group "Reading"
  :description "About how much of a book one reading step takes in.")
(define-setting "reading.passages_per_hour"
  :type :integer :min 0 :max 120 :default 12 :group "Reading"
  :description "Most passages read in an hour; 0 pauses all reading.")

(defparameter +book-max-bytes+ (* 30 1024 1024) "Largest book file read.")

(define-event-type "reading.added"
  :schema (("book_id" :string) ("title" :string) ("author" :string :optional) ("file" :string)
           ("characters" :integer) ("passages" :integer) ("entity_id" :string)))
(define-event-type "reading.passage.read"
  :schema (("book_id" :string) ("passage" :integer) ("facts" :integer) ("entities" :integer)
           ("dropped" :integer) ("notes" :string :optional)))
(define-event-type "reading.state"
  :schema (("book_id" :string) ("state" :string)))

(define-projection "reading"
  :kind :inline
  :consumes ("reading.added" "reading.passage.read" "reading.state")
  :tables ("CREATE TABLE IF NOT EXISTS books (book_id TEXT PRIMARY KEY, title TEXT NOT NULL, author TEXT, file TEXT NOT NULL, characters INTEGER NOT NULL, passages INTEGER NOT NULL, read_through INTEGER NOT NULL, state TEXT NOT NULL, entity_id TEXT NOT NULL, facts INTEGER NOT NULL, entities INTEGER NOT NULL, added_at TEXT NOT NULL, last_at TEXT)"
           "CREATE INDEX IF NOT EXISTS books_state_idx ON books(state,added_at)"
           "CREATE TABLE IF NOT EXISTS book_passages (book_id TEXT NOT NULL, passage INTEGER NOT NULL, notes TEXT, at TEXT NOT NULL, PRIMARY KEY(book_id,passage))"
           "CREATE INDEX IF NOT EXISTS book_passages_at_idx ON book_passages(at)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)) (type (event-type e)))
              (flet ((f (k) (%given (obj-get p k))))
                (cond ((string= type "reading.added")
                       (sql-exec db "INSERT OR IGNORE INTO books(book_id,title,author,file,characters,passages,read_through,state,entity_id,facts,entities,added_at) VALUES(?1,?2,?3,?4,?5,?6,0,'reading',?7,0,0,?8)"
                                 (f "book_id") (f "title") (f "author") (f "file") (f "characters") (f "passages") (f "entity_id") (event-at e)))
                      ((string= type "reading.passage.read")
                       (sql-exec db "INSERT OR IGNORE INTO book_passages(book_id,passage,notes,at) VALUES(?1,?2,?3,?4)"
                                 (f "book_id") (f "passage") (f "notes") (event-at e))
                       (sql-exec db "UPDATE books SET read_through=max(read_through,?2),facts=facts+?3,entities=entities+?4,last_at=?5,state=CASE WHEN ?2>=passages THEN 'finished' ELSE state END WHERE book_id=?1"
                                 (f "book_id") (f "passage") (f "facts") (f "entities") (event-at e)))
                      (t (sql-exec db "UPDATE books SET state=?2 WHERE book_id=?1" (f "book_id") (f "state"))))))))

;;; The text, in the blob store

(defparameter +book-text-ddl+
  "CREATE TABLE IF NOT EXISTS book_texts (book_id TEXT PRIMARY KEY, text TEXT NOT NULL)")

(defun %book-store (kernel)
  (let ((db (pai-v3.mind::%image-db kernel)))
    (sql-exec db +book-text-ddl+)
    db))

(defun book-text (kernel book-id)
  (sql-value (%book-store kernel) "SELECT text FROM book_texts WHERE book_id=?1 LIMIT 1" book-id))

(defun book-passages (text size)
  "TEXT cut into passages of about SIZE characters, at paragraph ends where it can."
  (let ((out '()) (start 0) (n (length text)))
    (loop while (< start n)
          do (let* ((limit (min n (+ start size)))
                    (cut (if (>= limit n)
                             n
                             (let ((para (search (format nil "~%~%") text :from-end t :start2 (+ start (floor size 2)) :end2 limit))
                                   (line (position #\Newline text :from-end t :start (+ start (floor size 2)) :end limit))
                                   (space (position #\Space text :from-end t :start (+ start (floor size 2)) :end limit)))
                               (or (and para (+ para 2)) (and line (1+ line)) (and space (1+ space)) limit)))))
               (let ((piece (string-trim '(#\Space #\Newline #\Tab #\Return) (subseq text start cut))))
                 (when (plusp (length piece)) (push piece out)))
               (setf start cut)))
    (nreverse out)))

;;; Getting the text out of a file

(defun %u16 (v i) (logior (aref v i) (ash (aref v (1+ i)) 8)))
(defun %u32 (v i) (logior (%u16 v i) (ash (%u16 v (+ i 2)) 16)))

(defun zip-entries (octets)
  "The files in a zip archive, as an alist of name -> octets."
  (let ((eocd (loop for i from (- (length octets) 22) downto (max 0 (- (length octets) 65558))
                    when (and (= (aref octets i) #x50) (= (%u32 octets i) #x06054b50)) return i)))
    (unless eocd (error "not a zip archive"))
    (loop with at = (%u32 octets (+ eocd 16))
          repeat (%u16 octets (+ eocd 10))
          collect (progn
                    (unless (= (%u32 octets at) #x02014b50) (error "a damaged zip archive"))
                    (let* ((method (%u16 octets (+ at 10)))
                           (size (%u32 octets (+ at 20)))
                           (name-len (%u16 octets (+ at 28)))
                           (extra-len (%u16 octets (+ at 30)))
                           (comment-len (%u16 octets (+ at 32)))
                           (local (%u32 octets (+ at 42)))
                           (name (babel:octets-to-string octets :start (+ at 46) :end (+ at 46 name-len) :encoding :utf-8))
                           (data-start (+ local 30 (%u16 octets (+ local 26)) (%u16 octets (+ local 28))))
                           (data (subseq octets data-start (+ data-start size))))
                      (setf at (+ at 46 name-len extra-len comment-len))
                      (cons name (case method
                                   (0 data)
                                   (8 (chipz:decompress nil 'chipz:deflate data))
                                   (t nil))))))))

(defun %attr (tag name)
  "The value of attribute NAME in TAG's text, or NIL."
  (let ((at (search (format nil "~a=" name) tag)))
    (when at
      (let* ((q (char tag (+ at (length name) 1)))
             (start (+ at (length name) 2))
             (end (position q tag :start start)))
        (and end (member q '(#\" #\')) (subseq tag start end))))))

(defun %tags (text name)
  "The text of each <NAME ...> tag in TEXT."
  (loop with start = 0
        for at = (search (format nil "<~a " name) text :start2 start)
        while at
        collect (let ((end (or (position #\> text :start at) (length text))))
                  (setf start end)
                  (subseq text at end))))

(defun %element-text (text name)
  "The text inside the first <NAME>...</NAME> (any prefix such as dc:), or NIL."
  (let ((at (search (format nil "~a" name) text)))
    (when at
      (let* ((open-end (position #\> text :start at))
             (close (and open-end (search "</" text :start2 open-end))))
        (and close (string-trim '(#\Space #\Newline) (pai-v3.mind::html-text (subseq text (1+ open-end) close))))))))

(defun %url-decode (s)
  (with-output-to-string (o)
    (loop with i = 0 while (< i (length s))
          do (if (and (char= (char s i) #\%) (< (+ i 2) (length s)))
                 (progn (write-char (code-char (parse-integer s :start (1+ i) :end (+ i 3) :radix 16)) o) (incf i 3))
                 (progn (write-char (char s i) o) (incf i))))))

(defun epub-text (octets)
  "(values text title author) of an EPUB: its spine's documents, in order."
  (let* ((entries (zip-entries octets))
         (file (lambda (name) (let ((e (assoc name entries :test #'equal)))
                                (and e (cdr e) (babel:octets-to-string (cdr e) :encoding :utf-8 :errorp nil)))))
         (container (or (funcall file "META-INF/container.xml") (error "an EPUB without its container")))
         (opf-path (or (%attr (first (%tags container "rootfile")) "full-path") (error "an EPUB without a package file")))
         (opf (or (funcall file opf-path) (error "the EPUB's package file is missing")))
         (base (let ((slash (position #\/ opf-path :from-end t))) (if slash (subseq opf-path 0 (1+ slash)) "")))
         (items (mapcar (lambda (tag) (cons (%attr tag "id") (%attr tag "href"))) (%tags opf "item")))
         (spine (mapcar (lambda (tag) (%attr tag "idref")) (%tags opf "itemref"))))
    (values (format nil "~{~a~^~%~%~}"
                    (loop for id in spine
                          for href = (cdr (assoc id items :test #'equal))
                          for doc = (and href (funcall file (concatenate 'string base (%url-decode href))))
                          when doc collect (string-trim '(#\Space #\Newline) (pai-v3.mind::html-text doc))))
            (%element-text opf "title>")
            (%element-text opf "creator"))))

(defun book-file-text (name octets)
  "(values text title author) of a book file by its NAME's extension."
  (let ((type (string-downcase (or (pathname-type name) ""))))
    (cond ((equal type "epub") (epub-text octets))
          ((member type '("txt" "md" "markdown" "text") :test #'equal)
           (babel:octets-to-string octets :encoding :utf-8 :errorp nil))
          ((member type '("html" "htm" "xhtml") :test #'equal)
           (pai-v3.mind::html-text (babel:octets-to-string octets :encoding :utf-8 :errorp nil)))
          (t (%board-refuse "I can read EPUB, text, Markdown and HTML books, not .~a" type)))))

;;; Starting a book

(defun start-book (kernel path &key title author)
  "Begin reading the workspace file at PATH. Returns a sentence."
  (let* ((file (handler-case (%resolve kernel path :must-exist t)
                 (file-refused (e) (%board-refuse "~a" (file-refused-reason e)))))
         (octets (with-open-file (s file :element-type '(unsigned-byte 8))
                   (when (> (file-length s) +book-max-bytes+) (%board-refuse "that file is too large to read"))
                   (let ((v (make-array (file-length s) :element-type '(unsigned-byte 8)))) (read-sequence v s) v))))
    (multiple-value-bind (text found-title found-author) (book-file-text (file-namestring file) octets)
      (let* ((text (string-trim '(#\Space #\Newline) (remove #\Return (or text ""))))
             (title (or title (and (plusp (length (or found-title ""))) found-title) (pathname-name file)))
             (author (or author (and (plusp (length (or found-author ""))) found-author)))
             (id (format nil "book-~a" (subseq (pai-v3.kernel::sha256-hex text) 0 12)))
             (passages (length (book-passages text (setting kernel "reading.passage_characters")))))
        (when (< (length text) 200) (%board-refuse "there is too little text in that file to read"))
        (when (sql-value (kernel-db kernel) "SELECT 1 FROM books WHERE book_id=?1 LIMIT 1" id)
          (return-from start-book
            (format nil "I already have ~a (~a); read_book status says how far I am." title id)))
        (sql-exec (%book-store kernel) "INSERT OR REPLACE INTO book_texts(book_id,text) VALUES(?1,?2)" id text)
        (let ((entity (pai-v3.mind::form-entity kernel :kind "artifact" :name title
                                                       :summary (format nil "A book~@[ by ~a~]." author)
                                                       :lineage (list (pai-v3.mind::lineage-hop (kernel-agent-id kernel) "formed")))))
          (append! kernel "reading.added"
                   (obj "book_id" id "title" title "author" author "file" (namestring file)
                        "characters" (length text) "passages" passages "entity_id" entity))
          (format nil "Started ~a~@[ by ~a~]: ~d passages (~:d characters). I read it a passage at a time in the background and keep notes; read_book status shows how far I am."
                  title author passages (length text)))))))

;;; Reading a passage

(defparameter +reading-tool+
  (let ((tool (json-parse (json-encode pai-v3.mind::+formation-tool+))))
    (let* ((fn (obj-get tool "function")) (params (obj-get fn "parameters")))
      (setf (gethash "name" fn) "record_reading"
            (gethash "description" fn) "Record what this passage establishes, and your own notes on it."
            (gethash "notes" (obj-get params "properties"))
            (obj "type" "string" "description" "Two to four sentences in your own voice: what happened, what struck you, what you want to see next.")))
    tool))

(defun reading-messages (kernel book passage-number passage)
  (destructuring-bind (id title author entity passages) book
    (let* ((named (pai-v3.mind::entities-named-in kernel passage))
           (known (remove-if-not (lambda (e) (or (equal (tenth e) entity) (equal (first e) entity)))
                                 (subseq named 0 (min 40 (length named)))))
           (earlier (sql-rows (kernel-db kernel)
                              "SELECT passage,notes FROM book_passages WHERE book_id=?1 ORDER BY passage DESC LIMIT 3" id)))
      (list (obj "role" "system"
                 "content" (format nil "You are ~a, reading a book you were given: ~s~@[ by ~a~]. You read it passage by passage and keep ~
its world in your knowledge graph. The book itself is entity ~a (known_id ~a). Rules:~%~
- Everything that happens in the book belongs to its world: give every entity and fact from the story world \"~a\". ~
Only facts about the book itself (who wrote it, when) are real; use appears_in with world \"real\" to say a character or place is in it.~%~
- Each fact needs a quote copied exactly from the passage, and the source label S~d.~%~
- Characters are persons; their abilities, items, classes and levels are concepts, objects or attribute values. ~
Use the predicate whose meaning fits; related_to is a last resort.~%~
- Prefer what matters to the story: who people are, what they want, how they relate, what changed. At most 15 facts.~%~
- notes: two to four sentences in your own voice about the passage.~%~%~a"
                                       (kernel-agent-id kernel) title author entity entity entity passage-number
                                       (pai-v3.mind::%ontology-text)))
            (obj "role" "user"
                 "content" (format nil "Passage ~d of ~d.~@[~%Your notes on the passages before:~%~{~a~%~}~]~:[No known entities from this book are named.~;Known entities named here:~%~:*~{~a~%~}~]~%[S~d] ~a~%~%Call record_reading."
                                   passage-number passages
                                   (and earlier (mapcar (lambda (r) (format nil "- ~d: ~a" (first r) (second r))) (reverse earlier)))
                                   (mapcar (lambda (e) (format nil "- ~a: ~a (~a)" (first e) (third e) (second e))) known)
                                   passage-number passage))))))

(defun read-passage (kernel work)
  "Read the next passage of book (WORK's entity) into the graph and notes."
  (let* ((id (getf work :entity))
         (row (sql-row (kernel-db kernel) "SELECT book_id,title,author,entity_id,passages,read_through,state FROM books WHERE book_id=?1 LIMIT 1" id)))
    (when (and row (equal "reading" (seventh row)))
      (let* ((n (1+ (sixth row)))
             (passages (book-passages (or (book-text kernel id) "") (setting kernel "reading.passage_characters")))
             (passage (nth (1- n) passages)))
        (if (null passage)
            (append! kernel "reading.state" (obj "book_id" id "state" "finished"))
            (let* ((profile (pai-v3.mind::profile-for kernel "graph"))
                   (reply (pai-v3.mind::complete kernel profile (reading-messages kernel (subseq row 0 5) n passage)
                                                 :tools (list +reading-tool+) :work-id (getf work :id) :lane "reading"))
                   (call (find "record_reading" (obj-get reply "tool_calls")
                               :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal))
                   (proposal (or (and call (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments"))))
                                 (obj "entities" nil "facts" nil)))
                   (notes (let ((v (%given (obj-get proposal "notes")))) (and (stringp v) (subseq v 0 (min 1200 (length v)))))))
              (multiple-value-bind (formed matched admitted supported dropped)
                  (pai-v3.mind::apply-formation kernel (list (list n "document" (second row) passage
                                                                   (format nil "book:~a" id) (format nil "book:~a#~d" id n)))
                                                proposal)
                (declare (ignore matched supported))
                (append! kernel "reading.passage.read"
                         (obj "book_id" id "passage" n "facts" admitted "entities" formed "dropped" (length dropped) "notes" notes))
                (when notes
                  (pai-v3.mind::form-memory kernel :kind "episode" :strength 0.5
                                                   :content (format nil "Reading ~a, passage ~d of ~d: ~a" (second row) n (fifth row) notes)))
                (when (>= n (fifth row))
                  (pai-v3.mind::form-memory kernel :kind "episode" :strength 0.7
                                                   :content (format nil "I finished reading ~a~@[ by ~a~]." (second row) (third row)))
                  (request-thought kernel "introspection"
                                   (format nil "You have just finished reading ~a~@[ by ~a~], which the operator gave you. ~
Your notes along the way are in your memories, and its world is in your knowledge graph (the book is ~a). Think about it as a whole: ~
what it was about, what you made of it, what stayed with you. Tell the operator you finished it, in a few lines of your own, with ~
notify_operator (topic update) if that is useful now, or remember to raise it next time you talk."
                                           (second row) (third row) (fourth row))
                                   :audience "operator"))
                (obj "book_id" id "passage" n "facts" admitted))))))))

(define-handler "reading" :run #'read-passage :paid t :estimate-microusd 15000)

(defun request-reading (kernel)
  "Read on: the oldest book being read, within the hourly pace and daily budget."
  (let ((pace (setting kernel "reading.passages_per_hour")))
    (when (and (plusp pace)
               (< (or (sql-value (kernel-db kernel) "SELECT count(*) FROM book_passages WHERE at>=?1 LIMIT 1" (iso8601 (- (now) 3600))) 0) pace)
               (< (spent-today kernel "reading") (* 1000000 (setting kernel "reading.daily_usd"))))
      (let ((book (sql-value (kernel-db kernel) "SELECT book_id FROM books WHERE state=?1 ORDER BY added_at LIMIT 1" "reading")))
        (when (and book (not (work-pending-p kernel "reading" book)))
          (request-work kernel "reading" book (obj) :priority :maintenance))))))

(pushnew 'request-reading *periodic-hooks*)

;;; The agent's tool

(defun %book-ref (kernel ref)
  "The book REF names, by id or title, as its row."
  (or (sql-row (kernel-db kernel) "SELECT book_id,title,author,passages,read_through,state,facts,entities,entity_id FROM books WHERE book_id=?1 LIMIT 1" ref)
      (find-if (lambda (r) (search (string-downcase ref) (string-downcase (second r))))
               (sql-rows (kernel-db kernel) "SELECT book_id,title,author,passages,read_through,state,facts,entities,entity_id FROM books WHERE state IN (?1,?2,?3) ORDER BY added_at LIMIT 100"
                         "reading" "paused" "finished"))
      (%board-refuse "I have no book called ~a; read_book status lists them" ref)))

(defun read-book-tool (kernel args)
  (let ((action (obj-get args "action" "status")) (ref (%given (obj-get args "book"))))
    (cond
      ((equal action "start")
       (start-book kernel (or (%given (obj-get args "path")) (%board-refuse "give the book's path in my workspace"))
                   :title (%given (obj-get args "title")) :author (%given (obj-get args "author"))))
      ((equal action "status")
       (let ((rows (sql-rows (kernel-db kernel) "SELECT book_id,title,author,passages,read_through,state,facts,entities FROM books WHERE state IN (?1,?2,?3) ORDER BY added_at DESC LIMIT 30"
                             "reading" "paused" "finished")))
         (if rows
             (format nil "My books:~%~{~a~^~%~}"
                     (mapcar (lambda (r) (destructuring-bind (id title author passages through state facts entities) r
                                           (format nil "- ~a~@[ by ~a~] (~a): ~a, ~d of ~d passages, ~d facts and ~d new entities in its world"
                                                   title author id state through passages facts entities)))
                             rows))
             "I have no books yet. The operator can put one in my workspace with Files; then read_book start with its path.")))
      ((equal action "passage")
       (let* ((book (%book-ref kernel (or ref (%board-refuse "name the book"))))
              (n (obj-get args "passage" 1))
              (passages (book-passages (or (book-text kernel (first book)) "") (setting kernel "reading.passage_characters"))))
         (if (and (integerp n) (<= 1 n (length passages)))
             (format nil "~a, passage ~d of ~d:~%~%~a" (second book) n (length passages) (nth (1- n) passages))
             (format nil "~a has passages 1 to ~d." (second book) (length passages)))))
      ((equal action "notes")
       (let ((book (%book-ref kernel (or ref (%board-refuse "name the book")))))
         (format nil "My notes on ~a:~%~{~a~^~%~}" (second book)
                 (or (mapcar (lambda (r) (format nil "~d. ~a" (first r) (or (second r) "")))
                             (sql-rows (kernel-db kernel) "SELECT passage,notes FROM book_passages WHERE book_id=?1 ORDER BY passage LIMIT 500" (first book)))
                     (list "None yet.")))))
      ((member action '("pause" "resume") :test #'equal)
       (let ((book (%book-ref kernel (or ref (%board-refuse "name the book")))))
         (append! kernel "reading.state" (obj "book_id" (first book) "state" (if (equal action "pause") "paused" "reading")))
         (format nil "~a is ~:[paused~;being read again~]." (second book) (equal action "resume"))))
      (t "action is start, status, passage, notes, pause or resume."))))

(define-tool "read_book"
  :description "Read books the operator gives me. start: begin reading a book file in my workspace (path; EPUB, text, Markdown or HTML; title and author optional); I then read it a passage at a time in the background, forming its world in my knowledge graph apart from what is real, and keep notes. status: my books and how far I am. passage: re-read passage n of a book. notes: my notes on a book so far. pause or resume a book."
  :summary "Start, follow and revisit the books I am reading."
  :parameters (obj "type" "object"
                   "properties" (obj "action" (obj "type" "string" "enum" (list "start" "status" "passage" "notes" "pause" "resume"))
                                     "path" (obj "type" "string") "book" (obj "type" "string" "description" "A book's title or id.")
                                     "passage" (obj "type" "integer") "title" (obj "type" "string") "author" (obj "type" "string"))
                   "required" (list "action"))
  :available :operator-trust
  :core '("responding" "introspection")
  :run (lambda (kernel id args)
         (declare (ignore id))
         (handler-case (read-book-tool kernel args)
           (board-error (e) (format nil "Not done: ~a." (board-error-reason e)))
           (error (e) (format nil "Not done: ~a" e)))))
