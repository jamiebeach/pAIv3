(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.api)

;;; Reading a book (bead v3-clp.21): a file from the workspace, read a passage
;;; at a time into the book's own world in the graph, with notes kept.

(defparameter *operator* (obj "principal_type" "operator" "auth_method" "passkey"))

(defun stored-zip (files)
  "A zip archive of FILES, an alist of name -> string, stored without compression."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (central '()))
    (labels ((u16 (v) (vector-push-extend (ldb (byte 8 0) v) out) (vector-push-extend (ldb (byte 8 8) v) out))
             (u32 (v) (u16 (ldb (byte 16 0) v)) (u16 (ldb (byte 16 16) v)))
             (bytes (v) (loop for b across v do (vector-push-extend b out))))
      (loop for (name . text) in files
            for data = (babel:string-to-octets text :encoding :utf-8)
            for nm = (babel:string-to-octets name :encoding :utf-8)
            for offset = (fill-pointer out)
            do (u32 #x04034b50) (u16 20) (u16 0) (u16 0) (u16 0) (u16 0) (u32 0)
               (u32 (length data)) (u32 (length data)) (u16 (length nm)) (u16 0) (bytes nm) (bytes data)
               (push (list nm (length data) offset) central))
      (let ((cd (fill-pointer out)))
        (dolist (c (reverse central))
          (destructuring-bind (nm size offset) c
            (u32 #x02014b50) (u16 20) (u16 20) (u16 0) (u16 0) (u16 0) (u16 0) (u32 0)
            (u32 size) (u32 size) (u16 (length nm)) (u16 0) (u16 0) (u16 0) (u16 0) (u32 0) (u32 offset) (bytes nm)))
        (let ((cd-size (- (fill-pointer out) cd)))
          (u32 #x06054b50) (u16 0) (u16 0) (u16 (length central)) (u16 (length central)) (u32 cd-size) (u32 cd) (u16 0))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defparameter *epub*
  (stored-zip
   (list (cons "mimetype" "application/epub+zip")
         (cons "META-INF/container.xml" "<?xml version=\"1.0\"?><container><rootfiles><rootfile full-path=\"OEBPS/content.opf\" media-type=\"application/oebps-package+xml\"/></rootfiles></container>")
         (cons "OEBPS/content.opf" "<package><metadata><dc:title>SYNTHETIC Neon Road</dc:title><dc:creator>Sam Writer</dc:creator></metadata><manifest><item id=\"c2\" href=\"ch2.xhtml\" media-type=\"application/xhtml+xml\"/><item id=\"c1\" href=\"Text/ch%201.xhtml\" media-type=\"application/xhtml+xml\"/></manifest><spine><itemref idref=\"c1\"/><itemref idref=\"c2\"/></spine></package>")
         (cons "OEBPS/Text/ch 1.xhtml" "<html><body><h1>One</h1><p>Mira Vale hacked the gate of Lowtown.</p></body></html>")
         (cons "OEBPS/ch2.xhtml" "<html><body><h1>Two</h1><p>Kade was Mira Vale's brother.</p></body></html>"))))

(deftest an-epub-gives-its-text-in-spine-order-with-title-and-author
  (multiple-value-bind (text title author) (pai-v3.api::epub-text *epub*)
    (is (equal "SYNTHETIC Neon Road" title))
    (is (equal "Sam Writer" author))
    (is (< (search "hacked the gate" text) (search "brother" text)) "chapter one first, as the spine says")
    (is (null (search "<p>" text)) "as text, not markup")))

(deftest a-damaged-or-oddly-written-epub-still-gives-its-text
  ;; A broken image, names written with ./ and backslashes, no container: the prose still comes through.
  (let* ((good (pai-v3.api::zip-entries *epub*))
         (odd (stored-zip
               (list (cons "mimetype" "application/epub+zip")
                     (cons "./OEBPS\\content.opf" (map 'string #'code-char (cdr (assoc "OEBPS/content.opf" good :test #'equal))))
                     (cons "OEBPS/Text/ch 1.xhtml" "<html><body><p>Mira Vale hacked the gate of Lowtown.</p></body></html>")
                     (cons "OEBPS/ch2.xhtml" "<html><body><p>Kade was Mira Vale's brother.</p></body></html>")))))
    (is (null (pai-v3.api::%inflate (make-array 5 :element-type '(unsigned-byte 8) :initial-contents '(120 156 99 0 1))))
        "a truncated entry is NIL, not an error")
    (is (search "brother" (pai-v3.api::epub-text odd)))))

(deftest a-book-is-cut-into-passages-at-paragraph-ends
  (let* ((para (format nil "~a~%~%" (make-string 900 :initial-element #\a)))
         (text (format nil "~{~a~}" (loop repeat 10 collect para)))
         (passages (pai-v3.api::book-passages text 2000)))
    (is (= 5 (length passages)))
    (is (every (lambda (p) (<= (length p) 2000)) passages))
    (is (equal (remove #\Newline text) (remove #\Newline (format nil "~{~a~}" passages))) "nothing is lost")))

(defun reading-provider (k)
  "A provider that answers every reading call with one fact from the passage."
  (declare (ignore k))
  (make-provider-profile
   :name "reading-test" :base-url "http://fake" :model "m"
   :transport (lambda (url headers body &key timeout stream-fn)
                (declare (ignore url headers timeout stream-fn))
                (let* ((user (obj-get (second (obj-get (json-parse body) "messages")) "content"))
                       (label (let ((at (search "[S" user))) (subseq user (1+ at) (position #\] user :start at))))
                       (args (if (search "brother" user)
                                 (obj "entities" (list (obj "ref" "e1" "name" "Kade" "kind" "person" "world" "BOOK")
                                                       (obj "ref" "e2" "name" "Mira Vale" "kind" "person" "world" "BOOK"))
                                      "facts" (list (obj "subject" "e1" "predicate" "sibling_of" "object" "e2" "world" "BOOK"
                                                         "statement" "Kade is Mira Vale's brother." "source" label
                                                         "quote" "Kade was Mira Vale's brother"))
                                      "notes" "SYNTHETIC-NOTE Kade turns up.")
                                 (obj "entities" '() "facts" '() "notes" "SYNTHETIC-NOTE Mira breaks in."))))
                  ;; The book's world is its entity id, given in the system prompt.
                  (let* ((system (obj-get (first (obj-get (json-parse body) "messages")) "content"))
                         (at (search "known_id " system))
                         (book (subseq system (+ at 9) (position #\) system :start at))))
                    (values 200 nil
                            (pai-v3.kernel::json-encode
                             (obj "choices" (list (obj "message" (obj "content" ""
                                                                      "tool_calls" (list (obj "id" "c1" "type" "function"
                                                                                              "function" (obj "name" "record_reading"
                                                                                                              "arguments" (substitute-book (json-encode args) book)))))
                                                       "finish_reason" "stop"))
                                  "usage" (obj "prompt_tokens" 10 "completion_tokens" 10 "cost" 0.0001)))))))))

(defun substitute-book (json book)
  (let ((out json))
    (loop for at = (search "\"BOOK\"" out) while at
          do (setf out (concatenate 'string (subseq out 0 at) "\"" book "\"" (subseq out (+ at 6)))))
    out))

(deftest a-book-from-the-workspace-is-read-into-its-own-world
  (with-temp-kernel (k)
    (let ((root (format nil "/tmp/ws-read-~d/" (random 1000000000))))
      (unwind-protect
           (progn
             (change-setting k "shell.workspace" root :origin *operator*)
             (change-setting k "reading.passage_characters" 1500 :origin *operator*)
             (ensure-directories-exist (format nil "~abooks/" root))
             (with-open-file (o (format nil "~abooks/neon.epub" root) :direction :output :element-type '(unsigned-byte 8))
               (write-sequence *epub* o))
             (flet ((tool (&rest args) (funcall (pai-v3.mind::tool-run (gethash "read_book" pai-v3.mind::*tools*)) k 1 (apply #'obj args))))
               (is (search "too little text" (tool "action" "start" "path" "books/neon.epub")) "a tiny book is refused")
               ;; A longer edition: the same chapters with more around them.
               (with-open-file (o (format nil "~abooks/neon.txt" root) :direction :output :external-format :utf-8)
                 (format o "Mira Vale hacked the gate of Lowtown.~%~%~a~%~%Kade was Mira Vale's brother.~%" (make-string 2000 :initial-element #\z)))
               (is (search "Started neon" (tool "action" "start" "path" "books/neon.txt")))
               (is (search "already have" (tool "action" "start" "path" "books/neon.txt")) "once")
               (is (search "Not done" (tool "action" "start" "path" "../etc/passwd")) "only inside the workspace")
               (let* ((book (first (first (sql-rows (kernel-db k) "SELECT book_id FROM books WHERE state=?1 LIMIT 1" "reading"))))
                      (entity (sql-value (kernel-db k) "SELECT entity_id FROM books WHERE book_id=?1 LIMIT 1" book))
                      (pai-v3.mind::*thought-provider* (reading-provider k)))
                 (is (search "0 of 2" (tool "action" "status")))
                 (pai-v3.api::read-passage k (list :entity book :id nil))
                 (pai-v3.api::read-passage k (list :entity book :id nil))
                 (is (search "finished, 2 of 2" (tool "action" "status")))
                 (is (equal entity (sql-value (kernel-db k) "SELECT world FROM graph_facts WHERE text=?1 LIMIT 1" "Kade is Mira Vale's brother."))
                     "the story's facts are in the book's world")
                 (is (equal "document" (sql-value (kernel-db k) "SELECT kind FROM graph_evidence WHERE source_ref LIKE ?1 LIMIT 1" "book:%"))
                     "with the passage as evidence")
                 (is (search "SYNTHETIC-NOTE Kade" (tool "action" "notes" "book" "neon")))
                 (is (search "Kade was Mira" (tool "action" "passage" "book" "neon" "passage" 2)))
                 (is (sql-value (kernel-db k) "SELECT 1 FROM memory_nodes WHERE content LIKE ?1 LIMIT 1" "%finished reading neon%")
                     "the finish is remembered")
                 (is (sql-value (kernel-db k) "SELECT 1 FROM thoughts WHERE reason LIKE ?1 LIMIT 1" "%just finished reading neon%")
                     "and thinks it over, to tell the operator"))))
        (uiop:delete-directory-tree (uiop:ensure-directory-pathname root) :validate t :if-does-not-exist :ignore)))))

(run-suite "api-reading")
