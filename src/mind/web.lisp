(in-package #:pai-v3.mind)

;;; Reaching the web: web_fetch and brave_search (spec §16.10).
;;;
;;; Only http and https. Hosts that resolve to loopback, private or
;;; link-local addresses are refused, so a page or a person cannot steer the
;;; agent at the host or its network. Results are text; a long page is
;;; paged by the thought, never cut.

(defvar *fetch-allow-private* nil "Allow private addresses (tests only).")

(define-setting "net.egress_proxy"
  :type :string :default "" :group "Web"
  :description "The egress proxy every public request and shell command leaves through, such as http://specter@host.docker.internal:3128 (the name before @ says who is asking). Empty: direct.")

(defvar *http-get*
  (lambda (url headers)
    (multiple-value-bind (body status response-headers)
        (http-request :get url :headers headers :read-timeout 20 :allow-private *fetch-allow-private*)
      (values body status (or (and response-headers (gethash "content-type" response-headers)) "") response-headers)))
  "(lambda (url headers)) -> body, status, content-type and the response
headers (a hash table, or NIL), through the kernel's one outbound transport.
Replaced in tests.")


(defparameter *fetch-max-chars* 400000)

(defun %check-url (url)
  "Refuse early, with the transport's own rule, before anything is sent."
  (check-outbound-url url :allow-private *fetch-allow-private*
                          :proxied (and (not *fetch-allow-private*) (pai-v3.kernel::%egress-proxy)))
  url)

;;; HTML to text

(defun %decode-entities (text)
  (let ((pairs '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<") ("&gt;" . ">")
                 ("&quot;" . "\"") ("&#39;" . "'") ("&apos;" . "'") ("&mdash;" . "—") ("&ndash;" . "–"))))
    (dolist (pair pairs text)
      (loop for at = (search (car pair) text)
            while at
            do (setf text (concatenate 'string (subseq text 0 at) (cdr pair)
                                       (subseq text (+ at (length (car pair))))))))))

(defun %tag-at (html open from)
  "The position of OPEN (\"<tag\") in HTML at or after FROM where the tag name
ends there, so <head is not found inside <header (bead v3-33j: a page with a
<header> lost its whole body, dropped as if it were the head)."
  (loop for at = (search open html :start2 from :test #'char-equal)
        while at
        do (let ((next (+ at (length open))))
             (when (or (>= next (length html))
                       (member (char html next) '(#\Space #\Tab #\Newline #\Return #\> #\/)))
               (return at))
             (setf from next))))

(defun %drop-elements (html tag)
  "HTML without any <TAG ...>...</TAG> element."
  (let ((out (make-string-output-stream)) (start 0)
        (open (format nil "<~a" tag)) (close (format nil "</~a>" tag)))
    (loop for at = (%tag-at html open start)
          while at
          do (write-string html out :start start :end at)
             (let ((end (search close html :start2 at :test #'char-equal)))
               (setf start (if end (+ end (length close)) (length html))))
          finally (write-string html out :start start))
    (get-output-stream-string out)))

(defparameter +block-tags+ '("p" "div" "br" "li" "tr" "h1" "h2" "h3" "h4" "h5" "h6" "section" "article" "pre"))

(defun html-text (html)
  "Readable text of HTML: no scripts, styles or tags; blocks become lines."
  (let* ((html (reduce #'%drop-elements '("script" "style" "noscript" "svg" "head") :initial-value html))
         (out (make-string-output-stream)) (i 0) (n (length html)))
    (loop while (< i n)
          do (let ((c (char html i)))
               (if (char= c #\<)
                   (let* ((end (or (position #\> html :start i) n))
                          (tag (string-downcase (string-trim "/ " (subseq html (1+ i) (min end (or (position-if (lambda (ch) (member ch '(#\Space #\>))) html :start (1+ i)) end)))))))
                     (when (member tag +block-tags+ :test #'string=) (write-char #\Newline out))
                     (setf i (1+ end)))
                   (progn (write-char c out) (incf i)))))
    (let ((lines (loop for line in (%split-lines (%decode-entities (get-output-stream-string out)))
                       for trimmed = (string-trim '(#\Space #\Tab #\Return) (%squeeze line))
                       when (plusp (length trimmed)) collect trimmed)))
      (format nil "~{~a~^~%~}" lines))))

(defun %split-lines (text)
  (loop with start = 0
        for end = (position #\Newline text :start start)
        collect (subseq text start (or end (length text)))
        while end do (setf start (1+ end))))

(defun %squeeze (line)
  (with-output-to-string (s)
    (let ((space nil))
      (loop for c across line
            do (if (member c '(#\Space #\Tab #\Return))
                   (setf space t)
                   (progn (when space (write-char #\Space s)) (setf space nil) (write-char c s)))))))

(defun %title (html)
  (let* ((start (search "<title" html :test #'char-equal))
         (open-end (and start (position #\> html :start start)))
         (end (and open-end (search "</title>" html :start2 open-end :test #'char-equal))))
    (and end (string-trim '(#\Space #\Newline #\Tab #\Return) (%decode-entities (subseq html (1+ open-end) end))))))


;;; Manners (the operator and an agent, 2026-11-04): what looks like abuse from a host's
;;; side is volume, retrying after a refusal, and routing around one. So every
;;; fetch is recorded; a host is read at most web.fetches_per_host_hour times an
;;; hour; and a host that refuses (401, 403, 429) is not asked again for a day,
;;; by any route. A URL carried inside another (a reading proxy such as
;;; r.jina.ai/https://example.com) counts against both hosts.

(define-setting "web.fetches_per_host_hour"
  :type :integer :min 1 :max 500 :default 20 :group "Web"
  :description "Most web_fetch calls to one host in an hour.")

(defparameter +refusal-statuses+ '(401 403 429) "Answers that mean: this host does not want me here.")
(defparameter *refusal-hold-hours* 24)

(define-event-type "web.fetched"
  :schema (("url" :string) ("hosts" :array) ("status" :integer) ("thought_id" :integer :optional)
           ("refused_here" :string :optional)
           ;; What came of it (2026-11-06): text, unreadable, failed, refused or error,
           ;; and how many characters of text arrived, so "I read it" and "I tried and
           ;; it broke" stay apart in my own history (an agent).
           ("outcome" :string :optional) ("chars" :integer :optional)))

(define-projection "web_fetches"
  :kind :inline
  :consumes ("web.fetched")
  :tables ("CREATE TABLE IF NOT EXISTS web_fetches (event_id INTEGER NOT NULL, host TEXT NOT NULL, url TEXT NOT NULL, status INTEGER NOT NULL, thought_id INTEGER, refused_here TEXT, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS web_fetches_host_idx ON web_fetches(host,at)"
           "CREATE INDEX IF NOT EXISTS web_fetches_at_idx ON web_fetches(at)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (dolist (host (obj-get p "hosts"))
                (sql-exec db "INSERT INTO web_fetches(event_id,host,url,status,thought_id,refused_here,at) VALUES(?1,?2,?3,?4,?5,?6,?7)"
                          (event-id e) host (obj-get p "url") (obj-get p "status") (obj-get p "thought_id")
                          (obj-get p "refused_here") (event-at e))))))

(defun %url-host (url)
  (let* ((start (search "://" url))
         (rest (and start (subseq url (+ 3 start))))
         (end (and rest (position-if (lambda (c) (member c '(#\/ #\? #\# #\:))) rest))))
    (and rest (plusp (length rest)) (string-downcase (subseq rest 0 (or end (length rest)))))))

(defun fetch-hosts (url)
  "Every host URL reaches: its own, and any carried inside it, by a proxy."
  (let ((hosts '()) (start 0))
    (loop for at = (search "http" url :start2 start :test #'char-equal)
          while at
          do (let ((host (and (or (zerop at) (not (alpha-char-p (char url (1- at)))))
                              (%url-host (subseq url at)))))
               (when host (pushnew host hosts :test #'string=)))
             (setf start (1+ at)))
    (nreverse hosts)))

(defun %fetch-refusal (kernel hosts)
  "Why a fetch reaching HOSTS must not be sent now, or NIL."
  (let ((db (kernel-db kernel))
        (hour-ago (iso8601 (- (now) 3600)))
        (hold-from (iso8601 (- (now) (* 3600 *refusal-hold-hours*)))))
    (dolist (host hosts)
      (let ((refused (sql-row db (format nil "SELECT status,at FROM web_fetches WHERE host=?1 AND at>=?2 AND status IN (~{~d~^,~}) ORDER BY at DESC LIMIT 1"
                                         +refusal-statuses+)
                              host hold-from)))
        (when refused
          (return (format nil "Not fetched: ~a refused me (~d) at ~a. I do not try it again, by any route, for ~d hours after a refusal."
                          host (first refused) (or (ignore-errors (local-stamp kernel (second refused))) (second refused))
                          *refusal-hold-hours*))))
      (let ((count (or (sql-value db "SELECT count(*) FROM web_fetches WHERE host=?1 AND at>=?2 AND status>0 LIMIT 1" host hour-ago) 0)))
        (when (>= count (setting kernel "web.fetches_per_host_hour"))
          (return (format nil "Not fetched: I have read ~a ~d times in the last hour, its limit. Work with what I have, or come back to it later."
                          host count)))))))

(defun web-fetch (url &optional kernel thought-id)
  "The readable text of URL. With KERNEL, the fetch is checked against the
host's limits first and recorded after."
  (handler-case
      (progn
        (%check-url url)
        (let ((hosts (fetch-hosts url)))
          (when kernel
            (let ((refusal (%fetch-refusal kernel hosts)))
              (when refusal
                (append! kernel "web.fetched" (obj "url" (subseq url 0 (min 500 (length url))) "hosts" hosts "status" 0
                                                   "thought_id" thought-id "refused_here" (subseq refusal 0 (min 300 (length refusal)))))
                (return-from web-fetch refusal))))
        (multiple-value-bind (body status content-type)
            (funcall *http-get* url '(("User-Agent" . "pAI/3 (personal assistant)")))
          (let* ((body (if (> (length body) *fetch-max-chars*) (subseq body 0 *fetch-max-chars*) body))
                 (text (cond ((not (<= 200 status 299)) nil)
                             ((search "html" content-type) (html-text body))
                             ((or (search "text" content-type) (search "json" content-type) (search "xml" content-type)) body)))
                 (outcome (cond ((not (<= 200 status 299)) "failed") (text "text") (t "unreadable"))))
            (when kernel
              (append! kernel "web.fetched" (obj "url" (subseq url 0 (min 500 (length url))) "hosts" hosts "status" (or status 0)
                                                 "thought_id" thought-id "outcome" outcome "chars" (length (or text "")))))
            (cond ((not (<= 200 status 299)) (format nil "~a answered ~d." url status))
                  ((search "html" content-type) (format nil "~@[~a~%~]~a~%~%~a" (%title body) url text))
                  (text (format nil "~a~%~%~a" url text))
                  (t (format nil "~a is ~a, which I cannot read as text." url content-type)))))))
    (egress-refused (e) (egress-reason e))
    (error (e)
      (when kernel
        (ignore-errors (append! kernel "web.fetched" (obj "url" (subseq url 0 (min 500 (length url))) "hosts" (or (ignore-errors (fetch-hosts url)) '())
                                                          "status" 0 "thought_id" thought-id "outcome" "error"))))
      (format nil "Could not fetch ~a: ~a" url e))))

(define-tool "web_fetch"
  :description "Read a web page as text. Long pages come back in parts. Each site has an hourly limit, and a site that refuses me is left alone for a day, by any route: I never retry around a refusal."
  :parameters (obj "type" "object"
                   "properties" (obj "url" (obj "type" "string"))
                   "required" (list "url"))
  :run (lambda (kernel id args) (web-fetch (obj-get args "url" "") kernel id)))

;;; Brave search

(defun %strip-tags (text)
  (if (stringp text) (%decode-entities (html-text text)) ""))

(define-setting "web.searches_per_thought"
  :type :integer :min 0 :max 50 :default 4 :group "Web"
  :description "Most brave_search calls one thought may make; past it, the agent is told to read what it has with web_fetch. Searches cost credits; reading pages does not.")
(define-setting "web.searches_per_free_time"
  :type :integer :min 0 :max 50 :default 2 :group "Web"
  :description "Most brave_search calls one free-time thought may make.")

(define-event-type "web.searched"
  :schema (("query" :string) ("status" :integer) ("thought_id" :integer :optional)
           ("remaining" :string :optional) ("limit" :string :optional)))

(defun %searches-left (kernel thought-id)
  "How many more searches THOUGHT-ID may make, by its kind's cap."
  (let* ((db (kernel-db kernel))
         (reason (or (sql-value db "SELECT reason FROM thoughts WHERE id=?1 LIMIT 1" thought-id) ""))
         (cap (setting kernel (if (uiop:string-prefix-p "Free time." reason) "web.searches_per_free_time" "web.searches_per_thought")))
         (done (or (sql-value db "SELECT count(*) FROM thought_steps WHERE thought_id=?1 AND kind=?2 AND tool=?3 LIMIT 1"
                              thought-id "tool_result" "brave_search") 0)))
    (- cap done)))

(defun brave-search (query &key (count 5) kernel thought-id)
  "Web results for QUERY, numbered, with title, address and snippet. The key
is the secret named brave, bound to api.search.brave.com. With KERNEL, each
search is recorded with what Brave says is left of the quota."
  (handler-case
      (multiple-value-bind (body status type headers)
          (funcall *http-get*
                   (format nil "https://api.search.brave.com/res/v1/web/search?q=~a&count=~d"
                           (quri:url-encode query) (max 1 (min 20 count)))
                   (list (cons "Accept" "application/json")
                         (cons "X-Subscription-Token" (list :secret "brave" "~a"))))
        (declare (ignore type))
        (when kernel
          (let ((event (obj "query" (subseq query 0 (min 300 (length query))) "status" status))
                (remaining (and (hash-table-p headers) (gethash "x-ratelimit-remaining" headers)))
                (limit (and (hash-table-p headers) (gethash "x-ratelimit-limit" headers))))
            (when thought-id (setf (gethash "thought_id" event) thought-id))
            (when (stringp remaining) (setf (gethash "remaining" event) remaining))
            (when (stringp limit) (setf (gethash "limit" event) limit))
            (ignore-errors (append! kernel "web.searched" event))))
        (cond ((= status 429) "Web search is rate-limited just now. Do not search again in this thought; read what you have with web_fetch, or come back to it later.")
              ((= status 402) "Web search is out of credits. Do not search again; tell the operator once if it matters, and read pages you already know with web_fetch.")
              ((not (<= 200 status 299)) (format nil "Web search answered ~d." status))
              (t (let ((results (obj-get (obj-get (json-parse body) "web" (obj)) "results")))
                   (if (null results)
                       (format nil "No results for ~s." query)
                       (format nil "~{~a~^~%~%~}"
                               (loop for r in results for i from 1
                                     collect (format nil "~d. ~a~%~a~%~a" i
                                                     (%strip-tags (obj-get r "title" ""))
                                                     (obj-get r "url" "")
                                                     (%strip-tags (obj-get r "description" ""))))))))))
    (egress-refused (e)
      (if (search "no secret named" (string-downcase (egress-reason e)))
          "Web search is not set up yet: the operator has not given me a Brave Search key."
          (egress-reason e)))
    (error (e) (format nil "Web search failed: ~a" e))))

(define-projection "web_history"
  ;; What I fetched and searched, one row each, for web_history and the
  ;; provenance check (2026-11-06). Only addresses and queries: no page text or
  ;; titles, which are outside content.
  :kind :inline
  :consumes ("web.fetched" "web.searched")
  :tables ("CREATE TABLE IF NOT EXISTS web_history (event_id INTEGER PRIMARY KEY, kind TEXT NOT NULL, target TEXT NOT NULL, status INTEGER NOT NULL, outcome TEXT, chars INTEGER, thought_id INTEGER, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS web_history_at_idx ON web_history(at)"
           "CREATE INDEX IF NOT EXISTS web_history_thought_idx ON web_history(thought_id)")
  :reduce (lambda (db e)
            (let* ((p (event-payload e)) (search-p (equal (event-type e) "web.searched")))
              (sql-exec db "INSERT OR IGNORE INTO web_history(event_id,kind,target,status,outcome,chars,thought_id,at) VALUES(?1,?2,?3,?4,?5,?6,?7,?8)"
                        (event-id e) (if search-p "search" "fetch") (obj-get p (if search-p "query" "url"))
                        (or (obj-get p "status") 0)
                        (or (obj-get p "outcome") (and (obj-get p "refused_here") "refused"))
                        (obj-get p "chars") (obj-get p "thought_id") (event-at e)))))

(defun web-history-start (kernel)
  "When my web history begins: the time of its first row, or NIL."
  (sql-value (kernel-db kernel) "SELECT at FROM web_history ORDER BY at LIMIT 1"))

(defun web-history-rows (kernel &key terms (days 30) (most 40))
  "My fetches and searches in the last DAYS, newest first, as (kind target status
outcome chars at); with TERMS, only those whose address or query holds one."
  (let ((terms (mapcar #'string-downcase (remove-if (lambda (s) (< (length s) 3)) terms))))
    (loop for r in (sql-rows (kernel-db kernel)
                             "SELECT kind,target,status,outcome,chars,at FROM web_history WHERE at>=?1 ORDER BY at DESC LIMIT 2000"
                             (iso8601 (- (now) (* days 86400))))
          when (or (null terms) (let ((target (string-downcase (second r)))) (some (lambda (term) (search term target)) terms)))
            collect r into found
          when (>= (length found) most) return found
          finally (return found))))

(defun thought-web-record (kernel thought-id)
  "What the thought THOUGHT-ID (with its subthoughts) did on the web, counted
and said by the system, never by the agent: how many fetches, how many brought
text, how the others ended, and how many searches. Attempting nothing reads
apart from failing (an agent, 2026-11-07). NIL for no thought."
  (when thought-id
    (let* ((db (kernel-db kernel))
           (root (or (sql-value db "SELECT root_id FROM thoughts WHERE id=?1 LIMIT 1" thought-id) thought-id))
           (rows (sql-rows db "SELECT kind,outcome,status FROM web_history WHERE thought_id IN (SELECT id FROM thoughts WHERE root_id=?1) LIMIT 500" root))
           (fetches (remove "search" rows :key #'first :test #'string=))
           (searches (- (length rows) (length fetches)))
           (text (count "text" fetches :key #'second :test #'equal))
           (others (remove "text" fetches :key #'second :test #'equal)))
      (let ((ended (mapcar (lambda (r) (destructuring-bind (kind outcome status) r
                                         (declare (ignore kind))
                                         (cond ((equal outcome "failed") (format nil "failed ~d" status))
                                               (outcome outcome)
                                               (t (format nil "status ~d" status)))))
                           others)))
        (format nil "~a; ~a"
                (if fetches
                    (format nil "~d fetch~:*~[es~;~:;es~], ~d bringing text~@[ (the rest: ~{~a~^, ~})~]"
                            (length fetches) text ended)
                    "it fetched no page")
                (if (plusp searches) (format nil "~d search~:*~[es~;~:;es~]" searches) "no search"))))))

(defun memory-record (kernel memory-id)
  "For a memory I chose to keep, the record of the thought that wrote it, to be
read before the note: the system's count of what that thought did, beside what
the note says it did (2026-11-07, the crew's memory-notes thread). NIL for a
memory that is not one I wrote."
  (let ((row (sql-row (kernel-db kernel) "SELECT owner,source_event_id,created_at,kind FROM memory_nodes WHERE id=?1 LIMIT 1" memory-id)))
    ;; Episodes are written up by the system from the conversation itself, and
    ;; say so in their origin: only a note I chose to keep carries a record.
    (when (and row (equal "self" (first row)) (not (equal "episode" (fourth row))))
      (let ((thought (second row)) (start (web-history-start kernel)))
        (cond ((or (null thought) (null (sql-value (kernel-db kernel) "SELECT 1 FROM thoughts WHERE id=?1 LIMIT 1" thought)))
               "[record: none; kept before notes recorded the thought that wrote them]")
              ((or (null start) (string< (third row) start))
               "[record: none; written before my web history begins]")
              (t (format nil "[record: the thought that wrote this note: ~a]" (thought-web-record kernel thought))))))))

(defun web-history-line (kernel row)
  "One row of my web history as a line, saying what came of it."
  (destructuring-bind (kind target status outcome chars at) row
    (format nil "~a — ~:[fetched~;searched~] ~:[~a~;\"~a\"~] — ~a"
            (or (ignore-errors (local-stamp kernel at)) at) (equal kind "search") (equal kind "search") target
            (cond ((equal kind "search") (if (<= 200 status 299) "results returned" (format nil "answered ~d" status)))
                  ((equal outcome "text") (format nil "text arrived (~:d characters)" (or chars 0)))
                  ((equal outcome "unreadable") "it arrived but could not be read as text")
                  ((equal outcome "failed") (format nil "the site answered ~d" status))
                  ((equal outcome "refused") "not fetched: refused under my web manners")
                  ((equal outcome "error") "the fetch failed")
                  (t (format nil "status ~d (what came of it was not recorded then)" status))))))

(define-tool "web_history"
  :description "My own record of the web pages I fetched and the searches I ran, newest first, with what came of each (text arrived and how much, unreadable, failed). Use it to check, before saying so, what I actually read, checked or found, and when. about: words or part of an address to match (empty: everything); days: how far back (default 30). A fetch shows a page arrived, not that I read all of it."
  :summary "Check what I actually fetched and searched, and when."
  :available :operator-and-peers
  :core t
  :parameters (obj "type" "object"
                   "properties" (obj "about" (obj "type" "string") "days" (obj "type" "integer")))
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let* ((about (let ((a (obj-get args "about"))) (and (stringp a) (string-trim " " a))))
                (days (let ((d (obj-get args "days"))) (if (integerp d) (max 1 (min 365 d)) 30)))
                (rows (web-history-rows kernel :days days
                                               :terms (and about (plusp (length about))
                                                           (remove "" (uiop:split-string about :separator " ") :test #'string=))))
                (start (web-history-start kernel)))
           (format nil "~:[My web history is empty.~;My web history begins ~:*~a; nothing before then is in it.~]~%~:[Nothing~;~:*~d entr~:@p~]~@[ matching \"~a\"~] in the last ~d day~:p~:[.~;, newest first:~]~{~%- ~a~}"
                   (and start (or (ignore-errors (local-stamp kernel start)) start))
                   (and rows (length rows)) (and about (plusp (length about)) about) days rows
                   (mapcar (lambda (r) (web-history-line kernel r)) rows)))))

(define-tool "brave_search"
  :description "Search the web. Returns titles, addresses and snippets; read a page with web_fetch. Searches are few per thought and cost credits; reading pages is free, so search once well and then read."
  :parameters (obj "type" "object"
                   "properties" (obj "query" (obj "type" "string") "count" (obj "type" "integer"))
                   "required" (list "query"))
  :run (lambda (kernel id args)
         (if (and kernel id (<= (%searches-left kernel id) 0))
             "Not searched: this thought has used its searches. Read what you already found with web_fetch, follow links from those pages, or keep the question for another time."
             (brave-search (obj-get args "query" "") :count (obj-get args "count" 5) :kernel kernel :thought-id id))))
