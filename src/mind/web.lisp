(in-package #:pai-v3.mind)

;;; Reaching the web: web_fetch and brave_search (spec §16.10).
;;;
;;; Only http and https. Hosts that resolve to loopback, private or
;;; link-local addresses are refused, so a page or a person cannot steer the
;;; agent at the host or its network. Results are text; a long page is
;;; paged by the thought, never cut.

(defvar *fetch-allow-private* nil "Allow private addresses (tests only).")

(defvar *http-get*
  (lambda (url headers)
    (multiple-value-bind (body status response-headers)
        (http-request :get url :headers headers :read-timeout 20 :allow-private *fetch-allow-private*)
      (values body status (or (and response-headers (gethash "content-type" response-headers)) ""))))
  "(lambda (url headers)) -> body, status, content-type, through the kernel's
one outbound transport. Replaced in tests.")


(defparameter *fetch-max-chars* 400000)

(defun %check-url (url)
  "Refuse early, with the transport's own rule, before anything is sent."
  (check-outbound-url url :allow-private *fetch-allow-private*)
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

(defun %drop-elements (html tag)
  "HTML without any <TAG ...>...</TAG> element."
  (let ((out (make-string-output-stream)) (start 0)
        (open (format nil "<~a" tag)) (close (format nil "</~a>" tag)))
    (loop for at = (search open html :start2 start :test #'char-equal)
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

(defun web-fetch (url)
  "The readable text of URL."
  (handler-case
      (progn
        (%check-url url)
        (multiple-value-bind (body status content-type)
            (funcall *http-get* url '(("User-Agent" . "pAI/3 (personal assistant)")))
          (let ((body (if (> (length body) *fetch-max-chars*) (subseq body 0 *fetch-max-chars*) body)))
            (cond ((not (<= 200 status 299)) (format nil "~a answered ~d." url status))
                  ((search "html" content-type)
                   (format nil "~@[~a~%~]~a~%~%~a" (%title body) url (html-text body)))
                  ((or (search "text" content-type) (search "json" content-type) (search "xml" content-type))
                   (format nil "~a~%~%~a" url body))
                  (t (format nil "~a is ~a, which I cannot read as text." url content-type))))))
    (egress-refused (e) (egress-reason e))
    (error (e) (format nil "Could not fetch ~a: ~a" url e))))

(define-tool "web_fetch"
  :description "Read a web page as text. Long pages come back in parts."
  :parameters (obj "type" "object"
                   "properties" (obj "url" (obj "type" "string"))
                   "required" (list "url"))
  :run (lambda (kernel id args) (declare (ignore kernel id)) (web-fetch (obj-get args "url" ""))))

;;; Brave search

(defun %strip-tags (text)
  (if (stringp text) (%decode-entities (html-text text)) ""))

(defun brave-search (query &key (count 5))
  "Web results for QUERY, numbered, with title, address and snippet. The key
is the secret named brave, bound to api.search.brave.com."
  (handler-case
      (multiple-value-bind (body status)
          (funcall *http-get*
                   (format nil "https://api.search.brave.com/res/v1/web/search?q=~a&count=~d"
                           (quri:url-encode query) (max 1 (min 20 count)))
                   (list (cons "Accept" "application/json")
                         (cons "X-Subscription-Token" (list :secret "brave" "~a"))))
        (cond ((= status 429) "Web search is rate-limited just now; try again in a little while.")
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

(define-tool "brave_search"
  :description "Search the web. Returns titles, addresses and snippets; read a page with web_fetch."
  :parameters (obj "type" "object"
                   "properties" (obj "query" (obj "type" "string") "count" (obj "type" "integer"))
                   "required" (list "query"))
  :run (lambda (kernel id args)
         (declare (ignore kernel id))
         (brave-search (obj-get args "query" "") :count (obj-get args "count" 5))))
