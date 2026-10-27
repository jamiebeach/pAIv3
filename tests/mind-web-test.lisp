(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)

(defvar *requests* '())

(defun serve (body &key (status 200) (type "text/html"))
  (lambda (url headers)
    (push (cons url headers) *requests*)
    (values body status type)))

(deftest pages-become-readable-text
  (let* ((*fetch-allow-private* t) (*requests* '())
         (*http-get* (serve "<html><head><title>Tide &amp; Times</title><style>p{}</style></head>
<body><script>alert(1)</script><h1>High   tide</h1><p>At <b>6:40</b> today.</p><ul><li>One</li><li>Two</li></ul></body></html>"))
         (text (pai-v3.mind::web-fetch "https://example.org/tides")))
    (is (search "Tide & Times" text))
    (is (search (format nil "High tide~%At 6:40 today.~%One~%Two") text))
    (is (null (search "alert" text)) "scripts are dropped")
    (is (null (search "p{}" text)) "styles are dropped")))

(deftest only-the-public-web-is-fetched
  (let ((*http-get* (serve "should not be fetched")))
    (is (search "Only http and https" (pai-v3.mind::web-fetch "file:///etc/passwd")))
    (is (search "private network" (pai-v3.mind::web-fetch "http://127.0.0.1:8080/api")))
    (is (search "private network" (pai-v3.mind::web-fetch "http://10.1.2.3/")))
    (is (search "private network" (pai-v3.mind::web-fetch "http://localhost/")))
    (is (search "private network" (pai-v3.mind::web-fetch "http://[::1]/")))))

(deftest failures-are-text-not-errors
  (let ((*fetch-allow-private* t))
    (let ((*http-get* (serve "gone" :status 404)))
      (is (search "answered 404" (pai-v3.mind::web-fetch "https://example.org/x"))))
    (let ((*http-get* (serve (make-string 10 :initial-element #\x) :type "image/png")))
      (is (search "cannot read as text" (pai-v3.mind::web-fetch "https://example.org/x.png"))))))

(deftest search-needs-a-key
  ;; The real transport refuses a header naming a secret that is not stored.
  (let ((*http-get* (lambda (url headers)
                      (declare (ignore url))
                      (http-request :get "https://api.search.brave.com/" :headers headers))))
    (is (search "not set up" (pai-v3.mind::brave-search "tides")))))

(deftest search-returns-numbered-results
  (let* ((*requests* '())
         (*http-get* (serve (json-encode
                             (obj "web" (obj "results"
                                             (list (obj "title" "Tide <strong>tables</strong>" "url" "https://a.example"
                                                        "description" "Times for <strong>today</strong>")
                                                   (obj "title" "Moon" "url" "https://b.example" "description" "Phases")))))
                            :type "application/json"))
         (text (pai-v3.mind::brave-search "tide tables" :count 2)))
    (is (search (format nil "1. Tide tables~%https://a.example~%Times for today") text))
    (is (search "2. Moon" text))
    (destructuring-bind (url . headers) (first *requests*)
      (is (search "q=tide%20tables" url))
      (is (equal '(:secret "brave" "~a") (cdr (assoc "X-Subscription-Token" headers :test #'string=)))
          "the key is named, never held")))
  (let ((*http-get* (serve "{}" :status 429)))
    (is (search "rate-limited" (pai-v3.mind::brave-search "x")))))

(deftest searches-are-capped-per-thought-and-recorded
  (with-temp-kernel (k)
    (let* ((headers (let ((h (make-hash-table :test 'equal))) (setf (gethash "x-ratelimit-remaining" h) "1, 1900") h))
           (*http-get* (lambda (url hs) (declare (ignore url hs))
                         (values (json-encode (obj "web" (obj "results" '()))) 200 "application/json" headers)))
           (free (request-thought k "introspection" "Free time. This time is yours."))
           (work (request-thought k "work" "Look into the relay."))
           (run (pai-v3.mind::tool-run (gethash "brave_search" pai-v3.mind::*tools*))))
      (flet ((searched (thought)
               (append! k "thought.step" (obj "thought_id" thought "kind" "tool_result" "part" 0 "text" "r" "tool" "brave_search"))))
        (is (search "No results" (funcall run k free (obj "query" "tides"))) "the first search runs")
        (let ((e (event-payload (event-by-id k (first (latest-event-ids k "web.searched" 1))))))
          (is (and (equal "tides" (obj-get e "query")) (equal "1, 1900" (obj-get e "remaining")) (eql free (obj-get e "thought_id")))
              "each search is recorded with what Brave says is left"))
        (searched free) (searched free)
        (is (search "Not searched" (funcall run k free (obj "query" "more"))) "free time has two searches")
        (searched work) (searched work)
        (is (search "No results" (funcall run k work (obj "query" "relay"))) "other thinking has four")
        (searched work) (searched work)
        (is (search "Not searched" (funcall run k work (obj "query" "relay again"))))))
    (let ((*http-get* (serve "{}" :status 402)))
      (is (search "out of credits" (pai-v3.mind::brave-search "x")) "out of credits says stop, not retry"))))

(run-suite "mind-web")
