(in-package #:pai-v3.test)

(define-event-type "test.said" :version 2
  :schema (("text" :string) ("channel" :string :optional))
  :upcast ((0 . (lambda (p) (obj "text" (obj-get p "message" ""))))
           (1 . (lambda (p) p))))

(deftest append-and-read-back
  (with-temp-kernel (k)
    (let ((e (append! k "test.said" (obj "text" "hello" "channel" "web"))))
      (is (integerp (event-id e)))
      (is (= (event-id e) (event-root-id e)) "uncaused event is its own root")
      (let ((back (event-by-id k (event-id e))))
        (is (string= "hello" (obj-get (event-payload back) "text")))
        (is (= 2 (event-version back)))
        (is (= (event-seq e) (event-seq back)))))))

(deftest causal-root-is-inherited
  (with-temp-kernel (k)
    (let* ((a (append! k "test.said" (obj "text" "a")))
           (b (append! k "test.said" (obj "text" "b") :caused-by (event-id a)))
           (c (append! k "test.said" (obj "text" "c") :caused-by (event-id b))))
      (is (= (event-id a) (event-root-id c))))))

(deftest schema-is-enforced
  (with-temp-kernel (k)
    (signals event-type-error (append! k "test.said" (obj "channel" "web")))
    (signals event-type-error (append! k "test.said" (obj "text" 5)))
    (signals event-type-error (append! k "test.said" (obj "text" "x" "extra" 1)))
    (signals event-type-error (append! k "no.such.type" (obj)))
    (is (= 0 (ledger-head k)) "rejected appends write nothing")))

(deftest payload-cap-is-enforced
  (with-temp-kernel (k)
    (signals event-type-error
      (append! k "test.said" (obj "text" (make-string 20000 :initial-element #\x))))))

(deftest integrity-hash-is-sha256-of-stored-json
  (with-temp-kernel (k)
    (append! k "test.said" (obj "text" "hash me"))
    (destructuring-bind (json hash)
        (sql-row (kernel-db k) "SELECT event_json,integrity_hash FROM pai_events LIMIT 1")
      (is (string= hash (pai-v3.kernel::sha256-hex json))))))

(deftest legacy-v2-rows-upcast-on-read
  (with-temp-kernel (k)
    ;; A row shaped exactly as v2 wrote it (schema_version 1, no "v").
    (let ((json (json-encode (obj "schema_version" 1 "id" 41 "agent_id" "test-agent"
                                  "timestamp" "2026-09-01T00:00:00Z" "type" "test.said"
                                  "payload" (obj "message" "from v2") "tick_id" "t1"))))
      (sql-exec (kernel-db k) "INSERT INTO pai_events(event_id,storage_origin,agent_id,partition_status,event_type,occurred_at,event_json,integrity_hash) VALUES(41,'native','test-agent','verified','test.said','2026-09-01T00:00:00Z',?1,?2)"
                json (pai-v3.kernel::sha256-hex json)))
    (let ((e (event-by-id k 41)))
      (is (string= "from v2" (obj-get (event-payload e) "text")) "upcast 0 -> 1 -> 2")
      (is (= 2 (event-version e)))
      (is (string= "t1" (obj-get (pai-v3.kernel::event-legacy e) "tick_id"))))
    (is (= 42 (event-id (append! k "test.said" (obj "text" "next")))) "ids continue after v2")))

(deftest json-roundtrip-is-canonical
  (let ((o (obj "b" 1 "a" (list 1 "two" :null :false t) "c" (obj "x" 1.5d0))))
    (is (string= (json-encode o) (json-encode (json-parse (json-encode o)))))
    (is (string= "{\"a\":[1,\"two\",null,false,true],\"b\":1,\"c\":{\"x\":1.5}}" (json-encode o)))))

(deftest unicode-survives
  (with-temp-kernel (k)
    (let ((e (append! k "test.said" (obj "text" "café — 日本 🌙"))))
      (is (string= "café — 日本 🌙" (obj-get (event-payload (event-by-id k (event-id e))) "text"))))))

(run-suite "kernel-ledger")
