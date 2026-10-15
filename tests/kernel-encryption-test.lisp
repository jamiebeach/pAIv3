(in-package #:pai-v3.test)

;;; Encryption at rest (spec §17, ENC-1, ENC-8).

(define-event-type "test.said" :schema (("text" :string)))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8) :if-does-not-exist nil)
    (and in (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
              (read-sequence v in) v))))

(defun contains-p (octets text)
  (and octets (search (babel:string-to-octets text :encoding :utf-8) octets)))

(deftest nothing-readable-reaches-the-disk
  (let ((path (temp-path)) (marker "lighthouse-keeper-secret-7f3a"))
    (with-temp-kernel (k :path path)
      (append! k "test.said" (obj "text" marker))
      (is (null (contains-p (file-octets (format nil "~a-wal" path)) marker))
          "the write-ahead log is encrypted too")
      (is (null (contains-p (file-octets path) marker))))
    (let ((octets (file-octets path)))
      (is (null (contains-p octets marker)) "the store is encrypted after checkpoint")
      (is (null (contains-p octets "SQLite format 3")) "not even the header is plain")
      (is (null (contains-p octets "pai_events")) "nor the schema"))))

(deftest wrong-or-missing-keys-never-open-a-store
  (let ((path (temp-path)))
    (with-temp-kernel (k :path path) (append! k "test.said" (obj "text" "hello")))
    (signals store-locked (open-kernel path :key (ironclad:random-data 32)))
    (signals store-locked (open-kernel path))
    (with-temp-kernel (k :path path)
      (is (= 1 (ledger-head k)) "the right key still opens it, untouched"))))

(deftest a-missing-store-is-not-silently-created
  (let ((path (temp-path)))
    (signals store-locked (open-kernel path :key *test-key* :create nil))
    (is (null (probe-file path)) "nothing was created")))

(deftest rekeying-re-encrypts-the-whole-store
  (let ((path (temp-path)) (new-key (ironclad:random-data 32)))
    (with-temp-kernel (k :path path)
      (append! k "test.said" (obj "text" "before rekey"))
      (db-rekey (kernel-db k) new-key))
    (signals store-locked (open-kernel path :key *test-key*))
    (with-temp-kernel (k :path path :key new-key)
      (is (= 1 (ledger-head k))))))

(run-suite "kernel-encryption")
