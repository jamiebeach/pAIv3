(in-package #:pai-v3.kernel)

;;; Minimal SQLite binding. Statements are prepared per call and always
;;; finalized; every runtime query is expected to carry its own LIMIT.
;;; Connections are opened serialized (FULLMUTEX) because the scheduler and
;;; request threads share one; all transactions run under the kernel lock.

;;; The library is SQLite3 Multiple Ciphers (spec §17): stores are encrypted
;;; in the database. PAI_SQLITE_LIBRARY names it explicitly; otherwise it is
;;; looked up by its usual names. A plain SQLite would open unencrypted
;;; stores only, so keyed opens check that the cipher is present.

(cffi:define-foreign-library libsqlite3mc
  (:darwin "libsqlite3mc.dylib")
  (:unix "libsqlite3mc.so")
  (:windows (:or "sqlite3mc_x64.dll" "sqlite3mc.dll")))

(defun %library-beside-executable ()
  "The cipher library shipped next to the pai executable, if there is one."
  (let ((dir (and sb-ext:*runtime-pathname*
                  (make-pathname :name nil :type nil :defaults sb-ext:*runtime-pathname*))))
    (and dir
         (find-if #'probe-file
                  (mapcar (lambda (name) (merge-pathnames name dir))
                          '("libsqlite3mc.so" "libsqlite3mc.dylib" "sqlite3mc_x64.dll"))))))

(defun reload-sqlite-library ()
  "Load the cipher library: PAI_SQLITE_LIBRARY if set, else the copy beside
the executable, else the usual names. Called at load and at executable start."
  (let ((explicit (sb-ext:posix-getenv "PAI_SQLITE_LIBRARY"))
        (beside (%library-beside-executable)))
    (cond ((plusp (length explicit)) (cffi:load-foreign-library explicit))
          (beside (cffi:load-foreign-library (namestring beside)))
          (t (cffi:use-foreign-library libsqlite3mc)))))

(reload-sqlite-library)

(defconstant +ok+ 0)
(defconstant +row+ 100)
(defconstant +done+ 101)
(defconstant +open-readonly+ #x1)
(defconstant +open-readwrite+ #x2)
(defconstant +open-create+ #x4)
(defconstant +open-fullmutex+ #x10000)
(defconstant +type-integer+ 1)
(defconstant +type-float+ 2)
(defconstant +type-text+ 3)
(defconstant +type-blob+ 4)
(defconstant +type-null+ 5)

(cffi:defcfun ("sqlite3_open_v2" %open) :int
  (filename :string) (db :pointer) (flags :int) (vfs :pointer))
(cffi:defcfun ("sqlite3_close_v2" %close) :int (db :pointer))
(cffi:defcfun ("sqlite3_errmsg" %errmsg) :string (db :pointer))
(cffi:defcfun ("sqlite3_extended_errcode" %errcode) :int (db :pointer))
(cffi:defcfun ("sqlite3_busy_timeout" %busy-timeout) :int (db :pointer) (ms :int))
(cffi:defcfun ("sqlite3_prepare_v2" %prepare) :int
  (db :pointer) (sql :string) (nbyte :int) (stmt :pointer) (tail :pointer))
(cffi:defcfun ("sqlite3_step" %step) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_finalize" %finalize) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_bind_int64" %bind-int64) :int
  (stmt :pointer) (index :int) (value :int64))
(cffi:defcfun ("sqlite3_bind_double" %bind-double) :int
  (stmt :pointer) (index :int) (value :double))
(cffi:defcfun ("sqlite3_bind_text" %bind-text) :int
  (stmt :pointer) (index :int) (value :pointer) (nbytes :int) (destructor :pointer))
(cffi:defcfun ("sqlite3_bind_null" %bind-null) :int (stmt :pointer) (index :int))
(cffi:defcfun ("sqlite3_bind_blob" %bind-blob) :int
  (stmt :pointer) (index :int) (value :pointer) (nbytes :int) (destructor :pointer))
(cffi:defcfun ("sqlite3_column_blob" %column-blob) :pointer (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_column_count" %column-count) :int (stmt :pointer))
(cffi:defcfun ("sqlite3_column_type" %column-type) :int (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_column_int64" %column-int64) :int64 (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_column_double" %column-double) :double (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_column_text" %column-text) :pointer (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_column_bytes" %column-bytes) :int (stmt :pointer) (col :int))
(cffi:defcfun ("sqlite3_key_v2" %key) :int
  (db :pointer) (schema :string) (key :pointer) (nkey :int))
(cffi:defcfun ("sqlite3_backup_init" %backup-init) :pointer
  (dest :pointer) (dest-name :string) (source :pointer) (source-name :string))
(cffi:defcfun ("sqlite3_backup_step" %backup-step) :int (backup :pointer) (pages :int))
(cffi:defcfun ("sqlite3_backup_finish" %backup-finish) :int (backup :pointer))
(cffi:defcfun ("sqlite3_rekey_v2" %rekey) :int
  (db :pointer) (schema :string) (key :pointer) (nkey :int))

(define-condition sqlite-error (error)
  ((code :initarg :code :reader sqlite-error-code)
   (message :initarg :message :reader sqlite-error-message)
   (sql :initarg :sql :initform nil :reader sqlite-error-sql))
  (:report (lambda (c s)
             (format s "SQLite error ~a: ~a~@[ in ~s~]"
                     (sqlite-error-code c) (sqlite-error-message c)
                     (sqlite-error-sql c)))))

(defvar *connection-hooks* '()
  "Functions called with each newly opened DB handle pointer, e.g. to register
SQL functions. Modules push onto this at load time.")

(defstruct (db (:constructor %make-db (handle path)))
  handle path (depth 0) key)

;;; Diagnostics must never recursively print connection keys or frame arguments.
(defmethod print-object ((connection db) stream)
  (print-unreadable-object (connection stream :type t :identity t)
    (write-string "connection" stream)))

(defun print-safe-backtrace (stream &key (count 30))
  "Print function names only, never arguments, locals or source expressions."
  (handler-case
      (loop for frame = (sb-di:top-frame) then (sb-di:frame-down frame)
            for index below count while frame
            do (let ((*print-level* 4) (*print-length* 12) (*print-circle* t))
                 (format stream "~d: ~s~%" index
                         (sb-di:debug-fun-name (sb-di:frame-debug-fun frame)))))
    (error () (write-line "[remaining frames unavailable]" stream))))

(defun %check (db code &optional sql)
  (unless (= code +ok+)
    (error 'sqlite-error :code (%errcode (db-handle db))
                         :message (%errmsg (db-handle db)) :sql sql)))

(define-condition store-locked (error)
  ((path :initarg :path :reader store-locked-path)
   (reason :initarg :reason :reader store-locked-reason))
  (:report (lambda (c s) (format s "The store ~a cannot be opened: ~a"
                                 (store-locked-path c) (store-locked-reason c)))))

(defun %apply-key (handle function key)
  (let ((n (length key)))
    (cffi:with-foreign-object (buf :uint8 n)
      (dotimes (i n) (setf (cffi:mem-aref buf :uint8 i) (aref key i)))
      (prog1 (funcall function handle "main" buf n)
        (dotimes (i n) (setf (cffi:mem-aref buf :uint8 i) 0))))))

(defun db-open (path &key (busy-ms 5000) (cache-kib 65536) read-only key (create t))
  "Open PATH with a bounded page cache (spec §15). KEY (octets) opens or
creates an encrypted store; a wrong or missing key signals STORE-LOCKED
rather than ever treating the store as new. READ-ONLY opens an existing file
without write access (maintenance reads of copies). With CREATE false a
missing file is an error, never a new empty store."
  (unless (or create read-only (probe-file path))
    (error 'store-locked :path path :reason "it does not exist; run setup to create it"))
  (cffi:with-foreign-object (out :pointer)
    (let ((code (%open (namestring path) out
                       (if read-only
                           (logior +open-readonly+ +open-fullmutex+)
                           (logior +open-readwrite+ +open-create+ +open-fullmutex+))
                       (cffi:null-pointer))))
      (let ((db (%make-db (cffi:mem-ref out :pointer) path)))
        (%check db code)
        (when key
          (%check db (%apply-key (db-handle db) #'%key key))
          (setf (db-key db) (copy-seq key)))
        ;; Reading the schema proves the key (or the absence of one) is right.
        (handler-case (sql-value db "SELECT count(*) FROM sqlite_master WHERE type=?1 LIMIT 1" "table")
          (sqlite-error (e)
            (%close (db-handle db))
            ;; Busy or locked means another connection holds it, not a key problem.
            (when (member (logand (sqlite-error-code e) #xff) '(5 6))
              (error e))
            (error 'store-locked :path path
                                 :reason (if key "the key does not open it" "it is encrypted and no key was given"))))
        (%busy-timeout (db-handle db) busy-ms)
        (unless read-only
          (sql-exec db "PRAGMA journal_mode=WAL")
          (sql-exec db "PRAGMA synchronous=NORMAL")
          (sql-exec db "PRAGMA foreign_keys=ON"))
        (sql-exec db (format nil "PRAGMA cache_size=-~d" cache-kib))
        (sql-exec db "PRAGMA mmap_size=0")
        (dolist (hook *connection-hooks*) (funcall hook (db-handle db)))
        db))))

(defun db-rekey (db key)
  "Re-encrypt the whole store under KEY (octets). Rare and offline: unlock
methods change by rewrapping the key, not by rekeying (spec §17)."
  (%check db (%apply-key (db-handle db) #'%rekey key)))

(defun db-backup (source dest &key (pages-per-step 256) (pause 0.01))
  "Copy SOURCE into DEST with SQLite's online backup, a few pages at a time
so writers on SOURCE are never held up for long. Each side keeps its own
key: pages are decrypted from SOURCE and encrypted for DEST."
  (let ((backup (%backup-init (db-handle dest) "main" (db-handle source) "main")))
    (when (cffi:null-pointer-p backup)
      (error 'sqlite-error :code (%errcode (db-handle dest)) :message (%errmsg (db-handle dest))))
    (unwind-protect
         (loop for rc = (%backup-step backup pages-per-step)
               until (= rc +done+)
               do (unless (member rc '(0 5 6))               ; ok, busy, locked: keep going
                    (error 'sqlite-error :code rc :message "backup step failed"))
                  (sleep pause))
      (%backup-finish backup)))
  dest)

(defun db-close (db)
  (when (db-handle db)
    (%close (db-handle db))
    (setf (db-handle db) nil))
  nil)

(defun %bind (db stmt index value sql)
  (%check db
          (etypecase value
            (null (%bind-null stmt index))
            ((eql t) (%bind-int64 stmt index 1))
            (integer (%bind-int64 stmt index value))
            (real (%bind-double stmt index (coerce value 'double-float)))
            (string
             (let* ((octets (babel:string-to-octets value :encoding :utf-8))
                    (n (length octets)))
               (cffi:with-foreign-object (buf :uint8 (max n 1))
                 (dotimes (i n) (setf (cffi:mem-aref buf :uint8 i) (aref octets i)))
                 ;; SQLITE_TRANSIENT: SQLite copies the bytes before returning.
                 (%bind-text stmt index buf n (cffi:make-pointer
                                               (ldb (byte 64 0) -1))))))
            ((vector (unsigned-byte 8))
             (let ((n (length value)))
               (cffi:with-foreign-object (buf :uint8 (max n 1))
                 (dotimes (i n) (setf (cffi:mem-aref buf :uint8 i) (aref value i)))
                 (%bind-blob stmt index buf n (cffi:make-pointer (ldb (byte 64 0) -1)))))))
          sql))

(defun %column (stmt col)
  (let ((type (%column-type stmt col)))
    (cond ((= type +type-null+) nil)
          ((= type +type-integer+) (%column-int64 stmt col))
          ((= type +type-float+) (%column-double stmt col))
          ((= type +type-blob+)
           (let* ((n (%column-bytes stmt col))
                  (ptr (%column-blob stmt col))
                  (out (make-array n :element-type '(unsigned-byte 8))))
             (dotimes (i n out) (setf (aref out i) (cffi:mem-aref ptr :uint8 i)))))
          (t (let ((ptr (%column-text stmt col))
                   (n (%column-bytes stmt col)))
               (if (zerop n)
                   ""
                   (cffi:foreign-string-to-lisp ptr :count n :encoding :utf-8)))))))

(defun %call-with-statement (db sql params fn)
  (cffi:with-foreign-object (out :pointer)
    (%check db (%prepare (db-handle db) sql -1 out (cffi:null-pointer)) sql)
    (let ((stmt (cffi:mem-ref out :pointer)))
      (unwind-protect
           (progn
             (loop for value in params for i from 1 do (%bind db stmt i value sql))
             (funcall fn stmt))
        (%finalize stmt)))))

(defun sql-exec (db sql &rest params)
  "Run SQL to completion; return nothing."
  (%call-with-statement
   db sql params
   (lambda (stmt)
     (loop for code = (%step stmt)
           until (= code +done+)
           unless (= code +row+) do (%check db code sql))))
  (values))

(defun sql-rows (db sql &rest params)
  "Return all rows of SQL as lists. Callers bound the result with LIMIT."
  (%call-with-statement
   db sql params
   (lambda (stmt)
     (let ((n (%column-count stmt)))
       (loop for code = (%step stmt)
             while (= code +row+)
             collect (loop for c below n collect (%column stmt c))
             finally (unless (= code +done+) (%check db code sql)))))))

(defun sql-last-insert-rowid (db)
  "The rowid of the last row this connection inserted."
  (sql-value db "SELECT last_insert_rowid() LIMIT 1"))

(defun sql-row (db sql &rest params)
  (first (apply #'sql-rows db sql params)))

(defun sql-value (db sql &rest params)
  (first (apply #'sql-row db sql params)))

(defmacro with-transaction ((db) &body body)
  "BEGIN IMMEDIATE ... COMMIT, rolled back on any non-local exit."
  (let ((d (gensym)) (done (gensym)))
    `(let ((,d ,db) (,done nil))
       (sql-exec ,d "BEGIN IMMEDIATE")
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (sql-exec ,d "COMMIT")
              (setf ,done t))
         (unless ,done (ignore-errors (sql-exec ,d "ROLLBACK")))))))

(defmacro with-savepoint ((db) &body body)
  "Nested unit inside a transaction; rolled back alone on a non-local exit."
  (let ((d (gensym)) (name (gensym)) (done (gensym)))
    `(let* ((,d ,db)
            (,name (format nil "sp~d" (incf (db-depth ,d))))
            (,done nil))
       (sql-exec ,d (format nil "SAVEPOINT ~a" ,name))
       (unwind-protect
            (multiple-value-prog1 (progn ,@body)
              (sql-exec ,d (format nil "RELEASE ~a" ,name))
              (setf ,done t))
         (unless ,done
           (ignore-errors (sql-exec ,d (format nil "ROLLBACK TO ~a" ,name)))
           (ignore-errors (sql-exec ,d (format nil "RELEASE ~a" ,name))))
         (decf (db-depth ,d))))))
