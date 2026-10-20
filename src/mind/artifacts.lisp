(in-package #:pai-v3.mind)

;;; Durable authored documents. Events contain references and provenance only;
;;; immutable document bytes live in the encrypted blob store, like images.
;;; No cache: one document, <=32 MiB rendered, <=120k source characters per operation.
(defparameter *artifact-max-bytes* (* 32 1024 1024))
(defparameter +artifact-blobs-ddl+
  "CREATE TABLE IF NOT EXISTS artifact_blobs (digest TEXT PRIMARY KEY, source TEXT NOT NULL, html TEXT NOT NULL)")

(define-event-type "artifact.saved"
  :schema (("artifact_id" :string) ("title" :string) ("digest" :string)
           ("thought_id" :integer) ("work_id" :string :optional)))
(define-event-type "artifact.disposition"
  :schema (("artifact_id" :string) ("revision" :integer) ("state" :string) ("note" :string)))

(define-projection "artifacts"
  :kind :inline :consumes ("artifact.saved" "artifact.disposition")
  :tables ("CREATE TABLE IF NOT EXISTS artifacts (artifact_id TEXT NOT NULL, revision INTEGER NOT NULL, title TEXT NOT NULL, digest TEXT NOT NULL, thought_id INTEGER NOT NULL, work_id TEXT, state TEXT NOT NULL, note TEXT NOT NULL, at TEXT NOT NULL, PRIMARY KEY(artifact_id,revision))"
           "CREATE INDEX IF NOT EXISTS artifacts_recent_idx ON artifacts(revision DESC)"
           "CREATE INDEX IF NOT EXISTS artifacts_work_idx ON artifacts(work_id,revision DESC)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (if (equal (event-type e) "artifact.saved")
                  (sql-exec db "INSERT INTO artifacts VALUES(?1,?2,?3,?4,?5,?6,'draft','',?7)"
                            (obj-get p "artifact_id") (event-id e) (obj-get p "title")
                            (obj-get p "digest") (obj-get p "thought_id") (obj-get p "work_id") (event-at e))
                  (sql-exec db "UPDATE artifacts SET state=?3,note=?4 WHERE artifact_id=?1 AND revision=?2"
                            (obj-get p "artifact_id") (obj-get p "revision") (obj-get p "state") (obj-get p "note"))))))

(defun %artifact-db (kernel)
  (let ((db (or *image-store* (kernel-db kernel))))
    (sql-exec db +artifact-blobs-ddl+) db))

(defun %artifact-text (value maximum name &optional empty)
  (unless (and (stringp value) (<= (length value) maximum)
               (or empty (plusp (length (string-trim '(#\Space #\Newline #\Tab) value)))))
    (error "~a must be ~:[nonempty ~;~]text of at most ~d characters" name empty maximum))
  value)

(defun %artifact-escape (text)
  (with-output-to-string (out)
    (loop for c across text do
      (write-string (case c (#\& "&amp;") (#\< "&lt;") (#\> "&gt;")
                          (#\" "&quot;") (#\' "&#39;") (t (string c))) out))))

(defparameter +artifact-themes+
  '(("paper" "#f3efe6" "#fffdf8" "#292822" "#815b35")
    ("night" "#111a28" "#19263a" "#f0eee8" "#e8bd78")
    ("rose" "#f5e9eb" "#fff9f7" "#422e39" "#a34365")
    ("forest" "#e6eee7" "#f9fcf5" "#25392e" "#426e50")))

(defun render-document (kernel document)
  "Render escaped text and owned images only; no caller HTML, CSS or URLs."
  (unless (hash-table-p document) (error "document must be an object"))
  (let* ((title (%artifact-text (obj-get document "title") 160 "title"))
         (subtitle (%artifact-text (obj-get document "subtitle" "") 500 "subtitle" t))
         (theme (assoc (obj-get document "theme" "paper") +artifact-themes+ :test #'equal))
         (layout (obj-get document "layout" "report"))
         (sections (obj-get document "sections")) (images 0))
    (unless theme (error "theme must be paper, night, rose or forest"))
    (unless (member layout '("report" "card") :test #'equal) (error "layout must be report or card"))
    (unless (and (listp sections) (<= 1 (length sections) 24)) (error "give 1 to 24 sections"))
    (when (> (length (json-encode document)) 120000) (error "document source exceeds 120000 characters"))
    (with-output-to-string (out)
      (format out "<!doctype html><html lang=~s><head><meta charset=~s><meta name=~s content=~s><meta http-equiv=~s content=~s><title>~a</title><style>"
              "en" "utf-8" "viewport" "width=device-width,initial-scale=1"
              "Content-Security-Policy" "default-src 'none'; img-src data:; style-src 'unsafe-inline'; base-uri 'none'; form-action 'none'"
              (%artifact-escape title))
      (format out "*{box-sizing:border-box}body{margin:0;padding:clamp(16px,5vw,64px);background:~a;color:~a;font:18px/1.75 Georgia,'Times New Roman',serif}main{max-width:860px;margin:auto;padding:clamp(24px,6vw,72px);background:~a;border-radius:18px;box-shadow:0 16px 60px #0002;border-top:8px solid ~a}header{border-bottom:1px solid ~a;padding-bottom:24px;margin-bottom:36px}h1{font-size:clamp(34px,6vw,60px);line-height:1.12;letter-spacing:-.035em;margin:0 0 18px}h2{font:600 23px/1.3 system-ui,sans-serif;color:~a;margin-top:36px}p{white-space:pre-wrap;margin:16px 0}header p{font:18px/1.6 system-ui,sans-serif;opacity:.75}figure{margin:28px 0}img{display:block;width:100%;height:auto;border-radius:10px}figcaption{font:14px/1.5 system-ui,sans-serif;opacity:.7;margin-top:10px}.card{text-align:center;max-width:740px}.card h1{font-style:italic}.card p{font-size:21px}@media print{body{padding:0;background:white}main{box-shadow:none;border-radius:0}}</style></head><body><main class=~s><header><h1>~a</h1><p>~a</p></header>"
              (second theme) (fourth theme) (third theme) (fifth theme) (fifth theme) (fifth theme)
              layout (%artifact-escape title) (%artifact-escape subtitle))
      (dolist (section sections)
        (unless (hash-table-p section) (error "each section must be an object"))
        (let ((heading (%artifact-text (obj-get section "heading" "") 240 "heading" t))
              (text (%artifact-text (obj-get section "text" "") 12000 "section text" t))
              (image (obj-get section "image_id"))
              (caption (%artifact-text (obj-get section "caption" "") 500 "caption" t)))
          (format out "<section>~@[<h2>~a</h2>~]<p>~a</p>"
                  (and (plusp (length heading)) (%artifact-escape heading)) (%artifact-escape text))
          (when image
            (when (> (incf images) 4) (error "a document may include at most 4 images"))
            (let ((url (image-data-url kernel image)))
              (unless url (error "no stored image ~a" image))
              (format out "<figure><img src=~s alt=~s><figcaption>~a</figcaption></figure>"
                      url (%artifact-escape caption) (%artifact-escape caption))))
          (write-string "</section>" out)))
      (write-string "</main></body></html>" out))))

(defun artifact-row (kernel id &optional revision)
  (if revision
      (sql-row (kernel-db kernel) "SELECT artifact_id,revision,title,digest,thought_id,work_id,state,note,at FROM artifacts WHERE artifact_id=?1 AND revision=?2 LIMIT 1" id revision)
      (sql-row (kernel-db kernel) "SELECT artifact_id,revision,title,digest,thought_id,work_id,state,note,at FROM artifacts WHERE artifact_id=?1 ORDER BY revision DESC LIMIT 1" id)))

(defun artifact-json (row)
  (destructuring-bind (id revision title digest thought work state note at) row
    (obj "artifact_id" id "revision" revision "title" title "digest" digest
         "thought_id" thought "work_id" work "state" state "note" note "at" at
         "url" (format nil "/artifacts?id=~a&revision=~d" id revision))))

(defun artifact-list (kernel &optional work-id)
  (mapcar #'artifact-json
          (if work-id
              (sql-rows (kernel-db kernel) "SELECT artifact_id,revision,title,digest,thought_id,work_id,state,note,at FROM artifacts WHERE work_id=?1 ORDER BY revision DESC LIMIT 50" work-id)
              (sql-rows (kernel-db kernel) "SELECT artifact_id,revision,title,digest,thought_id,work_id,state,note,at FROM artifacts ORDER BY revision DESC LIMIT 50"))))

(defun artifact-content (kernel row)
  (let ((content (sql-row (%artifact-db kernel) "SELECT source,html FROM artifact_blobs WHERE digest=?1 LIMIT 1" (fourth row))))
    (unless content (error "artifact content unavailable; restore its blob store backup"))
    (unless (equal (fourth row)
                   (pai-v3.kernel::sha256-hex (concatenate 'string (first content) (string #\Newline) (second content))))
      (error "artifact content digest mismatch; restore its blob store backup"))
    content))

(defun %artifact-operator-thought (kernel thought-id)
  (unless (equal "operator" (getf (thought-row kernel thought-id) :audience))
    (error "documents are private to operator-audience thoughts")))

(defun save-document (kernel thought-id document &optional artifact-id)
  (%artifact-operator-thought kernel thought-id)
  (let* ((previous (and artifact-id (artifact-row kernel artifact-id)))
         (id (or artifact-id (format nil "a-~a" (base64url-encode (ironclad:random-data 12)))))
         (source (json-encode document))
         (html (render-document kernel document))
         (digest (pai-v3.kernel::sha256-hex (concatenate 'string source (string #\Newline) html)))
         (work (or (%docket-thought (kernel-db kernel) thought-id) (sixth previous))))
    (when (and artifact-id (null previous)) (error "no such artifact"))
    (when (> (length (babel:string-to-octets html :encoding :utf-8)) *artifact-max-bytes*)
      (error "rendered document exceeds 32 MiB"))
    ;; Persist bytes before their ledger reference. A crash can orphan bytes,
    ;; never publish an event whose content has not been stored.
    (sql-exec (%artifact-db kernel) "INSERT OR IGNORE INTO artifact_blobs VALUES(?1,?2,?3)" digest source html)
    (let ((event (append! kernel "artifact.saved"
                          (obj "artifact_id" id "title" (obj-get document "title") "digest" digest
                               "thought_id" thought-id "work_id" work))))
      (artifact-json (artifact-row kernel id (event-id event))))))

(defun set-artifact-state (kernel thought-id id revision state note)
  (%artifact-operator-thought kernel thought-id)
  (unless (member state '("kept" "delivered" "abandoned") :test #'equal)
    (error "state must be kept, delivered or abandoned"))
  (%artifact-text note 1200 "note" (not (equal state "delivered")))
  (bt:with-recursive-lock-held ((pai-v3.kernel::kernel-lock kernel))
    (let ((row (artifact-row kernel id revision)))
      (unless row (error "no such artifact revision"))
      (when (equal "delivered" (seventh row))
        (return-from set-artifact-state (artifact-json row)))
      (when (equal state "delivered") (artifact-content kernel row))
      ;; One kernel batch makes the notice and disposition atomic. Retrying
      ;; delivery after interruption cannot send the same revision twice.
      (append-batch! kernel
        (append (when (equal state "delivered")
                  (list (list "notice.posted"
                              (obj "channel" "web" "audience" "operator" "importance" "normal"
                                   "text" (format nil "~a~%[Open document](/artifacts?id=~a&revision=~d)" note id revision)))))
                (list (list "artifact.disposition"
                            (obj "artifact_id" id "revision" revision "state" state "note" note)))))
      (artifact-json (artifact-row kernel id revision)))))

(define-tool "document"
  :core '("docket")
  :available :operator-trust
  :summary "Create, revise, read and deliver a polished illustrated document or card."
  :description "Make useful deliverables, not only a description of work. save creates a private draft or new immutable revision of artifact_id. document has title, subtitle, layout (report/card), theme (paper/night/rose/forest), and 1-24 sections with heading, text, optional image_id and caption. Text is plain, not HTML/Markdown. Up to 4 existing stored images are embedded. list finds recent revisions (optionally work_id); read retrieves document source so work can resume. keep retains privately; deliver requires an introduction in note and sends an operator notice linking the exact revision; abandon sets it aside. Never claim delivery until deliver succeeds. PDF export is not available. Docket runs automatically link saved documents to their work item."
  :parameters (obj "type" "object" "properties"
    (obj "action" (obj "type" "string" "enum" '("save" "list" "read" "keep" "deliver" "abandon"))
         "artifact_id" (obj "type" "string") "revision" (obj "type" "integer")
         "work_id" (obj "type" "string") "note" (obj "type" "string")
         "document" (obj "type" "object" "properties"
           (obj "title" (obj "type" "string") "subtitle" (obj "type" "string")
                "layout" (obj "type" "string" "enum" '("report" "card"))
                "theme" (obj "type" "string" "enum" '("paper" "night" "rose" "forest"))
                "sections" (obj "type" "array" "items" (obj "type" "object" "properties"
                  (obj "heading" (obj "type" "string") "text" (obj "type" "string")
                       "image_id" (obj "type" "string") "caption" (obj "type" "string")))))
           "required" '("title" "sections"))) "required" '("action"))
  :run (lambda (kernel thought-id args)
         (%artifact-operator-thought kernel thought-id)
         (let* ((action (obj-get args "action")) (id (obj-get args "artifact_id"))
                (revision (obj-get args "revision")))
           (json-encode
             (cond ((equal action "save") (save-document kernel thought-id (obj-get args "document") id))
                   ((equal action "list") (artifact-list kernel (obj-get args "work_id")))
                   ((equal action "read")
                    (let ((row (artifact-row kernel id revision)))
                      (unless row (error "no such artifact"))
                      (obj "artifact" (artifact-json row) "document" (json-parse (first (artifact-content kernel row))))))
                   (t (unless revision (error "give the exact revision"))
                      (set-artifact-state kernel thought-id id revision
                        (cond ((equal action "keep") "kept") ((equal action "deliver") "delivered")
                              ((equal action "abandon") "abandoned") (t (error "unknown action")))
                        (obj-get args "note" ""))))))))
