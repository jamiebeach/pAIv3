(defsystem "pai-v3-maintenance"
  :description "Explicit maintenance operations: rebuild and verification.
Never loaded by the runtime image (engineering spec §4.1)."
  :license "Proprietary; not for publication"
  :depends-on ("pai-v3")
  :serial t
  :components
  ((:module "src/maintenance"
    :serial t
    :components ((:file "package")
                 (:file "rebuild")
                 (:file "census")
                 (:file "import-v2")))))
