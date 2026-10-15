(in-package #:pai-v3.test)
(use-package '#:pai-v3.mind)
(use-package '#:pai-v3.self)

;;; Inspection from inside (bead v3-clp.13, spec §16.6).

(defun inspect-tool (k &rest args)
  (funcall (pai-v3.mind::tool-run (gethash "inspect" pai-v3.mind::*tools*))
           k 1 (apply #'obj args)))

(deftest overview-lists-every-projection
  (with-temp-kernel (k)
    (let ((text (inspect-tool k "what" "overview")))
      (is (search "Ledger head" text))
      (is (search "settings (inline, pure, v1): ready" text))
      (is (search "live_changes" text)))))

(deftest rows-are-bounded-and-checked
  (with-temp-kernel (k)
    (change-setting k "self.eval_seconds" 20 :origin (obj "principal_type" "operator"))
    (is (search "self.eval_seconds" (inspect-tool k "what" "rows" "name" "settings")))
    (is (search "self.eval_seconds"
                (inspect-tool k "what" "rows" "name" "settings" "column" "name" "value" "self.eval_seconds")))
    (is (search "has no column" (inspect-tool k "what" "rows" "name" "settings" "column" "x; DROP TABLE settings")))
    (is (search "has no table" (inspect-tool k "what" "rows" "name" "settings" "table" "pai_events"))
        "the ledger and other projections' tables are out of reach")
    (is (search "credentials" (inspect-tool k "what" "rows" "name" "auth")))
    (is (search "no projection" (inspect-tool k "what" "rows" "name" "nope")))))

(deftest digests-follow-the-rows
  (with-temp-kernel (k)
    (let ((before (projection-digest k "settings")))
      (is (search "digest" (inspect-tool k "what" "digest" "name" "auth")) "credential tables still have digests")
      (change-setting k "self.eval_seconds" 30 :origin (obj "principal_type" "operator"))
      (is (string/= before (projection-digest k "settings")))
      (is (= 1 (gethash "settings" (projection-counts k "settings")))))))

(deftest quarantine-and-live-changes-are-visible
  (with-temp-kernel (k)
    (is (search "Nothing is quarantined" (inspect-tool k "what" "quarantine")))
    (lisp-eval k 1 "(defun shout (s) (string-upcase s))")
    (is (search "SHOUT (skill" (inspect-tool k "what" "live_changes")))))

(run-suite "self-inspect")
