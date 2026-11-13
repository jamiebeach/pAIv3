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

(deftest failures-are-tallied-beyond-the-five-shown
  ;; Bead v3-vpp: a pattern older than the latest five was out of sight.
  (with-temp-kernel (k)
    (dotimes (i 6)
      (append! k "provider.call.failed" (obj "profile" "graph" "model" "m" "class" "model" "status" 200
                                             "reason" "empty response (finish: length · 8192 completion tokens)"
                                             "milliseconds" 1)))
    (append! k "provider.call.failed" (obj "profile" "graph" "model" "m" "class" "model" "status" 200
                                           "reason" "empty response (finish: error · 6148 completion tokens, 6144 reasoning · 24,256 reasoning characters streamed)"
                                           "milliseconds" 1))
    (append! k "provider.call.failed" (obj "profile" "chat" "model" "m" "class" "model" "status" 200
                                           "reason" "empty response (finish: stop · 59 completion tokens, 0 reasoning · 0 reasoning characters streamed)"
                                           "milliseconds" 1))
    (dotimes (i 5)
      (append! k "provider.call.failed" (obj "profile" "chat" "model" "m" "class" "transient" "status" 0
                                             "reason" "transport: timeout" "milliseconds" 1)))
    (let ((text (inspect-tool k "what" "failures")))
      (is (search "Last 13 failed calls" text))
      (is (search "graph out of room (reasoning) ×7" text) "a burn ended with finish error is still a burn")
      (is (search "chat empty ×1" text) "an empty reply that was not reasoning stays empty")
      (is (search "chat transient ×5" text)))))

(run-suite "self-inspect")
