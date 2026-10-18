(in-package #:pai-v3.mind)

;;; LLM profiles and routes (bead v3-fu8.15). Both are settings, edited from
;;; the phone, by file import, never by flags.
;;;
;;; llm.profiles  [{"name": "openrouter-main", "base_url": "https://openrouter.ai/api/v1",
;;;                 "model": "...", "secret": "openrouter", "local": false,
;;;                 "max_tokens": 4096, "temperature": 0.7, "read_timeout": 120,
;;;                 "microusd_per_1k_prompt": 0, "microusd_per_1k_completion": 0,
;;;                 "data_policy": "zdr"}, ...]
;;; llm.routes    {"chat":     [{"profile": "fast", "when": {"arousal": {">": 0.7}}},
;;;                             {"profile": "openrouter-main"}],
;;;                "thinking": [{"profile": "local-qwen"}],
;;;                "default":  [{"profile": "openrouter-main"}]}
;;;
;;; A route is an ordered list; the first entry whose conditions hold is
;;; used. Conditions compare named signals (affect, budget strain, time of
;;; day...) that *ROUTE-SIGNALS* supplies; an entry naming a signal that is
;;; not available does not match. A purpose without a route uses "default".

(defun %validate-profiles (profiles)
  (loop for p in profiles
        for name = (and (hash-table-p p) (obj-get p "name"))
        unless (and name (stringp (obj-get p "base_url")) (stringp (obj-get p "model")))
          return "each profile needs a name, base_url and model"
        when (> (count name profiles :key (lambda (q) (obj-get q "name")) :test #'equal) 1)
          return (format nil "two profiles are named ~a" name)))

(defun %validate-routes (routes)
  (loop for purpose being the hash-keys of routes using (hash-value entries)
        unless (and (listp entries)
                    (every (lambda (e) (and (hash-table-p e) (stringp (obj-get e "profile")))) entries))
          return (format nil "route ~a must be a list of entries each naming a profile" purpose)))

(define-setting "llm.profiles"
  :type :array :default '() :group "Models" :validate #'%validate-profiles
  :description "The model endpoints available: OpenRouter, Nous, local servers. Keys are named by secret, never stored here.")

(define-setting "llm.routes"
  :type :object :default (obj) :group "Models" :validate #'%validate-routes
  :description "Which profile each purpose uses (chat, thinking, work, summary, judge, kg...), in order, with optional conditions on signals such as affect.")

(defvar *route-signals*
  (lambda (kernel thought) (declare (ignore kernel thought)) (obj))
  "(lambda (kernel thought)) -> object of signal name -> number, used by
route conditions. The affect module installs a richer one.")

(defvar *thought-provider* nil
  "A provider profile that overrides routing (tests and development only).")

(defun %condition-holds-p (when signals)
  (or (null when)
      (loop for signal being the hash-keys of when using (hash-value test)
            for value = (obj-get signals signal)
            always (and (realp value) (hash-table-p test)
                        (loop for op being the hash-keys of test using (hash-value limit)
                              always (and (realp limit)
                                          (funcall (cond ((string= op ">") #'>) ((string= op ">=") #'>=)
                                                         ((string= op "<") #'<) ((string= op "<=") #'<=)
                                                         (t #'=))
                                                   value limit)))))))

(defun %profile (p)
  (make-provider-profile :name (obj-get p "name") :base-url (obj-get p "base_url") :model (obj-get p "model")
                         :local (eq t (obj-get p "local"))
                         ;; A local server without a key is sent none.
                         :secret (obj-get p "secret" (if (eq t (obj-get p "local")) nil "provider"))
                         :max-tokens (obj-get p "max_tokens" 4096)
                         :vision (eq t (obj-get p "vision"))
                         :all-tools (eq t (obj-get p "all_tools"))
                         :reasoning (let ((r (obj-get p "reasoning"))) (and (hash-table-p r) r))
                         :temperature (obj-get p "temperature")
                         :read-timeout (obj-get p "read_timeout" 120)
                         :microusd-per-1k-prompt (obj-get p "microusd_per_1k_prompt" 0)
                         :microusd-per-1k-completion (obj-get p "microusd_per_1k_completion" 0)))

(defun profile-for (kernel purpose &optional (signals (obj)))
  "The provider profile PURPOSE should use now. Signals a source-class
PROVIDER-FAILURE, visible and not retried, when nothing is set up."
  (or *thought-provider*
      (let* ((routes (setting kernel "llm.routes"))
             (entries (or (obj-get routes purpose) (obj-get routes "default")))
             (profiles (setting kernel "llm.profiles")))
        (loop for e in entries
              when (%condition-holds-p (obj-get e "when") signals)
                do (let ((p (find (obj-get e "profile") profiles
                                  :key (lambda (q) (obj-get q "name")) :test #'equal)))
                     (if p
                         (return (%profile p))
                         (error 'provider-failure :class :source
                                                  :reason (format nil "route ~a names profile ~a, which does not exist"
                                                                  purpose (obj-get e "profile")))))
              finally (error 'provider-failure :class :source
                                               :reason (format nil "no model is set up for ~a: add a profile to llm.profiles and a route in llm.routes"
                                                               purpose))))))
