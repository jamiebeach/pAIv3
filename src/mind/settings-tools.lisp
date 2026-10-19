(in-package #:pai-v3.mind)

;;; The agent's view of its settings (bead v3-fu8.15). It can read every
;;; setting and change those declared as agent-changeable, within their
;;; bounds; each change is a recorded event with its reason. Everything else
;;; is the operator's, and the agent is told so it can propose instead.

(define-tool "settings"
  :description "Look at my settings: models and routes, budgets, thresholds, backups. Give part of a name or group to narrow the list."
  :parameters (obj "type" "object" "properties" (obj "about" (obj "type" "string")))
  :available :operator-trust
  :core '("introspection" "reflection")
  :run (lambda (kernel id args)
         (declare (ignore id))
         (let* ((about (string-downcase (obj-get args "about" "")))
                (rows (remove-if-not (lambda (s)
                                       (or (zerop (length about))
                                           (search about (string-downcase (obj-get s "name")))
                                           (search about (string-downcase (obj-get s "group")))))
                                     (settings-list kernel))))
           (if rows
               (format nil "~{~a~^~%~}"
                       (mapcar (lambda (s)
                                 (format nil "~a = ~a~:[~; (changed)~]; ~a~a"
                                         (obj-get s "name") (json-encode (obj-get s "value"))
                                         (eq t (obj-get s "changed"))
                                         (obj-get s "description")
                                         (if (equal "agent-within-bounds" (obj-get s "changed_by"))
                                             " I may change this within bounds." "")))
                               rows))
               "No setting matches that."))))

(define-tool "change_setting"
  :description "Change one of my settings that I am allowed to change, with the reason. Others belong to the operator; propose those instead."
  :parameters (obj "type" "object"
                   "properties" (obj "name" (obj "type" "string")
                                     "value" (obj "description" "The new value, as JSON would write it.")
                                     "reason" (obj "type" "string"))
                   "required" (list "name" "value" "reason"))
  :available :operator-trust
  :run (lambda (kernel id args)
         (handler-case
             (progn
               (change-setting kernel (obj-get args "name" "") (obj-get args "value")
                               :origin (obj "principal_type" "self" "principal_id" (kernel-agent-id kernel)
                                            "thought_id" id)
                               :reason (obj-get args "reason" ""))
               (format nil "~a is now ~a." (obj-get args "name") (json-encode (obj-get args "value"))))
           (setting-refused (e)
             (format nil "Not changed: ~a" (setting-refused-reason e))))))
