(in-package #:pai-v3.mind)

;;; Memories from conversation (bead v3-clp.24). v2 sealed conversation
;;; episodes: a stretch of talk with the operator, closed by two quiet hours
;;; or by its size, written up once by a model as a faithful summary with
;;; its subjects, retrieval cues and unresolved threads. Here each sealed
;;; episode becomes an episode memory, so recall and rumination have
;;; material without the agent remembering to remember. The write-up is
;;; checked before it is kept; an episode is sealed once, and how far the
;;; conversation is covered is recorded in the ledger.

(define-setting "memory.episodes"
  :type :boolean :default :false :group "Memory"
  :description "Seal finished stretches of conversation with the operator into episode memories.")
(define-setting "memory.episode_quiet_seconds"
  :type :integer :min 60 :max 604800 :default 7200 :group "Memory"
  :description "How long the conversation must be quiet before its episode is sealed.")
(define-setting "memory.episode_max_turns"
  :type :integer :min 2 :max 200 :default 24 :group "Memory"
  :description "Most turns and replies in one episode; a longer stretch is cut into several.")
(define-setting "memory.episode_max_characters"
  :type :integer :min 1000 :max 200000 :default 36000 :group "Memory"
  :description "Most characters of conversation in one episode.")
(define-setting "memory.episodes_daily_usd"
  :type :number :min 0 :max 100 :default 0.5d0 :group "Memory"
  :description "Most episode sealing may spend in a UTC day.")

(define-event-type "memory.episode.sealed"
  :schema (("first_event_id" :integer) ("last_event_id" :integer) ("turns" :integer)
           ("memory_id" :string) ("subjects" :array) ("retrieval_cues" :array)
           ("unresolved_threads" :array) ("model" :string)))

(define-projection "memory_episodes"
  :kind :inline
  :consumes ("memory.episode.sealed")
  :tables ("CREATE TABLE IF NOT EXISTS memory_episodes (event_id INTEGER PRIMARY KEY, audience TEXT NOT NULL, first_event_id INTEGER NOT NULL, last_event_id INTEGER NOT NULL, turns INTEGER NOT NULL, memory_id TEXT NOT NULL, at TEXT NOT NULL)"
           "CREATE INDEX IF NOT EXISTS memory_episodes_last_idx ON memory_episodes(audience,last_event_id)")
  :reduce (lambda (db e)
            (let ((p (event-payload e)))
              (sql-exec db "INSERT INTO memory_episodes(event_id,audience,first_event_id,last_event_id,turns,memory_id,at) VALUES(?1,'operator',?2,?3,?4,?5,?6)"
                        (event-id e) (obj-get p "first_event_id") (obj-get p "last_event_id")
                        (obj-get p "turns") (obj-get p "memory_id") (event-at e)))))

(defun episodes-sealed-through (kernel)
  (or (sql-value (kernel-db kernel) "SELECT last_event_id FROM memory_episodes WHERE audience=?1 ORDER BY last_event_id DESC LIMIT 1" "operator") 0))

;;; Finding the next episode

(defun split-episode (rows now &key quiet max-turns max-characters)
  "The first episode in ROWS (event-id kind speaker rendered at, oldest
first) when it is finished, else nil. An episode ends at a quiet gap or at
its size; it is finished when something follows it, or when the
conversation has been quiet since."
  (let ((taken '()) (used 0) (previous nil))
    (dolist (row rows)
      (let ((at (parse-iso8601 (fifth row))))
        (when (and taken
                   (or (>= (- at previous) quiet)
                       (>= (length taken) max-turns)
                       (> (+ used (length (fourth row))) max-characters)))
          (return-from split-episode (nreverse taken)))
        (push row taken)
        (incf used (length (fourth row)))
        (setf previous at)))
    (when (and taken (>= (- now previous) quiet))
      (nreverse taken))))

(defun next-episode (kernel)
  (split-episode (sql-rows (kernel-db kernel)
                           "SELECT event_id,kind,speaker,rendered,at FROM working_dialogue WHERE audience=?1 AND event_id>?2 AND kind IN (?3,?4) ORDER BY event_id LIMIT 400"
                           "operator" (episodes-sealed-through kernel) "turn" "reply")
                 (now)
                 :quiet (setting kernel "memory.episode_quiet_seconds")
                 :max-turns (setting kernel "memory.episode_max_turns")
                 :max-characters (setting kernel "memory.episode_max_characters")))

;;; Writing it up

(defparameter +episode-tool+
  (obj "type" "function"
       "function"
       (obj "name" "write_episode"
            "description" "Record this conversation episode as a memory."
            "parameters"
            (obj "type" "object"
                 "properties"
                 (obj "synopsis" (obj "type" "string" "description" "What happened, in a short paragraph, in the first person as me.")
                      "subjects" (obj "type" "array" "items" (obj "type" "string"))
                      "retrieval_cues" (obj "type" "array" "items" (obj "type" "string")
                                            "description" "Words or phrases someone might use when this episode should come back to mind.")
                      "unresolved_threads" (obj "type" "array" "items" (obj "type" "string")
                                                "description" "Questions or promises left open at the end."))
                 "required" (list "synopsis" "subjects" "retrieval_cues" "unresolved_threads")))))

(defun episode-messages (kernel rows)
  (list (obj "role" "system"
             "content" (format nil "You are ~a, writing a memory of a conversation you had with the operator. ~
Write a faithful, retrieval-oriented account of the episode below: keep the operator's concrete words, the people, ~
places and things named, decisions, events in their life, and what was left open. Do not add anything the episode ~
does not say. If it is about a story or other fiction, say so, so it is never remembered as real. ~
Call write_episode exactly once." (kernel-agent-id kernel)))
        (obj "role" "user"
             "content" (format nil "~{~a~^~%~}" (mapcar (lambda (r) (format nil "[S~d] ~a" (first r) (fourth r))) rows)))))

(defparameter *episode-synopsis-chars* 2400)

(defun %strings (value &optional (most 12))
  "VALUE's non-empty strings, trimmed and without repeats, at most MOST."
  (let ((out '()))
    (dolist (v (if (listp value) value '()))
      (when (stringp v)
        (let ((s (string-trim '(#\Space #\Tab #\Newline) v)))
          (when (and (plusp (length s)) (<= (length s) 200) (not (member s out :test #'string-equal)))
            (push s out)))))
    (let ((kept (nreverse out))) (subseq kept 0 (min most (length kept))))))

(defun admit-episode (proposal)
  "The checked write-up as (synopsis subjects cues threads), or nil and why."
  (let ((synopsis (obj-get proposal "synopsis")))
    (cond ((not (stringp synopsis)) (values nil "no synopsis"))
          ((< (length (string-trim " " synopsis)) 20) (values nil "the synopsis is too short to remember"))
          (t (list (let ((s (string-trim '(#\Space #\Newline) synopsis)))
                     (subseq s 0 (min (length s) *episode-synopsis-chars*)))
                   (%strings (obj-get proposal "subjects"))
                   (%strings (obj-get proposal "retrieval_cues") 16)
                   (%strings (obj-get proposal "unresolved_threads") 8))))))

(defun episode-memory-text (rows synopsis cues threads)
  (format nil "~a~@[~%Still open: ~{~a~^; ~}~]~@[~%Cues: ~{~a~^, ~}~]~%(Conversation of ~a, S~d to S~d.)"
          synopsis threads cues (subseq (fifth (first rows)) 0 10) (first (first rows)) (first (car (last rows)))))

(defun seal-episode (kernel work)
  "Seal the oldest finished episode of conversation into a memory."
  (let ((rows (next-episode kernel)))
    (when rows
      (let* ((profile (profile-for kernel "episodes"))
             (reply (complete kernel profile (episode-messages kernel rows)
                              :tools (list +episode-tool+) :work-id (getf work :id) :lane "episodes"))
             (call (find "write_episode" (obj-get reply "tool_calls")
                         :key (lambda (c) (obj-get (obj-get c "function") "name")) :test #'equal))
             (proposal (and call (ignore-errors (json-parse (obj-get (obj-get call "function") "arguments"))))))
        (multiple-value-bind (admitted why) (and proposal (admit-episode proposal))
          (unless admitted
            (error 'handler-failure :class :model
                                    :reason (or why "the episode was not written up with write_episode")))
          (destructuring-bind (synopsis subjects cues threads) admitted
            (let ((memory (form-memory kernel :kind "episode"
                                              :content (episode-memory-text rows synopsis cues threads)
                                              :topic (format nil "~{~a~^; ~}" (subseq subjects 0 (min 3 (length subjects))))
                                              :owner "self" :source-event-id (first (car (last rows))))))
              (append! kernel "memory.episode.sealed"
                       (obj "first_event_id" (first (first rows)) "last_event_id" (first (car (last rows)))
                            "turns" (length rows) "memory_id" memory "subjects" subjects
                            "retrieval_cues" cues "unresolved_threads" threads
                            "model" (provider-profile-model profile)))
              (obj "memory" memory "turns" (length rows)))))))))

(define-handler "memory.seal" :run #'seal-episode :paid t :estimate-microusd 10000)

(defun request-episode-sealing (kernel)
  "When an episode of conversation is finished, seal it."
  (when (and (eq t (setting kernel "memory.episodes"))
             (not (work-pending-p kernel "memory.seal" "operator"))
             (< (spent-today kernel "episodes") (* 1000000 (setting kernel "memory.episodes_daily_usd")))
             (next-episode kernel))
    (request-work kernel "memory.seal" "operator" (obj) :priority :maintenance)))
