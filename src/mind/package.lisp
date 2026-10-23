(defpackage #:pai-v3.mind
  (:use #:cl #:pai-v3.kernel)
  (:export
   ;; tokens
   #:estimate-tokens #:*chars-per-token*
   ;; dialogue
   #:receive-turn #:deliver-reply #:post-notice #:recent-dialogue
   #:dialogue-row-text #:dialogue-row-tokens #:dialogue-row-event-id
   #:dialogue-row-speaker #:dialogue-row-channel
   ;; provider
   #:provider-profile #:make-provider-profile #:complete #:provider-failure
   #:provider-failure-status #:provider-failure-partial
   ;; thoughts
   #:request-thought #:thought-row #:thought-steps #:define-tool #:*tools* #:think
   #:reflecting-p #:tools-for #:tools-in-hand #:tool-index-text #:tool-name #:*agent-defining*
   ;; web
   #:*http-get* #:*fetch-allow-private* #:html-text #:search-experience
   #:beads #:*beads-post* #:coding-agent-message #:coding-agent-for-token
   #:host-folder-request #:*host-files-post*
   #:observed-history #:observed-live #:model-capture #:*capture-hook*
   #:raise-concern #:close-concern #:open-concerns #:concern-row #:quiet-tick #:salience-of
   #:avatar-image-id #:docket-items #:open-work #:transition-work #:reopen-work
   #:carry-add #:carry-revise #:carry-consider-now #:carry-close #:carried-items #:carried-due #:carried-item
   ;; knowledge graph
   #:lineage-hop #:lineage-path #:render-lineage #:form-entity #:admit-fact #:retract-fact #:merge-entities
   #:graph-entity #:find-entities #:entity-facts #:fact-evidence #:graph-health #:graph-refused #:predicate-allows-p
   #:entities-named-in #:search-entities #:render-fact #:graph-search
   #:form-graph #:apply-formation #:formation-messages #:request-graph-formation
   ;; models
   #:profile-for #:*route-signals*
   #:*thought-policy* #:*thought-provider* #:*assemble-thought-context*
   #:*reply-delta-hook* #:interrupt-thought
   ;; persona
   #:render-identity #:*identity-order* #:adopt-trait #:adopt-influence
   #:affect-now #:render-affect #:modulator-drift #:drive-drift
   #:run-periodic #:*periodic-hooks* #:*quiet-starters* #:request-summaries #:working-summary #:*activity-hook* #:*progress-hook*
   #:add-schedule #:cancel-schedule #:run-due-schedules #:cron-next #:local-slot #:local-to-universal
   ;; context
   #:assemble-context #:define-context-profile #:define-slot-source
   ;; memory
   #:form-memory #:recall #:withheld-topics #:encode-vector #:decode-vector
   #:sensitivities-for #:*embedder* #:embed
   ;; grants
   ;; interaction recall
   #:handles-in #:handles-labelled #:experience-mentioning #:experience-near
   #:facts-near #:fact-statement #:live-fact #:entity-names
   #:open-worlds-for #:entity-knowledge #:fact-ended
   #:note-depiction #:note-depictions #:pictures-of #:depicted-in
   #:memory-strength #:memory-origin #:define-consolidation #:consolidation-budget-left #:define-embedding-source
   #:nearest-vectors #:unit-vector-octets
   #:define-open-items #:open-items-about #:mentions-any-p #:define-step-note #:local-stamp
   #:grant-topic #:revoke-topic #:granted-class #:authority-refused #:classify-status #:calibrated-chars-per-token))
