(defpackage #:pai-v3.recall
  (:use #:cl #:pai-v3.kernel #:pai-v3.mind)
  (:documentation "Interaction recall (docs/interaction-recall.md): what the agent perceives
or does brings to mind, once and briefly, its impression of what it touches.
System-1 work, on the mind's general extension points; the mind does not
know this module exists.")
  (:export #:impression #:render-impression #:revise-impression #:anchor-of-entity))
