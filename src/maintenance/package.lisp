(defpackage #:pai-v3.maintenance
  (:use #:cl #:pai-v3.kernel)
  (:import-from #:pai-v3.kernel
                #:%events-after #:%state #:%set-state #:%apply-one
                #:kernel-lock #:projection-name #:projection-kind
                #:projection-version #:projection-tables #:projection-consumes
                #:projection-derivation)
  (:export #:census #:census-markdown
           #:rebuild-projection #:verify-projection
           #:model-derived-rebuild-refused))
