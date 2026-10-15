(defsystem "pai-v3"
  :description "pAI v3 substrate."
  :license "Proprietary; not for publication"
  :depends-on ("cffi" "shasht" "ironclad" "babel" "bordeaux-threads" "dexador" "hunchentoot" "sb-posix" "local-time")
  :serial t
  :components
  ((:module "src/kernel"
    :serial t
    :components ((:file "package")
                 (:file "sqlite")
                 (:file "json")
                 (:file "encoding")
                 (:file "events")
                 (:file "ledger")
                 (:file "projections")
                 (:file "settings")
                 (:file "scheduler")
                 (:file "transport")))
   (:module "src/mind"
    :serial t
    :components ((:file "package")
                 (:file "tokens")
                 (:file "dialogue")
                 (:file "provider")
                 (:file "images")
                 (:file "llm")
                 (:file "tools")
                 (:file "thought")
                 (:file "persona")
                 (:file "context")
                 (:file "affect")
                 (:file "schedule")
                 (:file "summary")
                 (:file "grants")
                 (:file "memory")
                 (:file "web")
                 (:file "beads")
                 (:file "coding-agent")
                 (:file "observability")
                 (:file "concerns")
                 (:file "docket")
                 (:file "graph")
                 (:file "graph-import")
                 (:file "graph-read")
                 (:file "graph-form")
                 (:file "graph-recover")
                 (:file "graph-correct")
                 (:file "episodes")
                 (:file "experience")
                 (:file "settings-tools")))
   (:module "src/trust"
    :serial t
    :components ((:file "package")
                 (:file "webauthn")
                 (:file "auth")
                 (:file "keys")
                 (:file "recovery")
                 (:file "secrets")
                 (:file "keychain-windows" :if-feature :win32)))
   (:module "src/self"
    :serial t
    :components ((:file "package")
                 (:file "eval")
                 (:file "inspect")
                 (:file "source")
                 (:file "shell")))
   (:module "src/host"
    :serial t
    :components ((:file "host")
                 (:file "backup")))
   (:module "src/api"
    :serial t
    :components ((:file "server")
                 (:file "fleet")
                 (:file "notify")
                 (:file "pwa")))
   (:module "src/cli"
    :serial t
    :components ((:file "main")))))
