(defsystem "advent-of-code"
  :version "0.1.0"
  :author "0xcacti"
  :license "MIT"
  :depends-on 
    (:cl-ppcre)
  :components 
    ((:module "src"
      :components
      ((:module "utils"
        :components ((:file "package")
        (:file "common" :depends-on ("package"))
        (:file "math" :depends-on ("package"))
        (:file "queue" :depends-on ("package"))
        ))))))
