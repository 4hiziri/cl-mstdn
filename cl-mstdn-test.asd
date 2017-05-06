#|
  This file is a part of cl-mstdn project.
  Copyright (c) 2017 takagi seiya (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-mstdn-test-asd
  (:use :cl :asdf))
(in-package :cl-mstdn-test-asd)

(defsystem cl-mstdn-test
  :author "takagi seiya"
  :license ""
  :depends-on (:cl-mstdn
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-mstdn"))))
  :description "Test system for cl-mstdn"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
