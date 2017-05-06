#|
  This file is a part of cl-mstdn project.
  Copyright (c) 2017 takagi seiya (meirvg@gmail.com)
|#

#|
  Author: takagi seiya (meirvg@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-mstdn-asd
  (:use :cl :asdf))
(in-package :cl-mstdn-asd)

(defsystem cl-mstdn
  :version "0.1"
  :author "takagi seiya"
  :license ""
  :depends-on (:dexador
               :cl-json
               :cl-annot)
  :components ((:module "src"
                :components
                ((:file "cl-mstdn"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-mstdn-test))))
