;;;; package.lisp

(defpackage #:ralike
  (:use #:cl)
  (:export
           #:calc-chart
           #:calc-equidist-hist
           #:calc-summary
           #:column
           #:columns
           #:linear-regression
           #:mean
           #:median
           #:modal-value
           #:parse-common-logfile 
           #:summary
           #:transpose))
