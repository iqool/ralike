(defparameter *testlogfile*
  (remove-if
   (lambda (x) (< (length x) 2))
   (mapcar #'parse-common-logformat
           (read-textfile "/home/patrick/Desktop/iqool.de-access_log"))))
