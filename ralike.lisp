;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; R-alike. statistical tool. inspired by  R         ;;;;
;;;; Patrick Krusenotto, August 2016                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:ralike)

;;;;  Generic Stuff

(defun sq (x)
  "Quadrat einer Zahl"
  (* x x))

(defun transpose (x)
  "Transponierte einer Matrix in Listendarstellung"
  (apply #'mapcar (cons 'list x)))

(defun column (data coln)
  (etypecase data
    (cons  (loop for line in data collect
                (nth (1- coln) line)))
    (simple-array (loop for line across data collect
                       (elt line (1- coln))))))

(defun columns (data &rest colnums)
  (etypecase data
    (cons  (loop for line in data collect
                (loop for coln in colnums collect
                     (elt line (1- coln)))))
    (simple-array (loop for line across data collect
                       (loop for coln in colnums collect
                            (elt line (1- coln)))))))

(defun map-columns (data &rest functions)
  (loop
     for line in data
     collect
       (loop
          for f in functions
          for item in line
          collect (funcall f item))))

;;;; text- and logfile-processing

(defun read-textfile (name)
  (with-open-file (s name)
    (loop
       for l = (read-line s nil nil)
       until (null l) 
       collect l)))

(defparameter *testline* "64.242.88.10 - - [07/Mar/2004:17:53:45 -0800] \"GET /twiki/bin/search/Main/SearchResult?scope=text®ex=on&search=Office%20*Locations[^A-Za-z] HTTP/1.1\" 200 7771")

(defun parse-common-logformat (line)
  ;; FIXIT: Lame! Only 4000/s
  (cl-ppcre:register-groups-bind (ip x y date method url prot response size)
;      ("([\\.\\d]+)\\s(.+?)\\s(.+?)\\s\\[(.*?)\\]\\s\\\"(\\S+)\\s(.*?)\\s(\\S+)\\\"\\s(\\d+)\\s(\\d+)" line)
      ("(.+?)\\s(.+?)\\s(.+?)\\s\\[(.*?)\\]\\s\\\"(\\S+)\\s(.*?)\\s(\\S+)\\\"\\s(\\d+)\\s(\\d+)" line)
    (list ip x y date method url prot (parse-integer response) (parse-integer size))))

(defun parse-common-logfile (filename)
  (mapcar #'parse-common-logformat (read-textfile filename)))

;;;; statistical basics

(defun mean (a)
  (/ (etypecase a
       (cons (loop for x in a sum x))
       (array (loop for x across a sum x)))
     (length a)))

(defun median (a)
  (elt (sort (copy-seq a) #'<)
       (truncate (/ (length a) 2))))

;;;; Statistical Summary

(defun calc-summary (list)
  "Statistical summary of a list of numbers"
  (let* ((sorted (sort (copy-list list) #'<))
        (n (length list))
        (mean (/ (apply #'+ list) n)))   ; mean
    (values n                            ; N
            (apply #'+ list)
            (car sorted)                 ; min
            (nth (truncate  n 4) sorted) ; 1/4
            (nth (truncate  n 2) sorted) ; 1/2
            (nth (truncate (* 3 n) 4) sorted) ;3/4
            (car (last sorted))          ; max
            mean
            (sqrt (/ (loop for x in list sum (* (- x mean) (- x mean))) n))))) ;stddev

(defun summary (list &optional multiline)
  "formated output of  CALC-SUMMARY's results"
  (multiple-value-bind (n sum min first median third max mean sdev)
      (calc-summary list)
    (if multiline
        (format 
         t
         "N~14d.~%Σ   ~16,4f~%Mean~16,4f~%Min ~16,4f~%1stQ~16,4f~%Med.~16,4f~%3rdQ~16,4f~%Max ~16,4f~%σx  ~16,4f~%" 
         n sum mean min first median third max sdev)
        (format 
         t 
         "n ~d  Σ ~a  Mean ~a  Min ~a  1stQ ~a  Median ~a  3rdQ ~a  Max ~a σ ~a"
         n sum mean min first median third max sdev))))

(defun calc-chart (lst &optional (f #'identity))
  "make a chart of the elements of a list"
  (let ((htab (make-hash-table :test #'equal))
        (chart))
    (mapc (lambda (x) (incf (gethash (funcall f x) htab 0))) lst)
    (maphash (lambda (k v) (setf chart (cons (list v k) chart))) htab)
    (sort chart (lambda (a b) (> (car a) (car b))))))

(defun modal-value (list)
  (cadar (calc-chart list)))

(defun calc-equidist-hist (l a b n)
  "create a histogramm from list l consisting of n intervals im the range of a bis b"
  (let ((d (/ (- b a) n))
        (h (make-array n)))
    (dolist (x l)
      (let ((k (truncate (- x a) d)))
        (if (<= 0 k (1- n))
            (incf (aref h k))
            (if (eql x b)
                (incf (aref h (1- n)))))))
    h))

;;;; Linear Regression

(defun linear-regression (pairs)
  "calculate a linear regression from a list of pairs ((x1 y1).
   return n and m for y = mx+n and a lisp-function for f."
  (multiple-value-bind (n mx my)
      (loop for p in pairs
         sum (first p) into xs
         sum (second p) into ys
         sum 1 into n
         finally
           (return
             (values
              n
              (/ xs n)
              (/ ys n))))
    (multiple-value-bind (b sx sy)
        (loop
           for p in pairs
           for x   = (first  p)
           for y   = (second p)
           for dx  = (- x mx)
           for dy  = (- y my)
           for qdx = (sq dx)
           for qdy = (sq dy)
           sum qdx       into sqdx
           sum qdy       into sqdy
           sum (* dx dy) into num
           sum qdx       into den
           finally (return (values (/ num den)
                                   (sqrt (/ sqdx (1- n)))
                                   (sqrt (/ sqdy (1- n))))))
      (let ((a (- my (* b mx))))
        (values a                               ; Achsenabschnitt
                b                               ; Steigung
                (/ (* b sx) sy)                 ; Korelationskoeffizient
                (lambda (x) (+ (* b x) a))))))) ; Geraden-Funktion

;;;; CL-USER> (linear-regression '((1.9 22) (2.5 33) (3.2 30) (3.8 42) (4.7 38) (5.5 49) (5.9 42) (7.2 55)))
;;;; 15.728319   Achsenabschnitt n
;;;; 5.3364105   Steigung        m
;;;; 0.9162191   Korrelationskoeffizient r


