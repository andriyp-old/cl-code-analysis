(defpackage #:cl-syntaxic-analysis
  (:use #:cl
        #:iter 
        #:anaphora)
  (:export #:build-ast
           #:print-ast))
  
(in-package #:cl-syntaxic-analysis)

;(declaim (optimize (speed 0) (safety 0) (debug 3)))

;; множественный eq
(defmacro eqs (head &rest tail)
  `(and ,@(mapcar (lambda (x)
                    `(eq ,head ,x))
                  tail)))

;; домик для продукций
(defvar *productions* nil)

;; объявитель продукций
(defmacro defrule (name &rest constraints)
  `(setf *productions* (acons ',name ',constraints *productions*)))
  
;; субграмматика common-lisp
(defrule :<atom>
  (:or :symbol :number :string :character))
  
(defrule :<list>
  :LP (:* (:or :<atom> :<list>)) :RP)
  
(defrule :<compound-form>
  (:or :<defun>))  
  
(defrule :<defun>
  :LP (:symbol "defun") (:or (:symbol "nil") :<list>) (:* (:or :<compound-form> :<atom>)) :RP)
  
(defrule :<quote>
  :LP (:symbol "quote") (:or :<atom> :<list>) :RP)
  
(defrule :<cons>
  :LP (:symbol "cons") (:or :<atom> :<compound-form>) (:or :<atom> :<compound-form>) :RP)
  
(defrule :<car>
  :LP (:symbol "car") (:or :symbol :<compound-form>) :RP)
  
(defrule :<cdr>
  :LP (:symbol "cdr") (:or :symbol :<compound-form>) :RP)
  
(defrule :<list>
  :LP (:symbol "list") (:* (:or :<atom> :<compound-form>)) :RP)
  
(defrule :<+>
  :LP (:symbol "+") (:* (:or :symbol :number :<compound-form>)) :RP)
  
(defrule :<->
  :LP (:symbol "-") (:* (:or :symbol :number :<compound-form>)) :RP)  
  
(defrule :<*>
  :LP (:symbol "*") (:* (:or :symbol :number :<compound-form>)) :RP)

(defrule :</>
  :LP (:symbol "/") (:* (:or :symbol :number :<compound-form>)) :RP)  
    
;; фишечки                  
(defmacro %type  (x) `(car ,x))
(defmacro %value (x) `(cadr ,x))

;; 
(defun parentize (expr &optional (accum nil))
  (acase (car expr)    
    ((:RP nil) (values (nreverse accum) (cdr expr)))
    (:LP (multiple-value-bind (result rest)
             (parentize (cdr expr))
           (parentize rest (cons result accum))))
    (t (parentize (cdr expr) (cons it accum)))))
 
;; парсинг-по-ограничению
(defun parse-constraint (constraint tokens)
  (let* ((token (car tokens))
         (%fail `(:fail ',token))
         (result (list token))
         (parsed 1))
    ;(format t "constraint: ~a; token: ~a~%" constraint token)
    (if (cond ((null token) (setf result %fail))
              ;; атомарные, типовые ограничения
              ((eqs :LP constraint token) t)                    
              ((eqs :RP constraint token) t)
              ((and (listp token) (eqs :number constraint (%type token))) t)
              ((and (listp token) (eqs :symbol constraint (%type token))) t)
              ((and (listp token) (eqs :character constraint (%type token))) t)
              ((atom constraint)
               (aif (assoc constraint *productions*)
                    (return-from parse-constraint                               
                                (parse-production constraint tokens))
                    (setf result %fail)))
              ;; контролирующие ограничения
              ((case (%type constraint) ((:* :+) t) (t nil))
               (setf parsed 0
                     result nil)
               (loop (multiple-value-bind (v n)
                         (parse-constraint (%value constraint) tokens)
                       (when (eq :fail (car v))
                         (return (case (%type constraint)
                                   (:* t)
                                   (:+ (or (> parsed 0) 
                                           (setf result %fail
                                                 parsed 1))))))
                       (if (atom v)
                           (cond ((null result) (setf result v))
                                 ((atom result) (setf result (list result v)))
                                 (t (rplacd (last result) (list v))))
                           (setf result (nconc result v)))
                       (setf parsed (+ parsed n)    
                             tokens (nthcdr n tokens)))))
              ;; специализированные ограничения
              ((and (listp token)
                    (eqs :symbol (%type constraint)
                                 (%type token)))
               (if (string= (%value constraint) (%value token))
                   t (setf result %fail)))
              ;; составные ограничения
              ((eqs :or (%type constraint))
               (iter (for sub-constraint in (cdr constraint))
                     (if (not (eq :fail (car (parse-constraint sub-constraint tokens))))
                         (return t))
                     (finally (return (setf result %fail)))))
              ((eqs :and (%type constraint))
               (iter (for sub-constraint in (cdr constraint))
                     (if (not (parse-constraint sub-constraint tokens))
                         (return nil))
                     (finally (return t))))
              (t (setf result %fail)))
        (values result ;(progn (format t "parse-cst: ~a~%" result) result) 
                parsed))))        

;; парсинг-по-продукции
(defun parse-production (production tokens)
  (iter (for constraint in (cdr (assoc production *productions*)))
        (multiple-value-bind (parse-result parsed)
            (parse-constraint constraint tokens)
          (nconcing parse-result into result)
          (when (eq :fail (car parse-result))
                (setf production (cons :fail production))
                (finish))          
          (setf tokens (nthcdr parsed tokens)))
        (finally
          (return (if (eq :fail (car result))
                      (values (parentize result) (cdr tokens))
                      (values (cons production (parentize result)) tokens))))))                  

;; построение АСТ
(defun build-ast (tokens)
  (iter (until (null tokens))
        (collecting
            (iter (with result = nil)
                  (with rest = nil)
                  (for (production . constraints) in *productions*)
                  (multiple-value-bind (sub-result sub-rest)
                      (parse-production production tokens)
                    (if (not (eq :fail (car sub-result)))
                        (progn (setf tokens sub-rest)
                               (return sub-result)))
                    (if (null result)
                        (setf result sub-result))
                    (setf rest sub-rest))
                  (finally (setf tokens rest)
                           (return result))))))
                           
(defun print-ast (ast)
  (iter (for x in ast)
        (cond ((eq :fail (car x))
               (format t "!<undefined-fail> ::= ~a~%" (cdr x)))
              ((consp (car x))
               (format t "!<~a-fail> ::= ~a~%" (cdar x) (cdr x)))
              ((atom (car x))
               (format t "$~a ::= ~a~%" (car x) (cdr x))))))                           

;(print (parse/multi (assoc :<defun> *productions*)
;         '((:LP (:symbol "defun") (:or :<atom> :<list>) (:* (:or :<compound-form> :<atom> :<list>)) :RP))))

(print-ast (build-ast '((:symbol "lol") :LP (:symbol "defun") (:symbol "nil") (:number "55") :RP)))
;(print (build-ast '(:LP (:symbol "defun") (:symbol "nil") :LP (:number "55") :RP)))
 ;(defun build-ast (tokens)
;  ())

     
     
