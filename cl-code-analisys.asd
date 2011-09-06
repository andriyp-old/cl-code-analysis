;;;
;;; #:CL-CODE-ANALISYS
;;;

(defpackage #:cl-code-analisys-system
  (:use #:cl #:iterate #:anaphora #:asdf))

(in-package #:cl-code-analisys-system)
  
(defsystem :cl-code-analisys
    :description "Code-analisys for common-lisp subset."
    :components ((:file "cl-lexical-analisys" "cl-syntaxic-analisys")))
