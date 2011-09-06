;; конструктор продукций ошибок
(defun syntax-error (error-template form)
  (list :fail
        (with-output-to-string (s)
          (format s "SYNTAX-ERROR in ~a : ~a"
            form error-template))))
  
;; шаблоны для синтаксических ошибок
(defconstant +general-fail+ "cannot parse !")
(defconstant +plus-fail+ "inconsistency with `+` control !")