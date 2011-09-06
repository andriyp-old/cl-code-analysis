;;;
;;; ( c l e x l z . g u i )
;;;

(load "clexlz.lisp")

(defpackage #:cl-lexical-analysis.gui 
  (:nicknames #:clexlz.gui)
  (:use #:cl #:clexlz #:gobject #:gtk)
  (:export #:main))
  
(in-package #:clexlz.gui)

(defun main ()
  (within-main-loop
    (let* ((window (make-instance 'gtk-window 
                                  :title "(clexlz :demo :gtk-powered)" 
                                  :width-request 640 
                                  :height-request 480 
                                  :window-position :center))
           (box (make-instance 'v-box))
           (code-text-buffer  (make-instance 'text-buffer 
                                             :text "код сюда"))
           (token-text-buffer (make-instance 'text-buffer 
                                             :text "здесь будет результат разбора"))
           (code-text-view  (make-instance 'text-view 
                                           :buffer code-text-buffer 
                                           :wrap-mode :word 
                                           :height-request 220))
           (token-text-view (make-instance 'text-view 
                                           :buffer token-text-buffer 
                                           :wrap-mode :word 
                                           :height-request 220))
           (button (make-instance 'button 
                                  :label "(get-tokens!)" 
                                  :height-request 40)))
      (container-add window box)
      (box-pack-start box code-text-view :expand nil)
      (box-pack-start box button :expand nil)
      (box-pack-start box token-text-view :expand nil)      
      (connect-signal window "destroy"
        (lambda (w)
          (declare (ignore w))
          (gtk-main-quit)))
      (g-signal-connect button "clicked" 
        (lambda (button)
          (declare (ignore button))
          (setf (text-buffer-text token-text-buffer)
                (format nil "~A" (clexlz:get-tokens-from-string 
                                   (text-buffer-text code-text-buffer))))))
      (widget-show window))))
