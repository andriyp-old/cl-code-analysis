(defpackage #:cl-lexical-analysis  
  (:nicknames #:clexlz)
  (:use #:cl)
  (:export #:get-tokens-from-file
           #:get-tokens-from-string))
  
(in-package #:clexlz)

(declaim (optimize (speed 3) (safety 0) (debug 0))
         (inline get-tokens-from-string
                 get-tokens-from-file
                 whitespace-char-p
                 sequencer-char-p
                 ws/seq-p))

(defun whitespace-char-p (char)
  (or (char= char #\Tab)
      (char= char #\Space)
      (char= char #\Newline)))
      
(defun sequencer-char-p (char)
  (or (char= char #\()
      (char= char #\))
      (char= char #\")))

(defun ws/seq-p (char)
  (or (whitespace-char-p char)
      (sequencer-char-p char)))       
      
(defun get-tokens (code-stream)
  (let ((action 'read) (state 'S)
        (token nil) (table nil) (char nil))
    (loop
      (when (eq action 'read)
        (setf char (read-char code-stream nil)))
      (when (null char)
        (setf action 'finish
              char #\Space))
      (when (null action)
        (setf action 'read))
      (case state
        (S (case char 
             (#\( (push :LP table))
             (#\) (push :RP table))
             (#\; (setf state 'CL))
             (#\# (setf state 'C-P!A))
             (#\" (setf state 'S-P!A))
             (otherwise
               (cond ((digit-char-p char)
                        (push char token)
                        (setf state 'N-P!A))
                     ((not (whitespace-char-p char))
                        (push char token)
                        (setf state 'I-P))))))
        (CL (when (char= char #\Newline)
              (setf state 'S)))
        (N-P!A (cond ((digit-char-p char)
                        (push char token))
                     ((or (char= char #\.)
                          (char= char #\/))
                        (push char token)
                        (setf state 'N-P!B))
                     ((ws/seq-p char)
                        (push `(:number ,(coerce (nreverse token) 'string)) table)
                        (setf state 'S 
                              token nil                              
                              action nil))
                     (t (push char token)
                        (setf state 'I-P))))
        (N-P!B (cond ((digit-char-p char)
                        (push char token))
                     ((ws/seq-p char)
                        (push `(:number ,(coerce (nreverse token) 'string)) table)
                        (setf state 'S
                              token nil                              
                              action nil))
                     (t (push char token)
                        (setf state 'I-P))))
        (I-P (if (ws/seq-p char)
                 (progn (push `(:symbol ,(coerce (nreverse token) 'string)) table)
                        (setf state 'S
                              token nil
                              action nil))
                 (push char token)))
        (S-P!A (case char
                 (#\\ (setf state 'S-P!B))
                 (#\" (push `(:string ,(coerce (nreverse token) 'string)) table)
                      (setf token nil
                            state 'S))
                 (otherwise (push char token))))
        (S-P!B (if (char= char #\") 
                   (push #\" token)                          
                   (progn (push #\\ token)
                          (push char token)))
               (setf state 'S-P!A))
        (C-P!A (if (char= char #\\)
                   (setf state 'C-P!B)
                   (progn (push char token)
                          (setf state 'I-P))))
        (C-P!B (cond ((char= char #\")
                        (push #\" token)
                        (push `(:character ,(coerce (nreverse token) 'string)) table)
                        (setf state 'S
                              token nil))
                     ((ws/seq-p char)
                        (push `(:character ,(coerce (nreverse token) 'string)) table)
                        (setf state 'S
                              token nil                                
                              action nil))
                     (t (push char token)))))
    (when (eq action 'finish)
      (return (nreverse table))))))
        
(defun get-tokens-from-string (code)
  (with-input-from-string (stream code)
    (get-tokens stream)))
     
(defun get-tokens-from-file (filename)
  (with-open-file (stream filename)
    (get-tokens stream)))    
    
(print (get-tokens-from-string "(defun foo 567 #\\& (x y) '(+ x y))"))    

     
     
