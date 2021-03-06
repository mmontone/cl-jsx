(defpackage :jsx.who
  (:use :cl)
  (:export #:emit-who))

(in-package :jsx.who)

;; cl-who JSX compiler

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defun emit-who (jsx)
  (%emit-who (first jsx) jsx))

(defgeneric %emit-who (element-type element))

(defmethod %emit-who ((type (eql :element)) element)
  (destructuring-bind (_ tag-name attributes content) element
    `(progn
       (who:htm
        (,(make-keyword tag-name)
          ,@(emit-attributes attributes)
          ,@(emit-content content)))
       nil)))

(defun emit-content (content)
  (loop
     :for elem :in content
     :collect
     (cond
       ((stringp elem)
        `(who:str ,elem))
       ((eql (first elem) :jsx-escape)
        `(who:str ,(read-escaped-jsx (second elem))))
       ((eql (first elem) :element)
        (emit-who elem)))))

(defun emit-attributes (attributes)
  (loop
     :for (_ attr value) :in attributes
     :collect
     (make-keyword attr)
     :collect
     (emit-attribute-value value)))

(defun emit-attribute-value (value)
  (if (eql (first value) :jsx-escape)
      (read-escaped-jsx (second value))
      ;; else
      (let ((attr (gensym "ATTR")))
        `(with-output-to-string (,attr)
           ,@(loop
                :for x :in value
                :collect (cond
                           ((stringp x)
                            `(write-string ,x ,attr))
                           ((eql (first x) :jsx-escape)
                            (let ((val (gensym "ATTRVAL")))
                              `(let ((,val ,(read-escaped-jsx (second x))))
                                 (when ,val (princ ,val ,attr)))))))))))
(defvar *spliced-read* t)

(defun read-escaped-jsx (string)
  (let ((trimmed-string (string-trim (list #\space #\newline #\tab) string)))
    (if (and *spliced-read*
             (not (member (aref trimmed-string 0) (list #\" #\()))
             (some (lambda (separator)
                (find separator trimmed-string))
              (list #\space #\newline #\tab)))
        ;; Assume this is not an atom
        (read-from-string (format nil "(~A)" string))
        ;; else, read normally
        (read-from-string string))))
