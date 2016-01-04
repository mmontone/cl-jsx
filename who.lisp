(defpackage :jsx.who
  (:use :cl)
  (:export #:compile-jsx))

;; cl-who JSX compiler

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defun emit-who (jsx)
  (%emit-who (first jsx) jsx))

(defgeneric %emit-who (element-type element))

(defmethod %emit-who ((type (eql :element)) element)
  (destructuring-bind (_ tag-name attributes content) element
    `(,(make-keyword tag-name)
       ,@(emit-attributes attributes)
       ,@(emit-content content))))

(defun emit-content (content)
  (loop
     :for elem :in content
     :collect
     (cond
       ((stringp elem)
        `(who:str ,elem))
       ((eql (first elem) :jsx-escape)
        (read-from-string (second elem)))
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
      (read-from-string (second value))
      ;; else
      (let ((attr (gensym "ATTR")))
        `(with-output-to-string (,attr)
           ,@(loop
                :for x :in value
                :collect (cond
                           ((stringp x)
                            `(write-string ,x ,attr))
                           ((eql (first x) :jsx-escape)
                            `(princ ,(read-from-string (second x)) ,attr))))))))

;; Test

(emit-who (jsx.parser:parse-jsx "<foo></foo>"))
(emit-who (jsx.parser:parse-jsx "<foo>lala</foo>"))
(emit-who (jsx.parser:parse-jsx "<foo>yes{asdf}</foo>"))
(emit-who (jsx.parser:parse-jsx "<foo bar={yes}>lalal</foo>"))
(emit-who (jsx.parser:parse-jsx "<asdf now={now}>ff <foo></foo> asdf {haha}</asdf>"))
