(defpackage :jsx.parser
  (:use :cl :esrap)
  (:export #:parse-jsx))

(in-package :jsx.parser)

(defrule tag-name (+ (not (or #\> space)))
  (:text t))

(defrule space (or #\Newline #\Tab #\Return #\Space)
  (:text t))

(defrule space* (* space)
  (:text t))

(defrule space+ (+ space)
  (:text t))

(defrule attribute (and attribute-name
                        space*
                        #\=
                        space*
                        attribute-value)
  (:destructure (name space equal space value)
                (list :attribute name value)))

(defrule attribute-name (+ (not (or #\/ #\> #\= space)))
  (:text t))

(defrule attribute-value (or jsx-escape
                             attribute-value-literal
                             )
  )

(defrule attribute-value-literal-plain-text (+ (not (or jsx-escape #\")))
  (:text t))

(defrule attribute-value-literal-text (+
                                       (or jsx-escape
                                           attribute-value-literal-plain-text))
  )

(defrule attribute-value-literal (and #\"
                                      attribute-value-literal-text
                                      #\")
  (:destructure (begin text end)
                text))

(defrule attributes (* (and space+ attribute space*))
  (:function (lambda (attributes)
               (mapcar #'second attributes))))

(defrule open-element (and #\<
                           (! #\/) tag-name
                           attributes
                           #\>)
  (:destructure (open _ tag-name attributes close)
                (list :open tag-name attributes)))

(defrule close-element (and #\< #\/ tag-name #\>)
  (:destructure (open _ tag-name close)
                (list :close tag-name)))

(defrule jsx-escape-text (+ (not (or #\} #\{)))
  (:text t))

(defrule jsx-escape (and #\{ jsx-escape-text #\})
  (:destructure (start text end)
                (list :jsx-escape text)))

(defrule text (+ (or plain-text
                     jsx-escape))
  (:function (lambda (x)
               x)))

(defrule plain-text (+ (not (or open-element
                                close-element
                                jsx-escape)))
  (:text t))

(defrule element-content (+ (or element
                                text)))

(defrule element (and open-element
                      (? element-content)
                      close-element)
  (:destructure (open-tag content close-tag)
                (assert (equal (second open-tag)
                               (second close-tag))
                        nil "Error parsing element: ~A" open-tag)
                (list :element
                      (second open-tag)
                      (third open-tag)
                      (flatten-content content))))

(defun flatten-content (content)
  (loop
     :with result := nil
     :for elem :in content
     :do
     (if (eql (first elem) :element)
         (push elem result)
         ;; else
         (loop
            :for x :in elem
            :do (push x result)))
     :finally (return (nreverse result))))

;; Toplevel

(defun parse-jsx (text &rest args &key (start 0) end junk-allowed raw)
  (apply #'parse 'element text args))

;; Tests

(parse 'tag-name "asdf")
(parse 'tag-name "adfd>") ;; Error
(parse 'tag-name "asdf adsf") ;; Error

(parse 'jsx-escape "{asdfas}")
(parse 'jsx-escape "{asd asd fads}")
(parse 'jsx-escape "{asdf{}") ;; Error
(parse 'jsx-escape "{asdfas") ;; Error

(parse 'attribute "asdf=\"afff\"")
(parse 'attribute "adfs = \" asdfasdf \"")
(parse 'attribute "asdf={aaa}")
(parse 'attribute "asdf=\"aaa {hola}\"")

(parse 'plain-text "asdf")
(parse 'text "asdf {aaa} asdf" )

(parse 'open-element "<adsf>")
(parse 'open-element "<asdf asdf={aaa}>")
(parse 'open-element "<asdf asdf=\"asdf\">")
(parse 'open-element "<asdf asdf=\"asdf {adsf}\">")

(parse 'close-element "</asdf>")

(parse 'element "<asdf>asdf</asdf>")
(parse 'element "<asdf></asdf>")
(parse 'element "<asdf>ff <foo></foo> asdf {haha}</asdf>")
(parse 'element "<asdf foo={bar}>haha</asdf>")
