(in-package :cl-user)

(defpackage jsx.test
  (:use :cl
        :jsx
        :prove))

(in-package :jsx.test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-jsx)' in your Lisp.

(plan 4)

(setf prove:*enable-colors* nil)

(deftest sanity-check
  (pass "PROVE is loaded and ready to go.")
  (ok (= 1 1)
      "Numeric equality: (= 1 1) => T.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4.")
  (is (mod (+ 10 2) 10)
      2
      "Modulus: (mod (+ 10 2) 10) => 2."))

(deftest parser-tests
  (is (jsx.parser::parse 'jsx.parser::tag-name "asdf")
      "asdf")
  (is-error (jsx.parser::parse 'jsx.parser::tag-name "adfd>") 'error) ;; Error
  (is-error (jsx.parser::parse 'jsx.parser::tag-name "asdf adsf") 'error) ;; Error

  (is (jsx.parser::parse 'jsx.parser::jsx-escape "{asdfas}")
      (list :jsx-escape "asdfas"))
  (is (jsx.parser::parse 'jsx.parser::jsx-escape "{asd asd fads}")
      (list :jsx-escape "asd asd fads"))
  (is (jsx.parser::parse 'jsx.parser::jsx-escape "{hello {world}}")
      (list :jsx-escape "hello {world}"))
  (is (jsx.parser::parse 'jsx.parser::jsx-escape "{asdf{}")
      (list :jsx-escape "asdf{"))
  (is-error (jsx.parser::parse 'jsx.parser::jsx-escape "{asdfas") 'error) ;; Error

  (is (jsx.parser::parse 'jsx.parser::attribute "asdf=\"afff\"")
      '(:ATTRIBUTE "asdf" ("afff")))
  (is (jsx.parser::parse 'jsx.parser::attribute "adfs = \" asdfasdf \"")
      '(:ATTRIBUTE "adfs" (" asdfasdf ")))
  (is (jsx.parser::parse 'jsx.parser::attribute "asdf={aaa}")
      '(:ATTRIBUTE "asdf" (:JSX-ESCAPE "aaa")))
  (is (jsx.parser::parse 'jsx.parser::attribute "asdf=\"aaa {hola}\"")
      '(:ATTRIBUTE "asdf" ("aaa " (:JSX-ESCAPE "hola"))))
  (is (jsx.parser::parse 'jsx.parser::plain-text "asdf")
      "asdf")
  (is (jsx.parser::parse 'jsx.parser::text "asdf {aaa} asdf" )
      '("asdf " (:JSX-ESCAPE "aaa") " asdf"))

  (is (jsx.parser::parse 'jsx.parser::open-element "<adsf>")
      '(:OPEN "adsf" NIL))
  (is (jsx.parser::parse 'jsx.parser::open-element "<asdf asdf={aaa}>")
      '(:OPEN "asdf" ((:ATTRIBUTE "asdf" (:JSX-ESCAPE "aaa")))))
  (is (jsx.parser::parse 'jsx.parser::open-element "<asdf asdf=\"asdf\">")
      '(:OPEN "asdf" ((:ATTRIBUTE "asdf" ("asdf")))))
  (is (jsx.parser::parse 'jsx.parser::open-element "<asdf foo=\"foo\" bar=\"bar\">")
      '(:OPEN "asdf" ((:ATTRIBUTE "foo" ("foo")) (:ATTRIBUTE "bar" ("bar")))))
  (is (jsx.parser::parse 'jsx.parser::open-element "<asdf asdf=\"asdf {adsf}\">")
      '(:OPEN "asdf" ((:ATTRIBUTE "asdf" ("asdf " (:JSX-ESCAPE "adsf"))))))

  (is (jsx.parser::parse 'jsx.parser::close-element "</asdf>")
      '(:close "asdf"))

  (is (jsx.parser::parse 'jsx.parser::element "<asdf>asdf</asdf>")
      '(:ELEMENT "asdf" NIL ("asdf")))
  (is (jsx.parser::parse 'jsx.parser::element "<asdf></asdf>")
      '(:ELEMENT "asdf" NIL NIL))
  (is (jsx.parser::parse 'jsx.parser::element "<asdf>ff <foo></foo> asdf {haha}</asdf>")
      '(:ELEMENT "asdf" NIL
        ("ff " (:ELEMENT "foo" NIL NIL) " asdf " (:JSX-ESCAPE "haha"))))
  (is
   (jsx.parser::parse 'jsx.parser::element "<asdf foo={bar}>haha</asdf>")
   '(:ELEMENT "asdf" ((:ATTRIBUTE "foo" (:JSX-ESCAPE "bar"))) ("haha")))
  (is-error
   (jsx.parser::parse 'jsx.parser::element "<asdf foo={bar}>haha</fdas>")
   'error)
  (is
   (jsx.parser::parse 'jsx.parser::element "<lala aaa=\"asd\" yes=\"adf\">{(loop for x from 1 to 10
                   do #<p>{hello}</p>)}
                   </lala>")
   '(:ELEMENT "lala" ((:ATTRIBUTE "aaa" ("asd")) (:ATTRIBUTE "yes" ("adf")))
     ((:JSX-ESCAPE "(loop for x from 1 to 10
                   do #<p>{hello}</p>)")
      "
                   "))))

(deftest reader-tests
  (is
   (with-input-from-string (s "<lala></lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala></lala>" NIL))
  (is
   (with-input-from-string (s "<lala>asdf</lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala>asdf</lala>" NIL))
  #+nil(is
   (with-input-from-string (s "<lala>asdf<foo></lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala>asdf<foo></lala>" NIL))
  (is
   (with-input-from-string (s "<lala>asdf</foo></lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala>asdf</foo></lala>" NIL))
  (is
   (with-input-from-string (s "<lala>asdf<foo></foo></lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala>asdf<foo></foo></lala>" NIL))
  (is
   (with-input-from-string (s "<lala foo=\"bar\">asdf<foo></foo></lala>")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala foo=\"bar\">asdf<foo></foo></lala>" NIL))
  (is
   (with-input-from-string (s "<lala foo=\"bar\">asdf<foo></foo></lala>asdf")
     (list (jsx::read-jsx s) (read-line s nil nil)))
   '("<lala foo=\"bar\">asdf<foo></foo></lala>" "asdf"))
  (is (with-input-from-string (s "<div>{#<div></div>}</div>")
        (list (jsx::read-jsx s) (read-line s nil nil)))
      '("<div>{#<div></div>}</div>" nil)))

(defmacro render-who (jsx)
  `(who:with-html-output-to-string (html)
     ,(jsx.who:emit-who (jsx.parser:parse-jsx jsx))))

(deftest test-who-rendering
  ;; Test

  (is (render-who "<lala>hello</lala>")
      "<lala>hello</lala>")
  (is (render-who "<foo></foo>")
      "<foo></foo>")
  (is-error (render-who "<foo>yes{asdf}</foo>") 'error)
  (let ((asdf "asdf"))
    (is (render-who "<foo>yes{asdf}</foo>") "<foo>yesasdf</foo>"))
  (is
   (let ((yes "yep"))
     (render-who "<foo bar={yes}>lalal</foo>"))
   "<foo bar='yep'>lalal</foo>")
  (is (let ((now 22)
            (haha nil))
        (render-who "<asdf now={now}>ff <foo></foo> asdf{haha}</asdf>"))
      "<asdf now='22'>ff <foo></foo> asdf</asdf>")
  (is (render-who "<foo bar={nil}></foo>")
      "<foo></foo>")
  (is (render-who "<foo bar={t}><bar>lala</bar></foo>")
      "<foo bar='bar'><bar>lala</bar></foo>")
  (is (render-who "<a href={22}>{#<a>hello{33}</a>}</a>")
      "<a href='22'><a>hello33</a></a>"))

(run-test-all)
