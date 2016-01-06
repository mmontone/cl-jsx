(in-package :cl-user)

(defpackage jsx.test
  (:use :cl
        :jsx
        :prove))

(in-package :jsx.test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-jsx)' in your Lisp.

(plan 2)

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
  (is 
   (jsx.parser::parse 'jsx.parser::element "<lala aaa=\"asd\" yes=\"adf\">{(loop for x from 1 to 10
                   do #<p>{hello}</p>)}
                   </lala>")
   '(:ELEMENT "lala" ((:ATTRIBUTE "aaa" ("asd")) (:ATTRIBUTE "yes" ("adf")))
     ((:JSX-ESCAPE "(loop for x from 1 to 10
                   do #<p>{hello}</p>)")
      "
                   "))))

(run-test-all)
