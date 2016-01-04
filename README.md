# CL-JSX

[![Quicklisp](http://quickdocs.org/badge/cl-jsx.svg)](http://quickdocs.org/cl-jsx/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

JSX in Common Lisp

## Embedded JSX syntax

To enable JSX syntax, evaluate:

```lisp
(jsx:enable-jsx-syntax)
```

JSX elements are specified with ``#<tag></tag>``

Example:

```lisp
(who:with-html-output-to-string (html) 
           (let ((hello "asdf")) 
             #<lala aaa="asd {when nil "false"}" yes={hello}>
             {loop 
             for x from 1 to 10
                 do #<p class="{when t "asdf"}">{hello}</p>}
             </lala>))
```
Result:

```
=> "<lala aaa='asd ' yes='asdf'>
             <p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p><p class='asdf'>asdf</p>
             </lala>"
```

At the moment, the rendering is done using CL-WHO, but other backends implementations are also possible:

Macro expansion of JSX above:

```lisp
(WITH-OUTPUT-TO-STRING (HTML NIL)
  (CL-WHO:WITH-HTML-OUTPUT (HTML NIL :PROLOGUE NIL :INDENT NIL)
    (LET ((HELLO "asdf"))
      (PROGN
       (CL-WHO:HTM
        (:LALA :AAA
         (WITH-OUTPUT-TO-STRING (#:ATTR755)
           (WRITE-STRING "asd " #:ATTR755)
           (LET ((#:ATTRVAL756 (WHEN NIL "false")))
             (WHEN #:ATTRVAL756 (PRINC #:ATTRVAL756 #:ATTR755))))
         :YES HELLO
         (CL-WHO:STR "
             ")
         (CL-WHO:STR
          (LOOP FOR X FROM 1 TO 10
                DO (PROGN
                    (CL-WHO:HTM
                     (:P :CLASS
                      (WITH-OUTPUT-TO-STRING (#:ATTR757)
                        (LET ((#:ATTRVAL758 (WHEN T "asdf")))
                          (WHEN #:ATTRVAL758 (PRINC #:ATTRVAL758 #:ATTR757))))
                      (CL-WHO:STR HELLO)))
                    NIL)))
         (CL-WHO:STR "
             ")))
       NIL))))
```
