(in-package :jsx)

;; Reader syntax

(defun read-jsx (stream &key (parse-open t))
  ;; First, read the jsx element tag name
  (let ((tag-name nil)
        (content nil)
        (current-tag-chars nil))

    (when parse-open
      (let ((open-char (read-char stream t :eof t)))
        (assert (eql open-char #\<)
                nil "Error parsing JSX")
        (push open-char content)))

    (loop
       :for char := (peek-char nil stream nil nil t)
       :while (and char
                   (not (member char (list #\> #\Space))))
       :do
       (push char tag-name)
       (read-char stream nil nil t)
       (push char content)
       :finally (setf tag-name (coerce (nreverse tag-name) 'string)))

    ;; Read until last </tag-name> is found
    (loop
       :with tag-start := nil
       :with close-tag-start := nil
       :with tags-found := 0
       :for char := (read-char stream nil nil t)
       :while char
       :do
       (let ((current-tag-name (coerce (reverse current-tag-chars) 'string)))
         #+debug(format t "~A~%" (list :char char :tag tag-name
                                       :start tag-start :close close-tag-start
                                       :found tags-found))
         (cond
           ((and tag-start
                 (string= current-tag-name tag-name)
                 (member char (list #\space #\newline #\tab #\>)))
            (incf tags-found))
           ((and close-tag-start
                 (string= current-tag-name tag-name)
                 (eql char #\>))
            (if (zerop tags-found)
                (progn
                  (push char content)
                  (return-from read-jsx (coerce (nreverse content) 'string)))
                (decf tags-found)))
           ((and (not tag-start)
                 (eql char #\<))
            (setf tag-start t))
           ((and tag-start
                 (eql char #\/))
            (setf close-tag-start t)
            (setf tag-start nil))
           ((let ((new-tag-name (coerce (reverse (cons char current-tag-chars))
                                        'string)))
              (and (or tag-start close-tag-start)
                   (let ((pos (search new-tag-name tag-name)))
                     (and pos (zerop pos)))))
            (push char current-tag-chars))
           (t
            (setf current-tag-chars nil)
            (setf tag-start nil)
            (setf close-tag-start nil))))
       (push char content))

    ;; Error
    (error "Error reading JSX element: ~A" tag-name)))

;; Test
#+nil(progn
       (with-input-from-string (s "<lala></lala>")
         (values (read-jsx s) (read-line s nil nil)))

       (with-input-from-string (s "<lala>asdf</lala>")
         (values (read-jsx s) (read-line s nil nil)))

       (with-input-from-string (s "<lala>asdf<foo></lala>")
         (values (read-jsx s) (read-line s nil nil)))

       (with-input-from-string (s "<lala>asdf</foo></lala>")
         (values (read-jsx s) (read-line s nil nil)))

       (with-input-from-string (s "<lala>asdf<foo></foo></lala>")
         (values (read-jsx s) (read-line s nil nil)))

       (with-input-from-string (s "<lala foo=\"bar\">asdf<foo></foo></lala>")
         (values (read-jsx s) (read-line s nil nil))))

(named-readtables:defreadtable :jsx
  (:merge :standard)
  (:dispatch-macro-char
   #\# #\<
   (lambda (stream c1 c2)
     (jsx.who:emit-who
      (jsx.parser:parse-jsx
       (concatenate 'string "<" (read-jsx stream :parse-open nil)))))))


(defun enable-jsx-syntax ()
  (named-readtables:in-readtable :jsx))

;; Test

;; (who:with-html-output-to-string (html)
;;   #<lala>hello</lala>)
