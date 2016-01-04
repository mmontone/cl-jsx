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

    ;; Read until </tag-name> is found
    (loop
       :with close-tag-start := 0
       :for char := (read-char stream nil nil t)
       :while char
       :do
       (let ((current-tag-name (coerce (reverse current-tag-chars) 'string)))
         (cond
           ((and (string= current-tag-name tag-name)
                 (eql char #\>))
            (push char content)
            (return-from read-jsx (coerce (nreverse content) 'string)))
           ((and (zerop close-tag-start)
                 (eql char #\<))
            (incf close-tag-start))
           ((and (eql close-tag-start 1)
                 (eql char #\/))
            (incf close-tag-start))
           ((let ((new-tag-name (coerce (reverse (cons char current-tag-chars))
                                        'string)))
              (and (eql close-tag-start 2)
                   (let ((pos (search new-tag-name tag-name)))
                     (and pos (zerop pos)))))
            (push char current-tag-chars))
           (t
            (setf current-tag-chars nil)
            (setf close-tag-start 0))))
       (push char content))

    ;; Error
    (error "Error reading JSX stream")))

;; Test

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
  (values (read-jsx s) (read-line s nil nil)))

(set-dispatch-macro-character
 #\# #\<
 (lambda (stream c1 c2)
   (jsx.who:emit-who
    (jsx.parser:parse-jsx
     (concatenate 'string "<" (read-jsx stream :parse-open nil))))))

;; Test

;; (who:with-html-output-to-string (html)
;;   #<lala>hello</lala>)
