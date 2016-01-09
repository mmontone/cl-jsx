(in-package :jsx)

;; Reader syntax

(defun read-until-close-tag (tag-name stream content)
  (loop
     :with current-tag-chars := nil
     :with tag-start := nil
     :with close-tag-start := nil
     :for char := (read-char stream)
     :do
     (block continue
       (let ((current-tag-name
              (coerce
               (reverse current-tag-chars)
               'string)))
         #+debug(format t "~S~%" (list :char char :tag tag-name
                                       :current current-tag-name
                                       :start tag-start :close close-tag-start))
         (cond
           ((and (not tag-start)
                 (eql char #\<))
            ;; Opening a tag
            (setf tag-start t))
           ((and tag-start
                 (eql char #\/))
            ;; Closing a tag
            (setf close-tag-start t)
            (setf tag-start nil))
           ((and tag-start
                 (not (eql char #\/)))
            ;; It is tag open
            (unread-char char stream)
            (read-jsx-content stream content :parse-open nil)
            (setf current-tag-chars nil)
            (setf tag-start nil)
            (setf close-tag-start nil)
            (return-from continue))
           ((and close-tag-start
                 (eql char #\>)
                 (equalp current-tag-name tag-name))
            ;; Tag-name closed
            (vector-push-extend char content)
            (return-from read-until-close-tag))            
           ((and close-tag-start (not (eql char #\>)))
            ;; Build the closing tagname
            (push char current-tag-chars))
           (t
            ;; Nothing of the above, clear state variables
            (setf current-tag-chars nil)
            (setf tag-start nil)
            (setf close-tag-start nil)))
         (vector-push-extend char content)))))

(defun read-jsx (stream &key (parse-open t))
  (let ((content (make-array 10000 :fill-pointer 0)))
    (read-jsx-content stream content :parse-open parse-open)
    (coerce content 'string)))

(defun read-jsx-content (stream content &key (parse-open t))
  ;; First, read the jsx element tag name
  (let ((tag-name nil))
    (when parse-open
      (let ((open-char (read-char stream t :eof t)))
        (assert (eql open-char #\<)
                nil "Error parsing JSX")
        (vector-push-extend open-char content)))

    (loop
       :for char := (peek-char nil stream)
       :while (not (member char (list #\> #\Space)))
       :do
       (push char tag-name)
       (read-char stream)
       (vector-push-extend char content)
       :finally (setf tag-name (coerce (nreverse tag-name) 'string)))

    ;; Consume to the end of the opening tag
    (loop
       :for char := (read-char stream)
       :while (not (eql char #\>))
       :do (vector-push-extend char content)
       :finally (vector-push-extend char content))

    ;; Read until last </tag-name> is found
    (read-until-close-tag tag-name stream content)))

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
