(defpackage #:svk-util
    (:use :cl :asdf)
    (:export :with-chdir
             :make-memoized
             :unwrap
             :derived-nullable
             :curry
             :begins-with
             :ends-with
	     :with-inflate-stream
	     :infinite-loop
	     :append-to-list
	     :make-appendable
	     :append-to-appendable
	     :appendable->list
	     :dolist-unwrap
	     :first-n
	     :string-join
	     :intersperse
             :make-mock-mapper))
(in-package :svk-util)

#+allegro
(defmacro with-chdir (name &body body)
  (let ((old-dir-sym (gensym)))
    `(let ((,old-dir-sym (excl.osi:current-directory)))
       (excl.osi:chdir ,name)
       (unwind-protect (progn ,@body)
	 (excl.osi:chdir ,old-dir-sym)))))

(defun make-memoized (function)
  ;; Primary intended use is
  ;;   (make-memoized #'lookup-exact-frequency)
  ;; or
  ;;   (make-memoized #'lookup-binned-frequency)
  ;; once for each disambiguation context.
  (let ((memo-table (make-hash-table :test #'equal)))
    #'(lambda (args)
	(or (gethash args memo-table)
	    (setf (gethash args memo-table)
	      (funcall function args))))))

(defun %create-accessors (names root)
  (cond
   ((null names) nil)
   ((consp names)
    (nconc 
     (%create-accessors (car names)
		       `(car ,root))
     (%create-accessors (cdr names)
		       `(cdr ,root))))
   (t
    (list (cons names root)))))

(defun %create-let (names root body)
  (let ((bindings nil))
    (dolist (name-accessor (%create-accessors names
					     root))
      (push (list (car name-accessor)
		  (cdr name-accessor))
	    bindings))
    `(let ,bindings ,@body)))

(defmacro unwrap (names present &body body)
  (let ((my-sym (gensym)))
    `(let ((,my-sym ,present))
       ,(%create-let names my-sym body))))

(defmacro derived-nullable (parents value)
  `(if (some #'null (list ,@parents))
       nil
     ,value))

(defun %make-curried (f args)
  (lambda (&rest more-args) (apply f (append args more-args))))

(defmacro curry (f &rest args-macro)
  `(apply #'%make-curried (list ,f (list ,@args-macro))))

(defun begins-with (subseq seq &key (test #'eql))
  (do ((l (coerce seq 'list) (cdr l))
       (sl (coerce subseq 'list) (cdr sl)))
      ((or (null l)
	   (null sl))
       (null sl))
    (unless (funcall test (car l) (car sl))
      (return-from begins-with nil))))

(defun ends-with (subseq seq &key (test #'eql))
  (begins-with (reverse subseq)
	       (reverse seq)
	       :test test))

(defun make-mock-mapper (&key (test #'equal) (initial 1))
  (let ((table (make-hash-table :test test))
	(next (1- initial)))
    (lambda (key)
      (or (gethash key table nil)
	  (setf (gethash key table)
	    (incf next))))))

#+allegro
(defmacro with-inflate-stream ((variable filename) &body body)
  (let ((file-var (gensym)))
    `(let ((,file-var (open ,filename :direction :input)))
       (unwind-protect
	   (progn (util.zip:skip-gzip-header ,file-var)
		  (let ((,variable (make-instance 'util.zip:inflate-stream :input-handle ,file-var)))
		    ,@body))
	 (close ,file-var)))))

(defmacro infinite-loop (&body body)
  `(loop :do ,@body))

(defmacro append-to-list (element head tail)
  (let ((elt-sym (gensym)))
    `(let ((,elt-sym ,element))
       (if (null ,head)
	   (if (null ,tail)
	       (setf ,head (setf ,tail (cons ,elt-sym nil)))
	     (error "head null while tail is non-null"))
	 (setf ,tail (setf (cdr ,tail) (cons ,elt-sym nil)))))))

(defun make-appendable () (cons nil nil))
(defun append-to-appendable (element appendable)
  (append-to-list element (car appendable) (cdr appendable)))
(defun appendable->list (appendable)
  (car appendable))

(defmacro dolist-unwrap ((unwrapspec coll) &body body)
  (let ((temp (gensym)))
    `(dolist (,temp ,coll)
       (unwrap ,unwrapspec
	       ,temp
	       ,@body))))

(defun first-n (list n)
  (if (or (null list)
	  (<= n 0))
      (values nil list)
    (multiple-value-bind (a b)
	(first-n (cdr list) (1- n))
      (values (cons (car list) a) b))))

(defun intersperse (elt list)
  (cond ((or (null list)
	     (null (cdr list)))
	 list)
	(t
	 (cons (car list)
	       (cons elt
		     (intersperse elt (cdr list)))))))

(defun string-join (joiner list)
  (apply #'concatenate
	 (cons 'string
	       (intersperse joiner list))))
