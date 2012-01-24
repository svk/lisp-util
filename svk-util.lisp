(defpackage #:svk-util
    (:use :cl :asdf :cl-utilities)
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
	     :clamp
	     :string-join
	     :intersperse
	     :defun-cached
	     :defun-memoized
	     :mapply
	     :run
	     :bunch
	     :destructuring-lambda
	     :slots-lambda
	     :destructuring-dolist
	     :null-free-list
	     :lines-in-file
	     :whitespace?
	     :left-trim
	     :right-trim
	     :trim
	     :maphash-to-table
	     :maphash-to-unordered-list
	     :apply-compose
	     :wrapping-result
	     :hash-table-keys
	     :hash-table-values
             :make-mock-mapper))
(in-package :svk-util)

(defun hash-table-keys (table)
  (loop :for x :being :the hash-keys :in table :collect x))

(defun hash-table-values (table)
  (loop :for x :being :the hash-values :in table :collect x))

(defun key-value->key (key value)
  (declare (ignore value))
  key)

(defun key-value->value (key value)
  (declare (ignore key))
  value)

(defun apply-compose (&rest functions)
  #'(lambda (&rest arguments)
      (reduce #'apply functions
	      :initial-value arguments
	      :from-end t)))

(defun wrapping-result (f)
  #'(lambda (&rest arguments)
      (list (apply f arguments))))

(defun maphash-to-unordered-list (function table)
  (collecting (maphash (apply-compose #'collect (wrapping-result function)) table)))

(defun maphash-to-table (function table)
  (let ((new-table (make-hash-table :test (hash-table-test table))))
    (maphash #'(lambda (key value)
		 (setf (gethash key new-table)
		       (funcall function key value)))
	     table)))

(defun whitespace? (char)
  (find char '(#\Space #\Tab #\Newline #\Return) :test #'eql))

(defun left-trim (string &optional (filter-away? #'whitespace?))
  (let ((filtering t))
    (coerce
     (loop
	:for ch :in (coerce string 'list)
	:when (or (not filtering)
		  (not (funcall filter-away? ch)))
	:collect (progn (setf filtering nil)
			ch))
     'string)))

(defun right-trim (string &optional (filter-away? #'whitespace?))
  (coerce (reverse (left-trim (reverse (coerce string 'list)) filter-away?)) 'string))

(defun trim (string &optional (filter-away? #'whitespace?))
  (left-trim (right-trim string filter-away?) filter-away?))
      

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
    #'(lambda (&rest args)
	(or (gethash args memo-table)
	    (setf (gethash args memo-table)
	      (apply function args))))))

(defmacro defun-memoized (name lambda-list &body body)
  (let ((memo-table (gensym))
	(args (gensym)))
    `(let ((,memo-table (make-hash-table :test #'equal)))
       (defun ,name (&rest ,args)
	 (or (gethash ,args ,memo-table)
	     (setf (gethash ,args ,memo-table)
		   (destructuring-bind ,lambda-list
		       ,args
		     ,@body)))))))

(defmacro defun-cached (purge-name get-name &body calculation)
  (let ((cached (gensym))
	(is-cached? (gensym)))
    `(let ((,cached nil)
	   (,is-cached? nil))
       (defun ,purge-name () (setf ,cached nil ,is-cached? nil))
       (defun ,get-name ()
	 (setf ,cached
	       (if ,is-cached?
		   ,cached
		   (progn
		     (setf ,is-cached? t)
		     ,@calculation)))))))

;; Legacy macro from before I knew about destructuring-bind
(defmacro unwrap (names present &body body)
  `(destructuring-bind ,names ,present ,@body))

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
  `(destructuring-dolist ,unwrapspec ,coll ,body))

(defun %destructuring-lambda-body (arg-names arg-unwrapspecs body)
  (assert (eql (length arg-names) (length arg-unwrapspecs)))
  (when (null arg-names)
    (return-from %destructuring-lambda-body body))
  (let ((arg-name (car arg-names))
	(arg-spec (car arg-unwrapspecs)))
    (list `(destructuring-bind ,arg-spec ,arg-name
	     ,@(%destructuring-lambda-body (cdr arg-names)
					  (cdr arg-unwrapspecs)
					  body)))))

(defmacro destructuring-lambda (arg-unwrapspecs &body body)
  (let ((arg-names (mapcar #'(lambda (x) (declare (ignore x)) (gensym))
			   arg-unwrapspecs)))
    `(lambda ,arg-names
       ,@(%destructuring-lambda-body arg-names arg-unwrapspecs body))))

(defmacro slots-lambda (arg-slots &body body)
  (let ((arg-name (gensym)))
    `(lambda (,arg-name)
       (with-slots ,arg-slots ,arg-name ,@body))))

(defmacro destructuring-dolist ((unwrapspec coll) &body body)
  (let ((temp (gensym)))
    `(dolist (,temp ,coll)
       (destructuring-bind ,unwrapspec ,temp ,@body))))

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

(defun clamp (min value max)
  (max min (min max value)))

(defun mapply (function argument-lists)
  (mapcar #'(lambda (list) (apply function list)) argument-lists))

#+allegro
(defun run-program (command-line
		    &key
		    (search t)
		    (input-from-string nil)
		    (input-from-stream nil)
		    (output-to-string nil)
		    (output-to-stream nil))
  ;; Mostly untested!
  (when (not search)
    (warn (format nil "cannot avoid searching for ~a in allegro" (car command-line))))
  (let* ((command-line-array (make-array (list (length command-line))
					 :initial-contents command-line))
	 (results (excl.osi:command-output command-line-array
					   :whole t
					   :input (if input-from-stream
						      #'(lambda (ws)
							  (write-line (read-line input-from-stream)
								      ws))
						      input-from-string))))
    (when output-to-string
      (return-from run-program results))
    (when output-to-stream
      (princ results output-to-stream))))

#+sbcl
(defun run-program (command-line
		    &key
		    (search t)
		    (input-from-string nil)
		    (input-from-stream nil)
		    (output-to-string nil)
		    (output-to-stream nil))
  (let ((program (car command-line))
	(arguments (cdr command-line)))
    (when (or (and input-from-string input-from-stream)
	      (and output-to-string output-to-stream))
      (error "illegal arguments to run-program: duplicated input or output"))
    (let ((standard-args (list :search search
			       :error nil
			       :wait t)))
      (flet ((run (input-args)
	       (cond (output-to-string
		      (with-output-to-string (stream)
			(apply #'sb-ext:run-program (append (list program
								  arguments
								  :output stream)
							    standard-args
							    input-args))))
		     (t
		      (progn
			(apply #'sb-ext:run-program (append (list program
								  arguments
								  :output output-to-stream)
							    standard-args
							    input-args))
			nil)))))
	(cond (input-from-string
	       (with-input-from-string (stream input-from-string)
		 (run (list :input stream))))
	      (t
	       (run (list :input input-from-stream))))))))

(defun null-free-list (&rest args)
  (remove-if-not #'identity args))

(defun bunch (list size)
  (loop
     :until (null list)
     :collect (multiple-value-bind (bunch next-list)
		  (first-n list size)
		(setf list next-list)
		bunch)))

(defun lines-in-file (filename)
  (with-open-file (f filename :direction :input)
    (loop
       :for x = (read-line filename nil nil)
       :until (null x)
       :collect x)))
