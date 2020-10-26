(require 'nameless-defstruct)

(nameless-defstruct parse-lambda- parsed
  "See ‘parse-lambda’."
  lambda-sym argspec docstring declare-form interactive-form body)

(cl-defun parse-lambda--internal (lambda-sym argspec body)
  (cl-assert (listp body))
  (parse-lambda-make-parsed
   :lambda-sym lambda-sym
   :argspec argspec
   :docstring (and (stringp (car body)) (pop body))
   :declare-form (and (eq 'declare (car-safe (car body)))
                      (pop body))
   :interactive-form (and (eq 'interactive (car-safe (car body)))
                          (pop body))
   :body body))

(cl-defun parse-lambda (lambda-form)
  "Given a ‘lambda’-like form, produce a ‘parse-lambda-parsed’
object.

The fields of the resulting object are:

‘parse-lambda-parsed-lambda-sym’:
  The ‘car’ of LAMBDA-FORM.

‘parse-lambda-parsed-argspec’:
  The arguments (i.e. ‘cadr’) of LAMBDA-FORM.

‘parse-lambda-parsed-docstring’:
  The doc string of LAMBDA-FORM, or nil.

‘parse-lambda-parsed-declare-form’:
  The ‘declare’ form of LAMBDA-FORM, or nil.

‘parse-lambda-parsed-interactive-form’:
  The ‘interactive’ form of LAMBDA-FORM, or nil.

‘parse-lambda-parsed-body’:
  All the remaining forms of LAMBDA-FORM."
  (pcase-exhaustive lambda-form
    ((and `(,lambda-sym ,argspec . ,body)
          (guard (symbolp lambda-sym))
          (guard (listp argspec)))
     (parse-lambda--internal lambda-sym argspec body))))

(defun parse-lambda-body (body)
  "Given the ‘cdr’ of a ‘lambda’-like form (or ‘cddr’ of a
‘defun’ form, etc.), produce a ‘parse-lambda-parsed’ object.

See ‘parse-lambda’ for details."
  (parse-lambda--internal nil t body))

(defun parse-lambda-unparse (parsed)
  "Given an object produced by ‘parse-lambda’, produce a list
suitable for use as the ‘cdr’ of a ‘lamba’ form."
  (pcase-exhaustive parsed
    ((cl-struct parse-lambda-parsed
                lambda-sym
                argspec
                docstring
                declare-form
                interactive-form
                body)
     (append (and lambda-sym (list lambda-sym))
             (if (eq t argspec) nil
               (list argspec))
             (and docstring (list docstring))
             (and declare-form (list declare-form))
             (and interactive-form (list interactive-form))
             body))))

(defun parse-lambda-apply-to-body (fun body)
  "Given function and the body of a ‘lambda’-like form, apply FUN
to the regular forms of BODY and return a new ‘lambda’-like form
whose regular forms are the list returned by FUN.

For instance, given the input

  (\"Doc.\" (interactive) (foo) (bar))

the result is

  \\=`(\"Doc.\" (interactive) ,@(FUN \\='((foo) (bar))))"
  (let ((parsed (parse-lambda-body body)))
    (setf (parse-lambda-parsed-body parsed)
          (funcall fun (parse-lambda-parsed-body parsed)))
    (parse-lambda-unparse parsed)))

(provide 'parse-lambda)
