;; -*- lexical-binding: t -*-
(require 'dash)

(defmacro with-gensyms (symbol-list &rest body)
  "Evaluate BODY with each symbol in SYMBOL-LIST bound to a value
returned by `cl-gensym'.

TODO: Document extended calling convention."
  (declare (indent 1)
           (debug (sexp body)))
  (let ((sentinel (cons nil nil))
        gensym-bindings init-form-bindings inner-bindings)
    (dolist (symbol-spec (reverse symbol-list))
      (pcase symbol-spec
        ((or (and `(,value-symbol ,init-form))
             (and value-symbol
                  (let init-form sentinel))
             (pred (error "Invalid symbol-spec: %S")))
         (cl-assert (symbolp value-symbol))
         (let ((gensym-form `(cl-gensym ,(format "%s-" value-symbol)))
               (init-form-symbol (cl-gensym (format "%s-init-form" value-symbol))))
           (push `(,value-symbol ,gensym-form) gensym-bindings)
           (unless (eq init-form sentinel)
             (push `(,init-form-symbol ,init-form) init-form-bindings)
             (push `(list ,value-symbol ,init-form-symbol) inner-bindings))
           )))
      ;; (pcase symbol-spec
      ;;   ((or `(,value-symbol ,init-form)
      ;;        (and value-symbol
      ;;             (let init-form sentinel)))
      ;;    (cl-assert (symbolp value-symbol))
      ;;    (let ((gensym-form `(cl-gensym ,(format "%s-" value-symbol)))
      ;;          (form-symbol (cl-gensym (format "%s-form-" value-symbol))))
      ;;      (push `(,value-symbol ,gensym-form) gensym-bindings)
      ;;      (unless (eq init-form sentinel)
      ;;        (push `(,form-symbol ,value-symbol) form-bindings)
      ;;        (push `(list ,value-symbol ,init-form) inner-bindings)))))
      )
    `(let (,@init-form-bindings ,@gensym-bindings)
       `(let ,(list ,@inner-bindings) ,,@body)))


  ;; (let ((gensym-bindings
  ;;        (-map
  ;;         (lambda (value-symbol)
  ;;           (let ((gensym-form `(cl-gensym ,(format "%s-" value-symbol))))
  ;;             `(,value-symbol ,gensym-form)))
  ;;         symbol-list))
  ;;       inner-bindings
  ;;       form-bindings)
  ;;   `(let (,@form-bindings ,@gensym-bindings)
  ;;      `(let ,(list ,@inner-bindings) ,,@body)))
  )

;; (defmacro with-gensyms (symbol-list &rest body)
;;   "Evaluate BODY with each symbol in SYMBOL-LIST bound to a value returned by `cl-gensym'."
;;   (declare (indent 1)
;;            (debug (sexp body)))
;;   (cl-assert (-all? 'symbolp symbol-list))
;;   `(let ,(-map
;;           (lambda (symbol)
;;             `(,symbol (cl-gensym ,(format "%s-" symbol))))
;;           symbol-list)
;;      ,@body))


(defmacro with-eval-once (symbol-list &rest body)
  "Wrap BODY to ensure that each symbol in SYMBOL-LIST is evaluated once before any other form in BODY.

When the form returned by this macro is evaluated, each
expression named by SYMBOL-LIST will be evaluated exactly once in
the order in which it appears (which should generally match the
order in which it appears in the argument list of the macro,
although this may not always be sufficient with keyword
parameters).

Example:

  (defmacro thrice (x)
    (with-eval-once (x)
      `(list ,x ,x ,x)))

  (let ((counter 0))
    (thrice (cl-incf counter)))

  ==> returns (1 1 1)

This macro is deliberately designed so that the bindings for the
new gensyms shadow the original variables.  If you need access to
the original form, include a list of the form (SYMBOL
FORM-SYMBOL) in SYMBOL-LIST; this will bind SYMBOL as usual,
shadowing the original binding, but the original value will be
available as FORM-SYMBOL.
"
  (declare (indent 1)
           (debug (sexp body)))
  (let (form-bindings gensym-bindings inner-bindings bound-names)
    (dolist (symbol-spec (reverse symbol-list))
      (pcase symbol-spec
        ((or `(,value-symbol ,form-symbol)
             (and value-symbol
                  (let form-symbol
                    (cl-gensym (format "%s-form-" value-symbol))))
             (pred (error "Invalid symbol-spec: %S")))
         (cl-assert (symbolp value-symbol))
         (let ((gensym-form `(cl-gensym ,(format "%s-" value-symbol))))
           (dolist (symbol (list form-symbol value-symbol))
             (when (memq symbol bound-names)
               (error "Duplicate binding of %S in with-eval-once" symbol)))
           (push `(,form-symbol ,value-symbol) form-bindings)
           (push `(,value-symbol ,gensym-form) gensym-bindings)
           (push `(list ,value-symbol ,form-symbol) inner-bindings)
           ))))
    `(let (,@form-bindings ,@gensym-bindings)
       `(let ,(list ,@inner-bindings) ,,@body))))

(provide 'with-gensyms)
