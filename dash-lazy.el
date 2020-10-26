;; -*- lexical-binding: t -*-

;; Lazy values and lazy lists for Dash.
;;
;; LAZY VALUES
;;
;; The most basic forms in this module are the macro ‘-lazy’, which
;; produces lazy values by deferring the evaluation of some form(s),
;; and ‘-force’, which converts lazy values to non-lazy values by
;; evaluating the form(s) passed to ‘-lazy’.  Evaluation happens at
;; most once per call to ‘-lazy’; forcing the same lazy value more
;; than once simply returns the value produced the first time it was
;; forced.
;;
;; While it is possible to create values are doubly-lazy (or triply-,
;; etc.) by applying ‘-lazy’ for forms that produce lazy values, the
;; existence of multply-lazy values is transparent because ‘-force’
;; always evaluates lazy values until it reaches a non-lazy value.
;;
;; Most forms whose names start with “-lazy” produce lazy values, and
;; those whose names start with “-force” produce non-lazy values.
;;
;; LAZY LISTS
;;
;; Most of the functions in this module are concered with lazy lists.
;; operates on lazy lists.  All regular list values (i.e. values for
;; which ‘listp’ returns t), are considered lazy lists, but in
;; addition, a lazy value which evaluates to a ‘cons’ cell is also a
;; lazy list.
;;
;; A list may be lazy in either its values (i.e. the ‘car’ or each
;; cell), its structure (the ‘cdr’) of each cell, or both.  Functions
;; which consume lists that are lazy their structure but not
;; necessarily their values are described as operating on “lazy
;; lists”.  Those which are designed to operate on lists which are
;; lazy in both their structure and their values are described
;; operating on “lazy lists of lazy values”.  (No functions in this
;; module are designed to consume lists which are lazy in their values
;; but not their structure.)
;;
;; As a general rule, functions that are lazy analogs of higher-order
;; Dash list functions will force the values in a lazy list before
;; passing them to their function arguments, and those that are not
;; may pass lazy values to their function arguments.
;;
;; The primary forms for producing lazy lists are ‘-lazy-list’ and
;; ‘-lazy-list*’, which produce structurally-lazy lists that can be
;; converted to regular lists using ‘-force-cdrs’.
;;
;; The forms ‘-lazy-car-list’ and ‘lazy-car-list*’ produce lists which
;; are lazy in their values rather than their structure.  (There is no
;; point in making the structure of these lists lazy because the
;; structure is trivial to create.)
;;
;; INTEGRATION WITH OTHER PACKAGES
;;
;; Lazy lists can be traversed in for clauses of ‘cl-loop’ forms using
;; the new “in-lazy” and “on-lazy” keywords, which are analogous to the
;; standard “in” and “on” keywords.  The “in-lazy” keyword traverses lazy
;; lists and forces their values as they are encountered.  The “on-lazy”
;; keyword only forces the structure of the lists it traverses.
;;
;; Interoperation with generators is supported by the ‘-lazy-to-iter’
;; function, which produces an iterator over a values of a lazy list
;; of lazy values, and ‘-lazy-from-iter’, which produces a lazy list
;; that runs an iterator as it is consumed.

(require 'dash)
(require 'dash-functional)
(require 'generator)
(require 'parse-lambda)
(require 'with-gensyms)
(require 'subr-x)

(defvar -lazy--marker (make-symbol "lazy"))

(defsubst -lazy* (fn &rest args)
  "Create and return a thunk that, when evaluated with ‘-force’, calls FN with ARGS.

As this is an ordinary function, FN and ARGS are evaluated eagerly.

See also ‘-lazy’."
  (vector -lazy--marker nil fn args))

(defmacro -lazy (&rest body)
  "Lazy evaluation primitive.

Combine unevaluated forms in BODY into an thunk that can be
evaluated with ‘-force’ to compute (apply #'progn BODY).

Requires lexical binding.  See also ‘-lazy*’."
  (declare (debug (&define def-body)))
  (cl-assert lexical-binding)
  (pcase body
    ;; Optimized case if a call to a single function whose arguments,
    ;; if any, are all atoms or quoted forms.
    ((and `((,fn . ,args))
          (guard (symbolp fn))
          (guard (not (macrop fn)))
          (guard (--all? (pcase it
                           ((pred atom) t)
                           (`(quote ,_) t))
                         args)))
     `(-lazy* ',fn ,@args))
    ;; General case.
    (_
     `(-lazy* (lambda () ,@body)))))

(-lazy (ignore))
(-lazy (ignore 1 'a))

(defsubst -lazy-thunk-p (object)
  "Test whether OBJECT is a thunk produced by ‘-lazy’ or ‘-lazy*’."
  (and (vectorp object)
       (eq -lazy--marker (aref object 0))))

(defun -lazy-thunk-evaluated-p (object)
  (declare (obsolete "For debugging only." "always"))
  (or (not (-lazy-thunk-p object))
      (null (aref object 2))))

(defun -force (object)
  "Evaluate OBJECT.

Given a thunk OBJECT returned by `-lazy' or `-lazy*', evaluates
it (if it has not yet been evaluated) and returns the result.
Returns OBJECT if OBJECT is not a thunk.

Calling this function again on a previously-evaluated thunk
simply returns the value that was originally produced."
  (if (-lazy-thunk-p object)
      (progn
        (if-let (fun (aref object 2))
            ;; The thunk has not been evaluated yet.  Evaluate it,
            ;; save the result, and clear the function field so it's
            ;; not called again, and so it can be garbage collected.
            (setf (aref object 2) nil
                  (aref object 1)
                  ;; Recurively call -force to avoid ending up with
                  ;; chains of evaluated thunks pointing to each
                  ;; other.
                  (-force (apply fun (aref object 3)))
                  (aref object 3) nil))
        (aref object 1))
    object))

(defsubst -force-if (cond object)
  "Return OBJECT if COND is nil, (-force OBJECT) otherwise."
  (if cond
      (-force object)
    object))

(eval-when-compile
  (defun dash-lazy--params-to-args (params)
    (cl-assert (-all? #'symbolp params))
    (cl-assert (-none? #'keywordp params))
    (append params '(:force-cars force-cars))
    ;; (cl-loop with key = nil
    ;;          for param in params
    ;;          if key collect (intern (format ":%s" (car param)))
    ;;          and collect (car param)
    ;;          else if (eq param '&key) do (setq key t)
    ;;          else collect param)
    )

  (cl-defmacro dash-lazy--defmacro-anaphoric
      (-name (fun-param (&rest ana-params) &rest params)
             &rest body)
    "Macro for defining a function along with an anaphoric macro.
The function is defined in terms of the macro.  This matches the
style of dash, but it doesn't work for recursively-defined
functions, which most functions in this file are.

BODY must include a \",form\" term in a context where the
variables listed in ANA-PARAMS are bound."
    (declare (indent defun))
    (let* ((--name (intern (format "-%s" -name)))
           (parsed (parse-lambda-body body))
           (macro-body (parse-lambda-parsed-body parsed))
           (defun-body
             (progn
               (setf (parse-lambda-parsed-body parsed)
                     `((,--name (funcall ,fun-param ,@ana-params)
                                ,@(dash-lazy--params-to-args params))))
               (cdr (parse-lambda-unparse parsed)))))
      `(progn
         (cl-defmacro ,--name (form ,@params &key (force-cars t))
           ,(format "Anaphoric form of `%s'." -name)
           ,@macro-body)
         (cl-defun ,-name (,fun-param ,@params &key (force-cars t))
           ,@defun-body))))

  (defun dash-lazy--expand-defun-anaphoric (defun-word -name fun-param ana-params params body)
    "Macro for defining a function along with an anaphoric macro.
The macro is defined in terms of the function, which should be
recursive or have compiler macros (otherwise
‘dash-lazy--defmacro-anaphoric’ is more appropriate)."
    `(progn
       (,defun-word ,-name (,fun-param ,@params &key (force-cars t)) ,@body)
       (dash-lazy--defmacro-anaphoric-for-defun
        ,-name ((,@ana-params) ,@params))))

  (cl-defmacro dash-lazy--defun-anaphoric
      (-name (fun-param (&rest ana-params) &rest params) &rest body)
    "Macro for defining a function along with an anaphoric macro.
The macro is defined in terms of the function, which should be
recursive or have compiler macros (otherwise
‘dash-lazy--defmacro-anaphoric’ is more appropriate)."
    (declare (indent defun)
             (debug (&define name sexp def-body)))
    (dash-lazy--expand-defun-anaphoric 'cl-defun -name fun-param ana-params params body))

  (cl-defmacro dash-lazy--defsubst-anaphoric
      (-name (fun-param (&rest ana-params) &rest params) &rest body)
    "Macro for defining an inline function along with an anaphoric macro.
The macro is defined in terms of the function, which should be
recursive or have compiler macros (otherwise
‘dash-lazy--defmacro-anaphoric’ is more appropriate)."
    (declare (indent defun)
             (debug (&define name sexp def-body)))
    (dash-lazy--expand-defun-anaphoric 'cl-defsubst -name fun-param ana-params params body))

  (cl-defmacro dash-lazy--defmacro-anaphoric-for-defun
      (-name ((&rest ana-params) &rest params))
    "Macro for defining an anaphoric macro in terms of a function."
    (let ((--name (intern (format "-%s" -name))))
      `(cl-defmacro ,--name (form ,@params &key (force-cars t))
         ,(format "Anaphoric form of `%s'.

Equivalent to (apply #\\='%s (lambda %S FORM) PARAMS)" -name -name ana-params)
         (list ',-name (list 'lambda ',ana-params form) ,@(dash-lazy--params-to-args params))))))

(eval-and-compile
  (defun dash-lazy--norm-function-form (form)
    "Convert function forms to (lambda ...), (function ...), or a symbol when possible."
    (pcase form
      ;; 'x -> #'x
      ((and `',form
            (guard (symbolp form)))
       `#',form)
      ((and `(lambda (,var) ,body)
            (guard (eq var body)))
       (ignore var body)
       '#'identity)
      ;; (function (lambda ...)) -> (lambda ...)
      (`(function (lambda . ,lambda-body))
       (dash-lazy--norm-function-form `(lambda ,@lambda-body)))
      ;; (lambda (...) (funcall f ...)) -> f
      ;; (lambda (... &rest x) (apply f ... x)) -> f
      ((and (or (and
                 `(lambda ,vars (funcall ,fun . ,args))
                 (guard (equal vars args)))
                (and
                 `(lambda ,vars (apply ,fun . ,args))
                 (guard (eq '&rest (car (last vars 2))))
                 (guard (equal (append (butlast vars 2) (last vars))
                               args))))
            (guard (-all? 'symbolp vars)))
       (ignore vars args)
       (dash-lazy--norm-function-form fun))
      ;; (lambda (...) (f ...)) -> #'f
      ((and `(lambda ,vars (,fun . ,args))
            (guard (-all? 'symbolp vars))
            (guard (equal vars args)))
       (ignore vars args)
       (dash-lazy--norm-function-form `#',fun))
      ;; (-compose (lambda (x) ...) (lambda (x) ...))
      (`(-compose (lambda (,x) . ,g-body) (lambda (,x) . ,f-body))
       `(lambda (,x)
          (setq ,x ,(macroexp-progn f-body))
          ,@g-body))
      ;; (-compose ... (-compose ...) ...) -> (-compose ...)
      (`(-compose . ,args)
       (let ((new-args (--mapcat (pcase (dash-lazy--norm-function-form it)
                                   (`(-compose . ,args) args)
                                   (`#'identity nil)
                                   (_ (list it)))
                                 args)))
         (cond
          ((null new-args) '#'identity)
          ((null (cdr new-args)) (car new-args))
          ((equal args new-args) form)
          (t (dash-lazy--norm-function-form `(-compose ,@new-args))))))
      (_ form)))

  (defun dash-lazy--norm-lazy-list-form (form)
    "Try to remove redundant forms from a ‘-lazy-list’ form."
    (pcase form
      ((or `nil
           `(-lazy nil))
       `(-lazy-list))
      ((and (or
             `',values
             `(-lazy ',values))
            (guard (listp values))
            (guard (null (last values 0))))
       `(-lazy-list ,@(--map `',it values)))
      (`(-lazy (list ,x))
       `(-lazy-list ,x))
      (`(-lazy (cons ,x ,tail))
       `(-lazy-list ,x ,@(cdr (dash-lazy--norm-lazy-list-form tail))))
      (_ form))))

(defmacro -lazy-list (&rest forms)
  "Version ‘list’ which makes a lazy list.

Expands to a nested series of of (-lazy (cons ...)) forms that
can be evaluated to a regular list with ‘-force-cdrs’.

Laziness: nothing is evaluated immediately.  Each form in FORMS
is evaluated as the resulting lazy list is consumed.

See also ‘-lazy-car-list’ and ‘-lazy-list*’."
  (declare (debug t))
  `(-lazy-list* ,@forms nil))

(defmacro -lazy-list* (&rest forms)
  "Version of ‘cl-list*’ which makes a lazy list.

This macro is identical to ‘-lazy-list’, except the last form is
used as the ‘cdr’ rather than the ‘car’ of the innermost ‘cons’
form.  The last form may return any value, but will typically
return a lazy list.

Laziness: nothing is evaluated immediately.  Each form in FORMS
is evaluated as the resulting lazy list is consumed.  The value
produced by the last form in FORMS is not necessarily forced,
although most functions that operate on lazy lists will force
every ‘cdr’ of a lazy list as they traverse it."
  (declare (debug t))
  (if (cdr forms)
      `(-lazy (cons ,(car forms) (-lazy-list* ,@(cdr forms))))
    (car forms)))

(defmacro -lazy-car-list (&rest forms)
  "Version of ‘list’ which makes a list of lazy values.

Expands to a (list (-lazy ...)) form that can be evaluated to a
list of non-lazy values with ‘-force-list’.  The result is a
non-lazy list of lazy values.

Laziness: the list structure is produced immediately, but nothing
in FORMS is evaluated."
  (declare (debug t))
  `(list ,@(--map `(-lazy ,it) forms)))

(defmacro -lazy-car-list* (&rest forms)
  "Lazy equivalent of `cl-list*'.

Expands to a (cl-list* (-lazy ...)) form that can be evaluated to
a list of non-lazy values with ‘-force-list’.  The result is a
lazy list of lazy values.

Laziness: no form on FORMS is evaluated immediately.  When there
are N forms, the first N - 1 ‘cons’ cells are produced
immediately and populated with lazy values, and the ‘cdr’ of the
final cell is set to a lazy value which will evaluate the last
form when forced."
  (declare (debug t))
  `(cl-list* ,@(--map `(-lazy ,it) forms)))

(defsubst -force-cdrs (lazy-list)
  "Force evaluation of the structure of an entire lazy list.

Convert a lazy list to a regular list by calling `-force' on each
cell.  A lazy list is either a regular list, (-lazy nil),
or (-lazy (cons CAR CDR)), where CDR is a lazy list.  This
function also works for dotted lists, where the last cdr is
neither nil, a cons cell, nor a lazy cons cell.  See ‘-lazy’,
‘-lazy-list’, and ‘-force-list’.

Laziness: calling this function causes the list iself, and the
cdr of every cell in the list, to be forced.  Does not force the
car of any cell."
  (-force-list lazy-list :force-cars nil))

(defun -lazy-force-cars (lazy-list)
  "Given a lazy list of lazy values, return a lazy list of non-lazy values.

Works for regular and dotted lists.

Laziness: nothing is forced immediately.  The ‘car’ of each cell
in the list is forced as the list is traversed."
  (-lazy-map #'-force lazy-list))

(cl-defun -force-list (lazy-list &key (force-cars t))
  "Given a lazy list, return a non-lazy list of non-lazy values.

If LAZY-LIST is a function, it is coerced to a lazy list with
‘-lazy-from-iter’.

If FORCE-CARS is nil, the contents of the list are not forced.

Laziness: calling this function causes the entire list, and every
value in it, to be forced immediately."
  (if (functionp lazy-list)
      ;; Handle an iterator.
      (let (head tail)
        (condition-case nil
            (progn
              (setq head (cons (-force-if force-cars (iter-next lazy-list))
                               nil))
              (setq tail head)
              (while t
                (setcdr tail (cons (-force-if force-cars (iter-next lazy-list))
                                   nil))
                (setq tail (cdr tail))))
          (iter-end-of-sequence head)))
    ;; Handle a lazy list.
    (let ((input-cell (-force lazy-list)))
      (if (not (consp input-cell))
          input-cell
        (let* ((output-head (cons (-force-if force-cars (car input-cell)) nil))
               (output-last output-head))
          (while (when-let (input-cdr (-force (cdr input-cell)))
                   (if (consp input-cdr)
                       (let ((new-output-cell (cons (-force-if force-cars
                                                               (car input-cdr))
                                                    nil)))
                         (setcdr output-last new-output-cell)
                         (setq output-last new-output-cell)
                         (setq input-cell input-cdr))
                     (setcdr output-last input-cdr)
                     nil)))
          output-head)))))

(defsubst -force-car (lazy-list)
  "Eqivalent to (car (-force LAZY-LIST)).

Also equivalent to (-force-nth 0 LAZY-LIST); see ‘-force-nth’."
  (car (-force lazy-list)))

(defsubst -force-cdr (lazy-list)
  "Eqivalent to (cdr (-force LAZY-LIST)).

Also equivalent to (-force-nthcdr 1 LAZY-LIST); see ‘-force-nth’."
  (cdr (-force lazy-list)))

(defsubst -force-car-safe (lazy-list)
  "Eqivalent to (car-safe (-force LAZY-LIST))."
  (car-safe (-force lazy-list)))

(defsubst -force-cdr-safe (lazy-list)
  "Eqivalent to (cdr-safe (-force LAZY-LIST))."
  (cdr-safe (-force lazy-list)))

(defsubst -force-cadr (lazy-list)
  "Optimized version of (-force-nth 1 LAZY-LIST); see ‘-force-nth’."
  (-force-car (-force-cdr lazy-list)))

(defsubst -force-cddr (lazy-list)
  "Optimized version of (-lazy-nthcdr 2 LAZY-LIST); see ‘-lazy-nthcdr’."
  (-force-cdr (-force-cdr lazy-list)))

(defsubst -force-nth (n lazy-list)
  "Lazy version of ‘nth’.

Laziness: calling this function forces the first N + 1 elements
of the list.  If the list contains lazy values in the ‘car’ of
its cells, they are not forced."
  (declare (compiler-macro
            (lambda (form)
              (cl-case n
                (0 `(-force-car ,lazy-list))
                (1 `(-force-cadr ,lazy-list))
                (t form)))))
  (-force-car (-lazy-drop n lazy-list)))

(defun -force-nthcdr (n lazy-list)
  "Return the equivalent of calling ‘-force-cdr’ N times on LAZY-LIST.

Laziness: immediately forces the first N links of LAZY-LIST.  If
the list contains lazy values in the ‘car’ of any of its cells,
they are not forced.

See also ‘-lazy-drop’."
  (declare (compiler-macro
            (lambda (form)
              (cl-case n
                (0 lazy-list)
                (1 `(-force-cdr ,lazy-list))
                (2 `(-force-cddr ,lazy-list))
                (t form)))))
  (while (and lazy-list (> n 0))
    (cl-callf -force-cdr lazy-list)
    (cl-decf n))
  lazy-list)

(defun -lazy-drop (n lazy-list)
  "Lazier version of ‘-force-nthcdr’.

Given a lazy list LAZY-LIST, return a list with the first N items
removed, or nil if N is greater than the length of LAZY-LIST, or
if LAZY-LIST itself if N <= 0.

Laziness: nothing is evaluated immediately.  When the value
returned by this function is forced, the first N elements of
LAZY-LIST are immediately forced."
  (if (<= n 0)
      ;; Short circuit to avoid making a new thunk.
      lazy-list
    (-lazy (-force-nthcdr n lazy-list))))

(defun -lazy-take (n lazy-list)
  "Lazily get the first N elements of LAZY-LIST.

Given a lazy list LAZY-LIST, return a lazy list of the first N
items of LAZY-LIST, or all of LAZY-LIST if N is greater than the
length of LAZY-LIST, or nil if N <= 0.

Laziness: full."
  (declare (compiler-macro
            (lambda (form)
              (if n form lazy-list))))
  (if (not n) lazy-list
    (-lazy
     (if-let ((cell (-force lazy-list)))
         (when (> n 0)
           (cons (car cell)
                 (-lazy-take (1- n) (cdr cell))))))))

(defun -lazy-unfold (fun seed)
  "Lazy version of ‘-unfold’.

FUN should return ‘nil’ to stop the generating process, or a
cons (A . B), where A will be prepended to the result and B is
the new seed.  The resulting list contains no lazy values unless
FUN returns cells with a lazy ‘car’.

Laziness: FUN is called as needed when the result list is
consumed."
  (-lazy (if-let (pair (funcall fun seed))
             (cons (car pair)
                   (-lazy-unfold fun (cdr pair))))))

(defmacro --lazy-unfold (form seed)
  "Anaphoric form of ‘-lazy-unfold’.

Equivalent to (-lazy-unfold (lambda (it) FORM) SEED)."
  `(-lazy-unfold (lambda (it) ,form) ,seed))

(defun -lazy-range (from-or-to &optional to step)
  "Produce a lazy list of numbers.

With one argument, produces a list starting at 0 and counting up
to (but not including) FROM-OR-TO.  Produces an empty list if TO
is negative.

With two arguments, produces a list starting at FROM-OR-TO and
counting up to (but not including) TO.  Produces an empty list if
TO is less than or equal to FROM-OR-TO.

With a positive STEP argument, produces a list starting at
FROM-OR-TO counting up by STEP, and stopping when the value is
greater than or equal to TO.

With a negative STEP argument, produces a list starting at
FROM-OR-TO counting down by STEP, and stopping when the value is
less than or equal to TO."
  (declare (compiler-macro
            (lambda (form)
              (ignore from-or-to to step)
              ;; Expand a short list at compile time.
              (if (-all? 'atom form)
                  (let ((value (apply #'-lazy-range (cdr form))))
                    (if (null (-force-nth 8 value))
                        `',(-force-cdrs value)
                      form))
                form))))
  (let ((from (if (null to) 0 from-or-to))
        (to (or to from-or-to))
        (step (or step 1)))
    (cl-assert (numberp from))
    (cl-assert (numberp to))
    (cl-assert (numberp step))
    (cond
     ((> step 0)
      (--lazy-unfold (when (< it to) (cons it (+ it step))) from))
     ((< step 0)
      (--lazy-unfold (when (> it to) (cons it (+ it step))) from))
     (t (error "STEP may not be zero.")))))

(cl-defmacro --force-each (lazy-list &rest body &key (force-cars t) &allow-other-keys)
  "Anaphoric form of `-force-each'."
  (declare (indent 1) (debug t))
  (while (keywordp (car body))
    (setq body (cddr body)))
  `(-force-each ,lazy-list (lambda (it) ,@body) :force-cars ,force-cars))

(cl-defmacro --force-each-while (lazy-list pred &rest body &key (force-cars t) &allow-other-keys)
  "Anaphoric form of `-force-each-while'."
  (declare (indent 2) (debug t))
  (while (keywordp (car body))
    (setq body (cddr body)))
  `(-force-each-while ,lazy-list (lambda (it) ,pred) (lambda (it) ,@body)
                      :force-cars ,force-cars))

(cl-defsubst -force-each (lazy-list fn &key (force-cars t))
  "Version of ‘-each’ that operates on lazy lists of lazy values.

Eqvivalent to (-each (-force-list LAZY-LIST) FN) except the list
is forced incrementally.

Laziness: forces list, and the values within it, as the list is
traversed."
  (declare
   (compiler-macro
    (lambda (form)
      (ignore fn lazy-list)
      (pcase-exhaustive form
        (`(-force-each ,lazy-list ,fn . ,rest)
         (pcase-exhaustive (dash-lazy--fuse-with-map `(-force-each ,fn ,lazy-list ,@rest))
           (`(-force-each ,fn ,lazy-list . ,rest)
            (let ((new-form `(-force-each ,lazy-list ,fn ,@rest)))
              (if (equal new-form form)
                  form
                new-form)))))))))
  (-force-each-while lazy-list nil fn :force-cars force-cars))

(cl-defun -force-each-while (lazy-list pred fn &key (force-cars t))
  "Version of `-each-while' that operates of lazy lists of lazy values.

Eqvivalent to (-each-while (-force-list LAZY-LIST) PRED FN)
except the list is forced incrementally.

Laziness: forces the list, and the values within it, as the list
is traversed."
  (while (when-let (cell (-force lazy-list))
           (let ((it (-force-if force-cars (car cell))))
             (when (or (not pred)
                       (funcall pred it))
               (funcall fn it)
               (setq lazy-list (cdr cell)))))))

(eval-and-compile
  (defun dash-lazy--build-compose (g f)
    (pcase (-map #'dash-lazy--norm-function-form (list g f))
      (`(,g ,f) (dash-lazy--norm-function-form `(-compose ,g ,f))))))

(eval-and-compile
  (defun dash-lazy--get-force-cars (forms)
    (pcase-exhaustive forms
      (`(:force-cars ,form) form)
      (`() t))))

(eval-and-compile
  (defun dash-lazy--norm-lazy-map (form)
    (pcase form
      (`(-lazy-map ,fn ,list)
       `(-lazy-map ,fn ,list :force-cars t))
      (`(--lazy-map ,form . ,rest)
       (dash-lazy--norm-lazy-map
        `(-lazy-map (lambda (it) ,form) ,@rest)))
      (_ form))))

(eval-and-compile
  (defun dash-lazy--force-cars-compatible (inner outer)
    (and (atom inner)
         (atom outer)
         (or (eq t outer)
             (equal inner outer)))))

(eval-and-compile
  (defun dash-lazy--fuse-with-map (form &rest _)
    (pcase form
      ((and `(,func ,outer-fn
                    ,(app dash-lazy--norm-lazy-map
                          `(-lazy-map ,inner-fn ,lazy-list
                                      :force-cars ,inner-force-cars))
                    . ,(app dash-lazy--get-force-cars outer-force-cars))
            (guard (dash-lazy--force-cars-compatible inner-force-cars outer-force-cars)))
       (ignore inner-force-cars)
       `(,func ,(dash-lazy--build-compose outer-fn inner-fn) ,lazy-list
               :force-cars ,outer-force-cars))
      (_ form))))

(eval-and-compile
  (defun dash-lazy--expand-lazy-foldl (form &rest _)
    (pcase form
      ((and `(,func ,outer-fn ,initial-value
                    ,(app dash-lazy--norm-lazy-map
                          `(-lazy-map ,inner-fn ,lazy-list
                                      :force-cars ,inner-force-cars))
                    . ,(app dash-lazy--get-force-cars outer-force-cars))
            (guard (dash-lazy--force-cars-compatible inner-force-cars outer-force-cars)))
       (ignore inner-force-cars)
       `(,func (lambda (acc it)
                 (funcall ,outer-fn acc (funcall ,inner-fn it)))
               ,initial-value ,lazy-list
               :force-cars ,outer-force-cars))
      (_ form))))

(eval-and-compile
  (defun dash-lazy--expand-lazy-foldr (form &rest _)
    (pcase form
      ((and `(,func ,outer-fn ,initial-value
                    ,(app dash-lazy--norm-lazy-map
                          `(-lazy-map ,inner-fn ,lazy-list
                                      :force-cars ,inner-force-cars))
                    . ,(app dash-lazy--get-force-cars outer-force-cars))
            (guard (dash-lazy--force-cars-compatible inner-force-cars outer-force-cars)))
       (ignore inner-force-cars)
       `(,func (lambda (it acc)
                 (funcall ,outer-fn (funcall ,inner-fn it) acc))
               ,initial-value ,lazy-list
               :force-cars ,outer-force-cars))
      (_ form))))

(dash-lazy--defun-anaphoric -lazy-foldr (fn (it acc) initial-value lazy-list)
  "Lazy right-fold."
  (declare (compiler-macro dash-lazy--expand-lazy-foldr))
  (-lazy
   (if-let (cell (-force lazy-list))
       (funcall fn
                (-force-if force-cars (car cell))
                (-lazy-foldr fn initial-value (cdr cell)))
     initial-value)))

;; (defun -lazy-compose (f g)
;;   (lambda (x)
;;     (funcall (-force f)
;;              (funcall (-force g)
;;                       (-force x)))))

(dash-lazy--defun-anaphoric -lazy-foldl (fn (acc it) initial-value lazy-list)
  "Lazy left-fold.

Start by setting ACC to INITIAL-VALUE.  Then traverse the list,
passing ACC along with the ‘car’ of each cell to FN, producing a
new value for ACC.  Return the final value of ACC."
  (declare (compiler-macro dash-lazy--expand-lazy-foldl))
  ;; (-lazy (funcall (-force (--lazy-fold-map
  ;;                          (-rpartial fn it)
  ;;                          (-lazy-compose acc it)
  ;;                          #'identity
  ;;                          lazy-list))
  ;;                 initial-value))

  (let ((acc initial-value))
    (while (when-let (cell (-force lazy-list))
             (setq acc (-force-if force-cars (funcall fn acc (car cell))))
             (setq lazy-list (cdr cell))))
    acc))

;; Reductions generally can't be done lazily, so we just define some
;; wrapped functions that force the whole list and then apply a dash
;; function or macro.

(eval-when-compile
  (defmacro dash-lazy--define-reduction (-name)
    (let ((-force-name (intern (format "-force%s" -name))))
      `(defun ,-force-name (lazy-list)
         ,(format "Variant of %s that accepts a lazy list argument."
                  -name)
         (,-name (-force-list lazy-list))))))

(eval-when-compile
  (cl-defmacro dash-lazy--define-anaphoric-reduction
      (-name fn (&rest fn-args) &rest args)
    (let ((-force-name (intern (format "-force%s" -name)))
          (--name (intern (format "-%s" -name))))
      `(dash-lazy--defmacro-anaphoric ,-force-name (,fn (,@fn-args) ,@args lazy-list)
         ,(format "Variant of ‘%s’ that accepts a lazy list of lazy values as an argument.

Laziness: none.  The entire list and its contents are forced
immediately.  A future revision may alter the semantics slightly
so that the list contents are forced only as needed, but so long
as no side-effects are observed while the list is being
traversed, there will be no observable change in behavior."
                  -name)
         `(,',--name ,form ,@(list ,@args) (-force-list ,lazy-list :force-cars ,force-cars))))))

(dash-lazy--define-anaphoric-reduction -min-by comparator (it other))
(dash-lazy--define-anaphoric-reduction -max-by comparator (it other))
(dash-lazy--define-reduction -sum)
(dash-lazy--define-reduction -product)
(dash-lazy--define-reduction -min)
(dash-lazy--define-reduction -max)

(dash-lazy--defun-anaphoric -force-count (pred (it) lazy-list)
  "Version of ‘-count’ that operates on lazy lists of lazy values.

Never passes a lazy value to PRED.

Laziness: forces each value in LAZY-LIST as it is needed to pass
to PRED."
  (-force (--lazy-foldl (if (funcall pred it)
                            (1+ acc)
                          acc)
                        0 lazy-list :force-cars force-cars)))

(dash-lazy--defun-anaphoric -force-reduce (fn (acc it) lazy-list)
  "Version of ‘-reduce’ that operates on lazy lists of lazy values.

Unless FN returns a lazy value, this function never returns a
lazy value or passes a lazy value as an argument to FN.

Laziness: always forces at least one cell of LAZY-LIST.
Additional cells, and their ‘car’ values, are forced as needed so
they can be passed to FN."
  (if-let (cell (-force lazy-list))
      (if-let (tail (-force (cdr cell)))
          (-force-reduce-from fn (car cell) tail :force-cars force-cars)
        (car cell))
    (funcall fn)))

(dash-lazy--defun-anaphoric -force-reduce-from (fn (acc it) initial-value lazy-list)
  "Version of ‘-reduce-from’ that operates on lazy lists of lazy values.

Unless FN returns a lazy value, this function never returns a
lazy value or passes a lazy value as an argument to FN.

Laziness: INITIAL-VALUE and the first item (if any) of LAZY-LIST
are forced immediately.  Additional values are forced as they are
need to pass into FN."
  (declare (compiler-macro dash-lazy--expand-lazy-foldl))
  (-force (--lazy-foldl (funcall fn acc (-force it))
                        (-force initial-value)
                        lazy-list :force-cars force-cars)))

(dash-lazy--defun-anaphoric -force-reduce-r (fn (it acc) lazy-list)
  "Version of ‘-reduce-r’ that operates on lazy lists of lazy values.

Unless FN returns a lazy value, this function never returns a
lazy value or passes a lazy value as an argument to FN.

Laziness: every cell of LAZY-LIST is forced immediately.  The
‘car’ of each cell is forced (starting from the end) it is needed
to pass to FN."
  (-reduce-r (lambda (&rest args)
               (if args
                   (funcall fn
                            (-force-if force-cars (car args))
                            (-force-if force-cars (cadr args)))
                 (funcall fn)))
             (-force-cdrs lazy-list)))

(dash-lazy--defun-anaphoric -force-reduce-r-from (fn (it acc) initial-value lazy-list)
  "Version of ‘-reduce-r-from’ that operates on lazy lists of lazy values.

Unless FN returns a lazy value, this function never returns a
lazy value or passes a lazy value as an argument to FN.

Laziness: INITIAL-VALUE, every cell in LAZY-LIST, and the last
item (if any) of LAZY-LIST are forced immediately.  Additional
values are forced, starting from the end, as they are need to
pass into FN."
  (declare (compiler-macro dash-lazy--expand-lazy-foldr))
  (-force-list
   (-lazy-foldr
    (lambda (it acc) (funcall fn (-force-if force-cars it) acc))
    initial-value lazy-list)))

(cl-defsubst -force-reverse (lazy-list &key (force-cars t))
    "A version of ‘reverse’ that accepts a lazy list of lazy values.

Always returns a regular list of non-lazy values."
    (nreverse (-force-list lazy-list :force-cars force-cars)))

(cl-defsubst -lazy-reverse (lazy-list &key (force-cars t))
  "A version of ‘reverse’ that accepts a lazy list.

Laziness: nothing is evaluated immediately.  As soon as the first
cell of the result is forced, every cell of the input is forced.
Never forces the ‘car’ of any cell."
  (-lazy (-force-reverse lazy-list :force-cars force-cars)))

(defsubst -lazy-join (lazy-lists)
  "Combine all the elements of a lazy list of lazy lists into a
single lazy list.

Laziness: full."
  (--lazy-foldr (-lazy-foldr #'cons acc it)
                nil lazy-lists))

(defsubst -lazy-concat (&rest lazy-lists)
  "Lazy version of `-concat'.

Laziness: since this is a normal function, the arguments are
grouped into an eager list, but the result is fully lazy.  See
also `-lazy-join' for even more laziness."
  (-lazy-join lazy-lists))

(dash-lazy--defun-anaphoric -lazy-mapcat (fn (it) lazy-list)
  "TODO."
  (declare (compiler-macro dash-lazy--fuse-with-map))
  (-lazy
   (when-let (cell (-force lazy-list))
     (-lazy-concat
      (funcall fn (-force-if force-cars (car cell)))
      (-lazy-mapcat fn (cdr cell))))))

(dash-lazy--defun-anaphoric -lazy-map (fn (it) lazy-list)
  "A lazy version of `-map'.

Given a lazy list of lazy values, produces a lazy list of
results.  The contents of the list are not forced before being
passed to FN.  Handles dotted lists.

Laziness: the cells of LAZY-LIST, are forced only when the
corresponding values of the new list are forced."
  (declare (compiler-macro dash-lazy--fuse-with-map))
  ;; This could be defined in terms of --lazy-foldr if not for
  ;; the requirement to handled dotted lists:
  ;; (--lazy-foldr (cons (funcall fn it) acc) nil lazy-list)
  (-lazy
   (let ((cell (-force lazy-list)))
     (if (consp cell)
         (cons (funcall fn (-force-if force-cars (car cell)))
               (-lazy-map fn (cdr cell) :force-cars force-cars))
       cell))))

(cl-defmacro --lazy-iterate (form init)
  "Anaphoric form of ‘-lazy-iterate’.

Equivalent to to (-lazy-iterate (lambda (it) FORM) INIT)."
  `(--lazy-unfold (cons it ,form) ,init))

(cl-defsubst -lazy-iterate (fun init)
  "Lazy version of ‘-iterate’.

Produces an infinite list of values which are not lazy unless FUN
returns lazy values.

Laziness: nothing is evaluated immediately.  FUN is called as
needed when the result list is traversed."
  (--lazy-iterate (funcall fun it) init))

(defun -force-length (lazy-list)
  (-force (-lazy-foldl (lambda (acc _) (1+ acc))
                       0 lazy-list)))

(defun -lazy-tails (lazy-list)
  (-lazy-concat
   (-lazy-take-while #'identity
                     (-lazy-concat
                      (-lazy-iterate #'-force-cdr lazy-list)))
   '(nil)))

(defun -lazy-repeat (value)
  "Return an inifinte lazy list whose values are all VALUE."
  (-lazy-unfold (lambda (_) (cons value nil)) nil))

(defun -lazy-cycle (values)
  "Return a list list of VALUES concatenated to itself infinitely."
  (-lazy-join
   (-lazy-unfold (lambda (_) (cons values nil)) nil)))

(dash-lazy--defmacro-anaphoric -force-any? (pred (it) lazy-list)
  "Lazy version of `-any?'.

Laziness: cells and their ‘car’ values are forced until PRED
returns non-nil."
  `(cl-block nil (--force-each ,lazy-list
                   (when ,form (cl-return t))
                   :force-cars ,force-cars)))

(defalias '-force-any-p '-force-any?)
(defalias '-force-some? '-force-any?)
(defalias '-force-some-p '-force-any?)
(defalias '--force-any-p '--force-any?)
(defalias '--force-some? '--force-any?)
(defalias '--force-some-p '--force-any?)

(dash-lazy--defmacro-anaphoric -force-all? (pred (it) lazy-list)
  "Lazy version of `-all?'.

Laziness: cells and their ‘car’ values are forced until PRED
returns nil."
  `(not (--force-any? (not ,form) ,lazy-list :force-cars ,force-cars)))

(defalias '-force-all-p '-force-all?)
(defalias '-force-every? '-force-all?)
(defalias '-force-every-p '-force-all?)
(defalias '--force-all-p '--force-all?)
(defalias '--force-every? '--force-all?)
(defalias '--force-every-p '--force-all?)


(dash-lazy--defmacro-anaphoric -force-none? (pred (it) lazy-list)
  "Lazy version of `-none?'.

Laziness: cells and their ‘car’ values are forced until PRED
returns non-nil."
  `(not (--force-any? ,form ,lazy-list :force-cars ,force-cars)))

(defalias '-force-none-p '-force-none?)
(defalias '--force-none-p '--force-none?)

(dash-lazy--defun-anaphoric -lazy-drop-while (pred (it) lazy-list)
  "Lazy version of ‘-drop-while’.

Given a lazy list LAZY-LIST of lazy values, and a unary function
PRED, returns the lazy list formed by dropping the longest
sequence of items for which PRED returns non-nil.

Laziness: nothing is evaluated immediately.  When the value
returned by this function is forced, PRED is called N
times (where N > 0 unless LAZY-LIST is empty), and the first N
values of LAZY-LIST are forced, producing a result which is
shorter than LAZY-LIST by N - 1 elements."
  (-lazy
   (while (when-let (cell (-force lazy-list))
            (when (funcall pred (-force-if force-cars (car cell)))
              (setq lazy-list (cdr cell)))))
   lazy-list))

(dash-lazy--defun-anaphoric -lazy-take-while (pred (it) lazy-list)
  "Lazy version of ‘-take-while’.

Given a lazy list LAZY-LIST of lazy values, and a unary function
PRED, returns the longest prefix of LAZY-LIST for which PRED
returns non-nil for every element.

Laziness: Nothing is evaluated immediately.  When the value
returned by this function is forced, PRED is called N
times (where n > 0 unless LAZY-LIST is empty), and first first N
+ 1 items of LAZY-LIST are forced, producing a result of length N
- 1."
  (-lazy
   (when-let (cell (-force lazy-list))
     (when (funcall pred (-force (car cell)))
       (cons (car cell) (-lazy-take-while pred (cdr cell)
                                          :force-cars force-cars))))))

(defun -lazy-zip-with (fun &rest lazy-lists)
  "Lazy version of `-zip-with'."
  (-lazy (let ((heads (-map '-force lazy-lists)))
           (and (-every-p #'identity heads)
                (cons (apply fun (-map 'car heads))
                      (apply '-lazy-zip-with fun (-map 'cdr heads)))))))

;; Macro defined separately becuase it has a more restrictive
;; signature.
(dash-lazy--defmacro-anaphoric-for-defun -lazy-zip-with
                                ((it other) lazy-list1 lazy-list2))

(defsubst -lazy-zip (&rest lazy-lists)
  "Lazy version of `-zip', but does not duplicate deprecated
special case for two arguments."
  (apply '-lazy-zip-with 'list lazy-lists))

(defsubst -lazy-zip-pair (lazy-list1 lazy-list2)
  "Lazy version of `-zip-pair'."
  (-lazy-zip-with #'cons lazy-list1 lazy-list2))

(defsubst -force-uniq (lazy-list)
  "Version of ‘-uniq’ that operators on lazy lists.
Returns a non-lazy list.

Laziness: the entire list, and its contents, are forced."
  (-uniq (-force-list lazy-list)))

(defalias '-force-distinct '-force-uniq)

(iter-defun -lazy-to-iter (lazy-list)
  "Converts a lazy list to an iterator on which `iter-next' may
be called."
  ;; The obvious definitions in terns of -force-each or --force-each cause
  ;; strange compiler warnings.
  (while (when-let (cell (-force lazy-list))
           (iter-yield (-force (car cell)))
           (setq lazy-list (cdr cell))
           t)))

(defun -lazy-from-iter (iterator)
  "Produces a lazy list from an iterator by calling `iter-next'."
  (-lazy (condition-case nil
             (cons (iter-next iterator)
                   (-lazy-from-iter iterator))
           (iter-end-of-sequence
            nil))))

(dash-lazy--defmacro-anaphoric -lazy-filter (pred (it) lazy-list)
  "Lazy version of `-filter'."
  `(--lazy-foldr (if ,form (cons it acc) acc)
                 nil ,lazy-list :force-cars ,force-cars))

(dash-lazy--defmacro-anaphoric -lazy-remove (pred (it) lazy-list)
  "Lazy version of `-remove'."
  `(--lazy-filter (not ,form) ,lazy-list :force-cars ,force-cars))

(dash-lazy--defun-anaphoric -lazy-remove-first (pred (it) lazy-list)
  "Lazy version of `-remove-first'."
  (-lazy (when-let (cell (-force lazy-list))
           (if (funcall pred (-force-if force-cars (car cell)))
               (cdr cell)
             (-lazy-remove-first pred (cdr cell) :force-cars force-cars)))))

(dash-lazy--defmacro-anaphoric -lazy-remove-last (pred (it) lazy-list)
  "Lazy version of `-remove-last'."
  `(-lazy (--remove-last ,form (-force-list ,lazy-list :force-cars ,force-cars))))

(cl-defsubst -lazy-non-nil (lazy-list &key (force-cars t))
  "Lazy version of `-non-nil'."
  (-lazy-filter #'identity lazy-list :force-cars force-cars))

(dash-lazy--defsubst-anaphoric -lazy-keep (fn (it) lazy-list)
  "Lazy version of `-keep'."
  (-lazy-non-nil (-lazy-map fn lazy-list :force-cars force-cars)))

(defsubst -lazy-slice (lazy-list from &optional to step)
  "Lazy version of `-slice'."
  (dash-lazy--slice-1 lazy-list from to (or step 1)))

(defun dash-lazy--slice-1 (lazy-list from to step)
  (declare (compiler-macro
            (lambda (form)
              (if (not (-all? 'atom (list from to step)))
                  form
                (pcase (list from to step)
                  (`(,_ nil 1)
                   `(-lazy-drop ,from ,lazy-list))
                   (`(,_ ,_ 1)
                   `(-lazy-drop ,from (-lazy-take ,to ,lazy-list)))
                  (`(and (,_ ,_ 1)
                         (guard (< from to)))
                   `(-lazy (dash-lazy--slice-2 ,lazy-list ,from ,to ,step)))
                  (_ form))))))
  (if (or (< from 0)
          (and to (< to 0)))
      (-slice (-force-cdrs lazy-list) from to step)
    (-lazy
     (cl-assert (> step 0))
     (cond
      ((= step 1)
       (-lazy-drop from (-lazy-take to lazy-list)))
      ((or (null to) (< from to))
       (dash-lazy--slice-2 lazy-list from to step))))))

(defun dash-lazy--slice-2 (lazy-list from to step)
  (let ((skip (1- step)))
    (when-let (cell (-force (-lazy-drop from lazy-list)))
      (cons (car cell)
            (-lazy-slice
             (cdr cell) skip
             (and to (- to skip from))
             step)))))

(defvar cl--loop-args)

(defun -lazy--handle-loop-for-in (var)
  (with-gensyms (head-var)
    (cl-callf2 append
        `(for ,head-var = (-force ,(pop cl--loop-args))
              then (-force (cdr ,head-var))
              while ,head-var
              for ,var = (-force (car ,head-var)))
        cl--loop-args)))

(put '-in-lazy 'cl-loop-for-handler '-lazy--handle-loop-for-in)

(defun -lazy--handle-loop-for-on (var)
  (cl-callf2 append
      `(for ,var = ,(pop cl--loop-args)
            then (-force-cdr ,var)
            while (-force ,var))
      cl--loop-args))

(put '-on-lazy 'cl-loop-for-handler '-lazy--handle-loop-for-on)

(provide 'dash-lazy)
