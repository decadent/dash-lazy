;; -*- lexical-binding: t -*-

(require 'ert)
(require 'dash-lazy)
(require 'with-gensyms)
(require 'cl)

(defmacro should-all (form &rest args)
  (declare (indent 1))
  `(progn
     ,@(--map `(should ,(append form (list it)))
              args)))

(defmacro idempotent-call-p (fun arg)
  (with-eval-once (arg)
    (with-gensyms (result-var)
      `(let ((,result-var (funcall ,fun ,arg)))
         (eq ,result-var ,arg)))))

;; (defmacro dash-lazy--ll (&rest forms)
;;   "Make a list that is lazy in its structure and values."
;;   (declare (debug t))
;;   `(-lazy-list ,@(--map `(-lazy ,it) forms)))

;; (defmacro dash-lazy--ll-exploding (&rest forms)
;;   "Make a list that is lazy in its structure and values."
;;   (declare (debug t))
;;   `(-lazy-list ,@(--map `(-lazy ,it) forms)
;;                (error "should not get here")))

;; (defun dash-lazy--links-evaluated (lazy-list)
;;   (let ((count 0))
;;     (while (-lazy-thunk-evaluated-p lazy-list)
;;       (cl-incf count)
;;       (setq lazy-list (cdr lazy-list)))))

(defvar dash-lazy--list nil)
(defvar dash-lazy--counter nil)

(defun dash-lazy--cars-evaluated (lazy-list)
  (let ((count 0))
    (while (-lazy-thunk-evaluated-p lazy-list)
      (when (-lazy-thunk-evaluated-p (car lazy-list))
        (cl-incf count))
      (setq lazy-list (cdr lazy-list)))))

(ert-deftest -lazy-force ()
  (should (eq nil (-force nil)))
  (should (= 42 (-force 42)))
  (should (= 42 (-force (-lazy (-lazy 42)))))
  (let* ((times-called 0)
         (ref (-lazy
               (cl-incf times-called)
               42)))
    (should (= 0 times-called))
    (should (= 42 (-force ref)))
    (should (= 1 times-called))
    (should (= 42 (-force ref)))
    (should (= 1 times-called))
    (should (eq nil (-force nil)))))

(ert-deftest lazy-list-1 ()
  (setq dash-lazy--counter 0)
  (setq dash-lazy--list (-lazy-list (progn
                            (cl-incf dash-lazy--counter)
                            (-lazy (cl-incf dash-lazy--counter)))
                          (progn
                            (cl-incf dash-lazy--counter)
                            (-lazy (cl-incf dash-lazy--counter)))))
  (should (= 0 dash-lazy--counter))
  (should (eq (-force-car dash-lazy--list)
              (-force-nth 0 dash-lazy--list)))
  (should (= 1 dash-lazy--counter))
  (should (= 2 (-force (-force-nth 0 dash-lazy--list))))
  (should (eq (-force-car dash-lazy--list) (-force-nth 0 dash-lazy--list)))
  (should (= 2 dash-lazy--counter))
  (should (eq (-force-cdr dash-lazy--list) (-force-nthcdr 1 dash-lazy--list)))
  (should (= 2 dash-lazy--counter))
  (-force-nth 1 dash-lazy--list)
  (should (= 3 dash-lazy--counter))  
  (should (= 4 (-force (-force-nth 1 dash-lazy--list))))
  (should (eq (-force-cadr dash-lazy--list) (-force-nth 1 dash-lazy--list)))
  (should (= 4 dash-lazy--counter))
  (should (eq (-force-cddr dash-lazy--list) (-force-nthcdr 2 dash-lazy--list)))
  (should (= 4 dash-lazy--counter))
  (should (null (-force-nth 2 dash-lazy--list)))
  (should (= 4 dash-lazy--counter)))

(ert-deftest lazy-list-2 ()
  (setq dash-lazy--counter 0)
  (setq dash-lazy--list (-lazy-list (cl-incf dash-lazy--counter)
                          (cl-incf dash-lazy--counter)))
  (should (= 0 dash-lazy--counter))
  (should (eq dash-lazy--list (-force-nthcdr 0 dash-lazy--list)))
  (should (= 0 dash-lazy--counter))
  (should (= 1 (-force (-force-car (-force-nthcdr 0 dash-lazy--list)))))
  (should (= 1 dash-lazy--counter))
  (should (eq (-force-cdr dash-lazy--list) (-force-nthcdr 1 dash-lazy--list)))
  (should (= 1 dash-lazy--counter))
  (should (= 2 (-force (-force-car (-force-nthcdr 1 dash-lazy--list)))))
  (should (= 2 dash-lazy--counter))
  (should (null (-force (-force-nthcdr 2 dash-lazy--list))))
  (should (= 2 dash-lazy--counter)))

(ert-deftest lazy-list-3 ()
  (setq dash-lazy--counter 0)
  (setq dash-lazy--list (-lazy-list (cl-incf dash-lazy--counter)
                          (cl-incf dash-lazy--counter)))
  (should (= 0 dash-lazy--counter))
  (should (= 1 (-force (-force-car dash-lazy--list))))
  (should (= 1 dash-lazy--counter))
  (-force-cdr dash-lazy--list)
  (should (= 1 dash-lazy--counter))
  (should (= 2 (-force (-force-cadr dash-lazy--list))))
  (should (= 2 dash-lazy--counter))
  (should (null (-force (-force-cddr dash-lazy--list)))))

(ert-deftest lazy-list-4 ()
  (setq dash-lazy--counter 0)
  (setq dash-lazy--list (-lazy-list* (cl-incf dash-lazy--counter)
                           (cl-incf dash-lazy--counter)
                           (-lazy-list (cl-incf dash-lazy--counter)
                                       (cl-incf dash-lazy--counter))))
  (should (= 0 dash-lazy--counter))
  (should (= 1 (-force-car dash-lazy--list)))
  (should (= 1 dash-lazy--counter))
  (should (= 2 (-force-cadr dash-lazy--list)))
  (should (= 2 dash-lazy--counter))
  (should (= 3 (-force-nth 2 dash-lazy--list)))
  (should (= 3 dash-lazy--counter))
  (should (= 4 (-force-nth 3 dash-lazy--list)))
  (should (null (-force-nthcdr 4 dash-lazy--list)))
  (should (= 4 dash-lazy--counter)))

(ert-deftest lazy-list-5 ()
  (setq dash-lazy--counter 0)
  (setq dash-lazy--list (-lazy-list* (cl-incf dash-lazy--counter)
                           (-lazy-list (cl-incf dash-lazy--counter)
                                       (cl-incf dash-lazy--counter))))
  (should (= 0 dash-lazy--counter))
  (should (= 1 (-force-car dash-lazy--list)))
  (should (= 1 dash-lazy--counter))
  (should (= 2 (-force-cadr dash-lazy--list)))
  (should (= 2 dash-lazy--counter))
  (should (= 3 (-force-nth 2 dash-lazy--list)))
  (should (= 3 dash-lazy--counter))
  (should (null (-force-nthcdr 3 dash-lazy--list)))
  (should (= 3 dash-lazy--counter)))

(ert-deftest -lazy-range ()
  (should (null (-force-cdrs (-lazy-range 1 0))))
  ;; More values than the compiler will optimize.
  (should (equal '(1 2 3 4 5 6 7 8 9 10)
                 (-force-cdrs (-lazy-range 1 11))))
  (should-all (equal '(1 2 3 4))
    (-force-cdrs (-lazy-range 1 5))
    (-force-cdrs (-lazy-take 4 (-lazy-iterate #'1+ 1))))
  (should-all (equal '(4 3 2 1))
    (-force-cdrs (-lazy-range 4 0 -1))
    (-force-cdrs (-lazy-take 4 (-lazy-range 4 -100 -1))))
  (should-all (equal '(1 3 5 7))
    (-force-cdrs (-lazy-range 1 8 2))
    (-force-cdrs (-lazy-take 4 (-lazy-range 1 100 2)))))

(ert-deftest -force-list ()
  (setq dash-lazy--counter 0)
  (should (equal '(2 5)
                 (-force-list (-lazy-list
                               (progn
                                 (cl-incf dash-lazy--counter)
                                 (-lazy (cl-incf dash-lazy--counter)))
                               (progn
                                 (cl-incf dash-lazy--counter)
                                 (prog1 (-lazy (cl-incf dash-lazy--counter))
                                   (cl-incf dash-lazy--counter)))))))
  (should (equal 5 dash-lazy--counter)))

(ert-deftest -force-list-no-cars ()
  (setq dash-lazy--counter 0)
  (should (equal '(4 5) (-force-list
                         (-force-list
                          (-lazy-list
                           (progn
                             (cl-incf dash-lazy--counter)
                             (-lazy (cl-incf dash-lazy--counter)))
                           (progn
                             (cl-incf dash-lazy--counter)
                             (prog1 (-lazy (cl-incf dash-lazy--counter))
                               (cl-incf dash-lazy--counter))))
                          :force-cars nil))))
  (should (equal 5 dash-lazy--counter)))

(ert-deftest -force-list-iter ()
  (setq dash-lazy--counter 0)
  (should (equal '(2 4)
                 (-force-list (funcall (iter-lambda ()
                                         (cl-incf dash-lazy--counter)
                                         (iter-yield (cl-incf dash-lazy--counter))
                                         (cl-incf dash-lazy--counter)
                                         (iter-yield (cl-incf dash-lazy--counter))
                                         (cl-incf dash-lazy--counter))))))
  (should (equal 5 dash-lazy--counter)))

(ert-deftest -force-each ()
  ;; --force-each-while traverses lazily
  (let ((count1 0)
        (count2 0)
        (values-seen nil))
    (--force-each-while (-lazy-list (cl-incf count1)
                                    (cl-incf count1)
                                    (cl-incf count1)
                                    (cl-incf count1))
        (prog1 (< it 3) (cl-incf count2))
      (push it values-seen))
    (should
     (equal
      '(2 1)
      values-seen))
    (should
     (= 3 count1))
    (should
     (= 3 count2)))
  ;; -force-each-while traverses lazily
  (let ((count1 0)
        (count2 0)
        (values-seen nil))
    (-force-each-while (-lazy-list (cl-incf count1)
                                   (cl-incf count1)
                                   (cl-incf count1)
                                   (cl-incf count1))
                       (lambda (x) (prog1 (< x 3) (cl-incf count2)))
                       (lambda (x) (push x values-seen)))
    (should (equal '(2 1) values-seen))
    (should (= 3 count1))
    (should (= 3 count2)))
  ;; --force-each traverses lazily
  (let ((count 0)
        (values-seen nil))
    (--force-each (-lazy-list (cl-incf count)
                              (cl-incf count)
                              (cl-incf count))
      (push it values-seen))
    (should (equal '(3 2 1) values-seen))
    (should (equal 3 count)))
  ;; -force-each traverses lazily
  (let ((count 0)
        (values-seen nil))
    (-force-each (-lazy-list (cl-incf count)
                             (cl-incf count)
                             (cl-incf count))
                 (lambda (x) (push x values-seen)))
    (should (equal '(3 2 1) values-seen))
    (should (= 3 count)))
  ;; --force-each et. al. support regular lists
  (let ((values-seen nil))
    (--force-each '(1 2 3)
      (push it values-seen))
    (should (equal '(3 2 1) values-seen))))


(ert-deftest force-cdrs ()
  (should-all (equal '(1 2 3 4))
    (-force-cdrs (-lazy-list 1 2 3 4))
    (-force-cdrs (-lazy-list* 1 2 '(3 4)))))


(ert-deftest -lazy-foldl ()
  (let* ((count 0)
         (list (-lazy-foldl (-flip 'cons)
                            nil
                            (-lazy-list (cl-incf count)
                                        (cl-incf count)
                                        (cl-incf count)))))
    (should (equal '(3 2 1) (-force-cdrs list)))))

(ert-deftest -lazy-foldr ()
  (let* ((count 0)
         (list (-lazy-foldr 'cons
                            nil
                            (-lazy-list (cl-incf count)
                                        (cl-incf count)
                                        (cl-incf count)))))
    (should (equal '(1 2 3) (-force-cdrs list))))
  )

(ert-deftest -force-reverse ()
  (should (equal '(3 2 1) (-force-reverse (-lazy-list 1 2 3)))))

(ert-deftest -lazy-reverse ()
  (should (equal '(3 2 1) (-force-cdrs (-lazy-reverse (-lazy-list 1 2 3))))))

(ert-deftest -force-reductions ()
  (should (= 3 (-force-min-by '< (-lazy-list 1 2 3))))
  (should (= 3 (--force-min-by
                (< it other)
                (-lazy-list 1 2 3))))
  (should (= 10 (-force-sum
                 (-lazy-list 1 2 3 4))))
  (should (= 24 (-force-product
                 (-lazy-list 1 2 3 4))))
  (should (equal '(1 2 3)
                 (-force-cdrs (-lazy-foldr 'cons nil
                                           (-lazy-list 1 2 3))))))

(ert-deftest -force-reduce ()
  (should-all (equal 1)
    (-reduce #'error '(1))
    (-force-reduce #'error (-lazy-list 1)))

  (should-all (equal 1)
    (-reduce (lambda () 1) nil)
    (-force-reduce (lambda () 1) (-lazy nil)))
  
  (should-all (equal '(1 2 . 3))
    ;; Sanity check.
    (-reduce (-flip #'cons) '(3 2 1))
    (--reduce (cons it acc) '(3 2 1))
    
    ;; Actual test.
    (-force-list (-force-reduce (-flip #'cons) (-lazy-list 3 2 1)))
    (-force-list (--force-reduce (cons it acc) (-lazy-list 3 2 1)))))

(ert-deftest -force-reduce-from ()
  (should-all (equal '(1 2 . 3))
    ;; Sanity check.
    (-reduce-from (-flip #'cons) 3 '(2 1))
    (--reduce-from (cons it acc) 3 '(2 1))

    ;; Actual test.
    (-force-list (-force-reduce-from (-flip #'cons) 3 (-lazy-list 2 1)))
    (-force-list (--force-reduce-from (cons it acc) 3 (-lazy-list 2 1)))))

(ert-deftest -force-reduce-r ()
  (should-all (equal 1)
    (-reduce-r #'error '(1))
    (-force-reduce-r #'error (-lazy-list 1)))

  (should-all (equal 1)
    (-reduce-r (lambda () 1) nil)
    (-force-reduce-r (lambda () 1) (-lazy nil)))
  
  (should-all (equal '(1 2 . 3))
    ;; Sanity check.
    (-reduce-r #'cons '(1 2 3))
    (--reduce-r (cons it acc) '(1 2 3))
    
    ;; Actual test.
    (-force-list (-force-reduce-r #'cons (-lazy-list 1 2 3)))
    (-force-list (--force-reduce-r (cons it acc) (-lazy-list 1 2 3)))))

(ert-deftest -force-reduce-r-from ()
  (should-all (equal '(1 2 . 3))
    ;; Sanity check.
    (-reduce-r-from #'cons 3 '(1 2))
    (--reduce-r-from (cons it acc) 3 '(1 2))

    ;; Actual test.
    (-force-list (-force-reduce-r-from #'cons 3 (-lazy-list 1 2)))
    (-force-list (--force-reduce-r-from (cons it acc) 3 (-lazy-list 1 2)))))

(ert-deftest -lazy-map ()
  (let* ((count1 0)
         (count2 0)
         (list (-lazy-map (lambda (x)
                            (cl-incf count2)
                            (* 2 x))
                          (-lazy-list (cl-incf count1)
                                      (cl-incf count1)))))
    (should (= 0 count1))
    (should (= 0 count2))
    (should (= 2 (-force-nth 0 list)))
    (should (= 1 count1))
    (should (= 1 count2))
    (should (= 4 (-force-nth 1 list)))
    (should (= 2 count1))
    (should (= 2 count2)))
  (should-all (equal '(2 3 4))
    (-force-cdrs (-lazy-map '1+ (-lazy-list 1 2 3)))
    (-force-cdrs (-lazy-map '1+ '(1 2 3)))))


(ert-deftest -lazy-take-drop ()
  ;; Test optimized cases.
  (should (equal '(1 2 3) (-force-cdrs (-lazy-drop 0 '(1 2 3)))))
  (should (equal '(2 3) (-force-cdrs (-lazy-drop 1 '(1 2 3)))))
  (should (equal '(3) (-force-cdrs (-lazy-drop 2 '(1 2 3)))))

  (should-all (equal '(1 2))
    (-force-cdrs (-lazy-take 2 (-lazy-list 1 2 3 4)))
    (-force-cdrs (-lazy-take-while (-rpartial '< 3)
                          (-lazy-list 1 2 3 4 1))))
  (should-all (equal '(3 4 1))
    (-force-cdrs (-lazy-drop 2 (-lazy-list 1 2 3 4 1)))
    (-force-cdrs (-lazy-drop-while (-rpartial '< 3)
                          (-lazy-list 1 2 3 4 1)))))

(ert-deftest -lazy-zip ()
  (should-all (equal '((1 a) (2 b)))
    (-force-cdrs (-lazy-zip (-lazy-list 1 2 3 4) (-lazy-list 'a 'b)))))


(ert-deftest -lazy-list-iterators ()
  (should-all (equal '(1 2 3))
    (let ((count 0))
      (-force-cdrs (-lazy-from-iter
                    (funcall (iter-lambda ()
                               (iter-yield (cl-incf count))
                               (iter-yield (cl-incf count))
                               (iter-yield (cl-incf count)))))))
    (let ((count 0))
      (-force-cdrs (-lazy-from-iter
                    (-lazy-to-iter
                     (-lazy-list (cl-incf count)
                                 (cl-incf count)
                                 (cl-incf count)))))))
  (let* ((count 0)
         (iter (-lazy-to-iter (-lazy-list (cl-incf count)
                                          (cl-incf count)
                                          (cl-incf count)))))
    (should (= 0 count))
    (should (= 1 (iter-next iter)))
    (should (= 1 count))
    (should (= 2 (iter-next iter)))
    (should (= 2 count))
    (should (= 3 (iter-next iter)))
    (should (= 3 count))

    ;; should-error doesn't work here.
    (should
     (condition-case nil
         (prog1 nil (iter-next iter))
       (iter-end-of-sequence t)))))

(ert-deftest -lazy-slice ()
  ;; (should (idempotent-call-p (-cut -lazy-slice <> 0)
  ;;                            (-lazy-range 1 3)))
  (should-all (equal '(2 3))
    (-slice '(1 2 3) 1)
    (-force-cdrs (-lazy-slice (-lazy-range 1 4) 1)))
  (should-all (equal '(1 3))
    (-slice '(1 2 3 4) 0 nil 2)
    (-slice '(1 2 3 4 5 6) 0 4 2)
    (-force-cdrs (-lazy-slice (-lazy-range 1 5) 0 nil 2))
    (-force-cdrs (-lazy-slice (-lazy-range 1 7) 0 4 2)))
  (should-all (equal '(2))
    (-force-cdrs (-lazy-slice (-lazy-range 1 4) 1 2))
    (-force-cdrs (-lazy-slice (-lazy-range 1 4) 1 -1))
    (-force-cdrs (-lazy-slice (-lazy-range 1 4) -2 2))
    (-force-cdrs (-lazy-slice (-lazy-range 1 4) -2 -1))))

(ert-deftest lazy-concat ()
  ;; Test basic functionality.
  (should-all (equal '(1 2 3 4))
    (-force-cdrs (-lazy-concat (-lazy-list 1 2 3 4)))
    (-force-cdrs (-lazy-concat (-lazy-list 1 2)
                               (-lazy-list 3 4)))
    (-force-cdrs (-lazy-join
                  (-lazy-list
                   (-lazy-list 1 2)
                   (-lazy-list 3 4)))))
  ;; Prove that it's really as lazy as advertized.
  (let* ((count 0)
         (list (-lazy-join
                (-lazy-list
                 (-lazy-list (cl-incf count) (cl-incf count))
                 (-lazy-list (cl-incf count) (cl-incf count))))))
    (should (= 0 count))
    (should (= 1 (-force-nth 0 list)))
    (should (= 1 count))
    (should (= 2 (-force-nth 1 list)))
    (should (= 2 count))
    (should (= 3 (-force-nth 2 list)))
    (should (= 3 count))
    (should (= 4 (-force-nth 3 list)))
    (should (= 4 count))))

(ert-deftest lazy-for ()
  (should
   (equal '(1 2 3)
          (cl-loop for x -in-lazy
                   (-lazy-list 1 2 3)
                   collect x)))
  (should
   (equal '((1 2 3) (2 3) (3))
          (cl-loop for x -on-lazy
                   (-lazy-list 1 2 3)
                   collect
                   (-force-cdrs x)))))

(ert-deftest -lazy-filter ()
  (should (equal '(2 4 6)
                 (-force-cdrs
                  (-lazy-filter 'evenp
                            (-lazy-range 1 7))))))

(ert-deftest dash-lazy--norm-function-form-quote ()
  (should (equal '#'x (dash-lazy--norm-function-form ''x))))

(ert-deftest dash-lazy--norm-function-form-identity ()
  (should (equal '#'identity
                 (dash-lazy--norm-function-form '(lambda (x) x)))))

(ert-deftest dash-lazy--norm-function-form-function-lambda ()
  (should (equal '(lambda (x) y)
           (dash-lazy--norm-function-form '#'(lambda (x) y)))))

(ert-deftest dash-lazy--norm-function-form-lambda-apply ()
  (should (equal 'f
           (dash-lazy--norm-function-form '(lambda (x y &rest z)
                                 (apply f x y z))))))

(ert-deftest dash-lazy--norm-function-form-lambda-funcall ()
  (should (equal 'f
           (dash-lazy--norm-function-form '(lambda (x y)
                                 (funcall f x y))))))

(ert-deftest dash-lazy--norm-function-form-lambda ()
  (should (equal '#'f
                 (dash-lazy--norm-function-form '(lambda (x y)
                                         (f x y))))))

(ert-deftest dash-lazy--norm-function-form-trivial-compose ()
  (should (equal '(lambda (x) (setq x (f x)) (g x))
                 (dash-lazy--norm-function-form
                  '(-compose (lambda (x) (g x))
                             (lambda (x) (f x)))))))

(ert-deftest dash-lazy--norm-function-form-nested-compose ()
  (should (equal '(-compose f g h)
                 (dash-lazy--norm-function-form
                  '(-compose f 'identity
                             (-compose g (-compose) h))))))

(ert-deftest dash-lazy--norm-lazy-list-form-nil ()
  (should (equal '(-lazy-list)
           (dash-lazy--norm-lazy-list-form '(-lazy nil)))))

(ert-deftest dash-lazy--norm-lazy-list-form-lazy-nil ()
  (should (equal '(-lazy-list)
           (dash-lazy--norm-lazy-list-form '(-lazy nil)))))

(ert-deftest dash-lazy--norm-lazy-list-form-lazy-quoted ()
  (should (equal '(-lazy-list 'a 'b 'c)
           (dash-lazy--norm-lazy-list-form '(-lazy '(a b c))))))

(ert-deftest dash-lazy--norm-lazy-list-form-lazy+list ()
  (should (equal '(-lazy-list x)
           (dash-lazy--norm-lazy-list-form '(-lazy (list x))))))

(ert-deftest dash-lazy--norm-lazy-list-form-lazy-cons ()
  (should (equal '(-lazy-list x 'y 'z)
           (dash-lazy--norm-lazy-list-form
            '(-lazy (cons x '(y z)))))))

(ert-deftest -lazy-any? ()
  (should (eq t
              (-force-any? #'cl-evenp
                          (-lazy-list
                           (-lazy 1)
                           (-lazy 2)
                           (error))))))


(ert-deftest dash-lazy--expand-lazy-foldl ()
  (should (equal '(-lazy-foldl #'(lambda (acc it)
                                   (g acc (f it)))
                               x0 xs :force-cars t)
                 (macroexpand-all
                  '(-lazy-foldl #'g x0 (-lazy-map #'f xs))))))

(ert-deftest dash-lazy--expand-lazy-foldr ()
  (should (equal '(-lazy-foldr #'(lambda (it acc)
                                   (g (f it) acc))
                               x0 xs :force-cars t)
                 (macroexpand-all
                  '(-lazy-foldr #'g x0 (-lazy-map #'f xs))))))

(ert-deftest dash-lazy--expand-lazy-mapcat ()
  (should (equal '(-lazy-mapcat (-compose #'g #'f)
                                xs :force-cars t)
                 (macroexpand-all
                  '(-lazy-mapcat #'g (-lazy-map #'f xs))))))

(ert-deftest dash-lazy--expand-lazy-map ()
  (should (equal '(-lazy-map (-compose #'g #'f)
                             xs :force-cars t)
                 (macroexpand-all
                  '(-lazy-map #'g (-lazy-map #'f xs)))))
  (should (equal '(-lazy-map #'g (-lazy-map #'f xs)
                             :force-cars nil)
                 (macroexpand-all
                  '(-lazy-map
                    #'g (-lazy-map #'f xs)
                    :force-cars nil))))
  (should (equal '(-lazy-map
                   #'g (-lazy-map #'f xs :force-cars (f a))
                   :force-cars (f a))
                 (macroexpand-all
                  '(-lazy-map
                    #'g (-lazy-map #'f xs :force-cars (f a))
                    :force-cars (f a)))))
  (should (equal '(-lazy-map (-compose #'g #'f)
                             xs :force-cars t)
                 (macroexpand-all
                  '(-lazy-map
                    #'g (-lazy-map #'f xs :force-cars nil)))))
  (should (equal '(-lazy-map (-compose #'g #'f)
                             xs :force-cars a)
                 (macroexpand-all
                  '(-lazy-map
                    #'g (-lazy-map #'f xs :force-cars a)
                    :force-cars a)))))

(ert-deftest dash-lazy--expand-force-each ()
  (should (equal '(-force-each xs (-compose #'g #'f) :force-cars t)
                 (macroexpand-all
                  '(-force-each (-lazy-map #'f xs) #'g)))))

(ert-deftest dash-lazy--expand--force-each ()
  (should
   (equal '(-force-each xs
                        #'(lambda (it)
                            (setq it (f2 (f1 it)))
                            (g2 (g1 it)))
                        :force-cars t)
          (macroexpand-all
           '(--force-each
                (--lazy-map (f2 (f1 it)) xs)
              (g2 (g1 it)))))))

;; (ert-deftest -lazy-foldr-expand ()
;;   (should
;;    ;; This *looks* like it could be simplified further, but how?  The
;;    ;; outermost funcall could be eliminated or tranformed into a `let'
;;    ;; form, but it hardly seems with the trouble.
;;    (equal '(funcall
;;             #'(lambda (c n)
;;                 (-lazy-fold-map #'identity
;;                             #'(lambda (x ys)
;;                                 (funcall c (funcall g x) ys))
;;                             n xs))
;;             k z)
;;           (macroexpand-all
;;            '(-lazy-foldr k z (-lazy-map g xs)))))
;;   (should
;;    (equal '(funcall #'(lambda (c n)
;;                         (-lazy-fold-map #'identity
;;                                     #'(lambda (x ys)
;;                                         (funcall c (g x) ys))
;;                                     n xs))
;;                     #'k z)
;;           (macroexpand-all
;;            '(--lazy-foldr (k it acc) z (--lazy-map (g it) xs))))))
