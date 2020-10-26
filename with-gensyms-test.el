;; -*- lexical-binding: t -*-

(require 'ert)
(require 's)
(require 'with-gensyms)

(ert-deftest with-gensyms--test-with-gensyms ()
  (seq-let
      (a b c d)
      (eval '(progn
               (defmacro get-syms (x y)
                 (with-gensyms (x-sym y-sym)
                   `(list ',x ',y ',x-sym ',y-sym)))
               (get-syms a b)))
    (should (equal a 'a))
    (should (equal b 'b))
    (should-not (equal c 'a))
    (should-not (equal c 'x))
    (should-not (equal c 'x-sym))
    (should-not (equal d 'b))
    (should-not (equal d 'y))
    (should-not (equal d 'y-sym))
    (should (s-prefix? "x-sym" (symbol-name c)))
    (should (s-prefix? "y-sym" (symbol-name d)))))

(ert-deftest with-gensyms--test-with-gensyms--ext ()
  (should (equal '(1 1 1 1 z)
                 (progn
                   (defmacro with-gensyms--twice (x y)
                     (with-gensyms ((x x) (y-sym y) z-sym)
                       `(let ((,z-sym 'z))
                          (list ,x ,x ,y-sym ,y-sym ,z-sym))))
                   (let ((x-counter 0)
                         (y-counter 0))
                     (with-gensyms--twice (cl-incf x-counter)
                            (cl-incf y-counter)))))))

(ert-deftest with-gensyms--test-with-gensyms--preserves-order ()
  (let (accum)
    (defmacro with-gensyms--test (x y)
      (with-gensyms ((x x) (y y))
        `(list ,y ,x ,y ,x)))
    (should (equal '((b a) (a) (b a) (a))
                   (with-gensyms--test (push 'a accum)
                           (push 'b accum))))
    (should (equal '(b a) accum))))


(ert-deftest with-gensyms--test-with-eval-once ()
  (should (equal '(1 1 1 1 (cl-incf y-counter))
                 (eval '(progn
                          (defmacro with-gensyms--twice (x y)
                            (with-eval-once (x (y y-form))
                              `(list ,x ,x ,y ,y ',y-form)))
                          (let ((x-counter 0)
                                (y-counter 0))
                            (with-gensyms--twice (cl-incf x-counter)
                                     (cl-incf y-counter))))))))

(ert-deftest with-gensyms--test-with-eval-once-preserves-order ()
  (eval
   '(let (accum)
      (defmacro with-gensyms--test (x y)
        (with-eval-once (x y)
          `(list ,y ,x ,y ,x)))
      (should (equal '((b a) (a) (b a) (a))
                     (with-gensyms--test (push 'a accum)
                            (push 'b accum))))
      (should (equal '(b a) accum)))))
