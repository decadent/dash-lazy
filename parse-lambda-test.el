;; -*- lexical-binding: t -*-

(require 'ert)
(require 'parse-lambda)

(ert-deftest parse-lambda--test-full ()
  (should (equal (parse-lambda-make-parsed :lambda-sym 'lambda-ish
                               :argspec '(x y)
                               :docstring "Docstring."
                               :declare-form '(declare woot)
                               :interactive-form '(interactive "P")
                               :body '((+ x y)))
                 (parse-lambda '(lambda-ish (x y)
                                            "Docstring."
                                            (declare woot)
                                            (interactive "P")
                                            (+ x y))))))

(ert-deftest parse-lambda--test-body-abbreviated ()
  (should (equal (parse-lambda-make-parsed :argspec t
                               :body '((+ x y)))
                 (parse-lambda-body '((+ x y))))))

(ert-deftest parse-lambda--test-update-body ()
  (should (equal '("Docstring."
                   (declare woot)
                   (interactive "P")
                   (progn :transformed (+ x y)))
                 (parse-lambda-apply-to-body (lambda (body)
                                   `((progn :transformed ,@body)))
                                 '("Docstring."
                                   (declare woot)
                                   (interactive "P")
                                   (+ x y))))))
