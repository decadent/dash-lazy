;; -*- lexical-binding: t -*-

(require 'ert)
(require 'nameless-defstruct)

(nameless-defstruct nameless-defstruct-test- foo bar)

(ert-deftest nameless-defstruct--test ()
  (let ((s (nameless-defstruct-test-make-foo :bar "bar")))
    (should (nameless-defstruct-test-foo-p s))
    (should (equal s (nameless-defstruct-test-copy-foo s)))
    (should (equal "bar" (nameless-defstruct-test-foo-bar s)))
    (should (equal "bar" (cl-struct-slot-value 'nameless-defstruct-test-foo 'bar s)))
    (setf (nameless-defstruct-test-foo-bar s) "baz")
    (should (equal "baz" (nameless-defstruct-test-foo-bar s)))))
