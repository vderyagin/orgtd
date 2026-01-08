;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-contained-in-todo-p"
  (it "handles top-level headings"
    (with-org "* foo"
      (expect (orgtd-contained-in-todo-p) :to-be nil)))

  (it "handles headings with todo parent"
    (with-org "* TODO foo
** <POINT> bar"
      (expect (orgtd-contained-in-todo-p) :to-be-truthy)))

  (it "handles funny nesting"
    (with-org "* a
** TODO b
*** c
**** <POINT> d"
      (expect (orgtd-contained-in-todo-p) :to-be-truthy)))

  (it "rejects todo items in hierarchy of plain headings"
    (with-org "* a
** b
*** c
**** d
***** TODO <POINT> e"
      (expect (orgtd-contained-in-todo-p) :to-be nil)))

  (it "gracefully returns when invoked from top level"
    (with-org ""
      (expect (orgtd-contained-in-todo-p) :to-be nil))))
