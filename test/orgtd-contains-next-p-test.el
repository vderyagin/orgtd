;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-contains-next-p"
  (it "rejects headings without todo keywords"
    (with-org "* plain heading"
      (expect (orgtd-contains-next-p) :to-be nil)))

  (it "rejects bare NEXT items"
    (with-org "* NEXT foo"
      (expect (orgtd-contains-next-p) :to-be nil)))

  (it "accepts heading with nested NEXT item"
    (with-org "* plain heading
** NEXT next thing"
      (expect (orgtd-contains-next-p) :to-be-truthy)))

  (it "accepts heading with deeply nested NEXT item"
    (with-org "* TODO project
** TODO subproject
*** TODO subsubproject
**** NEXT next task"
      (expect (orgtd-contains-next-p) :to-be-truthy)))

  (it "is not fooled by non-heading containing NEXT keyword"
    (with-org "* heading
foo bar * NEXT baz")
    (expect (orgtd-contains-next-p) :to-be nil))

  (it "accepts other keywords if set"
    (with-org "* TODO project
** TODO subproject
*** TODO subsubproject
**** FOOBAR next task"
      (let ((orgtd-next-task-keywords '("FOOBAR")))
        (with-org-todo-keywords '("TODO" "FOOBAR" "DONE")
          (expect (orgtd-contains-next-p) :to-be-truthy))))))
