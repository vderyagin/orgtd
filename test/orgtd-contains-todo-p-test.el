;;; -*- lexical-binding: t -*-

(describe "orgtd-contains-todo-p"
  (it "handles headings with nothing inside"
    (with-org "* heading"
      (expect (orgtd-contains-todo-p) :to-be nil)))

  (it "ignores todo keyword on heading itself"
    (with-org "* TODO heading"
      (expect (orgtd-contains-todo-p) :to-be nil )))

  (it "gracefully handles invocation outside of headings"
    (with-org "some text"
      (expect (orgtd-contains-todo-p) :to-be nil)))

  (it "handles headings containing a todo item"
    (with-org "* heading
** DONE second-level heading"
      (expect (orgtd-contains-todo-p) :to-be-truthy)))

  (it "finds deeply tested todo items"
    (with-org "* level 1
** level 2
*** level 3
**** level 4
***** TODO level 5"
      (expect (orgtd-contains-todo-p) :to-be-truthy)))

  (it "limits itself to tree at point"
    (with-org "* heading a
** subheading
* heading b
** TODO subheading"
      (expect (orgtd-contains-todo-p) :to-be nil))))
