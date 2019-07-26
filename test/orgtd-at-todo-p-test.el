(describe "orgtd-at-todo-p"
  (it "handles todos"
    (with-org "* TODO foobar"
      (expect (orgtd-at-todo-p) :to-be-truthy)))

  (it "handles non-todos"
    (with-org "* foobar"
      (expect (orgtd-at-todo-p) :to-be nil)))

  (it "handles non-todos with what appears to be a todo keyword, but is not"
    (with-org "* NXT foobar"
      (expect (orgtd-at-todo-p) :to-be nil))))
