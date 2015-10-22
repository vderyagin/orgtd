(require 'orgtd)

(ert-deftest handles-todos ()
  (with-org "* TODO foobar"
    (should (orgtd-todo-p))))

(ert-deftest handles-non-todos ()
  (with-org "* foobar"
    (should-not (orgtd-todo-p))))

(ert-deftest handles-non-todos-with-what-appears-to-be-a-todo-keyword-but-is-not ()
  (with-org "* NXT foobar"
    (should-not (orgtd-todo-p))))
