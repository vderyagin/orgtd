;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-parent-project-location"
  (it "just returns `nil' when there is nothing there"
    (with-org ""
      (expect (orgtd-parent-project-location) :to-be nil)))

  (it "just returns `nil' when called outside of any project"
    (with-org "* TODO a standalone task"
      (expect (orgtd-parent-project-location) :to-be nil)))

  (it "just returns `nil' when called at project heading"
    (with-org "* TODO <POINT> a project
** TODO some task"
      (expect (orgtd-parent-project-location) :to-be nil)))

  (it "finds project if it is an immediate parent"
    (with-org "* TODO project heading
** TODO <POINT> task heading"
      (expect (orgtd-parent-project-location)
              :to-equal
              (save-excursion
                (goto-char (point-min))
                (point-marker)))))


  (it "finds project if it is itself nested"
    (with-org "* just some heading
** TODO project heading
*** TODO <POINT> task heading"
      (expect (orgtd-parent-project-location)
              :to-equal
              (save-excursion
                (search-backward "project")
                (move-beginning-of-line 1)
                (point-marker)))))

  (it "finds project location for deeply nested todo items"
    (with-org "* TODO project
** foo
*** bar
**** TODO <POINT> a task"
      (expect (orgtd-parent-project-location)
              :to-equal
              (save-excursion
                (goto-char (point-min))
                (point-marker)))))

  (it "ignores subprojects in tree above"
    (with-org "* TODO top project heading
** TODO subproject heading
*** TODO task heading <POINT>"
      (expect (orgtd-parent-project-location)
              :to-equal
              (save-excursion
                (search-backward "top project")
                (move-beginning-of-line 1)
                (point-marker)))))

  (it "finds project location for deeply nested todo items that are nested under other todo items"
    (with-org "* TODO top project
** foo
*** TODO subproject
**** bar
***** baz
****** TODO <POINT> a task"
      (expect (orgtd-parent-project-location)
              :to-equal
              (save-excursion
                (search-backward "top project")
                (move-beginning-of-line 1)
                (point-marker))))))
