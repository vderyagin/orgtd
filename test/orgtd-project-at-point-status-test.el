;;; -*- lexical-binding: t -*-

(require 'test/test-helper)

(describe "orgtd-project-at-point-status"
  (it "returns :stuck for project without NEXT tasks"
    (with-org "* TODO project
** TODO task one
** TODO task two"
      (expect (orgtd-project-at-point-status) :to-equal :stuck)))

  (it "returns :active for project with NEXT task"
    (with-org "* TODO project
** NEXT active task
** TODO other task"
      (expect (orgtd-project-at-point-status) :to-equal :active)))

  (it "returns :active for project with scheduled task"
    (with-org "* TODO project
** TODO scheduled task
SCHEDULED: <2017-02-26 Sun>"
      (expect (orgtd-project-at-point-status) :to-equal :active)))

  (it "returns :active for project with task with deadline"
    (with-org "* TODO project
** TODO task with deadline
DEADLINE: <2017-02-26 Sun>"
      (expect (orgtd-project-at-point-status) :to-equal :active)))

  (it "returns :suspended for HOLD project"
    (with-org "* HOLD suspended project
** TODO task"
      (with-org-todo-keywords '("TODO" "HOLD" "DONE")
        (expect (orgtd-project-at-point-status) :to-equal :suspended))))

  (it "returns :finished for DONE project"
    (with-org "* DONE finished project
** TODO leftover task"
      (expect (orgtd-project-at-point-status) :to-equal :finished)))

  (it "raises error when not at project heading"
    (with-org "* plain heading"
      (expect (orgtd-project-at-point-status) :to-throw 'error)))

  (it "raises error when at standalone task"
    (with-org "* TODO standalone task"
      (expect (orgtd-project-at-point-status) :to-throw 'error))))
