(ert-deftest returns-nil-if-called-at-empty-heading ()
  (with-org "* some heading"
    (should-not (orgtd-last-clock-out-time))))

(ert-deftest returns-nil-for-heading-with-some-timestamps-but-no-clocking ()
  (with-org "* heading
[2016-01-07 Thu]--[2016-01-14 Thu]"
    (should-not (orgtd-last-clock-out-time))))

(ert-deftest returns-clock-out-time-if-been-clocked-only-once ()
  (let ((timestamp "[2016-01-06 Wed 11:31]"))
    (with-org (format "* heading
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:"
                      timestamp)
      (should (equal (float-time (org-time-string-to-time timestamp))
                     (orgtd-last-clock-out-time))))))

(ert-deftest detects-time-stamps-in-deeply-nested-headings ()
  (let ((timestamp "[2016-01-06 Wed 11:31]"))
    (with-org (format "* heading
** subheading
*** subsubheading
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:"
                      timestamp)
      (should (equal (float-time (org-time-string-to-time timestamp))
                     (orgtd-last-clock-out-time))))))

(ert-deftest detects-latest-timestamp-if-multiple-are-present ()
  (let ((timestamp-earlier "[2016-01-06 Wed 11:31]")
        (timestamp-latest "[2016-01-07 Thu 11:31]"))
    (with-org (format "* heading
** foo
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:

** bar
:LOGBOOK:
CLOCK: [2016-01-07 Thu 11:00]--%s =>  0:31
:END:
"
                      timestamp-earlier timestamp-latest)
      (should (equal (float-time (org-time-string-to-time timestamp-latest))
                     (orgtd-last-clock-out-time))))))

(ert-deftest gets-timestamp-from-only-within-a-tree ()
  (let ((timestamp-earlier "[2016-01-06 Wed 11:31]")
        (timestamp-latest "[2016-01-07 Thu 11:31]"))
    (with-org (format "* heading
** <POINT> foo
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:

** bar
:LOGBOOK:
CLOCK: [2016-01-07 Thu 11:00]--%s =>  0:31
:END:
"
                      timestamp-earlier timestamp-latest)
      (should (equal (float-time (org-time-string-to-time timestamp-earlier))
                     (orgtd-last-clock-out-time))))))
