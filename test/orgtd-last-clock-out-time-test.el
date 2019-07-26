(describe "orgtd-last-clock-out-time"
  (it "returns nil if called at empty heading"
    (with-org "* some heading"
      (expect (orgtd-last-clock-out-time) :to-be nil)))

  (it "returns nil for heading with some timestamps, but no clock history"
    (with-org "* heading
[2016-01-07 Thu]--[2016-01-14 Thu]"
      (expect (orgtd-last-clock-out-time) :to-be nil)))

  (it "returns clock-out time if been clocked only once"
    (let ((timestamp "[2016-01-06 Wed 11:31]"))
      (with-org (format "* heading
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:"
                        timestamp)
        (expect (float-time (org-time-string-to-time timestamp))
                :to-equal
                (orgtd-last-clock-out-time)))))

  (it "detects timestamps in deeply nested headings"
    (let ((timestamp "[2016-01-06 Wed 11:31]"))
      (with-org (format "* heading
** subheading
*** subsubheading
:LOGBOOK:
CLOCK: [2016-01-06 Wed 11:00]--%s =>  0:31
:END:"
                        timestamp)
        (expect (float-time (org-time-string-to-time timestamp))
                :to-equal
                (orgtd-last-clock-out-time)))))

  (it "detects latest timestamp if multiple are present"
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
        (expect  (float-time (org-time-string-to-time timestamp-latest))
                 :to-equal
                 (orgtd-last-clock-out-time)))))

  (it "gets timestamp from only within a tree"
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
        (expect  (float-time (org-time-string-to-time timestamp-earlier))
                 :to-equal
                 (orgtd-last-clock-out-time))))))
