;;;; caldav.lisp --- The caldav package: the agenda shared with other calendars over CalDAV.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; Needs: Tuatara 0.90.28
;;;;
;;;; One line in the init file loads it: (load-package "caldav").

(defpackage #:caldav
  (:use #:cl #:tuatara)
  (:documentation "The agenda shared with GNOME Calendar, Thunderbird and phones through a
Calendaring Extensions to WebDAV (CalDAV) server all of them use (Radicale, or
EteSync through etesync-dav). It handles calendar files (iCalendar, the
Internet Calendaring and Scheduling format), read and written, nothing lost;
their values, time zones and rules of recurrence; and the occurrences of their
events and tasks within the days asked for. A calendar file is never trusted: what it
holds is only read, and its size, nesting and occurrences are bounded."))

(in-package #:caldav)

(declare-package-needs
 '("curl" "talks to the calendar server" :required t :install "install curl")
 '("secret-tool" "reads an account's password from the desktop's keyring"
   :install "install libsecret-tools (Debian, Ubuntu) or libsecret (Fedora)"))

(define-package-setting *caldav-file* "agenda-caldav.org"
  "The Org file the synced calendars are kept in.
Each calendar has a heading, its events under it. The agenda reads it whether
or not *org-agenda-files* names it. It may be one of your agenda files
\(\"~/org/agenda.org\"): then the calendars are headings among your own, and only
what is under a calendar's heading is synced, the rest of the file left as it
is.
- a name alone: a file beside the first of the agenda files.
- a path: that file."
  :values file)

(define-package-setting *caldav-accounts* '()
  "The accounts whose calendars are synced.
Each is a Calendaring Extensions to WebDAV (CalDAV) account, a list of
keywords and values:
- :NAME: what its calendars' headings are named after (\"EteSync\").
- :URL: the server's address, https, or http to this machine alone (EteSync's
  own bridge, etesync-dav, at http://localhost:37358/).
- :USER: the name it signs in with.
- :CALENDARS: the names of those of its calendars to sync, every one when it
  is not given (M-x caldav-calendars lists them).
- :PASSWORD-COMMAND: a command that gives the password, run without a shell;
  the first line it prints is the password. A GnuPG file's passphrase is asked
  in gpg-agent's window.

An example:

  (:name \"EteSync\" :url \"http://localhost:37358/\" :user \"you\"
   :calendars (\"Personal\" \"Family\"))

The password is kept in the desktop's keyring, never here:

  secret-tool store --label=\"Tuatara calendars\" service tuatara-caldav user USER

or in a file of your own a command gives it from, as mbsync's PassCmd does:

  :password-command \"gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.etesync-pass.gpg\""
  :values (list-of "(:NAME NAME :URL URL :USER USER :CALENDARS (NAME ...))"))

(define-package-setting *caldav-minutes* 1
  "How often the calendars are synced on their own, in minutes.
The first sync is a few seconds after Tuatara starts, then every so many
minutes, in the background, saying nothing unless something changed or failed
\(a failure is told once, not again while it lasts). Never more often than once
a minute.
- a number above 0: the minutes between syncs.
- 0 or less: sync only when asked.
- nil: sync only when asked (\\[caldav-sync])."
  :values (or (member nil) number))

(define-package-setting *caldav-subscriptions* '()
  "Calendar files at an address, read into the agenda and never sent back.
Public holidays, a team's calendar. Each is fetched at every sync and written
as an Org file of its own in the cache, the agenda reading it as one of its
files. Each is a list of keywords and values:
- :NAME: what its heading is named.
- :URL: its address (https).

An example:

  ((:name \"Holidays\" :url \"https://example.org/holidays.ics\"))"
  :values (list-of "(:NAME NAME :URL URL)"))

(dolist (file '("lines" "values" "recur" "zones" "occurrences" "events" "server" "sync" "commands"))
  (load-package-file (format nil "~a.lisp" file)))
