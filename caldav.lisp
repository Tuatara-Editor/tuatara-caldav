;;;; caldav.lisp --- The caldav package: the agenda shared with other calendars over CalDAV.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One line in the init file loads it: (load-package "caldav").

(defpackage #:caldav
  (:use #:cl #:tuatara)
  (:documentation "The agenda shared with GNOME Calendar, Thunderbird and phones through a
CalDAV server all of them use (Radicale, or EteSync through etesync-dav):
calendar files (iCalendar) read and written, nothing lost; their values,
time zones and rules of recurrence; and the occurrences of their events and
tasks within the days asked for. A calendar file is never trusted: what it
holds is only read, and its size, nesting and occurrences are bounded."))

(in-package #:caldav)

(define-package-setting *caldav-file* "agenda-caldav.org"
  "The Org file the calendars synced are kept in, a heading each, their
events under it. A name alone is a file beside the first of the agenda
files; a path is that file. It is read by the agenda whether or not
*org-agenda-files* names it. It may be one of your agenda files
\(\"~/org/agenda.org\"): then the calendars are headings among your own,
and only what is under a calendar's heading is synced, the rest of the file
left as it is.")

(define-package-setting *caldav-accounts* '()
  "The CalDAV accounts whose calendars are synced, each a list of keywords
and values: :NAME, what its calendars' headings are named after (\"EteSync\");
:URL, the server's address, https, or http to this machine alone (EteSync's
own bridge, etesync-dav, at http://localhost:37358/); :USER, the name it
signs in with; and :CALENDARS, the names of those of its calendars to sync,
every one when it is not given (M-x caldav-calendars lists them):

  (:name \"EteSync\" :url \"http://localhost:37358/\" :user \"you\"
   :calendars (\"Personal\" \"Family\"))

The password is kept in the desktop's keyring, never here:

  secret-tool store --label=\"Tuatara calendars\" service tuatara-caldav user USER

or in a file of your own a command gives it from, as mbsync's PassCmd: an
account's :PASSWORD-COMMAND is run, no shell, and the first line it prints
is the password, a GnuPG file's passphrase asked in gpg-agent's window:

  :password-command \"gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.etesync-pass.gpg\"")

(define-package-setting *caldav-minutes* 1
  "How often the calendars are synced on their own, in minutes: the first
sync a few seconds after Tuatara starts, then every so many minutes, in the
background, saying nothing unless something changed or failed (a failure
told once, not again while it lasts). NIL syncs only when asked
\(\\[caldav-sync]).")

(define-package-setting *caldav-subscriptions* '()
  "Calendar files at an address read into the agenda, never sent back, each a
list of keywords and values: :NAME, what its heading is named, and :URL, its
address (https). Public holidays, a team's calendar. Each is fetched at
every sync and written as an Org file of its own in the cache, the agenda
reading it as one of its files:

  ((:name \"Holidays\" :url \"https://example.org/holidays.ics\"))")

(dolist (file '("lines" "values" "recur" "zones" "occurrences" "events" "server" "sync" "commands"))
  (load-package-file (format nil "~a.lisp" file)))
