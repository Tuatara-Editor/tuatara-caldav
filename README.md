# caldav

Tuatara's agenda shared with GNOME Calendar, Thunderbird and phones, through
a CalDAV server all of them use: Radicale, which Fedora, Debian, Ubuntu and
Arch package, or EteSync through its own local bridge, etesync-dav, which
keeps the calendars encrypted end to end. One of the Tuatara project's
packages, not part of the editor.

It needs Tuatara 0.90.10 or newer, and two small programs most Linux
systems have already: `curl` and `secret-tool`.

## Setting it up, step by step

You need three things: a calendar server, the package in Tuatara, and the
server's password in your desktop's keyring. Pick **one** of the two ways
to have a server:

- **A. EteSync**, if you have an EteSync account. Your calendars stay
  encrypted end to end; a small program on your computer, etesync-dav,
  lets Tuatara (and Thunderbird, GNOME Calendar) talk to them.
- **B. Your own Radicale**, free, on a computer of yours that is on, reached
  from your laptop and phone over **Tailscale**, a private network between
  your own devices. Nothing is open to the internet.

Everywhere below, write your own names where the examples say `you`,
`EteSync` or `home-server`.

### 1. Check curl and secret-tool

In a terminal:

```sh
curl --version
secret-tool --help
```

If one says *command not found*, install it:

| System          | Command                                   |
|-----------------|-------------------------------------------|
| Fedora          | `sudo dnf install curl libsecret`         |
| Debian, Ubuntu  | `sudo apt install curl libsecret-tools`   |
| Arch, Omarchy   | `sudo pacman -S curl libsecret`           |
| Fedora Silverblue, other atomic systems | `rpm-ostree install libsecret`, then restart |

### 2A. EteSync: run etesync-dav

1. Download etesync-dav for Linux from its releases page,
   <https://github.com/etesync/etesync-dav/releases>, into `~/.local/bin/`,
   and make it runnable:

   ```sh
   chmod +x ~/.local/bin/etesync-dav
   ```

   (Installing it with `pipx install etesync-dav` works too, but on the
   newest Python versions one of its parts may not build; the download
   always works.)

2. Start it, and add your account:

   ```sh
   etesync-dav manage add you      # asks your EteSync password once
   etesync-dav manage get you      # shows the password for programs here
   etesync-dav                     # keep this running
   ```

   The password `manage get` shows is **not** your EteSync password: it is
   one etesync-dav made for programs on this computer. That is the one
   Tuatara needs. (Its page, <http://localhost:37358/>, shows the same with
   *Copy Password*.)

3. Have it start with your computer: add `etesync-dav` to your desktop's
   *Startup Applications* (GNOME: the *Tweaks* app, *Startup
   Applications*; KDE: *System Settings*, *Autostart*).

etesync-dav answers on `http://localhost:37358/` and only to this computer,
which is why Tuatara may use plain http for it.

### 2B. Your own Radicale, reached over Tailscale

Do this on the computer that keeps your calendars (a desktop that is on,
a small home server or a Raspberry Pi), called `home-server` here.

1. **Install Radicale and the password tools:**

   | System          | Command                                                   |
   |-----------------|-----------------------------------------------------------|
   | Fedora          | `sudo dnf install radicale httpd-tools python3-bcrypt`    |
   | Debian, Ubuntu  | `sudo apt install radicale apache2-utils python3-bcrypt`  |
   | Arch, Omarchy   | `sudo pacman -S radicale apache python-bcrypt`            |

2. **Make your user and its password** (a long one of its own, not used
   anywhere else; `-B` keeps it as a bcrypt hash, never as it is):

   ```sh
   sudo htpasswd -B -c /etc/radicale/users you
   ```

3. **Tell Radicale to listen to this computer alone** and to ask for that
   password. Edit `/etc/radicale/config` (`sudo nano /etc/radicale/config`)
   so these sections say:

   ```ini
   [server]
   hosts = localhost:5232

   [auth]
   type = htpasswd
   htpasswd_filename = /etc/radicale/users
   htpasswd_encryption = autodetect

   [rights]
   type = owner_only
   ```

   `owner_only` lets each user see their own calendars alone.

4. **Start it, now and at every start:**

   ```sh
   sudo systemctl enable --now radicale
   ```

   Open <http://localhost:5232/> on that computer, log in, and make a
   calendar (*Create new addressbook or calendar*). Tuatara syncs the
   calendars that are there; it never makes one.

5. **Install Tailscale** on this computer and on every device that should
   see your calendars (laptop, phone): <https://tailscale.com/download>.
   Sign in to the same account on each, and on Linux:

   ```sh
   sudo tailscale up
   ```

6. **Turn on HTTPS for your tailnet**, once: in the Tailscale admin page,
   <https://login.tailscale.com/admin/dns>, enable **MagicDNS** and
   **HTTPS Certificates**.

7. **Give Radicale an https address on your tailnet**, on `home-server`:

   ```sh
   sudo tailscale serve --bg 5232
   tailscale serve status
   ```

   The status shows its address, such as
   `https://home-server.tail1234.ts.net/`. Only your own devices, signed in
   to your tailnet, can reach it, with a real certificate; nothing is open
   to the internet. **Never use `tailscale funnel`**, which opens it to
   everyone, and never forward a port on your router for it.

On `home-server` itself, Tuatara may use `http://localhost:5232/`; on your
other computers, the `https://...ts.net/` address.

### 3. Keep the password in your desktop's keyring

On each computer Tuatara syncs from (never write the password in the init
file):

```sh
secret-tool store --label="Tuatara calendars" service tuatara-caldav user you
```

It asks for the password: for EteSync the one `etesync-dav manage get`
showed, for Radicale the one you gave `htpasswd`.

#### If the password was stored for calendar-sync

Before Tuatara 0.90.13 the package was called calendar-sync, and its
password was kept under the service `tuatara-calendar-sync`. Copy it to the
new name once, on each computer that has it (`you` being the account's
`:user`); the password is not shown:

```sh
secret-tool lookup service tuatara-calendar-sync user you | secret-tool store --label="Tuatara calendars" service tuatara-caldav user you
```

Once `C-c s l` (`caldav-calendars`) lists the account's calendars, Tuatara
has found the password under its new name, and the old entry can go:

```sh
secret-tool clear service tuatara-calendar-sync user you
```

#### Or: the password in a GnuPG file

Instead of the keyring, the password can be kept in a file encrypted with
GnuPG, and given by a command, as mbsync's `PassCmd` gives one:

```sh
printf '%s\n' 'THE-PASSWORD' | gpg --symmetric --output ~/.etesync-pass.gpg   # asks a passphrase for the file
chmod 600 ~/.etesync-pass.gpg
```

(Clear your shell's history of that line afterwards, or type the password
into a file and encrypt the file, deleting it after.) Then give the account
a `:password-command` in step 4:

```lisp
'((:name "EteSync" :url "http://localhost:37358/" :user "you"
   :password-command "gpg --quiet --for-your-eyes-only --no-tty --decrypt ~/.etesync-pass.gpg"))
```

At each sync Tuatara runs the command, no shell (a word beginning with `~/`
is a path in your home folder), and takes the first line it prints as the
password; gpg-agent asks for the file's passphrase in a window of its own,
and remembers it a while (10 minutes as standard, each use counting again,
so syncing every minute keeps it remembered up to gpg-agent's limit, two
hours as standard; then it asks again). The password is never
written anywhere: it goes from gpg to curl through pipes, and is wiped from
Tuatara's memory once the sync is over. When the command fails, Tuatara
says so, never what the command printed.

### 4. Load the package in Tuatara

Open your init file: in Tuatara press `C-x C-f`, type
`~/.config/tuatara/init.lisp` and press `RET`. Add this to it, with **one**
of the two accounts (delete the other's lines, or leave them starting with
`;;`, which Tuatara reads past), then save with `C-x C-s`:

```lisp
(load-package "caldav" :source "https://github.com/Tuatara-Editor/tuatara-caldav"
              :version "latest"
  :config
  (setf *caldav-accounts*
        ;; A. EteSync
        '((:name "EteSync" :url "http://localhost:37358/" :user "you"))
        ;; B. Radicale over Tailscale (on home-server itself: "http://localhost:5232/")
        ;; '((:name "Home" :url "https://home-server.tail1234.ts.net/" :user "you"))
        ))
```

Start Tuatara again. It asks once whether to load the package (`y`).

Every calendar of the account is synced, and shown in the agenda, unless
you name the ones to sync with `:calendars`. `C-c s l` (in an Org file or
the agenda) says the names Tuatara finds:

```lisp
'((:name "EteSync" :url "http://localhost:37358/" :user "you"
   :calendars ("Personal" "Family")))
```

### 5. Sync

It syncs on its own: a few seconds after Tuatara starts, then every minute,
in the background, saying nothing unless something changed or failed (a
failure is told once, not again every minute while it lasts). Set how often
with `*caldav-minutes*`, or `nil` to sync only when asked:

```lisp
(setf *caldav-minutes* 5)
```

To sync at once, open an Org file or the agenda and press `C-c s s`. The first sync writes
`agenda-caldav.org` beside your agenda file, a heading for each calendar,
and the agenda shows its events. Press `C-c s s` whenever you want to sync
again.

Only `agenda-caldav.org` is synced: your own agenda file and notes are never
read for it nor changed. To keep the calendars in your agenda file instead,
set `*caldav-file*` to it (see *Where the calendars are kept*); even
then only what is under a calendar's heading, the one with a `CALENDAR`
property, is synced.

### 6. The same calendars elsewhere

| Where                  | How                                                        |
|------------------------|------------------------------------------------------------|
| Thunderbird            | *File*, *New*, *Calendar*, *On the Network*; your user name and the address; *Find Calendars* |
| GNOME Calendar         | *Settings*, *Online Accounts*, *Calendar, Contacts and Files* (WebDAV); the address, user and password |
| Android                | the Tailscale app, then DAVx⁵ (*Login with URL and user name*), and any calendar app |
| iPhone, iPad           | the Tailscale app, then *Settings*, *Calendar*, *Accounts*, *Add Account*, *Other*, *Add CalDAV Account* |
| Phones, with EteSync   | the EteSync app itself; etesync-dav is for computers       |

### If something goes wrong

Nothing in the messages at all? The sync was not started: `C-c s s` works
in an Org file or the agenda only; `M-x caldav-sync` works anywhere. Start
with `C-c s l`, which only asks the server for its calendars and says what
it found, or why it could not.

After each sync Tuatara says what it did in one line, and what failed after
`failed:`: which account or calendar, and why, in curl's own words when the
server could not be reached (`Could not connect to server`, `Could not
resolve host`, `Operation timed out`). `C-h e` shows the messages again. A
sync that failed is also told as a notification, on the desktop and in
`*Notifications*` (`C-h N`), so you see it wherever you are looking.

- **No password for … in the keyring**: step 3 was not done for that user
  name, or for a different one than the init file says.
- **refused the name and password**: the password in the keyring is not
  the server's (for EteSync, the `manage get` one, not your EteSync one).
- **not reached: Could not connect to server**: the server is not running (`etesync-dav`, or
  `sudo systemctl status radicale`), or Tailscale is off on this device
  (`tailscale status`).
- **is not https, nor on this machine**: an `http://` address of another
  computer. Use its `https://...ts.net/` address (step 2B.7).
- **says no calendars**: make a calendar first (step 2B.4, or in EteSync).

### Why this is safe

- Your password is kept by the desktop's keyring, or encrypted by GnuPG,
  given to curl on its standard input, wiped from memory after each sync, never on its command line or in a file, and sent only to
  the server's own address, whatever the server's answers name.
- Tuatara talks https to every other computer, and plain http only to
  this one; with Tailscale the calendars travel inside your own private,
  encrypted network as well.
- Radicale listens to its own computer alone, Tailscale's https address is
  for your devices alone, and each user sees their own calendars alone.
- Nothing a server sends is ever run: calendar files are read as data,
  bounded in size and time, and their words written into Org as words.


## Commands

In Org files and in the agenda, under `C-c s`:

| Key       | Command                  | What it does |
|-----------|--------------------------|--------------|
| `C-c s s` | `caldav-sync`            | Sync every account's calendars and fetch the subscriptions, in the background |
| `C-c s n` | `caldav-event-new`        | Write a new event: the calendar, its title, place and time asked, then its description in a form |
| `C-c s l` | `caldav-calendars`       | Say the calendars each account has, and which of them are synced: shows that Tuatara reaches the server |
| `C-c s d` | `caldav-deleted`         | Delete on the server the events whose entries you deleted, after asking; or put them back |
| `C-c s i` | `caldav-import`          | Read a calendar file (.ics) under one of your calendars, or under a heading of its own |
| `C-c s e` | `caldav-export`          | Write the entry at point and those under it, or the entries selected, as a calendar file (Org files only) |

A sync sends what you changed in an entry, fetches what changed on the
server since the last sync, and writes it into its entry. Only the lines
that differ are changed, as one step to undo, and the file is saved. Your
own lines in an entry (notes, checkboxes, clocks, properties of your own)
are kept when the server's version is written.

- **New entries** under a calendar's heading become events on its server;
  their UID is written into them.
- **Deleted on the server**: the entry is deleted too. One holding lines
  of yours (notes, checkboxes, clocks, properties of your own) stays,
  tagged `removed`, no longer synced, its times made inactive
  (`[2026-11-05 Thu]`), so that the agenda shows it no more; delete it
  when you are done with it.
- **Deleted here**: nothing happens on the server until `C-c s d` asks you;
  answered no, the entries are put back.
- **Cancelled here** (`C-c C-x C`, keyword `CANCELLED` or `CANCELED`): its
  event is deleted on the server at the next sync, so it leaves your other
  devices. The entry stays, tagged `removed`, no longer synced; the agenda
  still shows it, cancelled. One cancelled before it was ever sent is not
  sent.
- **Changed on both sides**: the server's version stands in the entry, and
  yours is kept beside it, tagged `conflict`, no longer synced. Copy what
  you want from it and delete it.

## Calendar files

A calendar file is read into its components (a calendar, its events, tasks
and time zones) and their properties, kept as written, so a file read and
written again loses nothing. Lines are written folded at 75 octets, never
inside a character.

- **Values** are read when asked for: text with its escapes, dates and times
  (floating, UTC, or in a named time zone), durations, offsets and rules of
  recurrence.
- **Time zones**: a time zone a file names is found in the file's own
  definition first, then among the system's (`/usr/share/zoneinfo`), the
  name checked to be a zone's name and nothing more. One is written for a
  zone by its yearly rule.
- **Recurrences**: every part of a rule of recurrence, extra dates, dates
  taken out, and occurrences changed one by one, expanded only within the
  days asked for. Checked against the examples in RFC 5545 itself.

## Where the calendars are kept

In one Org file, a heading for each calendar with its events under it:
`agenda-caldav.org` beside the first of your agenda files, unless the
setting says otherwise. The agenda reads it whether or not
`*org-agenda-files*` names it.

```lisp
(setf *caldav-file* "agenda-caldav.org")        ; the default
(setf *caldav-file* "~/Documents/agenda.org")   ; among your own entries
```

Set to one of your agenda files, the calendars are headings among your own,
and only what is under a calendar's heading is synced; the rest of the file
is left as it is.

## Accounts

Any CalDAV server: Radicale, or EteSync through its bridge, etesync-dav,
which serves your EteSync calendars at `http://localhost:37358/`, keeping
them encrypted end to end. The password is kept in the desktop's keyring,
never in the init file. The address is the server's own; the calendars are found from it as CalDAV
says (the principal, its calendar home), each a heading of its own in the
calendars' file, its address in the `CALENDAR` property.

```lisp
(setf *caldav-accounts*
      '((:name "EteSync" :url "http://localhost:37358/" :user "you")))
```

```sh
secret-tool store --label="Tuatara calendars" service tuatara-caldav user you
```

## Subscriptions

A calendar file at an address, public holidays say, read into the agenda
and never sent back. Each is fetched at every sync and written as an Org
file of its own in the cache, which the agenda reads among its files.

```lisp
(setf *caldav-subscriptions*
      '((:name "Holidays" :url "https://example.org/holidays.ics")))
```

## What is kept, and where

What each calendar last agreed with its server, its sync token and a copy of
each event, is kept in `~/.local/state/tuatara/calendars/`, never in the Org
file. The copy gives what an entry does not say: the attendees, alarms and
properties other programs keep are sent back as they were.

## Safety

- Every request goes through curl, the password given on its standard
  input, never on its command line or in a file. https only, or http to
  this machine alone (etesync-dav); a redirect elsewhere is not followed,
  and an account's password goes to its own server's host alone, whatever
  the server's answers name.
- What a server answers is bounded: 32 MB and 60 seconds a request.
- Calendar files are read as data, nothing in them run or fetched.

## Events as Org entries

Each event or task is one heading; its UID is kept in the `UID` property.

| Calendar                        | Org                                        |
|---------------------------------|--------------------------------------------|
| `SUMMARY`                       | the heading                                |
| `DTSTART`, `DTEND`, `DURATION`  | an active timestamp on your computer's clock: `<2026-11-03 Tue 09:00-10:00>`, a range for several days |
| A weekly, monthly or yearly rule Org's repeater says | the repeater: `+1w`, `+2w`, `+1m`, `+1y` |
| The second Tuesday, Mondays and Fridays, every other week on two days, three times only, until a day | a date by rule, as the repeat menu of the date prompt writes it (`C-c C-s`, then `C-r`): `<%%(and (diary-block 11 10 2026 1 12 2027) (diary-float t 2 2)) 18:00-20:00>` |
| `EXDATE`, a time changed alone | a note in the logbook of the time cancelled or moved, as cancelling one time (`C-c C-x C`) and scheduling one time elsewhere (`C-c C-s`) write them |
| A rule Org cannot say (the last weekday of a month) | each time written out within the days kept (a year back, two ahead), the rule in the `RRULE` property; these times are the calendar's and are never sent back |
| A task's `DUE`, `DTSTART`, `COMPLETED` | `TODO` with `DEADLINE`, `SCHEDULED`, `DONE` and `CLOSED` |
| `LOCATION`                      | the `LOCATION` property                    |
| `CATEGORIES`                    | tags (`Public Holiday` is `Public_Holiday`) |
| `STATUS:CANCELLED`              | the tag `cancelled`                        |
| `DESCRIPTION`                   | an example block named `description`       |

Read back, an entry gives the same properties. Several timestamps in one
entry are an event each, the UID of the second and later ones followed by
their day. Everything else the calendar keeps of an event (attendees,
alarms, its own properties) is kept as it was, and times the entry still
says as the calendar did are sent as the calendar wrote them.

Words from a calendar stay words: a title stays on one line, a description
stays inside its block, and neither can become a heading, a link, a
timestamp, a clock or a note the agenda reads.

A location or description left blank is none: nothing is sent for it.

### A new event

`C-c s n` (`M-x caldav-event-new` anywhere) asks which calendar, when you
have more than one, then the event's title, its place (`RET` for none) and
its time in the calendar. A form opens with the cursor between the
description block's two lines: write the description there, or nothing.
`C-c C-c` files the event under the calendar's heading and the server gets
it at the next sync; `C-c C-w` files it under another heading instead, and
`C-c C-k` throws it away. The `#+name: description` line is what marks the
block as the description, so leave it as it is.

## Bounds

A calendar file comes from elsewhere and is never trusted. Nothing in it is
run or fetched, and the reader refuses a file larger than 16 MB or with
components nested more than 20 deep. No more than 10,000 occurrences of one
event are worked out at once.

## Working on it

```sh
tools/test.sh
```

runs its tests against a Tuatara source tree: `../tuatara` beside this
repository, or `../../tuatara` beside the folder it is in, unless
`TUATARA_SOURCE` names another. How a package is laid out and which of
Tuatara's names it may use are in Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org);
bug reports, ideas and how changes are made in
[CONTRIBUTING.md](CONTRIBUTING.md).
