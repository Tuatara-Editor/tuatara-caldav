# Contributing to caldav

caldav is a package of the Tuatara project's own: its maintainers write it,
keep it working, and decide what is added. It takes no pull requests; Tuatara's approved contributors work on the
editor itself (see its
[CONTRIBUTING.md](https://github.com/Tuatara-Editor/tuatara/blob/main/CONTRIBUTING.md)).

## What is welcome

- **A bug report**: open an issue, saying what happened, and how to make it happen again. Never attach a
  file you are not free to share: a small made-up one that shows the same
  is enough.
- **An idea** for caldav, or for a new package: open an issue
  that says what you want to do and how you do it today. If the project
  takes the idea up, its maintainers write it; an idea is not a promise
  that it will be.

## A package of your own

Write it in a repository of your own and keep it there: you look after it,
take its issues and answer its users. Tuatara loads it from there as it
loads this one, after asking the user, its source offered to read:

```lisp
(package-source "kahikatea" "https://example.org/you/kahikatea")
(load-package "kahikatea" :source "kahikatea")
```

Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org)
says how a package is laid out and tested, and which of Tuatara's names it
may use: those Tuatara's API exports
(`lisp/api.lisp`), which are what the project keeps stable for packages. If
your package needs something the API lacks, ask for it in an issue on
[tuatara](https://github.com/Tuatara-Editor/tuatara/issues).

## For the maintainers

Changes come through pull requests, one change each, signed (`git commit -s
-S`) and reviewed, merged as one commit whose title becomes its first line.
A change needs:

- **Its tests.** A fix, or two ways of doing one thing merged into one, comes
  with a test that fails on the code before the change and passes after it.
  A fix for slowness measures what it fixes.
- **Its documentation.** Every definition has a docstring that a user reads in
  the help; README.md is updated where the change
  makes it wrong.
- **Tuatara's conventions**, in its
  [HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org):
  one way for each kind of thing, nothing slow on the editor's thread, no
  colours outside the theme, nothing in a file ever run. A package uses only
  the names Tuatara's API exports, and what it needs that Tuatara lacks is
  added to Tuatara rather than written again in the package.
- **English.** All text, from docstrings to commit messages, is in English;
  any spelling is welcome, kept the same within a package.
- **Nothing private.** No files, names, logos or text that belong to an
  employer, a customer or a case. Test files are small, made up for the test,
  and say nothing about real people.
- **No paths outside the project.** Code, tests, documentation and examples
  refer to no file or folder of anyone's own. They name only the project's
  own files, a test's own temporary folder, what every system has (`/tmp`,
  `/etc`), or plain made-up places (`~/notes/`, `~/Documents/report.org`).

The `Signed-off-by:` line is the author's statement, under the
[Developer Certificate of Origin](https://developercertificate.org/), that
they have the right to give the change under the project's licence, the GNU
General Public License, version 3 or later.

## Security faults

Report them privately, never in an issue: see
[SECURITY.md](https://github.com/Tuatara-Editor/.github/blob/main/SECURITY.md).
