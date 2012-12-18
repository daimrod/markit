
Markit
======

Markit is a GNU/Emacs minor mode which provides some Vim facilities to
Emacs such as vi( and va".

Installation and configuration

    (load "path-to-markit.el")
    (require 'markit)

The following bindings are used:

- C-c v i to mark the region, including the delimiters
- C-c v e to mark the region, excluding the delimiters

If you wish to have something like ci", enable `delete-selection-mode`

    (delete-selection-mode)
