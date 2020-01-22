# ellit-org.el
Emacs Lisp Literate programming tool

Generate documentation for Emacs Lisp package by extracting text from
comments of `.el` files, organizing output into `.org` file.

Similiar to https://github.com/tumashu/el2org

## Why?

Having separate files for code and documentanion is hard to get in
sync.  Once changing something in the source code file, you might
change comments as well and forget about documentation.

Also many things, useful for documentation, might be automatically
extracted from source code.  Such as:

* Keybindings
* Customizable options
* Docstrings for commands
* etc
