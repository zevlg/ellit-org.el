;;; ellit-org.el --- Emacs Lisp Literate programming tool  -*- lexical-binding:t -*-

;; Copyright (C) 2020 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jan 22 10:35:15 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/zevlg/ellit-org.el
;; Version: 0.1
(defconst ellit-org-version "0.1")

;; ellit-org is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ellit-org is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ellit-org.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; #+TITLE: [[file:ellit-org-logo.svg]] ellit-org
;;
;; Emacs Lisp Literate programming tool
;;
;; #+BEGIN_QUOTE
;; This file is automatically generated from {{{ellit-filename(verbatim)}}} by
;; [[https://github.com/zevlg/ellit-org.el][GitHub#ellit-org.el]] tool.
;; Do not edit manually.
;; #+END_QUOTE
;;
;; Generate documentation for Emacs Lisp package by extracting text
;; from =.org= files or comments from =.el= files and combining them
;; into single =.org= file.
;;
;; Idea is similiar to https://github.com/tumashu/el2org
;;
;; However =ellit-org= implements more features, such as:
;; - Easy to use comments extractor from =.el=, see [[* Commenting .el files]]
;; - Combining multiple files, =.org= or =.el=
;; - Templating, see [[* Templates]]
;; 
;; * Why?
;;
;; Separate files for code and documentanion is hard to get in sync.
;; Once changing something in source code files, you might change
;; comments as well and forget about documentation.  In other words
;; having documentation in source code is easier to maintain for the
;; developers.
;; 
;; Also many things, useful for documentation, might be automatically
;; extracted from source code.  Such as:
;; - Keybindings
;; - Customizable options
;; - Docstrings for commands
;; - etc
;;
;; * Using ellit-org in your project
;;
;; Sample Makefile to generate user manual for the project:
;;
;; #+BEGIN_SRC Makefile
;; EMACS=emacs -Q
;;
;; manual.org: srcfile.el <list-of-other-files-used-to-generate-manual>
;;      $(EMACS) -batch -f package-initialize -l ellit-org -l srcfile.el \
;;                  --eval '(ellit-org-file-el "srcfile.el" "manual.org")'
;; #+END_SRC
;;
;; See =ellit-org='s [[https://github.com/zevlg/ellit-org.el/blob/master/Makefile][Makefile]]
;; 
;; It is *important* to load =srcfile.el=, not just =srcfile=, to make
;; *possible for ~fundocX~ templates to emphasize function arguments
;; in case =srcfile.el= is already compiled.
;; 
;; * Commenting .el files
;;
;; 1. Use double-semicolon comments, otherwise processing won't start
;; 2. Processing starts when Org mode's property, heading or list is seen
;; 3. Processing starts only if matched comment line begins commentary
;;    block, i.e. previous line is a non-commentary line
;; 4. Processing stops on any non-commentary line
;;
;; Here is the example:
;; #+begin_src emacs-lisp
;;   ;; * Heading1                        <--- processing starts here
;;   ;; This line is included into output
;;   ;;
;;   ;; This line also included into output
;;                                        <--- processing stops here
;;   ;; This line is NOT included into output
;;   ;; * This line also NOT included
;;   ;;   Since it does not begin the commentary block, see 3.
;;
;;   ;; - However this line, is included  <--- processing starts here
;;   ;;
;;   ;;    Since new processing is started, and it will stop only on
;;   ;;    non-commentary line below
;;                                        <--- processing stops here
;;   ;; This line is *not* included
;; #+end_src
;;


;;; Code:
(require 'subr-x)                       ;`replace-region-contents'
(require 'org)                          ;`org-macro-replace-all'
(require 'svg)                          ;for `ellit-org--logo-image'

(defconst ellit-org-comment-start-regexp
  (rx (0+ (regexp "\s")) ";;"
      (or (regexp "\s") eol)))

(defvar ellit-org-start-regexp
  (rx (or buffer-start
          (and line-start (0+ (not ";")) "\n"))

      line-start (0+ (regexp "\s")) ";;" (1+ space)
      (or "#+"
          (and (or (1+ "*") "+" "-"
                   (and (1+ digit) (or "." ")")))
               space)))
  "Regexp matching start of the text to extract.")

;; * Templates
;;
;; ellit-org relies on Org mode's macro system by adding some useful
;; macroses.  See https://orgmode.org/manual/Macro-replacement.html
;;
;; Macro replacement is done *after* processing comments, so make
;; sure your macroses are in processed part of the comments.
;;
;; Templates syntax:
;; #+begin_example
;; {{{macro_name(arguments)}}}
;; #+end_example
;; ~ARGUMENTS~ are optional string supplied to function which does
;; processing for ~MACRO_NAME~.
;;
;; Supported templates:

(defvar ellit-org-macro-templates
  '(
    ;; - as-is(~STRING~) ::
    ;;   {{{fundoc1(ellit-org-template-as-is)}}}
    ("as-is" . "(eval (ellit-org-template-as-is $1))")

    ;; - ellit-el(~FILE~) ::
    ;;   {{{fundoc1(ellit-org-template-ellit-el)}}}
    ("ellit-el" . "(eval (ellit-org-template-ellit-el $1))")

    ;; - ellit-org(~FILE~) ::
    ;;   {{{fundoc1(ellit-org-template-ellit-org)}}}
    ("ellit-org" . "(eval (ellit-org-template-ellit-org $1))")

    ;; - ellit-filename(&optional ~VERBATIM~) ::
    ;;   {{{fundoc1(ellit-org-template-ellit-filename)}}}
    ("ellit-filename" . "(eval (ellit-org-template-ellit-filename $1))")

    ;; - kbd(~KEY~) ::
    ;;   {{{fundoc1(ellit-org-template-kbd)}}}
    ("kbd" . "(eval (ellit-org-template-kbd $1))")

    ;; - where-is(~COMMAND~, ~KEYMAP~) ::
    ;;   {{{fundoc1(ellit-org-template-where-is)}}}
    ("where-is" . "(eval (ellit-org-template-where-is $1 $2))")

    ;; - vardoc1(~VARIABLE~) ::
    ;;   {{{fundoc1(ellit-org-template-vardoc1)}}}
    ("vardoc1" . "(eval (ellit-org-template-vardoc1 $1))")

    ;; - vardoc(~VARIABLE~) ::
    ;;   {{{fundoc1(ellit-org-template-vardoc)}}}
    ("vardoc" . "(eval (ellit-org-template-vardoc $1))")

    ;; - fundoc1(~FUNCTION~) ::
    ;;   {{{fundoc1(ellit-org-template-fundoc1)}}}
    ("fundoc1" . "(eval (ellit-org-template-fundoc1 $1))")

    ;; - fundoc(~FUNCTION~) ::
    ;;   {{{fundoc1(ellit-org-template-fundoc)}}}
    ("fundoc" . "(eval (ellit-org-template-fundoc $1))")
    )
  "Alist of org macro templates.
Each element in form:
  (NAME . TEMPLATE)

See `org-macro-templates'.")

(defvar ellit-org--filename nil
  "Currently processing filename.
Used in `ellit-filename' template.")

(defun ellit-org--process-el ()
  "Extract org bits from emacs lisp comments of current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (cpont)
      (while (progn (setq cpont (point))
                    (re-search-forward ellit-org-start-regexp nil 'no-error))
        (beginning-of-line)
        (delete-region cpont (point))

        ;; Scan line by line, stopping at non-commentary string
        (while (looking-at ellit-org-comment-start-regexp)
          (let ((del-point (match-end 0))
                (eol-point (point-at-eol)))
            ;; DO NOT strip "\n"
            (when (> del-point eol-point)
              (setq del-point eol-point))
            (delete-region (point) del-point))

          (forward-line 1)
          (beginning-of-line))
        (delete-region (point-at-bol) (point-at-eol)))

      (delete-region cpont (point-max)))))

(defun ellit-org--macro-replace-all (&optional macro-templates)
  "Replace all macros in current buffer by their expansion.
MACRO-TEMPLATES - list of additional macro definitions."
  ;; NOTE: set `org-complex-heading-regexp' to string, otherwise
  ;; `org-macro-replace-all' fails.
  ;; `org-complex-heading-regexp' only used to recognize
  ;; commented/non-commented headings.
  (let ((org-complex-heading-regexp ""))
    (org-macro-initialize-templates)
    (let ((templates (nconc org-macro-templates
                            (copy-sequence macro-templates)
                            ellit-org-macro-templates)))
      (org-macro-replace-all templates))))

(defun ellit-org--process-org (&optional output-file template-alist)
  "Replace all org macros in current buffer, write output to OUTPUT-FILE.
If OUTPUT-FILE is nil, then return result as string.
TEMPLATE-ALIST is used as alist of custom templates."
  (ellit-org--macro-replace-all template-alist)

  (if output-file
      (write-region (point-min) (point-max) output-file nil 'quiet)
    (buffer-string)))

(defun ellit-org-file-org (org-file &optional output-file template-alist)
  "Extract documentation from ordinary ORG-FILE.
Write to OUTPUT-FILE, or return string if OUTPUT-FILE is nil.
TEMPLATE-ALIST is used as alist of custom templates."
  (let* ((ellit-dir (when ellit-org--filename
                      (file-name-directory ellit-org--filename)))
         (ellit-org--filename (expand-file-name org-file ellit-dir)))
    (with-temp-buffer
      (insert-file-contents ellit-org--filename)
      (ellit-org--process-org output-file template-alist))))

(defun ellit-org-file-el (el-file &optional output-file template-alist)
  "Extract documentation from emacs-lisp file EL-FILE.
Write to OUTPUT-FILE, or return string if OUTPUT-FILE is nil.
TEMPLATE-ALIST is used as alist of custom templates."
  (let* ((ellit-dir (when ellit-org--filename
                      (file-name-directory ellit-org--filename)))
         (ellit-org--filename (expand-file-name el-file ellit-dir)))
    (with-temp-buffer
      (insert-file-contents ellit-org--filename)
      (ellit-org--process-el)
      (ellit-org--process-org output-file template-alist))))

(defun ellit-org--logo-image (&optional size debug-p)
  "Generate logo for the `ellit-org'."
  (unless size (setq size 256))
  (let* ((logo-svg (svg-create size size))
         (border-size (/ size 32))
         (bracket-w (/ (- size (* border-size 1)) 8)))
    (svg-gradient logo-svg "cgrad1" 'linear
                  (list '(0 . "#8280c2") (cons size "#9146bc")))
    (svg-circle logo-svg (/ size 2) (/ size 2) (- (/ size 2) border-size)
                :stroke-width border-size
                :stroke-color "#592a80"
                :gradient "cgrad1")
    ;; Draw brackets
    (dotimes (n 6)
      (let ((x-off (+ (* border-size 2) (* n bracket-w))))
        ;; NOTE: swap `b-x' and `m-x' after third bracket
        (when (> n 2)
          (setq x-off (+ x-off (* 1.75 border-size))))

        (svg-text logo-svg (if (> n 2) "}" "{")
                  :font-size (/ size 1.75)
;                  :font-weight "bold"
                  :fill "white"
                  :font-family "OpenSans"
                  :x x-off
                  :y (+ (/ size 6.25) (/ size 2)))
        ))

    (when debug-p
      (svg-line logo-svg (/ size 2) 0 (/ size 2) size
                :stroke-width 1
                :stroke-color "black")
      (svg-line logo-svg 0 (/ size 2) size (/ size 2)
                :stroke-width 1
                :stroke-color "black"))

    (svg-image logo-svg :scale 1.0
               :width size :height size
               :ascent 'center)))

(defun ellit-org--save-logo (file &optional size)
  "Write ellit svg logo into FILE."
  (let ((logo-image (ellit-org--logo-image size)))
    (write-region "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" nil
                  file nil 'quiet)
    (write-region (plist-get (cdr logo-image) :data) nil
                  file 'append 'quiet)))


;;; Templates
(defun ellit-org-template-as-is (string)
  "Insert STRING as is."
  string)

(defun ellit-org-template-ellit-el (file)
  "Insert results of the emacs-lisp FILE processing."
  ;; NOTE: remove trailing \n, to not insert double newline for
  ;; {{{ellit-el(file)}}} macro
  (let ((output (ellit-org-file-el file)))
    (if (string-suffix-p "\n" output)
        (substring output 0 -1)
      output)))

(defun ellit-org-template-ellit-org (file)
  "Insert results of the org FILE processing."
  ;; NOTE: remove trailing \n, to not insert double newline for
  ;; {{{ellit-org(file)}}} macro
  (let ((output (ellit-org-file-org file)))
    (if (string-suffix-p "\n" output)
        (substring output 0 -1)
      output)))

(defun ellit-org-template-ellit-filename (&optional verbatim)
  "Insert currently processing filename."
  (let ((verbatim-p (not (string-empty-p verbatim))))
    (concat (when verbatim-p "=")
            (file-name-nondirectory ellit-org--filename)
            (when verbatim-p "="))))

(defun ellit-org-template-kbd (key)
  "Insert HTML <kbd> tag with KEY contents."
  (concat "@@html:<kbd>@@" key "@@html:</kbd>@@"))

(defun ellit-org-template-where-is (command &optional keymap)
  "Insert list of keys that calls COMMAND.
KEYMAP is keymap where to lookup for COMMAND.  By default
`global-map' is considered."
  ;; {{{kbd(C-c 1)}}}, {{{kbd(C-c 2)}}} (~command~)
  (concat (mapconcat (lambda (key)
                       (ellit-org-template-kbd (key-description key)))
                     (where-is-internal command keymap) ", ")
          "(~" command "~)"
          (ellit-org-template-fundoc command))
  )

(defun ellit-org--vardoc (varname &optional first-line-p)
  "Return docstring for the variable named by VARNAME.
If FIRST-LINE-P is non-nil, then return only first line of the docstring."
  (let* ((varsym (intern varname))
         (vardoc (documentation-property varsym 'variable-documentation)))
    (when vardoc
      (if first-line-p
          (car (split-string vardoc "\n"))
        vardoc))))

(defun ellit-org-template-vardoc1 (variable)
  "Insert first line from docstring for the VARIABLE."
  (ellit-org--vardoc variable 'first-line))

(defun ellit-org-template-vardoc (variable)
  "Insert full docstring for the VARIABLE."
  (ellit-org--vardoc variable))

(defun ellit-org--funargs (funsym)
  "Return list of FUNSYM function arguments as list of strings."
  (cl-remove-if
   (lambda (s)
     (string-prefix-p "&" s))
   (mapcar 'upcase
           (mapcar 'symbol-name (help-function-arglist funsym)))))

(defun ellit-org--fundoc (funname &optional first-line-p)
  "Return docstring for the function named by FUNNAME.
If FIRST-LINE-P is non-nil, then return only first line of the docstring."
  (let* ((funsym (intern funname))
         (fundoc (documentation funsym)))
    ;; NOTE: emphasize arguments refs in fundoc with ~...~ syntax
    ;; TODO: emphasize `xxx' syntax
    (when fundoc
      (replace-regexp-in-string
       (regexp-opt (ellit-org--funargs funsym)) "~\\&~"
       (if first-line-p
           (car (split-string fundoc "\n"))
         fundoc)))))

(defun ellit-org-template-fundoc1 (function)
  "Insert first line from docstring for the FUNCTION."
  (ellit-org--fundoc function 'first-line))

(defun ellit-org-template-fundoc (function)
  "Insert full docstring for the FUNCTION."
  (ellit-org--fundoc function))

(provide 'ellit-org)

;;; ellit-org.el ends here
