;;; ellit-org.el --- Emacs Lisp Literate programming tool  -*- lexical-binding:t -*-

;; Copyright (C) 2020-2021 by Zajcev Evgeny.

;; Author: Zajcev Evgeny <zevlg@yandex.ru>
;; Created: Wed Jan 22 10:35:15 2020
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1"))
;; URL: https://github.com/zevlg/ellit-org.el
;; Version: 0.8
(defconst ellit-org-version "0.8")

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

;;; ellit-org:
;; #+OPTIONS: timestamp:nil \n:t
;; #+TITLE: [[ellit-org-logo64.png]] ellit-org (v{{{eval(ellit-org-version,t)}}})
;; #+STARTUP: showall
;;
;; [[https://github.com/zevlg/ellit-org.el/actions][https://github.com/zevlg/ellit-org.el/workflows/CI/badge.svg]]
;;
;; #+BEGIN_QUOTE
;; This file is automatically generated from {{{ellit-filename(verbatim)}}} by
;; [[https://github.com/zevlg/ellit-org.el][GitHub#ellit-org.el]] tool.
;; Do not edit manually.
;; #+END_QUOTE
;;
;; Ultimate tool to document your Emacs Lisp project without much effort.
;;
;; Generate documentation for your Emacs Lisp package by combining
;; =.org= files and useful bits from comments in =.el= files.
;;
;; Idea is similar to https://github.com/tumashu/el2org
;;
;; However =ellit-org= implements more features, such as:
;; - Easy to use comments extractor from =.el=, see [[#commenting-el-files][Commenting files]]
;; - [[#combining-multiple-files][Combining multiple files]], =.org= or =.el=
;; - GitHub friendly ~:CUSTOM_ID~ property generation for headings, so
;;   exporting html to GitHub Pages from resulting =.org= is easy as
;;   calling ~org-html-export-to-html~ function
;; - [[#templates][Templating]]
;;
;; * Why?
;;
;; Separate files for code and documentation is hard to get in sync.
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
;;      $(EMACS) -batch -f package-initialize -l ellit-org \
;;                  --eval '(ellit-org-export "srcfile.el" "manual.org")'
;; #+END_SRC
;;
;; ** Projects that uses ellit-org
;;    - =ellit-org= itself, see [[https://github.com/zevlg/ellit-org.el/blob/master/Makefile][Makefile]]
;;    - [[https://github.com/zevlg/telega.el][telega.el]] uses =ellit-org= to generate its [[https://zevlg.github.io/telega.el/][Manual]], see its [[https://github.com/zevlg/telega.el/blob/master/doc/Makefile][Makefile]]
;;    - [[https://github.com/zevlg/grammarbot.el][grammarbot.el]], see its [[https://github.com/zevlg/grammarbot.el/blob/master/Makefile][Makefile]]
;;
;; * Commenting .el files
;;
;; 1. Use ~;;; ellit-org: [LABEL]~ as trigger for ellit-org to start
;;    processing following comments
;; 2. Processing stops on any non-commentary line
;; 3. When processing stops, newline is emmited to output
;;
;; Here is the example:
;; #+BEGIN_SRC emacs-lisp
;;   ;;; ellit-org:
;;   ;; * Heading1                        <--- processing starts here
;;   ;; This line is included into output
;;   ;;
;;   ;; This line also included into output
;;                                        <--- processing stops here
;;   ;; This line is NOT included into output
;;   ;; * This line also NOT included
;;
;;   ;;; ellit-org:
;;   ;; - However this line, is included  <--- processing starts here
;;   ;;
;;   ;;    Since new processing is started, and it will stop only on
;;   ;;    non-commentary line below
;;                                        <--- processing stops here
;;   ;; This line is *not* included
;; #+END_SRC
;;
;; * Combining multiple files
;;
;; Multiple =.org= and =.el= files might be combined forming final
;; result as single =.org= file.
;;
;; *TODO*: Describe labels purpose inside ellit-driven files
;; 
;; *TODO*: Describe ~#+ELLIT-INCLUDE:~ directive, and its properties:
;;   - ~:eval-p~ to get include filename by evaluating sexp
;;   - ~:no-load~ do not load corresponding =.el= file
;;   - ~:label~ To include only given label from =.el= or =.org= file
;;   - ~:heading~ To include only given heading


;;; Code:
(require 'subr-x)                       ;`replace-region-contents'
(require 'org)
(require 'ox-org)
(require 'svg)                          ;for `ellit-org--logo-image'

(defconst ellit-org-comment-start-regexp
  (rx (0+ (regexp "\s")) ";;"
      (or (regexp "\s") eol)))

(defvar ellit-org-start-regexp
  (rx line-start (0+ (regexp "[\s\t]"))
      ";;; ellit-org:" (0+ (regexp "[\s\t]"))
      (group (0+ not-newline))
      line-end)
  "Regexp matching start of the text to extract.")

(defconst ellit-org--where-is-key-regexp
  (rx (optional "`")
      (optional "\\<" (group (1+ (not ">"))) ">")
      "\\[" (group (1+ (not "]"))) "]" (optional "'"))
  "Regexp to match command substitution.")

;;; ellit-org: templates
;; * Templates
;;
;; ellit-org relies on Org mode's macro system by adding some useful
;; macroses.  See https://orgmode.org/manual/Macro-replacement.html
;;
;; Macro replacement is done *after* processing comments, so make
;; sure your macroses are in processed part of the comments.
;;
;; Templates syntax:
;; #+BEGIN_EXAMPLE
;; {{{macro_name(arguments)}}}
;; #+END_EXAMPLE
;;
;; ~ARGUMENTS~ are optional string supplied to function which does
;; processing for ~MACRO_NAME~.
;;
;; Supported templates:

(defvar ellit-org-macro-templates
  '(
    ;;; ellit-org: templates
    ;; - eval(~SEXP~ [, ~AS-STRING~ ]) ::
    ;;   {{{fundoc(ellit-org-template-eval, 2)}}}
    ("eval" . "(eval (ellit-org-template-eval $1 $2))")

    ;;; ellit-org: templates
    ;; - as-is(~STRING~) ::
    ;;   {{{fundoc1(ellit-org-template-as-is)}}}
    ;;
    ;;   ~as-is(STRING)~ filter is equivalent to ~eval("STRING", t)~
    ("as-is" . "(eval (ellit-org-template-as-is $1))")

    ;;; ellit-org: templates
    ;; - ellit-filename([ ~VERBATIM~ ]) ::
    ;;   {{{fundoc(ellit-org-template-ellit-filename, 2)}}}
    ("ellit-filename" . "(eval (ellit-org-template-ellit-filename $1))")

    ;;; ellit-org: templates
    ;; - kbd(~KEY~) ::
    ;;   {{{fundoc(ellit-org-template-kbd, 2)}}}
    ("kbd" . "(eval (ellit-org-template-kbd $1))")

    ;;; ellit-org: templates
    ;; - where-is(~COMMAND~ [, ~KEYMAP~ [, ~MENU-ITEMS~ ]]) ::
    ;;   {{{fundoc(ellit-org-template-where-is, 2)}}}
    ("where-is" . "(eval (ellit-org-template-where-is $1 $2 $3))")

    ;;; ellit-org: templates
    ;; - vardoc1(~VARIABLE~) ::
    ;;   {{{fundoc(ellit-org-template-vardoc1, 2)}}}
    ("vardoc1" . "(eval (ellit-org-template-vardoc1 $1))")

    ;;; ellit-org: templates
    ;; - vardoc(~VARIABLE~ [, ~INDENT-LEVEL~ ]) ::
    ;;   {{{fundoc(ellit-org-template-vardoc, 2)}}}
    ("vardoc" . "(eval (ellit-org-template-vardoc $1 $2))")

    ;;; ellit-org: templates
    ;; - fundoc1(~FUNCTION~) ::
    ;;   {{{fundoc(ellit-org-template-fundoc1, 2)}}}
    ("fundoc1" . "(eval (ellit-org-template-fundoc1 $1))")

    ;;; ellit-org: templates
    ;; - fundoc(~FUNCTION~ [, ~INDENT-LEVEL~ ]) ::
    ;;   {{{fundoc(ellit-org-template-fundoc, 2)}}}
    ("fundoc" . "(eval (ellit-org-template-fundoc $1 $2))")
    )
  "Alist of org macro templates.
Each element in form:
  (NAME . TEMPLATE)

See `org-macro-templates'.")

(defvar ellit-org--filename nil
  "Currently processing filename.
Used in `ellit-filename' template.")

(defvar ellit-org-edit--buffer nil)
(make-variable-buffer-local 'ellit-org-edit--buffer)
(defvar ellit-org-edit--hunk-labels nil)
(make-variable-buffer-local 'ellit-org-edit--hunk-labels)
(defvar ellit-org-edit--hunk-position nil)
(make-variable-buffer-local 'ellit-org-edit--hunk-position)

(defvar ellit-org-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'ellit-org-edit-finish)
    map)
  "The keymap to be used when editing code messages.")

(define-derived-mode ellit-org-edit-mode org-mode nil
  "Edit ellit-org hunk in the .el file."
  )

(defun ellit-org-edit-hunk ()
  "Interactively edit ellit-org hunk at point."
  (interactive)
  (let ((hunk-buffer (current-buffer))
        hunk-start hunk-end hunk-labels)
    ;; extract the hunk
    (save-excursion
      (when (re-search-backward ellit-org-start-regexp nil 'no-error)
        (setq hunk-labels (split-string (match-string 1) "," t "\s+"))
        (setq hunk-start (point-at-bol))
        (forward-line 1)
        (while (looking-at ellit-org-comment-start-regexp)
          (forward-line 1))
        (setq hunk-end (point))))

    (unless hunk-start
      (user-error "No ellit-org hunk at point"))

    (switch-to-buffer (get-buffer-create "*Edit ellit-org hunk*"))
    (kill-all-local-variables)

    (setq-local ellit-org-edit--buffer hunk-buffer)
    (setq-local ellit-org-edit--hunk-labels hunk-labels)
    (setq-local ellit-org-edit--hunk-position (cons hunk-start hunk-end))
    (ellit-org-edit-mode)
    ))

(defun ellit-org-edit-finish ()
  "Accept ellit-org hunk after edition."
  (interactive)
  )


(defun ellit-org--process-el (&optional label single-hunk-p)
  "Extract org bits from Emacs Lisp comments of current buffer.
If LABEL is specified, then extract only parts under LABEL
i.e. starting with \";;; ellit-org: LABEL\".
If SINGLE-HUNK-P is non-nil, then process only fist ellit-org
hunk matching LABEL."
  (save-excursion
    (goto-char (point-min))
    (let ((done nil) cpont ellit-labels)
      (while (and (not done)
                  (setq cpont (point))
                  (re-search-forward ellit-org-start-regexp nil 'no-error))
        ;; Allow multiple labels delimited with ","
        (setq ellit-labels (split-string (match-string 1) "," t "\s+"))
        (forward-line)                  ;skip ;;; ellit-org:<ellit-label>
        (delete-region cpont (point))

        (when (or (not label) (member label ellit-labels))
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
          (delete-region (point-at-bol) (point-at-eol))

          ;; NOTE: Separent processing hunks with newline
          (insert "\n")
          (when single-hunk-p
            (setq done t))))

      (delete-region cpont (point-max))

      ;; Unneeded newline from processing last hunk
      (when (eq (char-before) ?\n)
        (delete-char -1))
      )))

(defun ellit-org--parse-include-args (element)
  "Parse arguments of #+ELLIT-INCLUDE keyword ELEMENT.
Return list where first element is filename and rest are properties."
  (let* ((value (org-element-property :value element))
         (eval-p (string-match (regexp-quote ":eval") value))
         (file (and (string-match (rx line-start (1+ (not space)))
                                  value)
                    (prog1 (match-string 0 value)
                      (setq value (replace-match "" nil nil value)))))
         (label (when (string-match ":label\s+\\(.*\\)" value)
                  (match-string 1 value)))
         (no-load (when (string-match (regexp-quote ":no-load")
                                      value)
                    (setq value (replace-match "" nil nil value))
                    t)))
    (list file :no-load no-load :label label :eval-p eval-p)))

(defun ellit-org--process-org (&optional props)
  "Process all ELLIT-INCLUDE keywords in current org buffer.
PROPS are properties, such as: `:heading'."
  ;; init org regexps
  (let ((org-inhibit-startup t)) (org-mode))

  ;; TODO: support for `:heading' property to include only given
  ;; heading.  Do it by removing everything except for `:heading'
  ;; contents

  ;; Expand all #+ELLIT-INCLUDE keywords
  (let ((ellit-include-re "^[ \t]*#\\+ELLIT-INCLUDE:")
        (heading (plist-get props :heading)))
    (goto-char (point-min))
    (while (re-search-forward ellit-include-re nil t)
      (unless (org-in-commented-heading-p)
        (let ((element (save-match-data (org-element-at-point))))
          (when (eq (org-element-type element) 'keyword)
            (beginning-of-line)
            (let ((include-args (ellit-org--parse-include-args element)))
              ;; delete #+ELLIT-INCLUDE: keyword
              (delete-region (point) (line-beginning-position 2))
              (apply #'ellit-org--include include-args))))))))

(defun ellit-org--include (ellit-file &rest props)
  "Include ELLIT-FILE.
PROPS is plist of properties, such as:
  `:eval-p'  - Filename to include specifies sexp to evaluate to get a
               filename.
  `:heading' - To include only given heading
  `:label'   - To include only given label from .el or .org file
  `:no-load' - Do not load .el ELLIT-FILE, loading is required to make
               macroses like {{{fundoc(xxx)}}} work."
  (when (plist-get props :eval-p)
    ;; NOTE: `ellit-file' specifies sexp form to evaluate to get real
    ;; filename
    (let ((real-ellit-file (eval (car (read-from-string ellit-file)))))
      (message "Evaluating ellit-file: %s -> %S" ellit-file real-ellit-file)
      (setq ellit-file real-ellit-file)))

  (let* ((ellit-dir (when ellit-org--filename
                      (file-name-directory ellit-org--filename)))
         (ellit-org--filename (expand-file-name ellit-file ellit-dir)))
    (insert (with-temp-buffer
              (insert-file-contents ellit-org--filename)

              (when (equal "el" (file-name-extension ellit-org--filename))
                (unless (plist-get props :no-load)
                  (condition-case err
                      (let ((load-prefer-newer t))
                        (load-file ellit-org--filename))
                    (t
                     (error "Error loading \"%s\": %S"
                            ellit-org--filename err))))
                (ellit-org--process-el (plist-get props :label)))

              (ellit-org--process-org props)
              (buffer-string)))))


;;; Ellit-Org export backend, retains `html' export snippets
(defun ellit-org-export-snippet (export-snippet _contents _info)
  "Transcode a EXPORT-SNIPPET object from Org to ellit-org.
Retains `html' export snippets.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (when (eq (org-export-snippet-backend export-snippet) 'html)
    (concat "@@html:" (org-element-property :value export-snippet) "@@")))

(defun ellit-org-toc--ref-github-style (text)
  "Return local anchor name created from TEXT."
  (replace-regexp-in-string
   "[^[:alnum:]_-]" ""
   (replace-regexp-in-string
    " " "-"
    (replace-regexp-in-string
     "--" "" (downcase (org-link-display-format text))))))

(defun ellit-org-toc (depth info)
  "Build a table of contents for `ellit-org' backend.
DEPTH is an integer specifying the depth of the table.
INFO is a plist used as a communication channel."
  (let ((toc-ents
         (mapcar (lambda (headline)
                   (unless (equal (org-element-property :TOC headline) "this")
                     (let ((desc (org-export-data-with-backend
                                  (org-export-get-alt-title headline info)
                                  (org-export-toc-entry-backend 'ellit-org)
                                  info)))
                     (list (org-export-get-relative-level headline info)
                           (ellit-org-toc--ref-github-style desc)
                           desc))))
                 (org-export-collect-headlines info depth))))
    (mapconcat (lambda (toc-ent)
                 (format "%s- [[#%s][%s]]" (make-string (* 2 (car toc-ent)) ?\s)
                         (cadr toc-ent) (caddr toc-ent)))
               (cl-remove-if 'null toc-ents) "\n")))

(defun ellit-org-export-keyword (keyword contents info)
  "Transcode KEYWORD element back into Org syntax.
CONTENTS is nil.  INFO is ignored."
  (let ((key (org-element-property :key keyword)))
    (if (and (plist-get info :with-ellit-toc)
             (equal key "TOC"))
        (let ((case-fold-search t)
              (value (org-element-property :value keyword)))
          (when (string-match "\\<headlines\\>" value)
            (let ((depth (and (string-match "\\<[0-9]+\\>" value)
                              (string-to-number (match-string 0 value)))))
              (ellit-org-toc depth info))))
      (org-org-keyword keyword contents info))))

(defun ellit-org-export-headline (headline contents info)
  "Transcode HEADLINE element back into Org syntax.
In case `:ellit-github-cid' export option is used, then generate
GitHub friendly `:CUSTOM_ID' property for this HEADLINE.
CONTENTS is its contents, as a string or nil.  INFO is ignored."
  (when (plist-get info :with-ellit-cid)
    (org-element-put-property headline :pre-blank 0)
    (setq contents (concat ":PROPERTIES:\n"
                           ":CUSTOM_ID: "
                           (ellit-org-toc--ref-github-style
                            (org-element-interpret-data
                             (org-element-property :title headline)))
                           "\n"
                           ":END:\n\n"
                           contents)))
  (org-org-headline headline contents info))

(org-export-define-derived-backend 'ellit-org 'org
  :options-alist '((:with-ellit-toc nil "ellit-toc" t)
                   (:with-ellit-cid nil "ellit-cid" t))
  :translate-alist '((headline . ellit-org-export-headline)
                     (keyword . ellit-org-export-keyword)
                     (export-snippet . ellit-org-export-snippet)))

;;;###autoload
(defun ellit-org-export (ellit-file output-org-file &rest props)
  "Export ELLIT-FILE to OUTPUT-ORG-FILE.
ELLIT-FILE could be one of \".org\" or \".el\".
PROPS is following property list:
  `:no-load' - Do not load corresponding .el file.
  `:heading' - Export only this heading section from ELLIT-FILE.
  `:no-ellit-macros' - Do not install ellit macro templates."
  (with-temp-buffer
    (apply #'ellit-org--include ellit-file props)
    ;; NOTE: `ellit-org--filename' is set for {{{ellit-filename}}} macro
    (let ((ellit-org--filename (expand-file-name ellit-file))
          (org-export-global-macros
           (nconc (unless (plist-get props :no-ellit-macros)
                    (copy-sequence ellit-org-macro-templates))
                  org-export-global-macros)))
      (org-export-to-file 'ellit-org output-org-file))))

(defun ellit-org--logo-image (&optional size debug-p)
  "Generate logo for the `ellit-org'.
SIZE is logo size in pixels.
DEBUG-P is debug purposes only."
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
  "Write ellit-org svg logo into FILE.
SIZE is passed directly to `ellit-org--logo-image'."
  (let ((logo-image (ellit-org--logo-image size)))
    (write-region "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" nil
                  file nil 'quiet)
    (write-region (plist-get (cdr logo-image) :data) nil
                  file 'append 'quiet)))


;;; Templates
(defun ellit-org-template-eval (sexp &optional as-string)
  "Insert results of the SEXP evaluation.
If AS-STRING is non-nil then use \"%s\" instead of \"%S\" for
formatting SEXP."
  (let ((as-string-p (not (string-empty-p as-string))))
    (format (if as-string-p "%s" "%S") (eval (read sexp)))))

(defun ellit-org-template-as-is (string)
  "Insert STRING as is."
  string)

(defun ellit-org-template-ellit-filename (&optional verbatim)
  "Insert currently processing filename.
If VERBATIM is specified, then outline filename with verbatim markup."
  (let ((verbatim-p (not (string-empty-p verbatim))))
    (concat (when verbatim-p "=")
            (file-name-nondirectory ellit-org--filename)
            (when verbatim-p "="))))

(defun ellit-org-template-kbd (key)
  "Insert HTML <kbd> tag with KEY contents."
  (concat "@@html:<kbd>@@" key "@@html:</kbd>@@"))

(defun ellit-org-template-where-is (command &optional keymap menu-items-p)
  "Insert list of keys that calls COMMAND.
KEYMAP is keymap where to lookup for COMMAND.  By default
`global-map' is considered.

If MENU-ITEMS-P is specified, then also insert commands inside
menu-items."
  ;; {{{kbd(C-c 1)}}}, {{{kbd(C-c 2)}}} (~command~)
  (let ((cmd-sym (intern command))
        (keymap-sym (intern keymap)))
    (concat (mapconcat (lambda (key)
                         (ellit-org-template-kbd (key-description key)))
                       (where-is-internal cmd-sym (symbol-value keymap-sym)
                                          nil (string-empty-p menu-items-p))
                       ", ")
            " (~" command "~)")))

(defun ellit--indented-docstring (docstring indent-level)
  (if (and indent-level (not (string-empty-p indent-level)))
      (mapconcat #'identity (split-string docstring "\n")
                 (concat "\n" (make-string
                               (string-to-number indent-level) ?\s)))
    docstring))

(defun ellit-org--where-is-key-replace-func (cmdstr)
  "Function to be used in `replace-regexp-in-string'."
  ;; NOTE: Handles `\\<keymap>\\[command]' syntax
  (let* ((keymap-str (match-string 1 cmdstr))
         (keys (where-is-internal (intern (match-string 2 cmdstr))
                                  (when keymap-str
                                    (symbol-value (intern keymap-str))))))
    (ellit-org-template-kbd (key-description (car keys)))))

(defun ellit-org--vardoc (varname &optional first-line-p)
  "Return docstring for the variable named by VARNAME.
If FIRST-LINE-P is non-nil, then return only first line of the docstring."
  (let* ((varsym (intern varname))
         (vardoc (documentation-property varsym 'variable-documentation t)))
    ;; Strip leading "*", seen in custom var docstring
    (when vardoc
      (replace-regexp-in-string
       (rx "`" (group (regexp "[^']+")) "'")
       "~\\1~"

       ;; NOTE: Handle `\\<keymap>\\[command]' syntax
       (replace-regexp-in-string
        ellit-org--where-is-key-regexp
        #'ellit-org--where-is-key-replace-func

        (string-trim-left
         (if first-line-p
             (car (split-string vardoc "\n"))
           vardoc)
         (concat "^" (regexp-quote "*"))))))))

(defun ellit-org-template-vardoc1 (variable)
  "Insert first line from docstring for the VARIABLE."
  (ellit-org--vardoc variable 'first-line))

(defun ellit-org-template-vardoc (variable &optional indent-level)
  "Insert full docstring for the VARIABLE."
  (ellit--indented-docstring (ellit-org--vardoc variable) indent-level))

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
  (let* ((funsym (if (stringp funname) (intern funname) funname))
         (fundoc (documentation funsym t)))
    (when fundoc
      (let (case-fold-search)
        ;; NOTE: emphasize `xxx' syntax
        (replace-regexp-in-string
         (rx "`" (group (regexp "[^']+")) "'")
         "~\\1~"

         ;; NOTE: Handle `\\<keymap>\\[command]' syntax
         (replace-regexp-in-string
          ellit-org--where-is-key-regexp
          #'ellit-org--where-is-key-replace-func

          ;; NOTE: emphasize arguments refs in fundoc with ~...~
          ;; syntax
          (replace-regexp-in-string
           (concat "\\<" (regexp-opt (ellit-org--funargs funsym)) "\\>")
           "~\\&~"
           (if first-line-p
               (car (split-string fundoc "\n"))
             fundoc))
          ))))))

(defun ellit-org-template-fundoc1 (function)
  "Insert first line from docstring for the FUNCTION."
  (ellit-org--fundoc function 'first-line))

(defun ellit-org-template-fundoc (function &optional indent-level)
  "Insert full docstring for the FUNCTION."
  (ellit--indented-docstring (ellit-org--fundoc function) indent-level))

(defun ellit-org-pp-code-block (value &optional indent-level)
  "Insert pretty-printed VALUE as code block."
  (let ((indent-str (when indent-level
                      (make-string indent-level ?\s)))
        (pp-val (replace-regexp-in-string
                 (rx (or (: string-start (* (any ?\r ?\n)))
                         (: (* (any ?\r ?\n)) string-end)))
                 "" (pp-to-string value))))
    (if (string-match-p (regexp-quote "\n") pp-val)
        (concat "\n" indent-str "#+begin_src emacs-lisp\n"
                pp-val "\n"
                indent-str "#+end_src\n")
      (concat "~" pp-val "~"))))

(provide 'ellit-org)

;;; ellit-org.el ends here
