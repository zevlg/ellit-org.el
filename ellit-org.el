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

;; telega is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ellit-org.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(defvar ellit-org-template-alist
  '(("KEY" . ellit-org-template-key)
    ("DOCSTR" . ellit-org-template-docstr))
  "Alist of available templates.
Each element in form:
  (TEMPLATE-NAME . TEMPLATE-FUNC)
TEMPLATE-FUNC is called with single string(or nil) argument.")

(defvar ellit-org-directory nil
  "Bind this var to the current ellit-doc directory.
Used to lookup files in >>>ELLIT<<< template chunks.")

(defun ellit-org-apply-template (name &optional arg)
  "Return value for the template chunk with NAME.
Optional string ARG could be given."
  (let ((template (assoc name ellit-org-template-alist)))
    (unless template
      (error "Unknown template name: %s" name))
    (funcall (cdr template) arg)))

(defun ellit-org-apply-all-templates ()
  "Replace all template chunks in current buffer with their values."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ">>>\\([a-zA-Z]+\\)\s*\\([^<]*\\)<<<" nil t)
      (replace-match
       (ellit-org-apply-template (match-string 1) (match-string 2))))))
    
(defun ellit-org-extract (el-file)
  "Extract strings from the EL-FILE."
  )

(defun ellit-org-file (el-file &optional output)
  "Process EL-FILE providing output into OUTPUT."
  (let* ((real-el-file (expand-file-name el-file ellit-org-directory))
         (ellit-org-directory (file-name-directory real-el-file)))
    (ellit-org-apply-all-template
     (ellit-org-extract real-el-file))
    ))


(defun ellit-org-template-key (arg)
  "Keybinding substitution.
ARG is either form:
  1) \"<command>\"  - lookup for <command> in `global-map'
  2) \"<keymap>:<command>\" - lookup for <command> in <keymap>
"
  )

(defun ellit-org-template-docstr (arg)
  "Docstring substitution.
ARG is in form: [v|f ]<symbol-name>
v to force variable.
f to force function.
If ommited, function symbol tried first."
  (let* ((args (split-string arg))
         (sym (intern (car (last args))))
         (func-p (or (string= (car args) "f")
                     (not (boundp sym)))))
    (if func-p
        (progn
          (cl-assert (fboundp sym))
          (documentation sym t))

      (cl-assert (boundp sym))
      (documentation-property sym 'variable-documentation))))

(provide 'ellit-org)

;;; ellit-org.el ends here
