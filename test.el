;;; test.el --- Testing routines for ellit-org.el  -*- lexical-binding:t -*-
(require 'ellit-org)

(defun ellit-org-file--on-str (content &rest props)
  "Execute `ellit-org--include' on CONTENT string."
  (let ((el-file (concat (make-temp-file "ellit-org-el-file") ".el")))
    (write-region content nil el-file nil 'quiet)
    (unwind-protect
        (with-temp-buffer
          (apply #'ellit-org--include el-file props)
          (buffer-string))
      (delete-file el-file))))

(defun ellit-org--should (content should-content &optional template-alist)
  "Same as `should', but takes into account `ellit-org' specifics."
  (should (equal (ellit-org-file--on-str content template-alist)
                 should-content)))


;; Tests
(ert-deftest ellit-org--comments-starting ()
  "Test processing starting point."
  ;; See "* Commenting .el files" section
  (ellit-org--should "\
;; * This starts processing, buffer-start
;; Processing continues
"
                     "\
* This starts processing, buffer-start
Processing continues
")
  (ellit-org--should "\

;; * This also starts processing, empty line after buffer-start
;; Processing continues
"
                     "\
* This also starts processing, empty line after buffer-start
Processing continues
")

  ;; Heading in the middle of the commentary block
  (ellit-org--should "\
;; * This starts processing, 1line
;; Processing continues
'(<--- stops processing here)
;; New not matching commentary block
;; * Heading in the middle
;; - List in the middle
;; 1) Another list
;;
;; * Second section

;; * New heading starting commentary block
"
                     "\
* This starts processing, 1line
Processing continues

* New heading starting commentary block
")

  (ellit-org--should "\
;; #+title: included1

;; Not included

;; * Included2
;; Also included
'(code here)
;; Not icluded
"
                     "\
#+title: included1

* Included2
Also included
")
  )

(ert-deftest ellit-org--comments-leading-trailing-strips ()
  "Test leading/trailing part of the file is trimmed."
  (ellit-org--should "\
'(Leading line1)
'(leading line2)
;; #+title: processing starts here
;; Processing continues
'(Processing stops here)
'(This is not processed)
"
                     "\
#+title: processing starts here
Processing continues
")
  )

(ert-deftest ellit-org--comments-empty-line ()
  "Test empty line comments is ok."
  (ellit-org--should "\
;; #+title: processing starts here
;;
;; Processing continues
;; Still continue processing
'(<------- Processing stops here)
;; - And continues here
;;
;; And here still processing
'(stop here)
"
                     "\
#+title: processing starts here

Processing continues
Still continue processing

- And continues here

And here still processing
")
  )

(ert-deftest ellit-org--comments-non-comment-stops ()
  "Test that non-comment line stops processing."
  (ellit-org--should "\
;; #+title: included1

;; Not included

;; * Included2
;; Also included
'(code here)
;; Not icluded
"
                     "\
#+title: included1

* Included2
Also included
")
  )

(ert-deftest ellit-org--comments-heading-in-the-middle ()
  "Test property/heading/list occurs in the middle of comment line."
  (ellit-org--should "\
;; #+title: this line is included
;; Processing continues

;; This line should - not be included
;; - This line is ALSO NOT included

;; This is * not * included

;; This line #+either should not be included
"
                     "\
#+title: this line is included
Processing continues
")
  )


;;; TODO Testing templates and exporting

;;; test.el ends here
