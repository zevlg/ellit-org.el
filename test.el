;;; test.el --- Testing routines for ellit-org.el  -*- lexical-binding:t -*-
(require 'ellit-org)

(defun ellit-org-file--on-str (content &optional custom-template-alist)
  "Execute `ellit-org-file' on CONTENT string.
CUSTOM-TEMPLATE-ALIST is passed directly to `ellit-org-file'."
  (let ((el-file (make-temp-file "ellit-org-el-file.el")))
    (write-region content nil el-file nil 'quiet)
    (unwind-protect
        (ellit-org-file el-file nil custom-template-alist)
      (delete-file el-file))))

(defun ellit-org--should (content should-content &optional custom-template-alist)
  (should (equal (ellit-org-file--on-str content custom-template-alist)
                 (if (string-prefix-p "\n" should-content)
                     (substring should-content 1)
                   should-content))))


;; Tests
(ert-deftest ellit-org--comments-leading-trailing-strips ()
  "Test leading/trailing part of the file is trimmed."
  (ellit-org--should "
Leading line1
leading line2
;; #+title: processing starts here
;; Processing continues
Processing stops here
This is not processed"
                     "
#+title: processing starts here
Processing continues
")
  )

(ert-deftest ellit-org--comments-empty-line ()
  "Test empty line comments is ok."
  (ellit-org--should "
;; #+title: processing starts here
;;
;; Processing continues
;; 
;; Still continue processing
Processing stops here
;; - And continues here
;; 
;; And here still processing
stop here"
                     "
#+title: processing starts here

Processing continues

Still continue processing
- And continues here

And here still processing
")
  )

(ert-deftest ellit-org--template-ellit ()
  "Test >>>ELLIT<<< template is working well."
  (let ((el-file2 (make-temp-file "el-file2.el")))
    (write-region "
leading
;; ** Heading from el-file2
;; String from el-file2
trailing
"
                  nil el-file2 nil 'quiet)

    (unwind-protect
        (ellit-org--should (format "
File
;; * Heading from el-file1
;; >>>ELLIT %s<<<
;; String from el-file1
Done" (file-name-nondirectory el-file2))
                           "
* Heading from el-file1
** Heading from el-file2
String from el-file2
String from el-file1
")

      (delete-file el-file2))))

(ert-deftest ellit-org--template-custom-alist ()
  "Test `custom-template-alist' arg is working."
  (let ((custom-template-alist
         '(("MYCUSTOM" . (lambda (_ignored)
                           "Looks like working")))))
    (ellit-org--should "
File
;; * Testing heading
;; >>>MYCUSTOM<<<
Done"
                       "
* Testing heading
Looks like working
"
                       custom-template-alist)
    ))

(ert-deftest ellit-org--teplate-modifying-match-data ()
  "Test modifying match data templates are working."
  )

;;; test.el ends here
