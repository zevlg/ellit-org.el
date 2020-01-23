;;; test.el --- Testing routines for ellit-org.el  -*- lexical-binding:t -*-
(require 'ellit-org)

(defun ellit-org-file--on-str (content &optional custom-template-alist)
  "Execute `ellit-org-file' on CONTENT string.
CUSTOM-TEMPLATE-ALIST is passed directly to `ellit-org-file'."
  (let ((el-file (make-temp-file "ellit-org-el-file")))
    (write-region content nil el-file nil 'quiet)
    (unwind-protect
        (ellit-org-file el-file nil custom-template-alist)
      (delete-file el-file))))


;; Tests
(ert-deftest ellit-org--comments-leading-trailing-strips ()
  "Test leading/trailing part of the file is trimmed."
  )

(ert-deftest ellit-org--comments-empty-line ()
  "Test empty line comments is ok."
  )

(ert-deftest ellit-org--teplate-modifying-match-data ()
  "Test modifying match data templates are working."
  )

(ert-deftest ellit-org-template-custom-alist ()
  "Test `custom-template-alist' arg is working."
  (let ((custom-template-alist
         '(("MYCUSTOM" . (lambda (_ignored)
                           "Looks like working")))))
    (should (equal (ellit-org-file--on-str "
File
;; * Testing heading
;; >>>MYCUSTOM<<<
Done"
                                           custom-template-alist)
                   "* Testing heading\nLooks like working\n"))
    ))

;;; test.el ends here
