;;; jdoc-jumper.el --- Jump from code directly to javadoc
;;
;; Author: LaurenceWarne
;; URL:
;;; Commentary:
;; 

;;; Code:

(defvar jdoc-jumper-javadocio-url
  "https://www.javadoc.io/doc"
  "URL of default search location.")

(setq test-string "jdt://contents/artemis-odb-2.2.0.jar/com.artemis/WorldConfiguration.class?=second-space-core/%5C/home%5C/laurencewarne%5C/.m2%5C/repository%5C/net%5C/onedaybeard%5C/artemis%5C/artemis-odb%5C/2.2.0%5C/artemis-odb-2.2.0.jar%3Ccom.artemis(WorldConfiguration.class")

(defvar jdoc-jumper-regex
  "^jdt://contents.*\.m2%5C/repository%5C/net%5C.*%3C.*(.*\.class$"
  "Regular expression which uris should match for the package to work.")

(defun jdoc-jumper-jump-from-point ()
  "Open a browser at the javadoc of the type of the object at the cursor."
  (interactive)
  (let* ((response
	 (lsp-request "textDocument/typeDefinition"
		      (lsp--text-document-position-params)))
	 (uri (gethash "uri" (car loc)))
	 (class-details (jdoc-get-jar-details uri)))
    ))

(defun jdoc-get-jar-details (string)
  (let* ((detail-list (split-string string ".m2%5C/repository%5C/"))
	 (org-name-version (car (cdr detail-list)))
	 (org-name-version-list (split-string org-name-version "%5C/"))
	 (jar-info (car (last org-name-version-list)))
	 (version (car (last org-name-version-list 2)))
	 (artefact (car (last org-name-version-list 3)))
	 (org (mapconcat 'identity (butlast org-name-version-list 3) ".")))
    '(org artefact version jar-info)))

(provide 'jdoc-jumper)

;;; jdoc-jumper.el ends here
