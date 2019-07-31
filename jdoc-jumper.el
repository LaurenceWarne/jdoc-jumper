;;; jdoc-jumper.el --- Jump from code directly to javadoc
;;
;; Author: LaurenceWarne
;; URL:
;;; Commentary:
;; 

;;; Code:

(require 'lsp)
(require 'lsp-java)

(defvar jdoc-jumper-javadocio-url
  "https://static.javadoc.io/"
  "URL of default search location.")

(defvar jdoc-jumper-regex
  "^jdt://contents.*\.m2%5C/repository%5C/net%5C.*%3C.*(.*\.class$"
  "Regular expression which uris should match for the package to work.")

(defun jdoc-get-jar-details (uri &optional maven-path)
  (let* ((detail-list
	  (if maven-path (split-string uri ".m2%5C/repository%5C/")
	    (split-string uri "modules-2%5C/files-2.1%5C/")))
	 (org-name-version (car (cdr detail-list)))
	 (org-name-version-list (split-string org-name-version "%5C/"))
	 (start-index (if maven-path 0 1))
	 (jar-info (car (last org-name-version-list)))
	 (version (car (last org-name-version-list (+ start-index 2))))
	 (artifact (car (last org-name-version-list (+ start-index 3))))
	 (org (mapconcat 'identity (butlast org-name-version-list (+ start-index 3)) "."))
	 (info-map (make-hash-table :test 'equal)))
    (puthash "organisation" org info-map)
    (puthash "artifact" artifact info-map)
    (puthash "version" version info-map)
    (puthash "jar-info" jar-info info-map)
    (identity info-map)))

(defun jdoc-jumper-jump-from-point ()
  "Open a browser at the javadoc of the type of the object at the cursor."
  (interactive)
  (let* ((response
	  (lsp-request "textDocument/typeDefinition"
		       (lsp--text-document-position-params)))
	 (uri (gethash "uri" (car response)))
	 (details-map
	  (cond
	   ;; Check if dependency in maven cache
	   ((string-match-p (regexp-quote ".m2%5C/repository%5C/") uri)
	    (jdoc-get-jar-details uri t))
	   ;; Check if dependency in gradle cache
	   ((string-match-p (regexp-quote "modules-2%5C/files-2.1%5C/") uri)
	    (jdoc-get-jar-details uri))))
	 (url
	  (concat jdoc-jumper-javadocio-url
		  (gethash "organisation" details-map) "/"
		  (gethash "artifact" details-map) "/"
		  (gethash "version" details-map))))
    (browse-url url)))

(provide 'jdoc-jumper)

;;; jdoc-jumper.el ends here
