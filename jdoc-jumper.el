;;; jdoc-jumper.el --- Jump from code directly to javadoc
;;
;; Author: LaurenceWarne
;; URL: https://github.com/LaurenceWarne/jdoc-jumper
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0") (lsp-java "6.0"))
;; Version: 0.2
;;
;;; Commentary:
;; 

;;; Code:

(defgroup jdoc-jumper nil
  "Tool for jumping to browser javadoc from source files."
  :prefix "jdoc-jumper-"
  :group 'applications
  :link '(url-link :tag "GitHub" "https://github.com/LaurenceWarne/jdoc-jumper"))

(defcustom jdoc-jumper-url-callback 'browse-url
  "Function to call when jdoc-jumper wants to open url."
  :group 'jdoc-jumper
  :type 'function)

(defcustom jdoc-jumper-javadocio-url "https://static.javadoc.io/"
  "Base url for maven/gradle dependencies."
  :group 'jdoc-jumper
  :type 'string)

(defcustom jdoc-jumper-jdk-url "https://docs.oracle.com/javase/8/docs/api/"
  "Base url for standard library classes."
  :group 'jdoc-jumper
  :type 'string)

(defun jdoc-jumper-get-url-from-dep-cache (uri &optional maven-path)
  (let* ((detail-list
	  (if maven-path (split-string uri ".m2%5C/repository%5C/")
	    (split-string uri "modules-2%5C/files-2.1%5C/")))
	 (org-name-version (car (cdr detail-list)))
	 (org-name-version-list (split-string org-name-version "%5C/"))
	 (start-index (if maven-path 0 1))
	 (jar-info (car (last org-name-version-list)))
	 (classpath (replace-regexp-in-string
				 "[\.(]" "/"
				 (substring (car (cdr (split-string jar-info "%3C"))) 0 -6)))
	 (version (car (last org-name-version-list (+ start-index 2))))
	 (artifact (car (last org-name-version-list (+ start-index 3))))
	 (org (mapconcat 'identity (butlast org-name-version-list (+ start-index 3)) ".")))
	(concat jdoc-jumper-javadocio-url
			org "/" artifact "/" version "/" classpath ".html")))

(defun jdoc-jumper-get-stdlib-url (uri)
  (let ((classpath (replace-regexp-in-string
					"[\.(]" "/"
					(substring (car (cdr (split-string uri "%3C"))) 0 -6))))
	(concat jdoc-jumper-jdk-url classpath ".html")))

(defun jdoc-jumper-jump-from-point ()
  "Open a browser at the javadoc of the type of the object at the cursor."
  (interactive)
  (let* ((response
	  (lsp-request "textDocument/typeDefinition"
		       (lsp--text-document-position-params)))
	 (uri (gethash "uri" (car response)))
	 (url (cond
		   ;; Check if dependency in maven cache
		   ((string-match-p (regexp-quote ".m2%5C/repository%5C/") uri)
			(jdoc-jumper-get-url-from-dep-cache uri t))
		   ;; Check if dependency in gradle cache
		   ((string-match-p (regexp-quote "modules-2%5C/files-2.1%5C/") uri)
			(jdoc-jumper-get-url-from-dep-cache uri))
		   ;; Check if class is in java standard library
		   ((string-match-p (regexp-quote "/rt.jar%3C") uri)
			(jdoc-jumper-get-stdlib-url uri)))))
    (funcall jdoc-jumper-url-callback url)))

(provide 'jdoc-jumper)

;;; jdoc-jumper.el ends here
