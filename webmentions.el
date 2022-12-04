;;; webmentions.el -- Webmention sending from Emacs

;; Copyright (C) 2022 William Kearney
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the “Software”), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: William Kearney <wkearn@gmail.com>
;; Version: 0.1
;; Created: 02 December 2022
;; Keywords: hypermedia tools

;;; Commentary:

;; This package provides very basic functionality for sending webmentions
;; from within Emacs, including automatic endpoint discovery.

(require 'url)

(defun webmention-get-links (posturl)
  "Collect outgoing links from the current buffer."
  (mapcar (lambda (node) (url-expand-file-name (dom-attr node 'href) posturl))
	  (dom-by-tag
	   (dom-by-class
	    (libxml-parse-html-region (point-min) (point-max))
	    "h-entry")
	   'a)))

(defconst webmention-link-header-regex "<\\([^<>]*\\)>; rel=['\"]?\\([a-z ]*webmention[a-z ]*\\)['\"]?")

(defun webmention-parse-link-header ()
  (goto-char (point-min)) ;; Go to the start of the buffer
  (re-search-forward "^link:" nil t) ;; Get to the link header
  (cond ((re-search-forward webmention-link-header-regex nil t) ;; Find webmention link
	 (match-string 1))))

(defun webmention-compare-link (node)
  (let ((rel (dom-attr node 'rel)))
    (if (and rel
	     (dom-attr node 'href) ;; Check to make sure the node has an href
	     )
	(string-match "^\\([a-z]*[[:blank:]]\\)*webmention\\( [a-z]*\\)*" rel))
    ))

(defun webmention-get-link-urls ()
  (let ((node (nth 0 (dom-search (libxml-parse-html-region (point) (point-max)) 'webmention-compare-link))))
    (cond (node (dom-attr node 'href)))))

(defun webmention-endpoint-search ()
    (cond ((webmention-parse-link-header))
	  ((webmention-get-link-urls))))

(defun webmention-discover-endpoint (url)
  "Return the webmention endpoint associated with URL
or nil if there is no endpoint found."
  (let ((url-request-method "GET")
	(webmention-callback-result nil)
	(request-done nil))
    (url-retrieve url (lambda (status)
			(setq webmention-callback-result (url-expand-file-name
							  (webmention-endpoint-search)
							  (plist-get status :redirect))
			      request-done t)))
    ;; Is there a better way to wait on the callback?
    (while (not request-done)
      (sleep-for 0 10))
    webmention-callback-result))

;;###autoload
(defun webmention-send-post (source target)
  "Send a webmention from SOURCE to TARGET via the
webmention endpoint discovered at the TARGET url. 
The data returned by the endpoint is displayed in a new buffer."
  (interactive "ssource: \nstarget: ")
  (let ((endpoint (webmention-discover-endpoint target)))
    (if endpoint
	(let ((url-request-method "POST")
	      (url-request-extra-headers
               '(("Content-Type" . "application/x-www-form-urlencoded")))
	      (url-request-data (concat
				 "source="
				 source
				 "&"
				 "target="
				 target)))
	  (display-buffer (url-retrieve-synchronously endpoint)))
      (message "Endpoint does not exist for target %s" target))))

(provide 'webmention)

;;; webmentions.el ends here
