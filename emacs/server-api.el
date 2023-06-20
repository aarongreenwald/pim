(defun pim-get-fuel-log()
  (interactive)
  (pim-api-request 'get "fuel-log"
		   :then `(lambda (data)
			  (progn		  
			    (pp-display-expression (alist-get 'summary data) "temp-result")))))


(defun pim-get-ls-dir()
  (interactive)
  (pim-api-request 'post "drive/ls-dir"
		   :body `(("path" . "./"))
		   :then `(lambda (data)
			  (progn		  
			    (pp-display-expression data "temp-result")
			    ))))

;; TODO pim-grid-buffer should accept json instead of csv
;; consider simplifying return to 2d arrays and parse that to list of lists to send to ctable
