(global-set-key (kbd "C-<f11>") 'pim-sql-scratch)
(define-key sql-mode-map (kbd "C-e") 'pim-exec-query-selection)

;; Interactive sql: M-x sql-sqlite
;;;;;;;;;;;;;;;;;;;;;;;
;; How to get sqli mode to output to a grid?

(defun pim-run-query (query bufname)
  (plz 'post "http://localhost:4321/api/queries/exec"
       :headers '(("Content-Type" . "application/json"))
       :body (json-encode  `(("format" . "csv")
			     ("sql" . ,query)))
       ;;  :as #'json-read
       :then `(lambda (data)
		(progn
		  (insert-to-pim-grid-buffer ',bufname data)
		  (message "Completed query"))))
  )

(defun pim-sample-query ()
  (interactive)
  (pim-run-query "select date(record_date / 1000, 'unixepoch') date, ils, usd from v_car_summary order by record_date desc" "pim-query"))

;; todo run sql query under point
(defun pim-exec-query-selection ()
  (interactive)
  (message "Running query...")
  (setq sql (buffer-substring-no-properties (mark) (point)))
  (pim-run-query sql "pim-query"))

(defun pim-sql-scratch ()
  (interactive)
  (get-buffer-create "pim-sql_scratch")
  (pop-to-buffer "pim-sql_scratch")
  (sql-mode))



