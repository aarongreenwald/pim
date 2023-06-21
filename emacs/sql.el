(global-set-key (kbd "C-<f11>") 'pim-sql-scratch)

(add-hook 'sql-mode-hook (lambda () (define-key sql-mode-map (kbd "C-e") 'pim-exec-query-selection)))
(add-hook 'sql-mode-hook (lambda () (define-key sql-mode-map (kbd "C-x C-e") 'pim-exec-query-selection-write)))
(add-hook 'sql-mode-hook (lambda ()
			   (setq pim-sql-current-overlay (make-overlay 1 1))
			   (overlay-put pim-sql-current-overlay 'face '(:background "brightblack"))))

;; Interactive sql: M-x sql-sqlite
;;;;;;;;;;;;;;;;;;;;;;;
;; How to get sqli mode to output to a grid?

;; Note: override pim-host with "http://localhost:4321/api/" for testing
(defun pim-run-query (query bufname write-mode &optional keymap temp-buffer)
  (pim-api-request 'post "queries/exec"
       :body  `(("format" . "csv")
		("sql" . ,query)
		("writeMode" . ,write-mode))
       :as 'string
       :then `(lambda (data)
		(progn
		  (if (equal ',write-mode 0) 
		      (insert-to-pim-grid-buffer ',bufname data ',keymap ',temp-buffer)
		    (message (concat "Succeeded: " data)))))
  ))

(defun pim-exec-query-selection ()
  "Run the query under the point with readonly permissions and put the result in a pim-grid buffer."
  (interactive)
  (pim-highlight-current-sql-statement)
  ;; todo save execution history (comint?)
  (message "Running query...")
  ;; for running selection instead of query under point
  ;; (setq sql (buffer-substring-no-properties (mark) (point)))
  (setq sql (pim-get-current-sql-statement))
  (pim-run-query sql "pim-query" 0))

(defun pim-exec-query-selection-write ()
  "Run the query under the point with write permissions. The response will be shown in the message pane (no grid)"
  (interactive)
  (pim-highlight-current-sql-statement)
  (message "Executing query...")
  (setq sql (buffer-substring-no-properties (mark) (point)))
  (pim-run-query sql nil 1)
  )

(defun pim-get-current-sql-statement-region ()
  (interactive)
  
  (save-excursion
    (search-backward "\n\n" nil -1)
    (unless (equal (point) 1) (forward-char))
    (setq begin (point))
    (search-forward "\n\n" nil -1)
    (setq end (point))
    (list begin end)))

(defun pim-get-current-sql-statement ()
  (interactive)
  (setq vals (pim-get-current-sql-statement-region))
  (setq begin (nth 0 vals))
  (setq end (nth 1 vals))
  (buffer-substring-no-properties begin end))


;; todo this should be called constantly? Or only on demand? 
(defun pim-highlight-current-sql-statement ()
  (interactive)
  (setq vals (pim-get-current-sql-statement-region))
  (setq begin (nth 0 vals))
  (setq end (nth 1 vals))
  (move-overlay pim-sql-current-overlay begin end (current-buffer)))


(defun pim-sql-scratch ()
  (interactive)
  ;; todo save contents to tmp file in home
  (get-buffer-create "pim-sql_scratch")
  (pop-to-buffer "pim-sql_scratch")
  (sql-mode))



