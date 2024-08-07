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
  (setq json-array-type 'list) ;; should be a let-binding here, but for some reason it stopped working in emacs 29
  (pim-api-request 'post "queries/exec"
		   :body  `(("format" . "list") ;; TODO eliminate, rely on the header instead.
			    ("sql" . ,query)
			    ("writeMode" . ,write-mode))
		   :as #'json-read
		   :then `(lambda (data)
			    (progn
			      (if (equal ',write-mode 0)
				  (progn
				    (message "Succeeded on %s" pim-host)
				    (insert-to-pim-grid-buffer ',bufname data ',keymap ',temp-buffer))
				(message "Succeeded on %s, result: %s" pim-host  data))))))

(defun pim-sql-show-view (view-name &optional keymap)
  "Helper function - selects an entire table/view and displays it in a temporary buffer"
  (pim-run-query (concat "select * from " view-name) view-name 0 keymap t))

(defun pim-sql-show-query (query bufname &optional keymap)
  "Helper function - runs a readonly query and displays it in a temporary buffer"
  (pim-run-query query bufname 0 keymap t))

(defun pim-sql-show-endpoint (url bufname &optional keymap)
  "Helper function - calls an endpoint (GET) and displays the results in a temporary buffer"
  (setq json-array-type 'list) ;; should be a let-binding here, but for some reason it stopped working in emacs 29
  (pim-api-request 'get url
		   :as #'json-read
		   :then `(lambda (data)
			    (progn
			      (message "Succeeded on %s%s" pim-host ',url)
			      (insert-to-pim-grid-buffer ',bufname data ',keymap t))
			    )))

(defun pim-exec-query-selection ()
  "Run the query under the point with readonly permissions and put the result in a pim-grid buffer."
  (interactive)
  (pim-highlight-current-sql-statement)
  ;; todo save execution history (comint?)
  (message (concat "Running query on " pim-host))
  ;; for running selection instead of query under point
  ;; (setq sql (buffer-substring-no-properties (mark) (point)))
  (setq sql (pim-get-current-sql-statement))
  (pim-run-query sql "pim-query" 0))

(defun pim-exec-query-selection-write ()
  "Run the query under the point with write permissions. The response will be shown in the message pane (no grid)"
  (interactive)
  (pim-highlight-current-sql-statement)
  (message "Executing query...")
  ;;  (setq sql (buffer-substring-no-properties (mark) (point)))
  (setq sql (pim-get-current-sql-statement))
  (pim-run-query sql nil 1)
  )

(defun pim-view-sql-schema ()
  (interactive)
  (let ((sql "
select name, type, sql
from sqlite_schema
where name not like 'sqlite_%'
order by type"))
    (pim-run-query sql "pim-schema" 0)))

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



