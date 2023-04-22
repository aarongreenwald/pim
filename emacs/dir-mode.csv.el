(define-derived-mode pim-dir-csv-mode csv-mode "pim-dir-csv"
  "Deprecated: Major mode for pim drive directories based on csv-mode"
  (csv-populate-fields-list)
  (csv-header-line)
  (csv-align-mode)
  (read-only-mode)
  (color-columns)
  (display-line-numbers-mode 0)
  (set-widths)
  
  ;; after inserting text we're at the end
  ;; this might not be the best place to do this
  ;; (goto-char 1)
  (goto-line 2) ;skip the header. todo: remove/hide the header
  )

(defun send-to-pim-dir-csv-buffer (data dir)
  "Deprecated: Send to buffer using pim-dir-mode based on csv"
  (insert-text-to-buffer "pim-ls-dir" data)
  (pim-dir-csv-mode)
  (setq pim-dir-current-dir dir))

(defun pim-dir-csv-mode-open-dir ()
  "Deprecated Opens a dir in pim-dir-mode based csv-mode"
  (interactive)  
  (setq line (buffer-substring (line-beginning-position) (line-end-position)))
  (csv-align-mode)
  (setq fields (split-string line ","))
  (setq dir (nth 0 fields)) ;; TODO improve this logic, use named fields
  (csv-align-mode)
  (setq fullname (concat pim-dir-current-dir dir "/"))
  (pim-ls-dir fullname)
  )

(define-key pim-dir-csv-mode-map (kbd "RET") 'pim-dir-csv-mode-open-dir)
(define-key pim-dir-csv-mode-map (kbd "r") 'pim-refresh-dir)
(define-key pim-dir-csv-mode-map (kbd "b") 'pim-back-dir)
