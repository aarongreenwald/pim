(defvar pim-drive-current-dir nil
  "Name of the current directory in a pim-dir buffer (can be grid-mode or dir-mode, either way this is used.)")
(make-variable-buffer-local 'pim-dir-current-dir)

(defun pim-ls-dir (&optional dir)
  (interactive)
  (or dir (setq dir "./"))

  (message (concat "Loading: " dir))
  
  (plz 'post "http://localhost:4321/api/drive/ls-dir"
    :headers '(("Content-Type" . "application/json"))
    :body (json-encode  `(("format" . "csv")
			  ("path" . ,dir)))
    ;;  :as #'json-read
    :then `(lambda (result)
	     (progn
	       ;; switch to one of these to use pim-dir-mode instead of grid-mode.
	       ;; (send-to-pim-dir-csv-buffer result ,dir)
	       ;; (send-to-pim-dir-buffer result ,dir)
	       ;; grid
	       (pim-dir-show-in-grid result ,dir (pim-dir-grid-keymap))
	       (message (concat "Retrieved: " ,dir))))))

(defun pim-dir-show-in-grid (data dir keymap)
  "pim-grid"
  (insert-to-pim-grid-buffer "pim-ls-dir" data keymap)
  (setq pim-dir-current-dir dir))


(defun pim-dir-grid-keymap ()
  (setq map (make-sparse-keymap))
  (define-key map (kbd "r") 'pim-refresh-dir)
  (define-key map (kbd "g") 'pim-dir-grid-open-dir)
  (define-key map (kbd "b") 'pim-back-dir)
  map)

(defun pim-dir-grid-open-dir ()
  "Opens dir in the current row. Currently assumes the relative path is the first column, this can be improved. "
  (interactive)
  (setq dir (nth 1 (pim-query-get-selected-row)))
  (setq fullname (concat pim-dir-current-dir dir "/"))
  (pim-ls-dir fullname)
  )

(defun pim-refresh-dir ()
  (interactive)
  (pim-ls-dir pim-dir-current-dir)
  )


(defun pim-back-dir ()
  "Given a pim-dir directory (eg pim-dir-current-dir is available), load pim-dir
for the parent directory"
  (interactive)
  (if (string-equal pim-dir-current-dir "./") ;; todo this isn't working
      (message "Already at root")
    (progn
        (setq sections (split-string pim-dir-current-dir "/"))
	(setq dir (concat (string-join (butlast sections 2) "/") "/"))
	(pim-ls-dir dir)	
	)    
    ))

