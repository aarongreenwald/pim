(defvar pim-drive-current-dir nil
  "Name of the current directory in a pim-dir buffer (can be grid-mode or dir-mode, either way this is used.)")
(make-variable-buffer-local 'pim-dir-current-dir)

(defun pim-dir-ls (&optional dir)
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
  (insert-to-pim-grid-buffer "pim-dir-ls" data keymap)
  (setq pim-dir-current-dir dir))


(defun pim-dir-grid-keymap ()
  (setq map (make-sparse-keymap))
  (define-key map (kbd "r") 'pim-refresh-dir)
  (define-key map (kbd "g") 'pim-dir-grid-open-dir)
  (define-key map (kbd "b") 'pim-back-dir)
  (define-key map (kbd "f") 'pim-dir-get-all-filenames-for-hash)
  (define-key map (kbd "v") 'pim-dir-get-versions-for-filename)
  map)

(defun pim-dir-grid-open-dir ()
  "Opens dir in the current row. Currently assumes the relative path is the first column, this can be improved. "
  (interactive)
  (setq dir (nth 1 (pim-query-get-selected-row)))
  (setq fullname (concat pim-dir-current-dir dir "/"))
  (pim-dir-ls fullname)
  )

(defun pim-refresh-dir ()
  (interactive)
  (pim-dir-ls pim-dir-current-dir)
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
	(pim-dir-ls dir)	
	)    
    ))

(defun pim-dir-get-all-filenames-for-hash ()
  "Returns all current filenames matching the current hash. Must be on a pim-grid cell called 'sha256'."
  (interactive)
  (setq col-name (pim-query-get-selected-cell-name))
  (if (not (string-equal col-name "sha256"))
      (message (concat col-name " is not a sha256 cell"))
    (setq sha256 (pim-query-get-selected-cell-value))
    ;; todo maybe show non-current as well? 
    (setq sql (format "select * from v_files_current where sha256 = '%s'" sha256))
    (pim-run-query sql "pim-filenames" nil 1)
  ))

(defun pim-dir-get-versions-for-filename ()
  "Returns all versions of a given filename. Must be on a pim-grid cell called 'name'"
  (interactive)
  (setq col-name (pim-query-get-selected-cell-name))
  (if (not (string-equal col-name "name"))
      (message (concat col-name " is not a name cell"))
    (setq filename (concat pim-dir-current-dir (pim-query-get-selected-cell-value)))
    (setq sql (format "select * from v_file where name = '%s' order by version desc" filename))
    (pim-run-query sql "pim-filenames" nil 1)
  ))
