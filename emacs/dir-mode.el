;; TODO how does autoload work?
;; pim-dir: custom output, currently based on csv-mode but it should be
;; rewritten to a custom mode for finer control. ctable could be nice but isn't exactly what I want. 
;; pim-dir can also be used in "file-mode", ie results of search that aren't
;; a single directory, some different columns and different shortcuts
;; For generic table (eg sql query output, see grid-mode)
;;;###autoload
(define-derived-mode pim-dir-mode csv-mode "pim-dir"
  "Major mode for pim drive directories."
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

(defun send-to-pim-dir-buffer (data dir)
  (insert-text-to-buffer "pim-ls-dir" data)
  (pim-dir-mode)
  (setq pim-mode-current-dir dir))

;; TODO - instead of csv-mode, a custom dir mode
(defun send-to-pim-dir-buffer2 (csv)
  "Given a csv, create a pim-dir buffer"
  ;; think about the API here. I can convert to lists first, or just iterate over the
  ;; csv and insert to the buffer properly formatted
  (setq lines (split-string csv "\n"))
  (dolist (line lines)
    (insert "\n")
    (insert (concat "Formatting" line))
    )
  )

(defvar pim-mode-current-dir nil
  "Name of the current directory in pim-mode")
(make-variable-buffer-local 'pim-mode-current-dir)

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
	       (send-to-pim-dir-buffer result ,dir)
	       (message (concat "Retrieved: " ,dir))))))

(defun pim-open-dir ()
  (interactive)  
  (setq line (buffer-substring (line-beginning-position) (line-end-position)))
  (csv-align-mode)
  (setq fields (split-string line ","))
  (setq dir (nth 0 fields)) ;; TODO improve this logic, use named fields
  (csv-align-mode)
  (setq fullname (concat pim-mode-current-dir dir "/"))
  (pim-ls-dir fullname)
  )

(defun pim-refresh-dir ()
  (interactive)
  (pim-ls-dir pim-mode-current-dir)
  )


(defun pim-back-dir ()
  (interactive)
  (if (string-equal pim-mode-current-dir "./") ;; todo this isn't working
      (message "Already at root")
    (progn
        (setq sections (split-string pim-mode-current-dir "/"))
	(setq dir (concat (string-join (butlast sections 2) "/") "/"))
	(pim-ls-dir dir)	
	)    
    ))

(define-key pim-dir-mode-map (kbd "r") 'pim-refresh-dir)
(define-key pim-dir-mode-map (kbd "e") 'pim-open-dir)
(define-key pim-dir-mode-map (kbd "b") 'pim-back-dir)