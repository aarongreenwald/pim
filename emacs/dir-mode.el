;; TODO how does autoload work?
;; pim-dir: custom output, currently based on csv-mode but it should be
    ;; rewritten to a custom mode for finer control. ctable could be nice but isn't exactly what I want. 
;; pim-dir can also be used in "file-mode", ie results of search that aren't
;; a single directory, some different columns and different shortcuts
;; from pim-dir it should be possible to "view as grid" and convert to pim-grid and then navigate from there. 
;; For generic table (eg sql query output, see grid-mode)

(define-derived-mode pim-dir-mode
  text-mode
  "pim-dir"
  "Deprecated: Major mode for pim drive directories based on csv-mode"

  (read-only-mode) 
  (goto-line 2)
  )

(defun send-to-pim-dir-buffer (text dir)
  "WIP: Given a data as a csv, create a pim-dir buffer"
  ;; think about the API here. I can convert to lists first, or just iterate over the
  ;; csv and insert to the buffer properly formatted
  (setq data-list (parse-csv-to-list text))
  (setq headers (pop data-list))
  (setq pim-dir-current-dir dir)  

  (get-buffer-create "pim-ls-dir")
  (pop-to-buffer "pim-ls-dir")
  (read-only-mode 0)
  (erase-buffer)
  
  (dolist (line data-list)
    (insert "\n")

    (setq name (nth 0 line))
    (setq is-dir (string-equal "dir" (nth 6 line )))
    (setq version (nth 9 line))
    (setq size (nth 5 line))
    (setq created (nth 1 line))
    (insert (concat
	     (if is-dir "D" "F") "  "
	     version " "
	     (human-formatted-size size) "\t"
	     created " | "	     
	     name
	     )))

  (pim-dir-mode)
  )

(defun human-formatted-size (bytes)
  "Formats number of bytes as human readable string."
  (setq num (string-to-number bytes))
  (cond ((> 1e3 num)  (number-to-string num))
	((> 1e6 num)  (concat (number-to-string (/ num (truncate 1e3))) " K"))
	((> 1e9 num)  (concat (number-to-string (/ num (truncate 1e6))) " M"))
	((> 1e12 num) (concat (number-to-string (/ num (truncate 1e9))) " G"))	
	(bytes)
	)
  )

;; (define-key pim-dir-mode-map (kbd "RET") 'pim-dir-mode-open-dir)
(define-key pim-dir-mode-map (kbd "r") 'pim-refresh-dir)
(define-key pim-dir-mode-map (kbd "b") 'pim-back-dir)
