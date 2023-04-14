;; Currently based on csv-mode, can it be based on ctable?
;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/


;; pim-grid should allow arbitrary funtions on cell under point as well row perhaps
(define-derived-mode pim-grid-mode csv-mode "pim-grid"
  "Major mode for output of pim sql queries."
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

(defun insert-to-pim-grid-buffer (bufname text)
  "Expects a csv in text, inserts to a buffer and sets to pim-grid-mode"
  ;; (csv-to-ctbl text)
  (insert-text-to-buffer bufname text)
  (pim-grid-mode))
