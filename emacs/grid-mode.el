;;;;;;;;;;;;;;;;;
;; Look into ses-mode, other grid-modes? cell-mode? ftable? table.el? 
;; https://vallyscode.github.io/note/tabulated-list-mode/
;; more samples: https://github.com/kiwanami/emacs-ctable/blob/master/samples/large-table.el
;; https://github.com/kiwanami/emacs-ctable/blob/48b73742757a3ae5736d825fe49e00034cc453b5/ctable.el

;;;;;;;;;;;;;;;;;;
;; Documentation: 
;; * hjkl to navigate, also pnbf
;; * c: jump to column
;; * e, a: first, last columns
;; TODO:
;; * How to add the keymap override to the doc? 
;; * mark/unmark cells/rows, change their color and use their values in functions (sum, avg, etc)
;; * Common functions on current value that run queries, ideally pass a specific keymap when inserting to grid that specifies which keys call which functions, the rest can be called interactively. On a generic sql query, there are no special functions
;; * Optionally set buffer to be disposable instead of reusing a permanent buffer, so that stacking is possible

(defun insert-to-pim-grid-buffer (bufname text &optional keymap temp-buffer)
  "Expects a csv in text, inserts to a buffer and sets to pim-grid-mode"
  (setq data-list (parse-csv-to-list text))
  (setq headers (pop data-list))

  (setq param (copy-ctbl:param ctbl:default-rendering-param))

  ;; todo styling the table
  ;; (setf (ctbl:param-fixed-header param) t) ; set header parameters
  ;; (setf (ctbl:param-hline-colors param)    ; horizontal line color
  ;;       '((0 . "#00000") (1 . "#909090") (-1 . "#ff0000") (t . "#00ff00"))) 
  ;; (setf (ctbl:param-draw-hlines param)     ; horizontal line draw conditions
  ;;       (lambda (model row-index)
  ;;         (cond ((memq row-index '(0 1 -1)) t)
  ;;               (t (= 0 (% (1- row-index) 5))))))
  ;; (setf (ctbl:param-bg-colors param)       ; cell background color
  ;;       (lambda (model row-id col-id str)
  ;;         (cond ((string-match "CoCo" str) "LightPink")
  ;;               ((= 0 (% (1- row-index) 2)) "Darkseagreen1")
  ;;               (t nil))))

  ;; open-table-buffer expects passed buffer to exist if it doesn't
  (get-buffer-create bufname)
  (pop-to-buffer bufname)

  (setq pim-grid-data (list headers data-list))
  
  (let* ((column-model (generate-column-model headers))
    (data data-list)
    (model 
     (make-ctbl:model
      :column-model column-model
      :data data))
    component)    

    (setq component
	  ;; this approach works, but setting the mode to pim-grid-mode breaks
	  ;; ctbl:component? Timing might be an issue
	  (ctbl:open-table-buffer
	   :model model
	   :param param	   
	   :buffer bufname
	   :custom-map keymap))
     ;; Alternative to open-table-buffer: get the raw text so we can insert directly to the buffer we choose
     ;; https://github.com/kiwanami/emacs-ctable/blob/48b73742757a3ae5736d825fe49e00034cc453b5/readme.md#view-components
     ;; (ctbl:get-table-text      
     ;;  :model model
     ;;  :param param)))

    ;; (insert-text-to-buffer bufname component)
    ;; (pim-grid-mode)   
    ;; (setq-local ctbl:component component) ;;doesn't seem to work right
    
    (ctbl:cp-add-selection-change-hook
     ctbl:component (lambda ()
		      ;; shut off line numbers because for some reason the line numbers
		      ;; mode is turned on only on the first selection change
		      (if display-line-numbers-mode (display-line-numbers-mode 0))
		      (ctbl:show-cell-in-tooltip)))

    (pim-grid-mode 1)
    (if temp-buffer (local-set-key (kbd "q") 'kill-buffer-and-window))
    ))


(defun generate-column-model (headers)
  "Expects a list of headers"
  (mapcar #'generate-column headers))

(defun generate-column (name)
  ;; TODO can I show elipsis when the width truncated?
  ;; ctbl:show-cell-in-tooltip automatically whenever entering a truncated cell? 
  (make-ctbl:cmodel
   :title name
   :sorter 'ctbl:sort-number-lessp ;;todo what can I do with this?
   ;;   :min-width 50
   :max-width (cond ((member name '("sha1" "sha256")) 15)
		(40))
   :align (cond ((member name '("usd" "ils" "amount" "size")) 'right)
		('left))
   ;; :click-hooks ;;header click hooks
   ))


(define-minor-mode pim-grid-mode
  "Minor mode for output of pim sql queries.

Ideally this would be a major mode derived from ctbl:table-mode 
but that breaks the state inside the mode (ctbl:component etc)
So this will have to do. 
"
  :init-value nil
  :lighter " pim-grid"

  )

(add-hook 'pim-grid-mode-on-hook
	  (lambda ()
	    (display-line-numbers-mode 0)
	    ))

;; Sample code for processing data
;; (defun ctbl:cp-get-selected-data-cell (component)
;;   "Return the selected cell data. If no cell is selected, return nil."
;;   (let* ((rows (ctbl:component-sorted-data component))
;;          (cell-id (ctbl:component-selected component))
;;          (row-id (car cell-id)) (col-id (cdr cell-id)))
;;     (if row-id
;;         (nth col-id (nth row-id rows))
;;       nil)))

(defun pim-query-get-column-by-name (col-name)
  (cl-position col-name (nth 0 pim-grid-data) :test 'string-equal)
  )

(defun pim-query-get-selected-cell-name ()
  (interactive)
  (setq column (cdr (ctbl:cp-get-selected ctbl:component)))
  (nth column (nth 0 pim-grid-data)))

(defun pim-query-get-selected-cell-value ()
  (interactive)
  (ctbl:cp-get-selected-data-cell ctbl:component))

(defun pim-query-get-selected-row()
  (interactive)
  ;;todo consider converting list to alist using the column headers
  ;;in (nth 0 pim-grid-data)
  (ctbl:cp-get-selected-data-row ctbl:component))

(defun pim-query-get-column-value-from-selected-row (col-name)
  (nth (pim-query-get-column-by-name col-name) (pim-query-get-selected-row)))


(defvar pim-grid-data nil
  "Raw data in the grid - first element is headers, second element is list of lists (rows of fields)")
(make-variable-buffer-local 'pim-grid-data)

(defvar pim-grid-mark-stats (list 0 0 0)
  "Structure holds: avg, sum, count of marked cells.")
(make-variable-buffer-local 'pim-grid-data-marked-stats)

(defvar pim-grid-marked-cells nil
  "Mapping between cell-id and overlay")
(make-variable-buffer-local 'pim-grid-data-marked-cells)


(defun pim-query-toggle-mark-cell()
  "Marks/unmarks the currently selected cell, overlaying it with a special style, and 
keeps stats (sum/count/average) on the selected cells."
  (interactive)
  ;; todo if it isn't a number it's treated as "0", I think, and messes up count and average. 
  (setq cell-value (string-to-number (pim-query-get-selected-cell-value)))
  (setq cell-id (ctbl:component-selected ctbl:component))

  (setq overlay (plist-get pim-grid-marked-cells cell-id))

  (when overlay
      (progn
	(message "unmarking")
	(delete-overlay overlay)
	(setq pim-grid-marked-cells (plist-put pim-grid-marked-cells cell-id nil))))    

  (unless overlay
    (message "marking")
    (pim-query-set-mark-overlay (ctbl:cp-get-selected ctbl:component)))
  
  (let ((cnt (nth 0 pim-grid-mark-stats))
	(sum (nth 1 pim-grid-mark-stats))
	(avg (nth 2 pim-grid-mark-stats)))
    (let ((new-cnt (+ cnt (if overlay -1 1)))
	  (new-sum (+ sum (if overlay (* -1 cell-value) cell-value))))
      (setq pim-grid-mark-stats (list new-cnt new-sum (if (= 0 new-cnt) 0 (/ new-sum new-cnt))))))
  
    (pim-query-show-mark-stats))

(defun pim-query-set-mark-overlay (cell-id)
  (setq component ctbl:component)
  (let ((last (ctbl:component-selected component))
        (dest (ctbl:component-dest component))
        (model (ctbl:component-model component)))
      (ctbl:find-all-by-cell-id dest cell-id
				(lambda (begin end)
				  (let ((overlay (make-overlay begin end)))
				    (overlay-put overlay 'face '(:background "brightmagenta"))
				    (setq pim-grid-marked-cells (plist-put pim-grid-marked-cells cell-id overlay))
				    )))))

(defun pim-query-reset-marks ()
  (interactive)
  ;; todo iterate over plist and delete all overlays
  ;;  (setq overlay (plist-get pim-grid-marked-cells cell-id))
  (setq pim-grid-mark-stats (list 0 0 0))
  (cl-loop for key in pim-grid-marked-cells
	   for i from 0
	   if (oddp i)
	   do (delete-overlay key))
  (setq pim-grid-marked-cells (list))
  )

(defun pim-query-show-mark-stats()
  (interactive)
      (message (concat "Count: " (number-to-string (nth 0 pim-grid-mark-stats))
		     " | Sum: " (number-to-string (nth 1 pim-grid-mark-stats))
		     " | Avg: " (number-to-string (nth 2 pim-grid-mark-stats))))
)

;; TODO this pollutes ctbl, if I could get pim-grid to work as a major/minor mode
;; reliably this can be moved to there. 
(define-key ctbl:table-mode-map (kbd "s") 'ctbl:show-cell-in-tooltip)
(define-key ctbl:table-mode-map (kbd "?") 'describe-mode)
(define-key ctbl:table-mode-map (kbd "x") 'pim-query-toggle-mark-cell)
(define-key ctbl:table-mode-map (kbd "u") 'pim-query-reset-marks)
;; (define-key ctbl:table-mode-map (kbd "M-m") 'pim-query-show-mark-stats)
