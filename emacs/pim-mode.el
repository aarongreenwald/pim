(setq pim-mode-map (make-sparse-keymap))
(defvar pim-home-screen-items (list ))

(define-minor-mode pim-mode
  "For use on readonly pim buffers. All pim-grid buffers should have this mode as well (pim-grid automatically enables this along with it)"
  :init-value nil
  :lighter " pim"
  :keymap pim-mode-map
  :after-hook (progn
		(display-line-numbers-mode 0)
		(read-only-mode 1)))

;; TODO a separate pim-editable-mode for editable buffers, which has a shortcut to enable/disable pim-mode (to enter readonly and enable shortcuts)
;; TODO all pim buffers should be temporary (reused on next query) with a shortcut to "fix" and take out of the reusable buffer list. Either way q closes, b buries. 

(defun pim--add-to-home (func keybinding title &optional section description)
  "Add an item to the PIM home screen"
  ;; TODO can the func's documentation comment automatically be the description?
  ;; Maybe some of the other fields can also be parsed from there?
  (add-to-list 'pim-home-screen-items (list title keybinding func section description))
  (define-key pim-mode-map keybinding func))

(defun pim-show-home ()
  "Open the PIM home screen"
  (interactive)
    (get-buffer-create "pim-home")
    (pop-to-buffer "pim-home")
    (read-only-mode 0)
    (erase-buffer)
    (dolist (elt pim-home-screen-items)
      (let ((title (nth 0 elt))
	    (kbd (nth 1 elt))
	    (func (nth 2 elt))
	    (section (nth 3 elt)) ;; todo list should be organized by sections
	    (description (nth 4 elt))
	    (map (make-sparse-keymap))
	    )
	(define-key map (kbd "RET") func) ;; is this the most efficient way to do this? It's really not that important anyway
	(insert (propertize title 'face 'button 'keymap map ))
	
	(insert (concat "  " (propertize (key-description kbd) 'face '(:foreground "orange"))))
	(insert "\n\n")
	
	;; TODO insert description, possibly hidden but searchable
	))
    
    (pim-mode 1))

(global-set-key (kbd "<f9>") 'pim-show-home)
