(global-set-key (kbd "C-x <f2>") 'reload-pim)

(use-package plz)
(use-package ctable)
(use-package csv-mode)
(require 'plz)
(require 'ctable)
(require 'cl)
(require 'color)
(require 'csv-mode)

(defun load-pim-file (name)
  (load-file (concat basedir "../pim/emacs/" name)))

(defun reload-pim ()
  (interactive)
  (save-some-buffers)
  (load-pim-file "./main.el"))

(add-to-list 'auto-mode-alist ' ("../notes/" . markdown-mode))

(load-pim-file "./dir-mode.el")
(load-pim-file "./dir-mode.csv.el")
(load-pim-file "./grid-mode.el")
(load-pim-file "./drive.el")
(load-pim-file "./utils.el")
(load-pim-file "./sql.el")

