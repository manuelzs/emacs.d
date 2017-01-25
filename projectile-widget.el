(defcustom projectile-widget-keymap-prefix (kbd "C-c w")
  "Projectile-Widget keymap prefix."
  :group 'projectile-widget
  :type 'string)

;;; Minor mode
(defvar projectile-widget-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'projectile-widget-go-to-template)
    (define-key map (kbd "s") #'projectile-widget-go-to-stylesheet)
    (define-key map (kbd "i") #'projectile-widget-go-to-view)
    (define-key map (kbd "4 t") #'projectile-widget-go-to-template-other-window)
    (define-key map (kbd "4 s") #'projectile-widget-go-to-stylesheet-other-window)
    (define-key map (kbd "4 i") #'projectile-widget-go-to-view-other-window)
    map)
  "Keymap for Projectile-Widget commands after `projectile-widget-keymap-prefix'.")

(fset 'projectile-widget-command-map projectile-widget-command-map)

(defvar projectile-widget-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-widget-keymap-prefix 'projectile-widget-command-map)
    map)
  "Keymap for Projectile-Widget mode.")

(defun projectile-widget-go-to-view ()
  "Go to view"
  (interactive)
  (find-file (concat default-directory "index.js")))

(defun projectile-widget-go-to-template ()
  "Go to template"
  (interactive)
  (find-file (concat default-directory "template.mustache")))

(defun projectile-widget-go-to-stylesheet ()
  "Go to style"
  (interactive)
  (find-file (concat default-directory "style.less")))

(defun projectile-widget-go-to-view-other-window ()
  "Go to view other window"
  (interactive)
  (find-file-other-window (concat default-directory "index.js")))

(defun projectile-widget-go-to-template-other-window ()
  "Go to template other window"
  (interactive)
  (find-file-other-window (concat default-directory "template.mustache")))

(defun projectile-widget-go-to-stylesheet-other-window ()
  "Go to style other window"
  (interactive)
  (find-file-other-window (concat default-directory "style.less")))

(define-minor-mode projectile-widget-mode
  "Minor mode to assist project navigation"
    :lighter projectile-widget-mode-line
    :keymap projectile-widget-mode-map
    :group 'projectile-bs
    :require 'projectile-bs)

(define-globalized-minor-mode projectile-widget-global-mode
  projectile-widget-mode
  projectile-widget-mode)

(provide 'projectile-widget)
