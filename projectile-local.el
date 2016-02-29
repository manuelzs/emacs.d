(defcustom projectile-bs-keymap-prefix (kbd "C-c m")
  "Projectile-BS keymap prefix."
  :group 'projectile-bs
  :type 'string)

;;; Minor mode
(defvar projectile-bs-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'projectile-bs-go-to-template)
    (define-key map (kbd "s") #'projectile-bs-go-to-stylesheet)
    (define-key map (kbd "v") #'projectile-bs-go-to-view)
    (define-key map (kbd "4 t") #'projectile-bs-go-to-template-other-window)
    (define-key map (kbd "4 s") #'projectile-bs-go-to-stylesheet-other-window)
    (define-key map (kbd "4 v") #'projectile-bs-go-to-view-other-window)
    map)
  "Keymap for Projectile-BS commands after `projectile-bs-keymap-prefix'.")

(fset 'projectile-bs-command-map projectile-bs-command-map)

(defvar projectile-bs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map projectile-bs-keymap-prefix 'projectile-bs-command-map)
    map)
  "Keymap for Projectile-BS mode.")

(defun projectile-bs-find-matching-file (file type)
  "Compute the name of the matching FILE."
  (if (member type '("template" "view" "style"))
      (let* (
             (basename (file-name-base file))
             (test-prefix
              (cond
               ((string-equal type "view") "views/")
               ((string-equal type "template") "templates/")
               ((string-equal type "style") "stylesheets/")))
             (test-suffix
              (cond
               ((string-equal type "view") ".js")
               ((string-equal type "template") ".mustache")
               ((string-equal type "style") ".less"))))
        (concat test-prefix basename test-suffix))))

(defun projectile-bs-find-by-type (file-name type)
     "Given a FILE-NAME return the matching template filename."
     (unless file-name (error "The current buffer is not visiting a file"))
     (let ((the-file (projectile-bs-find-matching-file file-name type)))
       (if the-file
           (expand-file-name (concat (file-name-directory file-name) "../" the-file))
         (error (concat "No matching" type)))))

(defun projectile-bs-go-to-view ()
  "Go to view"
  (interactive)
  (find-file
   (projectile-bs-find-by-type (buffer-file-name) "view")))

(defun projectile-bs-go-to-template ()
  "Go to template"
  (interactive)
  (find-file
   (projectile-bs-find-by-type (buffer-file-name) "template")))

(defun projectile-bs-go-to-stylesheet ()
  "Go to style"
  (interactive)
  (find-file
   (projectile-bs-find-by-type (buffer-file-name) "style")))

(defun projectile-bs-go-to-view-other-window ()
  "Go to view other window"
  (interactive)
  (find-file-other-window
   (projectile-bs-find-by-type (buffer-file-name) "view")))

(defun projectile-bs-go-to-template-other-window ()
  "Go to template other window"
  (interactive)
  (find-file-other-window
   (projectile-bs-find-by-type (buffer-file-name) "template")))

(defun projectile-bs-go-to-stylesheet-other-window ()
  "Go to style other window"
  (interactive)
  (find-file-other-window
   (projectile-bs-find-by-type (buffer-file-name) "style")))

(define-minor-mode projectile-bs-mode
  "Minor mode to assist project navigation"
    :lighter projectile-bs-mode-line
    :keymap projectile-bs-mode-map
    :group 'projectile-bs
    :require 'projectile-bs)

(define-globalized-minor-mode projectile-bs-global-mode
  projectile-bs-mode
  projectile-bs-mode)

(provide 'projectile-bs)
