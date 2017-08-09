(defcustom projectile-bs-keymap-prefix (kbd "C-c m")
  "Projectile-BS keymap prefix."
  :group 'projectile-bs
  :type 'string)

;;; Minor mode
(defvar projectile-bs-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "t") #'projectile-bs-go-to-template)
    (define-key map (kbd "p") #'projectile-bs-switch-project)
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




(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
  White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string)))


(defun projectile-bs-get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (trim-string (buffer-string))))

(defun projectile-bs-get-project-name (project)
  (let ((rc-file (concat project ".projectilerc")))
    (if (and (not (file-remote-p rc-file)) (file-exists-p rc-file))
        (concat "[" (projectile-bs-get-string-from-file rc-file) "] " project)
      (concat "" project))))


(defun projectile-bs-get-project-names (project-list)
  (mapcar 'projectile-bs-get-project-name project-list))

(defun projectile-bs-clean-project-name (project-name)
  (replace-regexp-in-string "\\[.*] " "" project-name))


(defun projectile-bs-switch-project (&optional arg)
  "Switch to a project we have visited before.
Invokes the command referenced by `projectile-switch-project-action' on switch.
With a prefix ARG invokes `projectile-commander' instead of
`projectile-switch-project-action.'"
  (interactive "P")
  (let* ((projects (projectile-relevant-known-projects))
         (project-names (projectile-bs-get-project-names projects)))
    (if projects
        (projectile-completing-read
         "Switch to project: " project-names
         :action (lambda (project-name)
                   (let ((project (projectile-bs-clean-project-name project-name)))
                     (projectile-switch-project-by-name project arg))))
      (user-error "There are no known projects"))))


(provide 'projectile-bs)
