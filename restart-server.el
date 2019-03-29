(defconst SIGHUP 1)
(defconst emacs-command "emacs-26.1")

(defun get-process-command (pid)
  (list pid (cdr (assq  'comm (process-attributes pid)))))

(defun get-all-processes ()
  (mapcar 'get-process-command (list-system-processes)))

(defun is-emacs-command-p (attrs)
  (string-match-p (regexp-quote emacs-command) (car (cdr attrs))))

(defun get-emacs-pid ()
  (car (seq-find 'is-emacs-command-p (get-all-processes))))

(defun restart-emacs-server ()
  (interactive)
  (signal-process (get-emacs-pid) SIGHUP))
