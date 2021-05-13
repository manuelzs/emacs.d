;; Copy path from env
(exec-path-from-shell-initialize)
(setenv "PATH"
  (concat
   "/Users/manuel/.virtualenvs/emacs/bin" ":"
   (getenv "PATH")
   ))
