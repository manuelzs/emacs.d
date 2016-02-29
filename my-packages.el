; my-packages.el
; defvar is the correct way to declare global variables
; you might see setq as well, but setq is supposed to be use just to set variables,
; not create them.

(defvar required-packages
  '(
    auto-complete
    dash
    f
    flx
    flx-ido
    flymake
    flymake-easy
    flymake-jshint
    flymake-json
    flymake-python-pyflakes
    gitty
    go-mode
    go-autocomplete
    guru-mode
    ido-vertical-mode
    jedi
    magit
    projectile
    rainbow-delimiters
    request
    s
    websocket
    yasnippet
    ) "a list of packages to ensure are installed at launch.")


; my-packages.el
(require 'cl)

; method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

; if not all packages are installed, check one by one and install the missing ones.
(unless (packages-installed-p)
  ; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))
