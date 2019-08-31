(setq backup-directory-alist `(("." . "~/.emacs_backups")))

;; Restart emacs server
(load-file "~/.emacs.d/restart-server.el")

;; Add melpa repo
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun init-env-path ()
  (interactive)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH"))

(when (memq window-system '(mac ns)) (init-env-path))

(define-coding-system-alias 'UTF-8 'utf-8)

(put 'downcase-region 'disabled nil)

;; Adding IDO mode
(add-to-list 'load-path "~/.emacs.d/elpa/ido-vertical-mode-20180618.2101/")
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(autoload 'idomenu "idomenu" nil t)
(global-set-key (kbd "C-c C-j") 'idomenu)

(add-to-list 'load-path "~/.emacs.d/elpa/flx-ido-20180117.1519/")
(require 'flx-ido)
(flx-ido-mode 1)

(load-file ' "~/.emacs.d/move-lines.el")
(require 'move-lines)
(move-lines-binding)

;; Browse kill ring
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; Show column
(setq column-number-mode t)

;; Scroll one line - dont jump
(setq scroll-step 1)

;; Clean whitespace on save
;;(add-hook 'before-save-hook 'whitespace-cleanup)
;; This might add tabs instead of spaces

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; Set fill column
(setq-default fill-column 80)

;; Enable mouse
(xterm-mouse-mode 1)

;; Trackpad scroll
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Copy region to clipboard
(defun copy-region-to-clipboard ()
  (interactive)
  (if (eq system-type 'darwin)
      (shell-command-on-region
       (region-beginning) (region-end)
       "pbcopy && echo 'Region copied to clipboard'")
    (message "Clipboard copy not supported")))

(global-set-key (kbd "C-c C-k") 'copy-region-to-clipboard)

;; Projectile
(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Helm
;; (require 'helm-config)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-go-gocode-bin "/Users/manuel/go/bin/gocode")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212121" "#CC5542" "#6aaf50" "#7d7c61" "#5180b3" "#DC8CC3" "#9b55c3" "#bdbdb3"])
 '(browse-url-browser-function (quote browse-url-default-macosx-browser))
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(default-input-method "TeX")
 '(ediff-split-window-function (quote split-window-horizontally))
 '(elm-format-on-save t)
 '(emms-player-vlc-command-name "/Applications/VLC.app/Contents/MacOS/VLC")
 '(epa-pinentry-mode (quote loopback))
 '(fci-rule-color "#2e2e2e")
 '(flycheck-python-flake8-executable "/Users/manuel/.virtualenvs/melt/bin/flake8")
 '(flycheck-python-mypy-executable "/Users/manuel/.virtualenvs/melt/bin/mypy")
 '(flycheck-python-pycompile-executable "/Users/manuel/.virtualenvs/melt/bin/python3")
 '(godef-command "/Users/manuel/go/bin/godef")
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(js2-basic-offset 4)
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-global-externs
   (list "window" "define" "require" "module" "exports" "process" "Buffer" "__dirname" "Parse" "sessionStorage" "localStorage" "describe" "it" "FileReader" "analytics" "setTimeout" "btoa" "atob" "FormData" "xdescribe" "xit" "context" "beforeEach"))
 '(mapscii-executable-path
   "/Users/manuel/Stuff/Dev/devel/emacs/mapscii/bin/mapscii.sh")
 '(package-selected-packages
   (quote
    (blacken xterm-color py-isort idomenu browse-kill-ring expand-region helm goto-char-preview doom-themes dracula-theme editorconfig prettier-js graphql-mode ac-js2 ack-and-a-half afternoon-theme alect-themes ample-theme ample-zen-theme async auto-complete auto-package-update butler clj-refactor clojure-mode clojure-snippets coffee-mode color-theme color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow color-theme-solarized company company-go concurrent ctable deferred direx docker ein elm-mode emms emojify exec-path-from-shell f feature-mode fill-column-indicator flx-ido flycheck flycheck-elm flycheck-flow flycheck-mypy flycheck-package flycheck-rust fsm gist git gitty go-autocomplete go-direx go-dlv go-mode go-projectile go-stacktracer gotest govet groovy-mode guru-mode haskell-mode ido-vertical-mode jedi jedi ido-vertical-mode jenkins js2-mode json-mode karma lsp-mode lsp-rust magit-gh-pulls markdown-mode neotree nose pivotal-tracker popwin powerline projectile protobuf-mode pycoverage rainbow-delimiters regex-tool request request-deferred restclient rich-minority rust-mode sass-mode smartparens solarized-theme soundklaus string-utils tabulated-list tern tern-auto-complete terraform-mode tide tldr twittering-mode typescript-mode use-package uuid uuidgen web-beautify web-mode webpack-server websocket wsd-mode)))
 '(powerline-utf-8-separator-left 9622)
 '(powerline-utf-8-separator-right 9623)
 '(projectile-keymap-prefix (kbd "C-c p"))
 '(projectile-switch-project-action (quote projectile-vc))
 '(projectile-use-git-grep t)
 '(python-shell-interpreter "ipython")
 '(python-shell-interpreter-args "--simple-prompt")
 '(require-final-newline t)
 '(send-mail-function (quote mailclient-send-it))
 '(soundklaus-access-token (get-string-from-file "~/.soundcloud_token"))
 '(split-height-threshold 100)
 '(split-width-threshold 190)
 '(tab-width 4)
 '(vc-annotate-background "#3b3b3b")
 '(vc-annotate-color-map
   (quote
    ((20 . "#dd5542")
     (40 . "#CC5542")
     (60 . "#fb8512")
     (80 . "#baba36")
     (100 . "#bdbc61")
     (120 . "#7d7c61")
     (140 . "#6abd50")
     (160 . "#6aaf50")
     (180 . "#6aa350")
     (200 . "#6a9550")
     (220 . "#6a8550")
     (240 . "#6a7550")
     (260 . "#9b55c3")
     (280 . "#6CA0A3")
     (300 . "#528fd1")
     (320 . "#5180b3")
     (340 . "#6380b3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(web-mode-code-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(webpack-server-host "0.0.0.0"))


(load-file "~/.emacs.d/language-init.el")
(load-file "~/.emacs.d/extra-init.el")
(load-file "~/.emacs.d/theme-init.el")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88"))))
 '(dired-marked ((t (:foreground "color-204" :weight bold))))
 '(ediff-current-diff-A ((t (:background "#1f0000"))))
 '(ediff-current-diff-B ((t (:background "#002f00"))))
 '(ediff-even-diff-A ((t (:background "color-233" :foreground "color-252"))))
 '(ediff-even-diff-B ((t (:background "color-233" :foreground "White"))))
 '(ediff-fine-diff-A ((t (:background "#995555"))))
 '(ediff-fine-diff-B ((t (:background "#449944"))))
 '(ediff-odd-diff-A ((t (:background "color-234" :foreground "White"))))
 '(ediff-odd-diff-B ((t (:background "color-234" :foreground "color-252"))))
 '(flycheck-error ((t (:foreground "#ff6655" :underline (:color "#ff6655" :style wave)))))
 '(font-lock-comment-face ((t (:foreground "#727272"))))
 '(font-lock-doc-face ((t (:foreground "color-28"))))
 '(font-lock-function-name-face ((t (:foreground "color-33"))))
 '(font-lock-keyword-face ((t (:foreground "brightmagenta"))))
 '(font-lock-string-face ((t (:foreground "color-128"))))
 '(font-lock-type-face ((t (:foreground "color-214"))))
 '(font-lock-variable-name-face ((t (:foreground "color-39"))))
 '(highlight ((t (:background "color-235"))))
 '(magit-diff-added ((t (:background "#222222" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "#111111" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "color-234" :foreground "color-245"))))
 '(magit-diff-hunk-heading ((t (:background "color-235" :foreground "color-251"))))
 '(magit-diff-removed ((t (:background "#222222" :foreground "#cc2222"))))
 '(magit-diff-removed-highlight ((t (:background "#111111" :foreground "#cc2222"))))
 '(magit-section-heading ((t (:foreground "color-208" :weight bold))))
 '(magit-section-highlight ((t (:background "color-234"))))
 '(menu ((t (:background "color-235"))))
 '(minibuffer-prompt ((t (:foreground "brightblue"))))
 '(mode-line ((t (:background "color-234" :foreground "color-249" :box (:line-width -1 :style released-button)))))
 '(mode-line-buffer-id ((t (:foreground "color-197"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "color-233" :foreground "color-238" :box (:line-width -1 :color "grey75") :weight light))))
 '(region ((t (:background "color-234"))))
 '(rst-level-1 ((t (:background "color-234"))))
 '(rst-level-2 ((t (:background "color-234"))))
 '(rst-level-3 ((t (:background "color-234"))))
 '(secondary-selection ((t (:background "color-130"))))
 '(smerge-lower ((t (:background "color-232"))))
 '(smerge-markers ((t (:background "color-234"))))
 '(smerge-mine ((t (:background "color-233"))) t)
 '(smerge-other ((t (:background "color-233"))) t)
 '(smerge-refined-added ((t (:inherit smerge-refined-change :background "color-235"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "color-52"))))
 '(smerge-upper ((t (:background "color-233"))))
 '(web-mode-html-attr-name-face ((t (:foreground "color-208"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "color-243"))))
 '(web-mode-html-tag-face ((t (:foreground "brightred"))))
 '(widget-field ((t (:background "color-235" :foreground "color-246")))))
