(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Set bg color to black
(set-background-color "black")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:inherit diff-changed :background "color-22"))))
 '(diff-removed ((t (:inherit diff-changed :background "color-88"))))
 '(ediff-current-diff-A ((t (:background "#1f0000"))))
 '(ediff-current-diff-B ((t (:background "#002f00"))))
 '(ediff-even-diff-A ((t (:background "color-233" :foreground "color-252"))))
 '(ediff-even-diff-B ((t (:background "color-233" :foreground "White"))))
 '(ediff-fine-diff-A ((t (:background "#995555"))))
 '(ediff-fine-diff-B ((t (:background "#449944"))))
 '(ediff-odd-diff-A ((t (:background "color-234" :foreground "White"))))
 '(ediff-odd-diff-B ((t (:background "color-234" :foreground "color-252"))))
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
