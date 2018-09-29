;; This is your private configuration file. It is loaded automatically, so feel free to add whatever you want.
;; This file will not be affected by Castlemacs updates.


;; =======
;; VISUALS


;; Modeline settings
;(use-package mode-icons
;  :ensure t
;  :config
;  (mode-icons-mode t))
(setq column-number-mode t)


;; Doom themes
(use-package doom-themes
  :ensure t)
(load-theme 'doom-one t)
;; Enable custom neotree theme
(doom-themes-neotree-config)
;; Orgmode improvements
(doom-themes-org-config)


;; Replace default font
(set-face-attribute 'default nil :font "Fira Code 14")


;; Please stop making noises
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)


;; ===========
;; PROGRAMMING


;; Clojure ecosystem
(use-package clojure-mode)
(use-package cider
  :ensure t
  :pin melpa-stable

  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (setq cider-repl-use-pretty-printing t)
  (setq cider-repl-display-help-banner nil)

  :bind (("M-r" . cider-namespace-refresh)
         ("C-c r" . cider-repl-reset)
         ("C-c ." . cider-reset-test-run-tests))
  )


;; Docker
(use-package dockerfile-mode
  :ensure t)


;; ========
;; ORG MODE

;; Store all my org files in ~/Dropbox/org.
(setq org-directory "~/Dropbox/org")

;; And all of those files should be in included agenda.
(setq org-agenda-files '("~/Dropbox/org"))
