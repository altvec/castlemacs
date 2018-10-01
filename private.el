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


;; Shackle
(use-package shackle
  :init
  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-rules '((helm-mode              :align below  :select t)
                        (helpful-mode           :align below)
                        (dired-mode             :ignore t)

                        (compilation-mode       :select t     :size 0.25)
                        ("*compilation*"        :select nil   :size 0.25)
                        ("*ag search*"          :select nil   :size 0.25)
                        ("*Flycheck errors*"    :select nil   :size 0.25)
                        ("*Warninigs*"          :select nil   :size 0.25)
                        ("*Error*"              :select nil   :size 0.25)

                        ("*Org Links*"          :select nil   :size 0.2)

                        ("*undo-tree*"          :align right)
                        (neotree-mode           :align left)
                        (magit-status-mode      :align bottom :size 0.5   :inhibit-window-quit t)
                        (magit-log-mode         :same t                   :inhibit-window-quit t)
                        (magit-commit-mode      :ignore t)
                        (magit-diff-mode        :select nil   :align left :size 0.5)
                        (git-commit-mode        :same t)
                        (vc-annotate-mode       :same t)
                        ("^\\*git-gutter.+\\*$" :regexp t     :size 15    :noselect t)))
  :config
  (shackle-mode 1))


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
         ("C-c ." . cider-reset-test-run-tests)))


;; Docker
(use-package dockerfile-mode
  :ensure t)


;; ========
;; ORG MODE

;; Store all my org files in ~/Dropbox/org.
(setq org-directory "~/Dropbox/org")

;; And all of those files should be in included agenda.
(setq org-agenda-files '("~/Dropbox/org"))

;; Put empty lines between headers
(setq org-cycle-separator-lines 1)

;; Insert closed date when TODO goes to DONE state
(setq org-log-done 'time)

;; Insert elisp code blocks
(eval-after-load 'org
  '(progn
    (add-to-list 'org-structure-template-alist '("el" "#+BEGIN_SRC emacs-lisp \n?\n#+END_SRC"))
    (define-key org-mode-map (kbd "C-'") nil)
    (global-set-key "\C-ca" 'org-agenda)))

;; Export links to HTML on save
(defun org-mode-export-links ()
  "Export links document to HTML automatically when 'links.org' is changed"
  (when (equal (buffer-file-name) "~/Dropbox/org/links.org")
    (progn
      (org-html-export-to-html)
      (message "HTML exported"))))

(add-hook 'after-save 'org-mode-export-links)


;; Org capture
(global-set-key (kbd "C-c c") 'org-capture)


;; Org babel. Used to run code in SRC blocks.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((javascript . t)
   (python . t)))
