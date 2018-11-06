;; This is your private configuration file. It is loaded automatically, so feel free to add whatever you want.
;; This file will not be affected by Castlemacs updates.


;; User info
(setq user-full-name "Sergey Kalistratov")
(setq user-mail-address "kalistratov@fastmail.com")

;; Fix dired errors 'ls does not support --dired'
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))


;; =======
;; VISUALS


;; Modeline settings
(setq column-number-mode t)


;; Replace default font
(set-face-attribute 'default nil :font "Monaco 14")


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
         ("C-c ." . cider-reset-test-run-tests)))


;; Docker
(use-package dockerfile-mode
  :ensure t)


;; Javascript
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(use-package js2-refactor)
(use-package xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js-mode-map (kbd "C-k") #'js2r-kill)

(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))


;; Python
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))


;; ========
;; ORG MODE

;; Nicer bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1)
                                         (match-end 1) "•"))))))
(use-package org-bullets
    :ensure t)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                             ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                             ((x-list-fonts "Verdana")         '(:font "Verdana"))
                             ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                             (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
       (base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline ,@variable-tuple))))
                          `(org-level-7 ((t (,@headline ,@variable-tuple))))
                          `(org-level-6 ((t (,@headline ,@variable-tuple))))
                          `(org-level-5 ((t (,@headline ,@variable-tuple))))
                          `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                          `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                          `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
                          `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
                          `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))



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
   (python . t)
   (shell . t)))
