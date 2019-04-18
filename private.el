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

(load-theme 'dracula)

;; Modeline settings
(setq column-number-mode t)

;; Replace default font
(set-face-attribute 'default nil :font "Fira Code 15")
(setq initial-frame-alist '((width . 135) (height . 55)))
(setq-default line-spacing 0)

;; Please stop making noises
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)


(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (setq sp-show-pair-delay 0)
  ;; no '' pair in emacs-lisp-mode
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'markdown-mode "`"   nil :actions '(wrap insert))  ;; only use ` for wrap and auto insertion in markdown-mode
  (sp-local-tag 'markdown-mode "s" "```scheme" "```")
  (define-key smartparens-mode-map (kbd "C-s-<right>") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-s-<left>") 'sp-forward-barf-sexp))

;; ===========
;; PROGRAMMING


;; Newline at end of file
(setq require-final-newline t)

;; Clojure ecosystem
(use-package clojure-mode)
(use-package cider
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
(use-package dockerfile-mode)

;; Javascript
(use-package js2-mode
  :interpreter (("node" . js2-mode))
  :bind (:map js2-mode-map ("C-c C-p" . js2-print-json-path))
  :mode "\\.\\(js\\|json\\)$"
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (setq js2-basic-offset 2
        js2-indent-level 2
        js2-highlight-level 3
        js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil))
(use-package js2-refactor
  :defer t
  :diminish js2-refactor-mode
  :commands js2-refactor-mode
  :init
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m"))
(use-package xref-js2)


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

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(eval-after-load 'markdown-mode
  `(define-key markdown-mode-map (kbd "C-s-<down>") 'markdown-narrow-to-subtree))

(eval-after-load 'markdown-mode
  `(define-key markdown-mode-map (kbd "C-s-<up>") 'widen))

(require 'markdown-mode)
(eval-after-load 'markdown-mode
  `(define-key markdown-mode-map (kbd "s-O") (lambda ()
                                               (interactive)
                                               (markdown-kill-ring-save)
                                               (let ((oldbuf (current-buffer)))
                                                 (save-current-buffer
                                                   (set-buffer "*markdown-output*")
                                                   (with-no-warnings (mark-whole-buffer))
                                                   (simpleclip-copy (point-min) (point-max)))))))


;; ========
;; ORG MODE

;; Nicer bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region
                                         (match-beginning 1)
                                         (match-end 1) "•"))))))
(use-package org-bullets)
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
   (shell . t)
   (http . t)
   (clojurescript . t)))
