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
(set-face-attribute 'default nil :font "Inconsolata LGC 14")
(setq initial-frame-alist '((width . 135) (height . 55)))
(setq-default line-spacing 0)
(set-face-background 'show-paren-match "grey84")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(show-paren-mode)


;; Please stop making noises
(defun my-bell-function ())
(setq ring-bell-function 'my-bell-function)
(setq visible-bell nil)


;; Splitting windows
(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "s-T") 'vsplit-last-buffer) ;; vertically split window
(global-set-key (kbd "s-t") 'hsplit-last-buffer) ;; horizontally split window


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


;; Store all my org files in ~/Dropbox/org.
(setq org-directory "~/Google Drive/org")

;; And all of those files should be in included agenda.
(setq org-agenda-files '("~/Google Drive/org"))

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
  (when (equal (buffer-file-name) "~/Google Drive/org/links.org")
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
