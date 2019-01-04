;; This file is used for the built-in customization UI in Emacs (M-x customize)
;; This file will not be affected by Castlemacs updates

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ob-clojurescript ob-go ob-http org-bullets pipenv js2-refactor xref-js2 js2-mode ob-typescript ob-javascript htmlize shackle doom-themes dockerfile-mode cider mode-icons emmet-mode web-mode markdown-mode haml-mode yaml-mode define-word powerthesaurus flyspell-correct-popup flyspell-correct company shell-pop git-gutter magit counsel-projectile ivy-rich avy flx smex counsel swiper ivy projectile multiple-cursors visual-regexp move-text expand-region which-key vi-tilde-fringe neotree rich-minority smartparens all-the-icons undo-tree simpleclip exec-path-from-shell use-package)))
 '(shell-pop-shell-type
   (quote
    ("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell)))))
 '(shell-pop-universal-key "s-="))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro" :height 1.5 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#383a42" :font "Source Sans Pro")))))
