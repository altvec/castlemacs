;; For Emacs 27 and above
(setq package-enable-at-startup nil)

;; Test C stack overflow issue
(setq max-specpdl-size 100000
      max-lisp-eval-depth 20000
      gc-cons-threshold 16777216
      gc-cons-percentage 0.1
      file-name-handler-alist nil)
