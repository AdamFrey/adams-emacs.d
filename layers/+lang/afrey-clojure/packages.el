(setq afrey-clojure-packages
      '(clojure-mode
        inf-clojure
        smartparens
        company
        clj-refactor
        org))

(defun afrey-clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode)))
    :config
    (progn
      ;; Indentation
      (setq clojure-defun-style-default-indent t)
      (define-clojure-indent
        (or 0)
        (and 0)
        (= 0)
        (not= 0)
        (+ 0)
        (- 0)
        (* 0)
        (/ 0)
        (concat 0))

      (add-to-list 'clojure-align-cond-forms "assoc")
      (add-to-list 'clojure-align-cond-forms "given")

      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (spacemacs/set-leader-keys-for-major-mode m
          "fl" 'clojure-align))

      (dolist (m '(clojure-mode-hook clojurescript-mode-hook))
        (add-hook m #'paredit-mode)))

    ;;(define-key clojure-mode-map "\C-c\C-k" 'clojure/reload-current-clj-ns)
    (define-key clojure-mode-map "\C-c\C-k" 'inf-clojure-eval-buffer)))

(defun afrey-clojure/init-inf-clojure ()
  (setq inf-clojure-generic-cmd "planck")
  (add-hook 'inf-clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
  (add-hook 'clojurescript-mode-hook 'inf-clojure-minor-mode))

(defun clojure/post-init-eldoc ()
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'inf-clojure-mode-hook 'eldoc-mode))

(defun afrey-clojure/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(clojure . t))
    (setq org-babel-clojure-backend 'cider)))
