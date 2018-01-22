(setq afrey-clojure-packages
      '(clojure-mode
        inf-clojure
        smartparens
        company
        clj-refactor
        org))

(defmacro comment (&rest body)
  "Comment out one or more s-expressions."
  nil)


(defun afrey-clojure-double-newlines ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ")\n\\(\n\\)(" nil t)
      (replace-match "\n\n" nil nil nil 1))))


(defun afrey-clojure-single-newline ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ")\n\\(\n\n\\)(" nil t)
      (replace-match "\n" nil nil nil 1))))

(defun afrey-clojure-unalign ()
  "replace all whitespace in the region with single spaces"
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (while (re-search-forward "[^\s\n]\\(\s+\\)" nil t)
        (replace-match " " nil nil nil 1)))))

(defvar format-clojure-community? t "If true, will format in the community mode before saving clojure files")

(defun toggle-community-clojure-indent ()
  (interactive)
  (if format-clojure-community?
      (progn
        (message "format-clojure-community? set to false")
        (setq format-clojure-community? nil))
    (progn
      (message "format-clojure-community? set to true")
      (setq format-clojure-community? t))))

(defun afrey-clojure-format-community ()
  (when format-clojure-community?
    (let (value)
      (dotimes (number 2 value)
        (save-excursion
          (let ((clojure-defun-style-default-indent nil))
            (indent-region (point-min) (point-max)))
          (afrey-clojure-unalign)
          ;;(afrey-clojure-single-newline)
          (delete-trailing-whitespace))))))

(defun clojure-format-afrey ()
  (save-excursion ;; do my mode
    (let ((clojure-defun-style-default-indent t))
      (indent-region (point-min) (point-max))
      (clojure-align (point-min) (point-max))
      ;;(afrey-clojure-double-newlines)
      (set-buffer-modified-p nil))))

(defun interactive-clojure-format (arg)
  (interactive "P")
  (if (equal arg '(4))
      (afrey-clojure-format-community)
    (clojure-format-afrey)))

(global-set-key (kbd "C-c C-t") 'interactive-clojure-format)

(comment
 (magit-with-toplevel
   (magit-unstaged-files)
   (get-file-buffer ".spacemacs")))

(defun add-clojure-save-hooks ()
  (add-hook 'before-save-hook
            'afrey-clojure-format-community
            nil 'local)
  
  (add-hook 'after-save-hook
            'clojure-format-afrey
            nil 'local)
  ;; https://github.com/clojure-emacs/clojure-mode/#indentation-options
  ;; http://cider.readthedocs.io/en/latest/indent_spec/
  )


(defun afrey-clojure/init-clojure-mode ()
  (use-package clojure-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.boot\\'" . clojure-mode))
      (add-to-list 'auto-mode-alist '("\\.repl\\'" . clojure-mode))
      ;; This regexp matches shebang expressions like `#!/usr/bin/env boot'
      (add-to-list 'magic-mode-alist '("#!.*boot\\s-*$" . clojure-mode))

      (load "~/projects/unrepl.el/unrepl-writer.el")
      (load "~/projects/unrepl.el/unrepl.el")
      (setq debug-on-error t))
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
        (concat 0)
        (some-fn 0)
        (conj 1)
        (cons 1))

      (add-to-list 'clojure-align-cond-forms "assoc")
      (add-to-list 'clojure-align-cond-forms "given")
      (add-to-list 'clojure-align-cond-forms "set-env!")
      (add-to-list 'clojure-align-cond-forms "s/cat")


      (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
        (spacemacs/set-leader-keys-for-major-mode m
          "fl" 'clojure-align))

      (dolist (m '(clojure-mode-hook clojurescript-mode-hook))
        (add-hook m #'paredit-mode))

      (add-hook 'clojure-mode-hook #'add-clojure-save-hooks)

      ;;(define-key clojure-mode-map "\C-c\C-k" 'clojure/reload-current-clj-ns)
      (define-key clojure-mode-map "\C-c\C-k" 'inf-clojure-eval-buffer)))

  (defun clojure/init-clj-refactor ()
    (use-package clj-refactor
      :defer t
      :init
      (add-hook 'clojure-mode-hook 'clj-refactor-mode)
      :config
      (progn
        (cljr-add-keybindings-with-prefix "C-c C-f")

        (setq clj-refactor--key-binding-prefixes
              '(("mr" . "refactor")
                ("mra" . "add")
                ("mrc" . "cycle/clean")
                ("mrd" . "destructure")
                ("mre" . "extract/expand")
                ("mrf" . "find/function")
                ("mrh" . "hotload")
                ("mri" . "introduce/inline")
                ("mrm" . "move")
                ("mrp" . "project/promote")
                ("mrr" . "remove/rename/replace")
                ("mrs" . "show/sort/stop")
                ("mrt" . "thread")
                ("mru" . "unwind/update")))
        (dolist (m '(clojure-mode
                     clojurec-mode
                     clojurescript-mode
                     clojurex-mode
                     cider-repl-mode
                     cider-clojure-interaction-mode
                     inf-clojure-mode))
          (mapc (lambda (x) (spacemacs/declare-prefix-for-mode
                             m (car x) (cdr x)))
                clj-refactor--key-binding-prefixes)
          (dolist (r cljr--all-helpers)
            (let* ((binding (car r))
                   (func (car (cdr r))))
              (when (not (string-prefix-p "hydra" (symbol-name func)))
                (spacemacs/set-leader-keys-for-major-mode m
                  (concat "r" binding) func)))))))))

(defun afrey/inf-clojure-project-repl-name ()
  (let ((repl-name-file (concat (inf-clojure-project-root) ".inf-clojure-repl-name")))
    (if (file-readable-p repl-name-file)
        (with-temp-buffer
          (insert-file-contents repl-name-file)
          (buffer-string)))))

(defun afrey/inf-clojure-project-repl-buffer-name ()
  (if-let ((repl-name (afrey/inf-clojure-project-repl-name)))
      (concat "*" repl-name "*")))

;; (defun afrey/inf-clojure-repl-switch ()
;;   (if-let ((specified-repl-name (afrey/inf-clojure-project-repl-name)))
;;       (setq inf-clojure-buffer (concat "*" specified-repl-name "*"))))


(defun afrey/inf-clojure-set-ns (prompt-for-ns)
  (interactive "P")
  (let ((ns (clojure-find-ns)))
    (print (format (inf-clojure-set-ns-form) ns))
    (comint-simple-send (inf-clojure-proc)
                       (format (inf-clojure-set-ns-form) ns))))

(defun afrey-clojure/init-inf-clojure ()
  (use-package inf-clojure
    :defer t
    :config
    (progn
      (add-hook 'inf-clojure-mode-hook #'paredit-mode)
      (add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
      (add-hook 'clojurescript-mode-hook 'inf-clojure-minor-mode))))

(defun generate-inf-clojure-proc-name ()
  (or (afrey/inf-clojure-project-repl-name)
      "inf-clojure"))

;;;###autoload

;;  "Returns the current inferior Clojure process.
;; See variable `inf-clojure-buffer'."
(comment
 (defun inf-clojure-proc ()
   (let ((proc (get-buffer-process (if (derived-mode-p 'inf-clojure-mode)
                                       (current-buffer)
                                     (or (afrey/inf-clojure-project-repl-buffer-name)
                                         inf-clojure-buffer)))))
     (or proc
         (error "No Clojure subprocess; see variable!! `inf-clojure-buffer'")))))


(defun clojure/post-init-eldoc ()
  (add-hook 'clojure-mode-hook 'eldoc-mode)
  (add-hook 'inf-clojure-mode-hook 'eldoc-mode))


(defun afrey-clojure/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(clojure . t))
    (setq org-babel-clojure-backend 'cider)))
