;;; packages.el --- elfeed Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq elfeed-packages
      '(elfeed
        elfeed-goodies
        elfeed-org
        elfeed-web
        ))

(defun elfeed/init-elfeed ()
  (use-package elfeed
    :defer t
    :init (spacemacs/set-leader-keys "af" 'elfeed)
    :config
    (progn
      (setq elfeed-db-directory "~/.dropbox/.elfeed")
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :eval-after-load elfeed-search
        :bindings
        "c"  'elfeed-db-compact
        "gr" 'elfeed-update
        "gR" 'elfeed-search-update--force
        "gu" 'elfeed-unjam
        "o"  'elfeed-load-opml
        "q"  'quit-window
        "w"  'elfeed-web-start
        "W"  'elfeed-web-stop)
      (evilified-state-evilify-map elfeed-show-mode-map
        :mode elfeed-show-mode
        :eval-after-load elfeed-show
        :bindings
        "q" 'quit-window
        (kbd "C-j") 'elfeed-show-next
        (kbd "C-k") 'elfeed-show-prev))))

(defun elfeed/init-elfeed-goodies ()
  (use-package elfeed-goodies
    :commands elfeed-goodies/setup
    :init (spacemacs|use-package-add-hook elfeed
            :post-config (progn
                           (elfeed-goodies/setup)
                           (evil-define-key 'evilified elfeed-show-mode-map "o" 'elfeed-goodies/show-ace-link)))))

(defun elfeed/init-elfeed-org ()
  (use-package elfeed-org
    :defer t
    :if (boundp 'rmh-elfeed-org-files)
    :init (spacemacs|use-package-add-hook elfeed
            :pre-config (elfeed-org))))

(defun elfeed/init-elfeed-web ()
  (use-package elfeed-web
    :defer t
    :commands elfeed-web-stop
    :init (when elfeed-enable-web-interface
            ;; TODO check if the port is already in use
            ;; hack to force elfeed feature to be required before elfeed-search
            (require 'elfeed)
            (elfeed-web-start))))

(defun xah-toggle-read-novel-mode ()
  "Setup current buffer to be suitable for reading long novel/article text.

• Line wrap at word boundaries.
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2017-02-27"
  (interactive)
  ;; TODO make this change on having a prefix argument
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins nil 0 9)
        (variable-pitch-mode 1)
        (setq line-spacing 0.5)
        (setq word-wrap t)
        (put this-command 'state-on-p t))
    (progn
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (setq line-spacing nil)
      (setq word-wrap nil)
      (put this-command 'state-on-p nil)))
  (redraw-frame (selected-frame)))
