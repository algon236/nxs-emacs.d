;;; emacs-nxs-dired.el --- Modern Dired with Nerd Icons -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, modern Dired setup for Emacs NXS.  It adds Nerd Font icons,
;; foldable directory trees, sensible highlighting and a few convenient keys.
;; The standard Dired commands remain unchanged.

;;; Code:

(require 'dired)
(require 'dired-x)

(defgroup emacs-nxs-dired nil
  "Modern Dired additions for Emacs NXS."
  :group 'emacs-nxs
  :prefix "emacs-nxs-dired-")

(defcustom emacs-nxs-dired-show-icons t
  "Show Nerd Font file icons in local Dired buffers."
  :type 'boolean
  :group 'emacs-nxs-dired)

(defcustom emacs-nxs-dired-hide-details t
  "Hide owner, group, permissions and other details initially."
  :type 'boolean
  :group 'emacs-nxs-dired)

(defun emacs-nxs-dired--setup ()
  "Apply the Emacs NXS presentation settings to the current Dired buffer."
  (hl-line-mode 1)
  (setq-local truncate-lines t)
  (setq-local line-spacing 0.08)
  (when emacs-nxs-dired-hide-details
    (dired-hide-details-mode 1)))

(defun emacs-nxs-dired-toggle-dotfiles ()
  "Toggle hidden files in the current Dired buffer."
  (interactive)
  (dired-omit-mode 'toggle)
  (revert-buffer))

(defun emacs-nxs-dired-open-externally ()
  "Open the file at point with the macOS default application."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (unless (file-exists-p file)
      (user-error "Filen findes ikke: %s" file))
    (start-process "emacs-nxs-open" nil "open" file)))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-dired
  :ensure t
  :after (dired nerd-icons)
  :hook (dired-mode . emacs-nxs-dired--enable-icons))

(defun emacs-nxs-dired--enable-icons ()
  "Enable Nerd Icons in local Dired buffers when configured."
  (when (and emacs-nxs-dired-show-icons
             (not (file-remote-p default-directory)))
    (nerd-icons-dired-mode 1)))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("<backtab>" . dired-subtree-cycle)))

(add-hook 'dired-mode-hook #'emacs-nxs-dired--setup)

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "." #'emacs-nxs-dired-toggle-dotfiles)
  (keymap-set dired-mode-map "O" #'emacs-nxs-dired-open-externally)
  (keymap-set dired-mode-map "(" #'dired-hide-details-mode)
  (keymap-set dired-mode-map "C-c C-r" #'dired-do-rename-regexp)
  (keymap-set dired-mode-map "C-c C-c" #'wdired-change-to-wdired-mode))

(provide 'emacs-nxs-dired)
;;; emacs-nxs-dired.el ends here
