;;; emacs-nxs-icons-ibuffer.el --- File type icons for ibuffer  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Adds a custom icon column to ibuffer that shows file type or
;; mode-based icons for each buffer.

;;; Code:

(use-package emacs-nxs-icons-ibuffer
  :if (memq 'ibuffer emacs-nxs-icon-modules)
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/ibuffer-icon-for-buffer (buf)
    "Return an icon for BUF: file-extension emoji if visiting a file,
otherwise mode-based emoji."
    (with-current-buffer buf
      (if-let* ((file (buffer-file-name)))
          ;; File-based icons
          (let* ((ext (file-name-extension file))
                 (icon (and ext (emacs-nxs/file-icon (downcase ext)))))
            (or icon (emacs-nxs/file-icon "diredfile")))
        ;; Mode-based icons for non-file buffers
        (cond
         ((derived-mode-p 'dired-mode)  (emacs-nxs/file-icon "direddir"))
         ((derived-mode-p 'eshell-mode) (emacs-nxs/file-icon "terminal"))
         ((derived-mode-p 'org-mode)    (emacs-nxs/file-icon "terminal"))
         ((derived-mode-p 'shell-mode)  (emacs-nxs/file-icon "terminal"))
         ((derived-mode-p 'term-mode)   (emacs-nxs/file-icon "terminal"))
         ((derived-mode-p 'help-mode)   (emacs-nxs/file-icon "info"))
         ((derived-mode-p 'erc-mode)    (emacs-nxs/file-icon "hash"))
         ((derived-mode-p 'rcirc-mode)  (emacs-nxs/file-icon "hash"))
         ((derived-mode-p 'gnus-mode)   (emacs-nxs/file-icon "mail"))
         ((derived-mode-p 'newsticker-treeview-mode)   (emacs-nxs/file-icon "news"))
         (t                             (emacs-nxs/file-icon "wranch"))))))

  (define-ibuffer-column icon
    (:name " ")
    (emacs-nxs/ibuffer-icon-for-buffer buffer))

  ;; Update ibuffer formats
  (setq ibuffer-formats
        '((mark modified read-only locked " "
                (icon 2 2 :left) " "
                (name 30 30 :left :elide) " "
                (size 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

(provide 'emacs-nxs-icons-ibuffer)
;;; emacs-nxs-icons-ibuffer.el ends here
