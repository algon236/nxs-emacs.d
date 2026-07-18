;;; emacs-nxs-icons-dired.el --- File type icons for Dired buffers  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Adds file type icons and executable/directory suffixes as
;; overlays to filenames in Dired buffers.

;;; Code:

(use-package emacs-nxs-icons-dired
  :if (memq 'dired emacs-nxs-icon-modules)
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/dired-icons-icon-for-file (file)
    (if (file-directory-p file)
        (emacs-nxs/file-icon "direddir")
      (let* ((ext (file-name-extension file))
             (icon (and ext (emacs-nxs/file-icon (downcase ext)))))
        (if (and icon (not (string-empty-p icon)))
            icon
          (emacs-nxs/file-icon "diredfile")))))

  (defun emacs-nxs/dired-icons-icons-regexp ()
    "Return a regexp that matches any icon we use."
    (let ((icons (mapcar (lambda (row) (emacs-nxs/file-icon (car row)))
                         emacs-nxs/file-icons)))
      (concat "^\\(" (regexp-opt (cons "📁" icons)) "\\) ")))

  (defun emacs-nxs/dired-icons-add-icons ()
    "Add icons and suffixes as overlays to filenames in Dired buffer."
    (when (and (derived-mode-p 'dired-mode)
               (not (file-remote-p default-directory))) ; skip icons on TRAMP — file-directory-p/file-executable-p per file kills performance
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max) 'emacs-nxs-dired-icon-overlay t)

        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (condition-case nil
                (when-let* ((file (dired-get-filename nil t)))
                  (dired-move-to-filename)
                  (let* ((beg (point))
                         (end (line-end-position))
                         (icon (emacs-nxs/dired-icons-icon-for-file file))
                         (suffix
                          (cond
                           ((file-directory-p file)
                            (propertize "/" 'face 'dired-directory))
                           ((file-executable-p file)
                            (propertize "*" 'face '(:foreground "#79a8ff")))
                           (t ""))))
                    ;; Add icon before filename
                    (let ((ov1 (make-overlay beg beg)))
                      (overlay-put ov1 'before-string (concat icon " "))
                      (overlay-put ov1 'emacs-nxs-dired-icon-overlay t))
                    ;; Add styled suffix after filename
                    (let ((ov2 (make-overlay end end)))
                      (overlay-put ov2 'after-string suffix)
                      (overlay-put ov2 'emacs-nxs-dired-icon-overlay t))))
              (error nil))
            (forward-line 1))))))

  (add-hook 'dired-after-readin-hook #'emacs-nxs/dired-icons-add-icons)
  (defvar-local emacs-nxs/dired-icons--last-mod-tick nil)

  (defun emacs-nxs/dired-icons-refresh-if-changed ()
    "Redraw dired icons when the buffer content changes."
    (when (derived-mode-p 'dired-mode)
      (let ((tick (buffer-modified-tick)))
        (unless (equal tick emacs-nxs/dired-icons--last-mod-tick)
          (setq emacs-nxs/dired-icons--last-mod-tick tick)
          (emacs-nxs/dired-icons-add-icons)))))

  (add-hook 'dired-mode-hook
            (lambda ()
              (setq emacs-nxs/dired-icons--last-mod-tick (buffer-modified-tick))
              (add-hook 'post-command-hook #'emacs-nxs/dired-icons-refresh-if-changed nil t))))

(provide 'emacs-nxs-icons-dired)
;;; emacs-nxs-icons-dired.el ends here
