;;; emacs-nxs-dired-gutter.el --- Git status indicators in Dired buffers  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: vc, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Overlays Git status indicators (added, modified, deleted, etc.)
;; on the first column of Dired buffers using `git status --porcelain'.

;;; Code:

(use-package emacs-nxs-dired-gutter
  :if emacs-nxs-enable-dired-gutter
  :ensure nil
  :no-require t
  :defer t
  :init
  (setq emacs-nxs-dired-gutter-enabled t)

  (defvar-local emacs-nxs/dired-git-status-overlays nil
    "List of active Git-status overlays in the current Dired buffer.")

  (defun emacs-nxs/dired--git-status-face (code)
    "Return a cons cell (STATUS . FACE) for a given Git porcelain CODE."
    (let* ((git-status-untracked "??")
           (git-status-modified " M")
           (git-status-modified-alt "M ")
           (git-status-deleted "D ")
           (git-status-added "A ")
           (git-status-renamed "R ")
           (git-status-copied "C ")
           (git-status-ignored "!!")
           (status (cond
                    ((string-match-p "\\?\\?" code) git-status-untracked)
                    ((string-match-p "^ M" code) git-status-modified)
                    ((string-match-p "^M " code) git-status-modified-alt)
                    ((string-match-p "^D" code) git-status-deleted)
                    ((string-match-p "^A" code) git-status-added)
                    ((string-match-p "^R" code) git-status-renamed)
                    ((string-match-p "^C" code) git-status-copied)
                    ((string-match-p "\\!\\!" code) git-status-ignored)
                    (t "  ")))
           (face (cond
                  ((string= status git-status-ignored) 'shadow)
                  ((string= status git-status-untracked) 'warning)
                  ((string= status git-status-modified) 'font-lock-function-name-face)
                  ((string= status git-status-modified-alt) 'font-lock-function-name-face)
                  ((string= status git-status-deleted) 'error)
                  ((string= status git-status-added) 'success)
                  (t 'font-lock-keyword-face))))
      (cons status face)))

  (defun emacs-nxs/dired-git-status-overlay ()
    "Overlay Git status indicators on the first column in Dired."
    (interactive)
    (require 'vc-git)
    (let ((git-root (ignore-errors (vc-git-root default-directory))))
      (when (and git-root
                 (not (file-remote-p default-directory))
                 emacs-nxs-dired-gutter-enabled)
        (setq git-root (expand-file-name git-root))
        (let* ((git-status (vc-git--run-command-string nil "status" "--porcelain" "--ignored" "--untracked-files=normal"))
               (status-map (make-hash-table :test 'equal)))
          (mapc #'delete-overlay emacs-nxs/dired-git-status-overlays)
          (setq emacs-nxs/dired-git-status-overlays nil)

          (dolist (line (split-string git-status "\n" t))
            (when (string-match "^\\(..\\) \\(.+\\)$" line)
              (let* ((code (match-string 1 line))
                     (file (match-string 2 line))
                     (fullpath (expand-file-name file git-root))
                     (status-face (emacs-nxs/dired--git-status-face code)))
                (puthash fullpath status-face status-map))))

          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (let* ((relative-file
                      (ignore-errors (dired-get-filename 'relative t)))
                     (file
                      (and relative-file
                           (not (member relative-file '("." "..")))
                           (expand-file-name relative-file))))
                (when file
                  (setq file (if (file-directory-p file) (concat file "/") file))
                  (let* ((status-face (gethash file status-map (cons "  " 'font-lock-keyword-face)))
                         (status (car status-face))
                         (face (cdr status-face))
                         (status-str (propertize (format " %s " status) 'face face))
                         ;; Keep the overlay zero-width.  Covering the first
                         ;; buffer character makes its `before-string'
                         ;; interact with omitted `.' and `..' entries and can
                         ;; shift the first visible Dired row to the right.
                         (bol (line-beginning-position))
                         (ov (make-overlay bol bol nil t nil)))
                    (overlay-put ov 'before-string status-str)
                    (overlay-put ov 'emacs-nxs-dired-git-status-overlay t)
                    (push ov emacs-nxs/dired-git-status-overlays))))
              (forward-line 1)))))))

  (add-hook 'dired-after-readin-hook #'emacs-nxs/dired-git-status-overlay))

(provide 'emacs-nxs-dired-gutter)
;;; emacs-nxs-dired-gutter.el ends here
