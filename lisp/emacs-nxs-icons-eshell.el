;;; emacs-nxs-icons-eshell.el --- File type icons for Eshell listings  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Inspired by: https://www.reddit.com/r/emacs/comments/xboh0y/how_to_put_icons_into_eshell_ls/

;;; Commentary:
;;
;; Adds file type icons to Eshell `ls' output via advice on
;; `eshell-ls-annotate'.  Files are clickable (RET to open,
;; D to delete).

;;; Code:

(use-package emacs-nxs-icons-eshell
  :if (memq 'eshell emacs-nxs-icon-modules)
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/eshell-icons (file)
    "Return a cons of propertized display string and file metadata.
FILE is a list (NAME IS-DIR EXECUTABLE ...), like from `eshell/ls`.
The full list is like:
(FILENAME IS-DIR SIZE OWNER GROUP MOD-TIME ACCESS-TIME CHANGE-TIME
SIZE-LONG PERMS HARDLINKS INODE DEVICE).
"
    (let* ((filename (car file))
           (is-dir (eq (cadr file) t))
           (perms (nth 9 file))
           (is-exec (and perms (string-match-p "x" perms)))
           (ext (and (not is-dir) (file-name-extension filename)))
           (found (and ext (emacs-nxs/file-icon ext)))
           (icon (if is-dir
                     (emacs-nxs/file-icon "direddir")
                   (if (and found (not (string-empty-p found)))
                       found
                     (emacs-nxs/file-icon "diredfile"))))
           (suffix (cond
                    (is-dir "/")
                    (is-exec "*")
                    (t "")))
           (display-text (propertize
                          (concat icon " " filename suffix)
                          'file-name filename
                          'mouse-face 'highlight
                          'help-echo (concat "Open " filename)
                          'keymap eshell-ls-file-keymap)))
      (cons display-text (cdr file))))


  (defvar eshell-ls-file-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "RET") #'eshell-ls-find-file)
      (define-key map (kbd "<return>") #'eshell-ls-find-file)
      (define-key map [mouse-1] #'eshell-ls-find-file)
      (define-key map (kbd "D") #'eshell-ls-delete-file)
      map)
    "Keymap active on Eshell file entries.")

  (defun eshell-ls-file-at-point ()
    "Get the full path of the Eshell listing at point."
    (get-text-property (point) 'file-name))

  (defun eshell-ls-find-file ()
    "Open the Eshell listing at point."
    (interactive)
    (find-file (eshell-ls-file-at-point)))

  (defun eshell-ls-delete-file ()
    "Delete the Eshell listing at point."
    (interactive)
    (let ((file (eshell-ls-file-at-point)))
      (when (yes-or-no-p (format "Delete file %s?" file))
        (delete-file file 'trash))))

  (advice-remove 'eshell-ls-decorated-name #'emacs-nxs/eshell-icons)
  (advice-add #'eshell-ls-annotate :filter-return #'emacs-nxs/eshell-icons))

(provide 'emacs-nxs-icons-eshell)
;;; emacs-nxs-icons-eshell.el ends here
