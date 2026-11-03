;;; emacs-nxs-icloud-trash.el --- Recover failed iCloud trash moves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Niels Søndergaard
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; macOS sometimes rejects NSFileManager trash moves from launchd Emacs,
;; although an ordinary rename into the existing iCloud trash succeeds.
;; Keep the native operation first.  Only recover local regular files in
;; Documents when both the iCloud domain and its trash marker are verified.
;; Files remain recoverable in Trash, but Finder's Put Back metadata is not
;; supplied by this fallback; recovering a file may require dragging it out.

;;; Code:

(defun emacs-nxs-icloud-trash--attribute (attribute path)
  "Return macOS extended ATTRIBUTE of PATH, or nil if unavailable."
  (with-temp-buffer
    (when (zerop (process-file "/usr/bin/xattr" nil '(t nil) nil
                              "-p" attribute path))
      (buffer-string))))

(defun emacs-nxs-icloud-trash--recover (operation filename)
  "Run native trash OPERATION on FILENAME, with a scoped iCloud fallback."
  (condition-case native-error
      (funcall operation filename)
    (file-error
     (let* ((source (expand-file-name filename))
            (documents (expand-file-name "~/Documents/"))
            (trash (expand-file-name "~/Library/Mobile Documents/.Trash/")))
       (if (and (eq system-type 'darwin)
                (not (file-remote-p source))
                (not (file-symlink-p source))
                (file-regular-p source)
                (file-in-directory-p source documents)
                (file-directory-p trash)
                (string-prefix-p
                 "com.apple.CloudDocs.iCloudDriveFileProvider/"
                 (or (emacs-nxs-icloud-trash--attribute
                      "com.apple.file-provider-domain-id" documents) ""))
                (emacs-nxs-icloud-trash--attribute
                 "com.apple.fileprovider.trash" trash))
           (let* ((name (file-name-nondirectory source))
                  (destination (expand-file-name name trash)))
             ;; Never overwrite an earlier trashed file, including a symlink.
             (while (or (file-exists-p destination)
                        (file-symlink-p destination))
               (setq destination
                     (concat (make-temp-name
                              (expand-file-name
                               (concat (file-name-base name) "-") trash))
                             (or (file-name-extension name t) ""))))
             ;; No overwrite: a collision occurring after the check fails safely.
             (rename-file source destination nil))
         (signal (car native-error) (cdr native-error)))))))

(when (and (eq system-type 'darwin)
           (fboundp 'system-move-file-to-trash))
  (advice-add 'system-move-file-to-trash :around
              #'emacs-nxs-icloud-trash--recover))

(provide 'emacs-nxs-icloud-trash)
;;; emacs-nxs-icloud-trash.el ends here
