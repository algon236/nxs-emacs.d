;;; emacs-nxs-formatter.el --- Configurable format-on-save with formatter registry  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: languages, tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Configurable format-on-save with a registry of formatters per
;; file extension.  Supports local (project-level) and global
;; formatter discovery with optional config file requirements.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(use-package emacs-nxs-formatter
  :ensure nil
  :no-require t
  :if emacs-nxs-enable-auto-formatter
  :init
  (defcustom emacs-nxs-formatter-alist
    '(;; Node.js ecosystem — try biome first, fall back to prettier
      (("js" "jsx" "ts" "tsx" "json" "css" "html" "sass" "yaml")
       . ((:cmd "biome" :args ("format") :stdin-path-option "--stdin-file-path" :local "node_modules/.bin/biome" :config "biome.json")
          (:cmd "prettier" :args nil :stdin-path-option "--stdin-filepath" :local "node_modules/.bin/prettier")))
      ;; Shell scripts
      (("sh" "bash")
       . ((:cmd "shfmt" :args nil :stdin-path-option "-filename"))))
    "Alist mapping file extensions to an ordered list of formatter candidates.
Each entry is (EXTENSIONS . FORMATTERS) where EXTENSIONS is a list of
file extension strings (without dots) and FORMATTERS is a list of plists.

Each formatter plist supports the following keys:
  :cmd    — executable name for `executable-find' (global fallback)
  :args   — arguments for formatting standard input to standard output
  :stdin-path-option — option followed by the original filename
  :local  — optional relative path from project root for local install lookup
  :config — optional config file that must exist in the project root for
            this formatter to be selected (e.g. biome needs \"biome.json\")"
    :type '(alist :key-type (repeat string)
                  :value-type (repeat plist))
    :group 'emacs-nxs)

  (defun emacs-nxs-formatter--find-formatter (file)
    "Find a suitable formatter for FILE based on `emacs-nxs-formatter-alist'.
Returns a plist (:executable CMD :args ARGS :source SOURCE) or nil."
    (let* ((ext (file-name-extension file))
           (project-root (or (locate-dominating-file file "node_modules")
                             (locate-dominating-file file ".git")))
           (entry (cl-find-if (lambda (e) (member ext (car e)))
                              emacs-nxs-formatter-alist)))
      (when entry
        (cl-loop for fmt in (cdr entry) do
                 (let* ((cmd (plist-get fmt :cmd))
                        (args (plist-get fmt :args))
                        (local-path (plist-get fmt :local))
                        (config (plist-get fmt :config))
                        ;; Check config requirement
                        (config-ok (or (null config)
                                       (and project-root
                                            (file-exists-p (expand-file-name config project-root)))))
                        ;; Find executable: local first, then global
                        (local-bin (and local-path project-root
                                        (let ((p (expand-file-name local-path project-root)))
                                          (and (file-executable-p p) p))))
                        (global-bin (executable-find cmd))
                        (executable (or local-bin global-bin))
                        (source (cond
                                 (local-bin (format "%s (local)" cmd))
                                 (global-bin (format "%s (global)" cmd)))))
                   (when (and config-ok executable)
                     (cl-return (list :executable executable
                                      :args args
                                      :stdin-path-option (plist-get fmt :stdin-path-option)
                                      :source source))))))))

  (defvar emacs-nxs-formatter--saving nil
    "Non-nil while saving formatted text or preparing a manual run.")
  (defvar-local emacs-nxs-formatter--process nil)

  (defun emacs-nxs-formatter--cancel ()
    "Cancel this buffer's previous formatter without accepting its output."
    (when (processp emacs-nxs-formatter--process)
      (let ((proc emacs-nxs-formatter--process))
        (setq emacs-nxs-formatter--process nil)
        (when (process-live-p proc) (delete-process proc)))))

  (defun emacs-nxs-formatter/format-current-file (&optional manual)
    "Format a snapshot on standard input; never let a process write the file.
Accept output only if the buffer and visited file are still unchanged.
MANUAL saves the buffer first, without recursively launching a formatter."
    (interactive (list t))
    (unless emacs-nxs-formatter--saving
      (let* ((file buffer-file-name)
             (buf (current-buffer))
             (result (and file (not (file-remote-p file))
                          (not buffer-read-only)
                          (emacs-nxs-formatter--find-formatter file))))
        (if (not result)
            (when manual (user-error "No local formatter available for this buffer"))
          (when manual
            (let ((emacs-nxs-formatter--saving t)) (save-buffer)))
          (unless (buffer-modified-p)
            (emacs-nxs-formatter--cancel)
            (let* ((tick (buffer-chars-modified-tick))
                   (output (generate-new-buffer " *NXS formatter output*"))
                   (errors (generate-new-buffer " *NXS formatter errors*"))
                   (option (plist-get result :stdin-path-option))
                   (command (append (list (plist-get result :executable))
                                    (plist-get result :args)
                                    (when option (list option file))))
                   (coding buffer-file-coding-system)
                   (source (plist-get result :source)))
              (condition-case err
                  (let ((proc
                         (make-process
                          :name "nxs-formatter" :buffer output :stderr errors
                          :command command :connection-type 'pipe
                          :coding (cons coding coding) :noquery t
                          :sentinel
                          (lambda (proc _event)
                            (when (memq (process-status proc) '(exit signal))
                              (unwind-protect
                                  (when (buffer-live-p buf)
                                    (with-current-buffer buf
                                      (when (eq proc emacs-nxs-formatter--process)
                                        (setq emacs-nxs-formatter--process nil)
                                        (cond
                                         ((not (and (eq (process-status proc) 'exit)
                                                    (zerop (process-exit-status proc))))
                                          (message "Formatter failed (%s): %s" source
                                                   (with-current-buffer errors
                                                     (string-trim (buffer-string)))))
                                         ((or (not (equal file buffer-file-name))
                                              buffer-read-only (buffer-modified-p)
                                              (/= tick (buffer-chars-modified-tick))
                                              (not (verify-visited-file-modtime buf)))
                                          (message "Formatter result discarded: file or buffer changed"))
                                         (t
                                          (atomic-change-group
                                            (save-restriction
                                              (widen)
                                              (replace-buffer-contents output)))
                                          (when (buffer-modified-p)
                                            (let ((emacs-nxs-formatter--saving t)
                                                  (before-save-hook nil))
                                              (save-buffer)))
                                          (message "Formatted with %s" source))))))
                                (when (buffer-live-p output) (kill-buffer output))
                                (when (buffer-live-p errors) (kill-buffer errors))))))))
                    (setq emacs-nxs-formatter--process proc)
                    (add-hook 'kill-buffer-hook #'emacs-nxs-formatter--cancel nil t)
                    (save-restriction
                      (widen)
                      (process-send-region proc (point-min) (point-max)))
                    (process-send-eof proc))
                (error
                 (emacs-nxs-formatter--cancel)
                 (when (buffer-live-p output) (kill-buffer output))
                 (when (buffer-live-p errors) (kill-buffer errors))
                 (message "Cannot start formatter: %s" (error-message-string err))))))))))

  (defun emacs-nxs-formatter/format-current-file-manual ()
    "Manually invoke format for current file (saves first)."
    (interactive)
    (emacs-nxs-formatter/format-current-file t))

  (defun emacs-nxs-formatter/enable-format-on-save ()
    "Add format-on-save to the current buffer's `after-save-hook'."
    (interactive)
    (add-hook 'after-save-hook #'emacs-nxs-formatter/format-current-file nil t)
    (message "Format-on-save enabled for this buffer."))

  (defun emacs-nxs-formatter/disable-format-on-save ()
    "Remove format-on-save from the current buffer's `after-save-hook'."
    (interactive)
    (remove-hook 'after-save-hook #'emacs-nxs-formatter/format-current-file t)
    (message "Format-on-save disabled for this buffer."))

  (defun emacs-nxs-formatter/toggle-format-on-save ()
    "Toggle format-on-save for the current buffer."
    (interactive)
    (if (memq #'emacs-nxs-formatter/format-current-file after-save-hook)
        (progn
          (remove-hook 'after-save-hook #'emacs-nxs-formatter/format-current-file t)
          (message "Formatting on save turned OFF"))
      (add-hook 'after-save-hook #'emacs-nxs-formatter/format-current-file nil t)
      (message "Formatting on save turned ON")))

  (defun emacs-nxs-formatter--maybe-enable ()
    "Auto-enable format-on-save if the file's extension has a registered formatter."
    (when-let* ((file (buffer-file-name))
                ((not (file-remote-p file)))
                (ext (file-name-extension file)))
      (when (cl-find-if (lambda (e) (member ext (car e)))
                        emacs-nxs-formatter-alist)
        (add-hook 'after-save-hook #'emacs-nxs-formatter/format-current-file nil t))))

  (add-hook 'find-file-hook #'emacs-nxs-formatter--maybe-enable)

  (global-set-key (kbd "C-c p") #'emacs-nxs-formatter/format-current-file-manual)
  (global-set-key (kbd "C-c C-p") #'emacs-nxs-formatter/format-current-file-manual)
  (global-set-key (kbd "C-c t f") #'emacs-nxs-formatter/toggle-format-on-save))

(provide 'emacs-nxs-formatter)
;;; emacs-nxs-formatter.el ends here
