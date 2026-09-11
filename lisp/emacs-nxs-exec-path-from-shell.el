;;; emacs-nxs-exec-path-from-shell.el --- Preserve shell and Emacs paths -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'subr-x)
(require 'cl-lib)

(defun emacs-nxs/set-exec-path-from-shell-PATH ()
  "Import the shell PATH without losing early-init paths or exec-directory.
Ignore startup chatter and leave the environment intact on shell failure."
  (interactive)
  (let* ((shell (or (getenv "SHELL") shell-file-name))
         (name (file-name-nondirectory shell))
         (marker "__NXS_PATH__=")
         (args (cond ((equal name "zsh")
                      '("-i" "-c" "printf '__NXS_PATH__=%s\\n' \"$PATH\""))
                     ((equal name "bash")
                      '("--login" "-c" "printf '__NXS_PATH__=%s\\n' \"$PATH\""))
                     ((equal name "fish")
                      '("-c" "printf '__NXS_PATH__=%s\\n' (string join : $PATH)")))))
    (when args
      (condition-case err
          (with-temp-buffer
            (let ((status (apply #'call-process shell nil (list t nil) nil args)))
              (goto-char (point-min))
              (if (and (equal status 0)
                       (re-search-forward (concat "^" marker "\\([^\n\r]+\\)") nil t))
                  (let* ((shell-path (split-string (match-string 1) path-separator t))
                         (paths (delete-dups
                                 (append (cl-remove-if-not
                                          #'file-directory-p emacs-nxs-extra-exec-path)
                                         shell-path exec-path (list exec-directory)))))
                    (setq exec-path paths)
                    (setenv "PATH" (mapconcat #'identity (delq nil (copy-sequence paths))
                                             path-separator)))
                (message "NXS: shell PATH import failed; existing paths retained"))))
        (error (message "NXS: PATH retained: %s" (error-message-string err)))))))

(add-hook 'after-init-hook #'emacs-nxs/set-exec-path-from-shell-PATH)
(provide 'emacs-nxs-exec-path-from-shell)
;;; emacs-nxs-exec-path-from-shell.el ends here
