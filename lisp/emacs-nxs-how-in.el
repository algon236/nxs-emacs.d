;;; emacs-nxs-how-in.el --- Query cheat.sh for programming answers  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: docs, tools, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Asynchronously fetches programming cheat sheets from cheat.sh
;; and displays them in a dedicated buffer with ANSI color support.

;;; Code:

(use-package emacs-nxs-how-in
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/how-in ()
    "Open a new Emacs buffer and asynchronously fetch cheat.sh data."
    (interactive)
    (let* (
           (program (read-string "How in (program name): " nil nil nil))
           (prompt  (read-string (concat "How in " program " would I: ") nil nil nil))
           (encoded-prompt (url-hexify-string prompt))
           (buffer (get-buffer-create "*HowIn*"))
           (url (format "curl -s 'cheat.sh/%s/%s'" program encoded-prompt)))
      (with-current-buffer buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert (concat "Answering: How in " program " would I " prompt "\n"))
        (read-only-mode 1))
      (switch-to-buffer buffer)
      (emacs-nxs--fetch-how-in url buffer)))

  (defun emacs-nxs--fetch-how-in (cmd buffer &optional)
    "Run CMD asynchronously and insert results into BUFFER."
    (make-process
     :name "how-in-fetch"
     :buffer (generate-new-buffer "*how-in-temp*")
     :command (list "sh" "-c" cmd)
     :sentinel
     (lambda (proc _event)
       (when (eq (process-status proc) 'exit)
         (let ((output (with-current-buffer (process-buffer proc)
                         (buffer-string))))
           (kill-buffer (process-buffer proc))

           (with-current-buffer buffer
             (read-only-mode -1)
             (insert output)
             (ansi-color-apply-on-region (point-min) (point-max))
             (goto-char (point-min))
             (read-only-mode 1))))))))

(provide 'emacs-nxs-how-in)
;;; emacs-nxs-how-in.el ends here
