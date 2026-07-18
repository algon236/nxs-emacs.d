;;; emacs-nxs-rainbow-delimiters.el --- Rainbow coloring for matching delimiters  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: faces, convenience
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;;
;; Colorizes matching parentheses, brackets, and braces at
;; different nesting depths using font-lock.
;;
;; FIXME: Make it play nice with treesitter modes

;;; Code:

(use-package emacs-nxs-rainbow-delimiters
  :if emacs-nxs-enable-rainbow-delimiters
  :ensure nil
  :no-require t
  :defer t
  :init
  (defun emacs-nxs/rainbow-delimiters ()
    "Apply simple rainbow coloring to (), [] and {} in the current buffer.
Opening and closing delimiters will have matching colors."
    (interactive)
    (let ((colors '(font-lock-function-name-face
                    font-lock-builtin-face
                    font-lock-type-face
                    font-lock-keyword-face
                    font-lock-variable-name-face
                    font-lock-constant-face
                    font-lock-string-face)))
      (font-lock-add-keywords
       nil
       `((,(rx (or "(" ")" "[" "]" "{" "}"))
          (0 (let* ((char (char-after (match-beginning 0)))
                    (depth (save-excursion
                             ;; Move to the correct position based on opening/closing delimiter
                             (if (member char '(?\) ?\] ?\}))
                                 (progn
                                   (backward-char) ;; Move to the opening delimiter
                                   (car (syntax-ppss)))
                               (car (syntax-ppss)))))
                    (face (nth (mod depth ,(length colors)) ',colors)))
               (list 'face face)))))))
    (font-lock-flush)
    (font-lock-ensure))

  (add-hook 'prog-mode-hook #'emacs-nxs/rainbow-delimiters))

(provide 'emacs-nxs-rainbow-delimiters)
;;; emacs-nxs-rainbow-delimiters.el ends here
