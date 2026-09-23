;;; emacs-nxs-elisp.el --- Emacs Lisp editing tools -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Niels Søndergaard
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Structured editing with Paredit in Emacs Lisp and IELM buffers.
;; Reuses NXS completion, Eldoc, Flymake and parenthesis highlighting.
;; Install Paredit through `emacs-nxs/install-missing-packages' if needed.
;;
;; Emacs Lisp buffers have a C-c e prefix:
;;   d: evaluate definition       b: evaluate buffer
;;   r: evaluate region          i: open IELM
;;   e: instrument with Edebug   t: run ERT tests
;;   p: check parentheses       c: check documentation
;;   k: byte-compile this file (asks to save modified source)
;; Evaluation runs code in the current Emacs session.
;; Standard C-M-x, C-x C-e, M-. and M-, bindings are preserved.

;;; Code:

(require 'elisp-mode)
(require 'elec-pair)

(declare-function paredit-mode "paredit" (&optional arg))
(autoload 'ielm "ielm" nil t)
(autoload 'edebug-defun "edebug" nil t)
(autoload 'ert "ert" nil t)
(autoload 'checkdoc "checkdoc" nil t)
(autoload 'byte-compile-file "bytecomp" nil t)

(defgroup emacs-nxs-elisp nil
  "Emacs Lisp editing in NXS."
  :group 'lisp)

(defcustom emacs-nxs-elisp-enable-paredit t
  "Whether to enable Paredit in newly opened Lisp and IELM buffers."
  :type 'boolean
  :group 'emacs-nxs-elisp)

(defun emacs-nxs-elisp-byte-compile ()
  "Byte-compile the current file, offering to save modified source first."
  (interactive)
  (unless (and buffer-file-name (derived-mode-p 'emacs-lisp-mode))
    (user-error "This command needs a file-backed Emacs Lisp buffer"))
  (when (buffer-modified-p)
    (unless (y-or-n-p "Save this buffer before compiling? ")
      (user-error "Compilation cancelled; buffer has unsaved changes"))
    (save-buffer))
  (byte-compile-file buffer-file-name))

(defvar emacs-nxs-elisp-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'eval-defun)
    (define-key map (kbd "b") #'eval-buffer)
    (define-key map (kbd "r") #'eval-region)
    (define-key map (kbd "i") #'ielm)
    (define-key map (kbd "e") #'edebug-defun)
    (define-key map (kbd "t") #'ert)
    (define-key map (kbd "p") #'check-parens)
    (define-key map (kbd "c") #'checkdoc)
    (define-key map (kbd "k") #'emacs-nxs-elisp-byte-compile)
    map)
  "Commands available under C-c e in Emacs Lisp buffers.")

(defun emacs-nxs-elisp-structured-editing ()
  "Enable Paredit locally, without competing electric pair insertion."
  (when (and emacs-nxs-elisp-enable-paredit
             (require 'paredit nil t))
    (paredit-mode 1)
    (electric-pair-local-mode -1)))

(defun emacs-nxs-elisp-setup ()
  "Set up an Emacs Lisp editing buffer."
  (setq-local indent-tabs-mode nil)
  (eldoc-mode 1)
  (flymake-mode 1)
  (emacs-nxs-elisp-structured-editing))

(define-key emacs-lisp-mode-map (kbd "C-c e") emacs-nxs-elisp-command-map)
(add-hook 'emacs-lisp-mode-hook #'emacs-nxs-elisp-setup)
(add-hook 'inferior-emacs-lisp-mode-hook #'emacs-nxs-elisp-structured-editing)

(with-eval-after-load 'which-key
  (which-key-add-keymap-based-replacements emacs-lisp-mode-map
    "C-c e" "Emacs Lisp"))

(provide 'emacs-nxs-elisp)
;;; emacs-nxs-elisp.el ends here
