;;; perinf-i18n.el --- Locale handling for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)

(defgroup perinf nil
  "Org-backed work management."
  :group 'applications
  :prefix "perinf-")

(defcustom perinf-interface-language 'en
  "Language used by the Personal Work and Information System interface."
  :type '(choice (const :tag "English" en)
                 (const :tag "Dansk" da)
                 (const :tag "Français" fr)
                 (const :tag "Deutsch" de)
                 (const :tag "Español" es))
  :group 'perinf)

(defconst perinf-i18n-supported-locales '(en da fr de es)
  "Locale identifiers shipped with Personal Work and Information System.")

(defvar perinf-i18n--locales (make-hash-table :test #'eq)
  "Registered locale tables.")

(defun perinf-i18n-register-locale (locale translations)
  "Register TRANSLATIONS for LOCALE.
TRANSLATIONS is an alist whose keys are language-independent symbols."
  (unless (memq locale perinf-i18n-supported-locales)
    (error "Unsupported Personal Work and Information System locale: %S" locale))
  (puthash locale translations perinf-i18n--locales))

(defun perinf-i18n-load-locales ()
  "Load all locale data bundled with Personal Work and Information System."
  (dolist (locale perinf-i18n-supported-locales)
    (require (intern (format "perinf-locale-%s" locale)))))

(defun perinf-i18n (key &optional locale)
  "Return translation for KEY in LOCALE.
English is the canonical fallback.  A visibly marked key is returned when
neither locale contains a translation."
  (let* ((requested (or locale perinf-interface-language))
         (table (gethash requested perinf-i18n--locales))
         (english (gethash 'en perinf-i18n--locales)))
    (or (alist-get key table)
        (alist-get key english)
        (format "[%s]" key))))

(defun perinf-i18n-validate-locale (locale)
  "Return missing and unknown keys for LOCALE compared with English."
  (let* ((canonical (mapcar #'car (gethash 'en perinf-i18n--locales)))
         (translated (mapcar #'car (gethash locale perinf-i18n--locales))))
    (list :missing (cl-set-difference canonical translated)
          :unknown (cl-set-difference translated canonical))))

(provide 'perinf-i18n)

;;; perinf-i18n.el ends here
