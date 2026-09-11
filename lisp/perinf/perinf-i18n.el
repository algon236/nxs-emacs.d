;;; perinf-i18n.el --- Locale handling for Personal Work and Information System -*- lexical-binding: t; -*-

;; Copyright (C) 2026, Niels Søndergaard, Nivaa, Denmark.
;; Author: Niels Søndergaard <niels@algon.dk>
;; Assisted-by: Codex:GPT-6

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is part of Personal Work and Information System.
;;
;; Personal Work and Information System is free software: you can redistribute
;; it and/or modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Personal Work and Information System is distributed in the hope that it will
;; be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Locale handling for Personal Work and Information System.

;;; Code:

(require 'cl-lib)

(defgroup perinf nil
  "Org-backed work management."
  :group 'applications
  :prefix "perinf-")

(defcustom perinf-interface-language 'da
  "Language used by the Personal Work and Information System interface."
  :type '(choice (const :tag "English" en)
                 (const :tag "Dansk" da)
                 (const :tag "Français" fr)
                 (const :tag "Deutsch" de)
                 (const :tag "Español" es))
  :group 'perinf)

(defcustom perinf-interface-language-override nil
  "Local UI language overriding project metadata, or nil to follow the project."
  :type '(choice (const :tag "Follow project" nil)
                 (const da) (const en) (const fr) (const de) (const es))
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
  (perinf-i18n-load-locales)
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

(defvar perinf-i18n-danish-errors nil
  "Danish validation messages keyed by their canonical English format string.")

(defun perinf-i18n-user-error (message &rest arguments)
  "Signal a localized user error using MESSAGE and ARGUMENTS.
Validation messages currently have Danish and canonical English versions."
  (perinf-i18n-load-locales)
  (apply #'user-error
         (or (and (eq perinf-interface-language 'da)
                  (cdr (assoc-string message perinf-i18n-danish-errors)))
             message)
         arguments))

(provide 'perinf-i18n)

;;; perinf-i18n.el ends here
