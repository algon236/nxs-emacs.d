;;; perinf-project.el --- Project metadata for Personal Work and Information System -*- lexical-binding: t; -*-

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

;; Project metadata for Personal Work and Information System.

;;; Code:

(require 'subr-x)

(require 'org)
(require 'org-id)
(require 'perinf-project-schema)
(require 'perinf-i18n)

(defvar perinf-current-project nil
  "Directory of the current Personal Work and Information System project, or nil.")

(defconst perinf-project-metadata-file "perinf-project.org"
  "File containing authoritative PerInf project metadata.")

(defconst perinf-project-directories
  '("data" "data/meetings" "data/transcripts" "data/minutes"
    "media" "media/audio" "archive" "config")
  "Directories created for a new Personal Work and Information System project.")

(defconst perinf-project-data-files
  '(("data/tasks.org" . "Tasks")
    ("data/people.org" . "People")
    ("data/contexts.org" . "Contexts")
    ("data/decisions.org" . "Decisions")
    ("media/audio/audio-index.org" . "Audio recordings"))
  "Initial shared Org data files and their canonical English titles.")

(defun perinf-project-p (directory)
  "Return non-nil when DIRECTORY contains PerInf project metadata."
  (file-regular-p
   (expand-file-name perinf-project-metadata-file directory)))

(defun perinf-project-read-metadata (directory)
  "Read and validate project metadata from DIRECTORY.
Return an alist with language-independent property names."
  (let ((file (expand-file-name perinf-project-metadata-file directory)))
    (unless (file-readable-p file)
      (perinf-i18n-user-error "Personal Work and Information System project metadata is not readable: %s" file))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (org-mode))
      (goto-char (point-min))
      (unless (re-search-forward org-heading-regexp nil t)
        (perinf-i18n-user-error "Personal Work and Information System project metadata has no heading: %s" file))
      (let ((metadata
             (mapcar
              (lambda (property)
                (cons property
                      (org-entry-get nil (symbol-name property))))
              perinf-project-required-metadata)))
        (dolist (entry metadata)
          (unless (cdr entry)
            (perinf-i18n-user-error "Missing project metadata property: %s" (car entry))))
        (unless (equal (alist-get 'SCHEMA_VERSION metadata)
                       (number-to-string perinf-current-schema-version))
          (perinf-i18n-user-error "Unsupported project schema version: %s"
                      (alist-get 'SCHEMA_VERSION metadata)))
        (unless (member (alist-get 'INTERFACE_LANGUAGE metadata)
                        (mapcar #'symbol-name perinf-i18n-supported-locales))
          (perinf-i18n-user-error "Unsupported interface language: %S"
                      (alist-get 'INTERFACE_LANGUAGE metadata)))
        metadata))))

(defun perinf-project--iso-now ()
  "Return the current time as an ISO 8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%:z"))

(defun perinf-project--safe-property-value (value)
  "Return VALUE as a safe, single-line Org property value."
  (string-trim (replace-regexp-in-string "[\n\r]+" " " value)))

(defun perinf-project--write-file (file content)
  "Write CONTENT to FILE, refusing to replace an existing file."
  (when (file-exists-p file)
    (perinf-i18n-user-error "Refusing to replace existing file: %s" file))
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region content nil file nil 'silent nil 'excl)))

(defun perinf-project-create (directory title language date-format time-format)
  "Create a Personal Work and Information System project in DIRECTORY.
TITLE is user-written project content.  LANGUAGE, DATE-FORMAT, and
TIME-FORMAT are language-independent setting symbols.  DIRECTORY must not
already exist.  Return the normalized project directory."
  (let* ((target (directory-file-name (expand-file-name directory)))
         (parent (file-name-directory target))
         (project-id (concat "project-" (org-id-uuid)))
         (metadata-id (concat "project-metadata-" (org-id-uuid)))
         (created-at (perinf-project--iso-now))
         (safe-title (perinf-project--safe-property-value title)))
    (when (file-exists-p target)
      (perinf-i18n-user-error "Project directory already exists: %s" target))
    (unless (file-directory-p parent)
      (perinf-i18n-user-error "Parent directory does not exist: %s" parent))
    (unless (memq language '(en da fr de es))
      (perinf-i18n-user-error "Unsupported interface language: %S" language))
    (unless (memq date-format
                  '(iso day-month-year-dash day-month-year-slash
                    month-day-year-slash localized-long))
      (perinf-i18n-user-error "Unsupported date format: %S" date-format))
    (unless (memq time-format '(twenty-four-hour twelve-hour))
      (perinf-i18n-user-error "Unsupported time format: %S" time-format))
    (when (string-empty-p safe-title)
      (perinf-i18n-user-error "Project title must not be empty"))
    (make-directory target)
    (condition-case error-data
        (progn
          (dolist (relative perinf-project-directories)
            (make-directory (expand-file-name relative target) t))
          (perinf-project--write-file
           (expand-file-name perinf-project-metadata-file target)
           (format
            (concat "#+title: %s\n"
                    "#+language: %s\n\n"
                    "* %s\n"
                    ":PROPERTIES:\n"
                    ":ID:                 %s\n"
                    ":PERINF_TYPE:        project-metadata\n"
                    ":PERINF_STATUS:      active\n"
                    ":PROJECT_ID:         %s\n"
                    ":PROJECT_TITLE:      %s\n"
                    ":SCHEMA_VERSION:     %d\n"
                    ":INTERFACE_LANGUAGE: %s\n"
                    ":DATE_FORMAT:        %s\n"
                    ":TIME_FORMAT:        %s\n"
                    ":CREATED_AT:         %s\n"
                    ":END:\n")
            safe-title language safe-title metadata-id project-id safe-title
            perinf-current-schema-version language date-format time-format
            created-at))
          (dolist (entry perinf-project-data-files)
            (perinf-project--write-file
             (expand-file-name (car entry) target)
             (format "#+title: %s\n#+startup: overview\n" (cdr entry))))
          (file-name-as-directory target))
      (error
       (when (file-directory-p target)
         (delete-directory target t))
       (signal (car error-data) (cdr error-data))))))

(provide 'perinf-project)

;;; perinf-project.el ends here
