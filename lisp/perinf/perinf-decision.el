;;; perinf-decision.el --- Decision workflow -*- lexical-binding: t; -*-

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

;; Decision workflow.

;;; Code:

(require 'seq)
(require 'perinf-date)
(require 'perinf-i18n)
(require 'perinf-storage)
(require 'perinf-selection)

(defun perinf-decision--setting (property)
  "Return current project PROPERTY as a symbol."
  (intern (alist-get
           property
           (perinf-storage-read-project perinf-current-project))))

(defun perinf-decision-create (&optional minutes-id)
  "Interactively register a decision, optionally sourced from MINUTES-ID."
  (interactive)
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (let* ((date-format (perinf-decision--setting 'DATE_FORMAT))
         (minutes
          (and minutes-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) minutes-id))
                (perinf-storage-list 'minutes perinf-current-project))))
         (meeting-id
          (and minutes
               (alist-get 'MEETING_ID (perinf-object-properties minutes))))
         (meeting
          (and meeting-id
               (seq-find
                (lambda (candidate)
                  (equal (perinf-object-id candidate) meeting-id))
                (perinf-storage-list 'meeting perinf-current-project))))
         (default-date
          (and meeting
               (substring
                (alist-get 'START_AT (perinf-object-properties meeting))
                0 10)))
         (title (string-trim
                 (read-string (perinf-i18n 'decision.title-prompt))))
         (date
          (perinf-date-normalize
           (read-string
            (format "%s (%s): "
                    (perinf-i18n 'decision.date-prompt)
                    (perinf-i18n
                     (intern (format "setting.%s" date-format))))
            (and default-date
                 (perinf-date-format default-date date-format)))
           date-format))
         (rationale
          (string-trim
           (read-string (perinf-i18n 'decision.rationale-prompt))))
         (decision
          (perinf-storage-create
           'decision
           `((title . ,title)
             (date . ,date)
             (rationale . ,rationale)
             (meeting-id . ,meeting-id)
             (minutes-id . ,minutes-id))
           perinf-current-project)))
    (message "%s" (perinf-i18n 'decision.created))
    (when (fboundp 'perinf-core-records)
      (perinf-core-records))
    decision))

(defun perinf-decision-create-from-minutes (minutes-id)
  "Register a decision sourced from final-approved MINUTES-ID."
  (interactive (list (perinf-selection-object 'minutes)))
  (let ((minutes
         (seq-find
          (lambda (candidate)
            (equal (perinf-object-id candidate) minutes-id))
          (perinf-storage-list 'minutes perinf-current-project))))
    (unless minutes
      (signal 'perinf-object-not-found (list minutes-id)))
    (unless (eq (perinf-object-status minutes) 'final-approved)
      (user-error "%s" (perinf-i18n 'decision.requires-approved-minutes)))
    (perinf-decision-create minutes-id)))

(provide 'perinf-decision)

;;; perinf-decision.el ends here
