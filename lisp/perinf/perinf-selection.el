;;; perinf-selection.el --- Shared object selection -*- lexical-binding: t; -*-

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

;; Shared, unambiguous interactive object selection for public commands.

;;; Code:

(require 'perinf-storage)
(require 'perinf-i18n)

(defun perinf-selection-from-objects (objects)
  "Prompt for one of OBJECTS and return its stable ID."
  (unless objects (user-error "%s" (perinf-i18n 'search.no-results)))
  (let ((choices (mapcar
                  (lambda (object)
                    (cons (format "%s [%s]" (perinf-object-title object)
                                  (perinf-object-id object))
                          (perinf-object-id object)))
                  objects)))
    (cdr (assoc (completing-read
                 (concat (perinf-i18n 'action.find-object) ": ") choices nil t)
                choices))))

(defun perinf-selection-object (type)
  "Prompt for an object of TYPE in the current project and return its ID."
  (unless perinf-current-project
    (user-error "%s" (perinf-i18n 'home.no-project)))
  (perinf-selection-from-objects
   (perinf-storage-list type perinf-current-project)))

(provide 'perinf-selection)
;;; perinf-selection.el ends here
