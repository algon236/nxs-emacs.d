;;; perinf-statuses.el --- Status registry -*- lexical-binding: t; -*-

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

;; Status registry.

;;; Code:

(defconst perinf-status-definitions
  '((task open active waiting completed cancelled)
    (person active inactive)
    (person-group active inactive)
    (meeting planned in-progress held postponed cancelled)
    (audio-recording expected available missing processing transcribed failed)
    (document available missing)
    (transcript queued processing raw failed)
    (minutes ai-draft manual-draft under-review secretary-approved
             awaiting-final-approval final-approved rejected superseded))
  "Language-independent statuses grouped by object type.")

(provide 'perinf-statuses)

;;; perinf-statuses.el ends here
