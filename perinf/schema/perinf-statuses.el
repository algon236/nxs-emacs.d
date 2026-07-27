;;; perinf-statuses.el --- Status registry -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defconst perinf-status-definitions
  '((task open active waiting completed cancelled)
    (person active inactive)
    (meeting planned in-progress held postponed cancelled)
    (audio-recording expected available missing processing transcribed failed)
    (transcript queued processing raw failed)
    (minutes ai-draft manual-draft under-review secretary-approved
             awaiting-final-approval final-approved rejected superseded))
  "Language-independent statuses grouped by object type.")

(provide 'perinf-statuses)

;;; perinf-statuses.el ends here
