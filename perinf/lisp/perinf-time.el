;;; perinf-time.el --- Time normalization for Personal Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun perinf-time-normalize (text format)
  "Normalize time TEXT according to FORMAT and return HH:MM:SS.
An empty value returns nil."
  (let ((value (upcase (string-trim text)))
        hour minute)
    (unless (string-empty-p value)
      (pcase format
        ('twenty-four-hour
         (unless (string-match
                  "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'" value)
           (user-error "Time does not match HH:MM"))
         (setq hour (string-to-number (match-string 1 value))
               minute (string-to-number (match-string 2 value)))
         (unless (and (<= 0 hour 23) (<= 0 minute 59))
           (user-error "Invalid time: %s" text)))
        ('twelve-hour
         (unless (string-match
                  "\\`\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)[[:space:]]*\\(AM\\|PM\\)\\'"
                  value)
           (user-error "Time does not match H:MM AM/PM"))
         (setq hour (string-to-number (match-string 1 value))
               minute (string-to-number (match-string 2 value)))
         (unless (and (<= 1 hour 12) (<= 0 minute 59))
           (user-error "Invalid time: %s" text))
         (when (= hour 12) (setq hour 0))
         (when (equal (match-string 3 value) "PM")
           (setq hour (+ hour 12))))
        (_ (user-error "Unsupported time format: %S" format)))
      (format "%02d:%02d:00" hour minute))))

(defun perinf-time-format (iso-datetime format)
  "Format ISO-DATETIME time component using FORMAT."
  (if (not (and iso-datetime
                (string-match "T\\([0-9]\\{2\\}\\):\\([0-9]\\{2\\}\\)"
                              iso-datetime)))
      ""
    (let ((hour (string-to-number (match-string 1 iso-datetime)))
          (minute (string-to-number (match-string 2 iso-datetime))))
      (pcase format
        ('twenty-four-hour (format "%02d:%02d" hour minute))
        ('twelve-hour
         (format "%d:%02d %s"
                 (let ((display (% hour 12)))
                   (if (= display 0) 12 display))
                 minute
                 (if (< hour 12) "AM" "PM")))
        (_ (format "%02d:%02d" hour minute))))))

(provide 'perinf-time)

;;; perinf-time.el ends here
