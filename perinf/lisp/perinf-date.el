;;; perinf-date.el --- Date normalization for Personal Work and Information System -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(defun perinf-date--valid-p (year month day)
  "Return non-nil when YEAR, MONTH, and DAY form a real date."
  (condition-case nil
      (let* ((time (encode-time 0 0 12 day month year))
             (decoded (decode-time time)))
        (and (= year (decoded-time-year decoded))
             (= month (decoded-time-month decoded))
             (= day (decoded-time-day decoded))))
    (error nil)))

(defun perinf-date-normalize (text format)
  "Normalize date TEXT using FORMAT and return YYYY-MM-DD.
An empty TEXT returns nil.  Numeric dates are interpreted only according to
the explicitly selected FORMAT."
  (let ((value (string-trim text))
        year month day)
    (unless (string-empty-p value)
      (let ((regexp
             (pcase format
               ('iso
                "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)\\'")
               ('day-month-year-dash
                "\\`\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{1,2\\}\\)-\\([0-9]\\{4\\}\\)\\'")
               ((or 'day-month-year-slash 'month-day-year-slash)
                "\\`\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{1,2\\}\\)/\\([0-9]\\{4\\}\\)\\'")
               (_ (user-error "Date input is not implemented for: %S"
                              format)))))
        (unless (string-match regexp value)
          (user-error "Date does not match the selected format"))
        (pcase format
          ('iso
           (setq year (string-to-number (match-string 1 value))
                 month (string-to-number (match-string 2 value))
                 day (string-to-number (match-string 3 value))))
          ((or 'day-month-year-dash 'day-month-year-slash)
           (setq day (string-to-number (match-string 1 value))
                 month (string-to-number (match-string 2 value))
                 year (string-to-number (match-string 3 value))))
          ('month-day-year-slash
           (setq month (string-to-number (match-string 1 value))
                 day (string-to-number (match-string 2 value))
                 year (string-to-number (match-string 3 value))))))
      (unless (perinf-date--valid-p year month day)
        (user-error "Invalid date: %s" value))
      (format "%04d-%02d-%02d" year month day))))

(defun perinf-date-format (iso-date format)
  "Format ISO-DATE for display using FORMAT."
  (if (not iso-date)
      ""
    (unless (string-match
             "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
             iso-date)
      (error "Invalid ISO date: %s" iso-date))
    (let ((year (match-string 1 iso-date))
          (month (match-string 2 iso-date))
          (day (match-string 3 iso-date)))
      (pcase format
        ('iso iso-date)
        ('day-month-year-dash (format "%s-%s-%s" day month year))
        ('day-month-year-slash (format "%s/%s/%s" day month year))
        ('month-day-year-slash (format "%s/%s/%s" month day year))
        (_ iso-date)))))

(provide 'perinf-date)

;;; perinf-date.el ends here
