;;; world-time-mode.el --- Show whole days of world-time diffs -*- lexical-binding: t -*-

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; URL: https://github.com/nicferrier/emacs-world-time-mode
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5"))
;; Keywords: tools, calendar
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.0.6

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Very useful productivity tool if you work in different time zones.

;;; Code:

(require 'cl-lib)
(require 'time)

;; Adapted from `time--display-world-list' in GNU Emacs 26 time.el.
(defun world-time--world-list ()
  "Return alist of time zones to show in `world-time-list'."
  (if (listp display-time-world-list)
      display-time-world-list
    ;; Determine if zoneinfo style timezones are supported by testing that
    ;; America/New York and Europe/London return different timezones.
    (let ((nyt (format-time-string "%z" nil "America/New_York"))
          (gmt (format-time-string "%z" nil "Europe/London")))
      (if (string-equal nyt gmt)
          (and (boundp 'legacy-style-world-list)
               (symbol-value 'legacy-style-world-list))
        (and (boundp 'zoneinfo-style-world-list)
             (symbol-value 'zoneinfo-style-world-list))))))

(defun world-time--zone-list (time)
  "Return the vector of zoned times for TIME."
  (apply 'vector
         (mapcar
          (lambda (zone)
            (let ((original (getenv "TZ")))
              (unwind-protect
                   (progn
                     (setenv "TZ" (car zone))
                     (list (format-time-string "%R %Z" time)))
                (setenv "TZ" original))))
          (world-time--world-list))))


(defun world-time--table-entries ()
  "Make the entry table for the list.

Based on the next hour after the current time."
  (let* ((currently (current-time))
         (time-now (time-to-seconds currently))
         (hours-since-epoch (/ time-now 3600))
         (last-hour (* 3600.00 (floor hours-since-epoch)))
         (next-hour (+ 3600.00 last-hour))
         (ref-time (seconds-to-time next-hour))
         (ref-list
          (mapcar
           (lambda (i)
             (list nil
                   (world-time--zone-list
                    (time-add ref-time (seconds-to-time (* 3600.00 i))))))
           (number-sequence 0 23))))
    (append (list (list nil (world-time--zone-list currently))) ref-list)))

(define-derived-mode
    world-time-table-mode tabulated-list-mode "World Time"
    "Major mode for seeing your world time list as a day."
    (setq tabulated-list-entries 'world-time--table-entries)
    ;; This is wrong! it needs to be derived from (world-time--world-list)
    (setq tabulated-list-format
          (cl-loop for time in (world-time--world-list)
             vconcat (list (list (car time) 20 nil))))
    (tabulated-list-init-header))

;;;###autoload
(defun world-time-list ()
  "Show `world-time--world-list' full day comparison."
  (interactive)
  (with-current-buffer (get-buffer-create "*world-time*")
    (world-time-table-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun list-world-time ()
  "Show `world-time--world-list' full day comparison."
  (interactive)
  (call-interactively 'world-time-list))

(provide 'world-time-mode)

;;; world-time-mode.el ends here
