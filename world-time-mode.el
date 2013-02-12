;;; world-time-mode.el --- show whole days of world-time diffs

;; Copyright (C) 2013  Nic Ferrier

;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: tools, calendar
;; Version: 0.0.1

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

(defun world-time/table-entrys ()
  "For listing the entries of the world-time day."
  (let ((zones-alist display-time-world-list)
        (old-tz (getenv "TZ"))
        (max-width 0)
        result)
    (unwind-protect
         (setq result
               (loop for i from 0 to 23
                  collect
                    (let ((my-time (time-add
                                    (current-time)
                                    (seconds-to-time (* i 3600)))))
                      (list nil
                            (loop for zone in zones-alist
                               vconcat
                                 (progn
                                   (setenv "TZ" (car zone))
                                   (list (format-time-string
                                    "%R %Z" ; display-time-world-time-format
                                    my-time))))))))
      (setenv "TZ" old-tz))
    result))

(define-derived-mode
    world-time-table-mode tabulated-list-mode "World Time"
    "Major mode for seeing your world time list as a day."
    (setq tabulated-list-entries 'world-time/table-entrys)
    (setq tabulated-list-format
          [("America/Los_Angeles" 20 nil)
           ("America/New_York" 20 nil)
           ("Europe/London"    20 nil)
           ("Europe/Berlin"    20 nil)
           ("Asia/Calcutta"    20 nil)])
    (tabulated-list-init-header))

;;;###autoload
(defun world-time-list ()
  "Show `display-time-world-list' full day comparison."
  (interactive)
  (with-current-buffer (get-buffer-create "*world-time*")
    (world-time-table-mode)
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

(provide 'world-time-mode)

;;; world-time-mode.el ends here
