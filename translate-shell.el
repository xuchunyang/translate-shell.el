;;; translate-shell.el --- an unofficial Emacs front-end for translate-shell (a command-line translator powered by Google Translate)

;; Copyright (C) 2015  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang56@gmail.com>
;; URL: https://github.com/xuchunyang/translate-shell.el
;; Created: Sat Jun 20 13:35:27 CST 2015
;; Version: 0.0.1
;; Keywords: google, translate

;; This file is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; `translate-shell.el' is an unofficial Emacs front-end for translate-shell
;; <https://github.com/soimort/translate-shell>. translate-shell is s a
;; command-line translator powered by Google Translate.
;;
;;
;; TODO
;; ====
;;
;; - Cache search result
;; - Save search history


;;; Code:

(defgroup translate-shell nil
  "An unofficial Emacs front-end for <https://github.com/soimort/translate-shell>."
  :group 'tools
  :prefix "translate-shell-")

(defcustom translate-shell-command "trans -brief -t zh %s"
  "The translate-shell command."
  :type 'string)

;;;###autoload
(defun translate-shell-brief (word)
  "Show the explanation of WORD using translate-shell in the echo area."
  (interactive
   (let* ((default (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (substring-no-properties (thing-at-point 'word))))
          (prompt (if (stringp default)
                      (format "Search (default \"%s\"): " default)
                    "Search : "))
          (string (read-string prompt nil nil default)))
     (list string)))
  (let ((result
         (shell-command-to-string
          (format translate-shell-command (shell-quote-argument word)))))
    (message (substring result
                        0 (string-match "\n" result)))))

(provide 'translate-shell)
;;; translate-shell.el ends here
