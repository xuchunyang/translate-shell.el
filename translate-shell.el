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


;;; Code:

(defgroup translate-shell nil
  "An unofficial Emacs front-end for <https://github.com/soimort/translate-shell>."
  :group 'tools
  :prefix "translate-shell-")

(defcustom translate-shell-command "trans -t zh %s"
  "The translate-shell command for the `translate-shell' command."
  :type 'string)

(defcustom translate-shell-brief-command "trans -brief -t zh %s"
  "The translate-shell command for the `translate-shell-brief' command."
  :type 'string)

(defvar translate-shell-history nil)

(defvar translate-shell-brief-cache nil) ; type: alist, format: (('word . "explanation"))
(defvar translate-shell-cache nil) ; type: alist, format: (('word . "explanation"))

(defun translate-shell--read-string ()
  "A `read-string' wrapper for translate-shell."
  (let* ((default (if (use-region-p)
                      (buffer-substring-no-properties
                       (region-beginning) (region-end))
                    (let ((word (thing-at-point 'word)))
                      (when word (substring-no-properties word)))))
         (prompt (if (stringp default)
                     (format "Search (default \"%s\"): " default)
                   "Search : ")))
    (read-string prompt nil 'translate-shell-history default)))

;;;###autoload
(defun translate-shell-brief (word)
  "Show the explanation of WORD in the echo area."
  (interactive
   (list (translate-shell--read-string)))
  (let ((word-sym (intern word)))
    (if (assq word-sym translate-shell-brief-cache)
        (message (assoc-default word-sym translate-shell-brief-cache))
      (let* ((output
              (shell-command-to-string
               (format translate-shell-brief-command (shell-quote-argument word))))
             (result (substring output
                                0 (string-match "\n" output))))
        (message result)
        (add-to-list 'translate-shell-brief-cache (cons word-sym result))))))

;;;###autoload
(defun translate-shell (word)
  "Show the explanation of WORD in buffer."
  (interactive
   (list (translate-shell--read-string)))
  (let ((word-sym (intern word))
        result)
    (if (assq word-sym translate-shell-cache)
        (setq result (assoc-default word-sym translate-shell-cache))
      (setq result (shell-command-to-string (format translate-shell-command
                                                    (shell-quote-argument word))))
      (add-to-list 'translate-shell-cache (cons word-sym result)))
    (with-current-buffer (get-buffer-create "*Translate Shell*")
      (erase-buffer)
      (insert result)
      (display-buffer (current-buffer)))))

(provide 'translate-shell)
;;; translate-shell.el ends here
