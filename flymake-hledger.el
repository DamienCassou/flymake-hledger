;;; flymake-hledger.el --- Flymake module to check hledger journals  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; URL: https://github.com/DamienCassou/flymake-hledger
;; Package-Requires: ((emacs "28.2"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds an hledger [1] checker to Flymake.
;;
;; [1] https://hledger.org/

;;; Code:
(require 'flymake)
(require 'subr-x)  ; string-trim, in Emacs 28

(defgroup flymake-hledger nil
  "Flymake module to check hledger journals."
  :group 'flymake)

(defcustom flymake-hledger-command '("hledger")
  "List of a program and its arguments to start hledger."
  :type '(repeat string))

(defcustom flymake-hledger-checks '("accounts" "commodities" "balanced")
  "List of tests to run on the journal.
See URL `https://hledger.org/1.41/hledger.html#check' for the meaning of each check."
  :type '(set
          ;; Strict checks
          (const "accounts")
          (const "balanced")
          (const "commodities")

          ;; Other checks
          (const "ordereddates")
          (const "payees")
          (const "recentassertions")
          (const "tags")
          (const "uniqueleafnames")))

(defvar-local flymake-hledger--process nil
  "The process executing hledger for the current buffer.")

;;;###autoload
(defun flymake-hledger-enable ()
  "Enable checking the hledger journal in the current buffer."
  (interactive)
  (flymake-mode t)
  (add-hook 'flymake-diagnostic-functions #'flymake-hledger-check-buffer nil t))

(defun flymake-hledger-disable ()
  "Stop checking the hledger journal in the current buffer."
  (interactive)
  (flymake-mode -1)
  (remove-hook 'flymake-diagnostic-functions #'flymake-hledger-check-buffer t))

;;;###autoload
(defun flymake-hledger-check-buffer (report-fn &rest _)
  "Start a hledger process on the current buffer and report to Flymake.
This function is meant to be added to `flymake-diagnostic-functions'.

REPORT-FN is Flymake's callback."
  (unless (flymake-hledger--should-enable-p)
    (user-error "The flymake-hledger backend is not meant for this buffer"))
  (when (process-live-p flymake-hledger--process)
    (kill-process flymake-hledger--process))
  (flymake-hledger--start-hledger-process report-fn))

(defun flymake-hledger--should-enable-p ()
  "Return non-nil if flymake-hledger should be enabled in the current buffer."
  (or
   ;; Either the user is using `hledger-mode':
   (derived-mode-p 'hledger-mode)
   ;; or the user is using `ledger-mode' with the binary path pointing
   ;; to "hledger":
   (and (bound-and-true-p ledger-binary-path)
        (string-suffix-p "hledger" ledger-binary-path))))

(defun flymake-hledger--start-hledger-process (report-fn)
  "Start hledger and report problems through REPORT-FN.

REPORT-FN is Flymake's callback."
  (let ((error-buffer (generate-new-buffer "*flymake-hledger*"))
        (source-buffer (current-buffer)))
    (setq flymake-hledger--process
          (make-process
           :name "flymake-hledger"
           :buffer error-buffer
           :command `(,@flymake-hledger-command "-f" "-" "check" ,@flymake-hledger-checks)
           :noquery t
           :connection-type 'pipe
           :sentinel (apply-partially #'flymake-hledger--sentinel source-buffer report-fn)))
    (save-restriction
      (widen)
      (process-send-region flymake-hledger--process (point-min) (point-max)))
    (process-send-eof flymake-hledger--process)))

(defun flymake-hledger--sentinel (source-buffer report-fn process _)
  "Parse output of the hledger process if finished.

SOURCE-BUFFER is the buffer containing the hledger journal to check.
REPORT-FN is Flymake's callback.  PROCESS is the system process running
hledger."
  (unwind-protect
      (when (and (buffer-live-p source-buffer) (eq 'exit (process-status process)))
        (flymake-hledger--handle-process-exit source-buffer report-fn process))
    (unless (process-live-p process)
      (kill-buffer (process-buffer process)))))

(defun flymake-hledger--handle-process-exit (source-buffer report-fn process)
  "Parse output of hledger in the PROCESS' buffer and report diagnostics.

SOURCE-BUFFER is the buffer containing the hledger journal to check.
REPORT-FN is Flymake's callback.  PROCESS is the system process running
hledger."
  (when (with-current-buffer source-buffer (eq process flymake-hledger--process))
    (with-current-buffer (process-buffer process)
      (let ((diags (flymake-hledger--make-diagnostics source-buffer)))
        (if (or diags (zerop (process-exit-status process)))
            (funcall report-fn diags)
          (funcall report-fn
                   :panic
                   :explanation
                   (buffer-substring (point-min) (point-max))))))))

;; hledger error messages have changed over time.  Here we support
;; hledger 1.40 and newer, including hledger 2.  Most error messages
;; start with a line like "hledger: Error: FILE:LINE[-ENDLINE][:COL[-ENDCOL]]:",
;; followed by an excerpt of the journal, then an empty line and one or
;; more lines of explanation, which we use as the diagnostic's text.  Eg
;; (see also https://github.com/simonmichael/hledger/tree/master/hledger/test/errors):
;;
;;     hledger: Error: -:4:8:
;;       | 2022-01-01
;;     4 |     a               0 = 1
;;       |                       ^^^
;;
;;     Balance assertion failed in a
;;     ...
;;
;; FILE is "-" (the standard input, where hledger reads the buffer from)
;; unless the error is in an included file.  Some errors (ordereddates,
;; uniqueleafnames) show two excerpts, separated by an empty line
;; (hledger 2) or by a " " or " ..." line (hledger 1).  Some errors have
;; no position, eg "hledger: Error: sorry, CSV files can't be included yet".
(defconst flymake-hledger--error-regexp
  (rx bol "hledger" (optional ".exe") ": Error: "
      (or
       ;; An error with a position, and one or more excerpts:
       (seq (group-n 6 (optional alpha ":") (one-or-more (not (any ":\n"))))
            ":"
            (group-n 7
              (group-n 1 (one-or-more digit))
              (optional "-" (group-n 3 (one-or-more digit)))
              (optional ":" (group-n 2 (one-or-more digit))
                        (optional "-" (group-n 4 (one-or-more digit)))))
            ":\n"
            (one-or-more
             (one-or-more (or (seq (one-or-more digit) " ") (>= 2 " "))
                          "|" (zero-or-more not-newline) "\n")
             (optional (optional " " (optional "...")) "\n"))
            (group-n 5 (zero-or-more anychar)))
       ;; An error without a position:
       (group-n 5 (zero-or-more anychar))))
  "Regular expression matching an hledger error.

Group 1 is for the starting line.
Group 2 is for the starting column.
Group 3 is for the ending line.
Group 4 is for the ending column.
Group 5 is for the error message.
Group 6 is for the file name.
Group 7 is for the whole position (lines and columns).")

(defun flymake-hledger--make-diagnostics (source-buffer)
  "Return a diagnostic for the hledger error in the current buffer, if any.

SOURCE-BUFFER should contain the ledger being checked.  The current buffer
should contain hledger's output.  hledger stops at the first error, so
this returns a list of at most one diagnostic."
  (goto-char (point-min))
  (save-match-data
    (when (re-search-forward flymake-hledger--error-regexp nil t)
      (let* ((number (lambda (group) (when (match-string group)
                                        (string-to-number (match-string group)))))
             (line (funcall number 1))
             (file (match-string 6))
             (message (string-trim (match-string 5)))
             (region
              (if (equal file "-")
                  (flymake-hledger--region source-buffer
                                           line (funcall number 2)
                                           (or (funcall number 3) line) (funcall number 4))
                ;; The error has no position, or is in another file
                ;; (included by this one): show it on the first line.
                (flymake-hledger--region source-buffer 1))))
        (when (and file (not (equal file "-")))
          (setq message (format "In %s:%s:\n%s"
                                (file-relative-name
                                 file (buffer-local-value 'default-directory source-buffer))
                                (match-string 7)
                                message)))
        (list (flymake-make-diagnostic source-buffer (car region) (cdr region)
                                       :error message))))))

(defun flymake-hledger--region (buffer line &optional column end-line end-column)
  "Return BUFFER's region (BEG . END) for an hledger error position.

LINE, COLUMN, END-LINE and END-COLUMN are that position.  If Flymake
can't find a region for it, return the first line's region."
  (let ((start (flymake-diag-region buffer line column))
        (end (flymake-diag-region buffer (or end-line line) end-column)))
    (if (and start end)
        (cons (car start) (cdr end))
      (or (flymake-diag-region buffer 1) (cons 1 1)))))

(provide 'flymake-hledger)
;;; flymake-hledger.el ends here

;; LocalWords:  Flymake flymake hledger backend ordereddates REPORT-FN
;; LocalWords:  balancednoautoconversion recentassertions uniqueleafnames
