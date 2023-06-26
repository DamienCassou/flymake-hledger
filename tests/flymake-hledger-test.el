;;; flymake-hledger-test.el --- Tests for flymake-hledger  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords:

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

;; Tests for flymake-hledger

;;; Code:
(require 'ert)
(require 'flymake-hledger)

(ert-deftest flymake-hledger-test--make-diagnostics ()
  (let* ((source-buffer (generate-new-buffer "flymake-hledger-test"))
         (process-buffer (generate-new-buffer "flymake-hledger-test")))
    (with-current-buffer process-buffer
      (insert
       ;; <start-line>-<end-line>:
       "hledger: Error: -:1-2:\n" " ^\n" "message 1\n"
       ;; <start-line>:<start-column>-<end-column>:
       "hledger: Error: -:3:1-5:\n" " ^\n" "message 2.1\n" "message 2.2\n"
       ;; <start-line>:<start-column>:
       "hledger: Error: -:4:1:\n" " ^\n" "message 3\n"
       ;; <start-line>:
       "hledger: Error: -:5:\n" " ^\n" "message 4.1\n" "message 4.2\n"))
    (with-current-buffer source-buffer
      (insert "line 1\n" "line 2\n" "line 3\n" "line 4\n" "line 5\n"))
    (let ((diagnostics (with-current-buffer process-buffer
                         (flymake-hledger--make-diagnostics source-buffer))))
      (message "coucou: %S" diagnostics)
      (should (length= diagnostics 4))
      (should (equal (flymake-diagnostic-beg (seq-elt diagnostics 0))
                     1))
      (should (equal (flymake-diagnostic-end (seq-elt diagnostics 0))
                     14))
      (should (equal (flymake-diagnostic-text (seq-elt diagnostics 0))
                     "message 1\n"))
      (should (equal (flymake-diagnostic-beg (seq-elt diagnostics 1))
                     15))
      (should (equal (flymake-diagnostic-end (seq-elt diagnostics 1))
                     20))
      (should (equal (flymake-diagnostic-text (seq-elt diagnostics 1))
                     "message 2.1\nmessage 2.2\n"))
      (should (equal (flymake-diagnostic-beg (seq-elt diagnostics 2))
                     22))
      (should (equal (flymake-diagnostic-end (seq-elt diagnostics 2))
                     28))
      (should (equal (flymake-diagnostic-text (seq-elt diagnostics 2))
                     "message 3\n"))
      (should (equal (flymake-diagnostic-beg (seq-elt diagnostics 3))
                     29))
      (should (equal (flymake-diagnostic-end (seq-elt diagnostics 3))
                     35))
      (should (equal (flymake-diagnostic-text (seq-elt diagnostics 3))
                     "message 4.1\nmessage 4.2\n")))))

(provide 'flymake-hledger-test)
;;; flymake-hledger-test.el ends here
