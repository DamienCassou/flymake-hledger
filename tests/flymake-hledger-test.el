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

(cl-defun flymake-hledger-test--compare-diagnostics (diagnostic &key beg end text)
  "Compare DIAGNOSTIC to BEG, END and TEXT."
  (should (equal (flymake-diagnostic-beg diagnostic) beg))
  (should (equal (flymake-diagnostic-end diagnostic) end))
  (should (equal (flymake-diagnostic-text diagnostic) text)))

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
      (should (length= diagnostics 4))
      (flymake-hledger-test--compare-diagnostics
       (seq-elt diagnostics 0)
       :beg 1 :end 14 :text "message 1\n")
      (flymake-hledger-test--compare-diagnostics
       (seq-elt diagnostics 1)
       :beg 15 :end 20 :text "message 2.1\nmessage 2.2\n")
      (flymake-hledger-test--compare-diagnostics
       (seq-elt diagnostics 2)
       :beg 22 :end 28 :text "message 3\n")
      (flymake-hledger-test--compare-diagnostics
       (seq-elt diagnostics 3)
       :beg 29 :end 35 :text "message 4.1\nmessage 4.2\n"))))

(provide 'flymake-hledger-test)
;;; flymake-hledger-test.el ends here
