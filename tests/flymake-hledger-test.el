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

(defconst flymake-hledger-test--source
  (mapconcat (lambda (n) (format "line %2d of the journal\n" n)) (number-sequence 1 12) "")
  "Contents of the journal being checked, in the tests below.
Each line is 22 characters long, so line N starts at position 1+23*(N-1).")

(defun flymake-hledger-test--diagnose (output)
  "Return the diagnostics flymake-hledger makes from hledger's OUTPUT."
  (let ((source-buffer (generate-new-buffer "flymake-hledger-test-source")))
    (unwind-protect
        (progn
          (with-current-buffer source-buffer (insert flymake-hledger-test--source))
          (with-temp-buffer
            (insert output)
            (flymake-hledger--make-diagnostics source-buffer)))
      (kill-buffer source-buffer))))

(cl-defun flymake-hledger-test--compare-diagnostics (diagnostic &key beg end text)
  "Compare DIAGNOSTIC to BEG, END and TEXT."
  (should (equal (flymake-diagnostic-beg diagnostic) beg))
  (should (equal (flymake-diagnostic-end diagnostic) end))
  (should (equal (flymake-diagnostic-text diagnostic) text)))

(defmacro flymake-hledger-test--deftest (name output &rest expected)
  "Define a test called NAME, parsing hledger's OUTPUT.
EXPECTED are the keyword arguments of
`flymake-hledger-test--compare-diagnostics', describing the only
diagnostic expected."
  (declare (indent 1))
  `(ert-deftest ,(intern (format "flymake-hledger-test--%s" name)) ()
     (let ((diagnostics (flymake-hledger-test--diagnose ,output)))
       (should (length= diagnostics 1))
       (flymake-hledger-test--compare-diagnostics (car diagnostics) ,@expected))))

(flymake-hledger-test--deftest line
  ;; hledger 2
  "hledger: Error: -:4:
  | 2022-01-01
4 |     (a)                                            1
  |      ^

Strict account checking is enabled, and
account \"a\" has not been declared.
Consider adding an account directive. Examples:

account a
"
  :beg 70 :end 92
  :text "Strict account checking is enabled, and
account \"a\" has not been declared.
Consider adding an account directive. Examples:

account a")

(flymake-hledger-test--deftest line-range
  "hledger: Error: -:5-7:
5 | 2022-01-01
  |     a                                              1 A
  |     b                                             -1 B

This multi-commodity transaction is unbalanced.
The real postings' sum should be 0 but is 1 A, -1 B
"
  :beg 93 :end 161
  :text "This multi-commodity transaction is unbalanced.
The real postings' sum should be 0 but is 1 A, -1 B")

(flymake-hledger-test--deftest line-column
  ;; No empty line before the message
  "hledger: Error: -:3:2:
  |
3 | 1
  |  ^
unexpected newline
expecting date separator or digit
"
  :beg 48 :end 69
  :text "unexpected newline
expecting date separator or digit")

(flymake-hledger-test--deftest line-column-range
  "hledger: Error: -:3:6-7:
3 | line  3 of the journal
  |      ^^

message
"
  :beg 52 :end 54
  :text "message")

(flymake-hledger-test--deftest two-excerpts-hledger-2
  "hledger: Error: -:10:
7 | 2022-01-02 p
  |     (a)                                            1

10 | 2022-01-01 p
   | ^^^^^^^^^^
   |     (a)                                            1

Ordered dates checking is enabled, and this transaction's
date (2022-01-01) is out of order with the previous transaction.
"
  :beg 208 :end 230
  :text "Ordered dates checking is enabled, and this transaction's
date (2022-01-01) is out of order with the previous transaction.")

(flymake-hledger-test--deftest two-excerpts-hledger-1
  ;; hledger 1 separated the excerpts by a line containing a space
  (concat "hledger: Error: -:10:
7 | 2022-01-02 p
  |     (a)               1
" " \n" "10 | 2022-01-01 p
   | ^^^^^^^^^^
   |     (a)               1

Ordered dates checking is enabled, and this transaction's
date (2022-01-01) is out of order with the previous transaction.
")
  :beg 208 :end 230
  :text "Ordered dates checking is enabled, and this transaction's
date (2022-01-01) is out of order with the previous transaction.")

(flymake-hledger-test--deftest two-excerpts-hledger-1-ellipsis
  ;; or by a " ..." line
  "hledger: Error: -:12:
3 | account a:c
 ...
12 |   (b:c)  1
   |    ^^^

Checking for unique account leaf names is enabled, and
account leaf name \"c\" is not unique.
"
  :beg 254 :end 276
  :text "Checking for unique account leaf names is enabled, and
account leaf name \"c\" is not unique.")

(flymake-hledger-test--deftest windows
  "hledger.exe: Error: -:5:
5 | 2022-01-01
  |     a                                              1

This transaction is unbalanced.
"
  :beg 93 :end 115
  :text "This transaction is unbalanced.")

(flymake-hledger-test--deftest no-position
  "hledger: Error: sorry, CSV files can't be included yet
"
  :beg 1 :end 23
  :text "sorry, CSV files can't be included yet")

(flymake-hledger-test--deftest no-position-several-lines
  "hledger: Error: first line
second line
"
  :beg 1 :end 23
  :text "first line
second line")

(flymake-hledger-test--deftest included-file
  ;; Reported on the first line, with the other file's name and position
  (format "hledger: Error: %s:2-4:
2 | 2022-01-02 bad
  |     a                                              1
  |     b                                              2

This transaction is unbalanced.
The real postings all have the same sign.
" (expand-file-name "inc.j"))
  :beg 1 :end 23
  :text "In inc.j:2-4:
This transaction is unbalanced.
The real postings all have the same sign.")

(flymake-hledger-test--deftest position-out-of-range
  ;; Eg because the buffer has changed: Flymake uses the last line
  "hledger: Error: -:99:
99 | 2022-01-01
   |     a                                              1

This transaction is unbalanced.
"
  :beg 254 :end 276
  :text "This transaction is unbalanced.")

(ert-deftest flymake-hledger-test--no-error ()
  (should (null (flymake-hledger-test--diagnose "")))
  (should (null (flymake-hledger-test--diagnose "some other output\n"))))

(provide 'flymake-hledger-test)
;;; flymake-hledger-test.el ends here
