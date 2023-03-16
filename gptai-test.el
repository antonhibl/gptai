;;; gptai-test.el --- test suite for gptai -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hibl, Anton

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; URL: https://github.com/antonhibl/gptai
;; Keywords: comm, convenience
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.1"))

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

;; A suite of tests for the gptai functions to ensure they work as
;; expected. To run these tests either:

;; A. M-x ert will open the ERT interactive test runner, where you can select
;; the tests you want to run. Press t to run all the tests, or use r to run a
;; specific test by its name. The test results will be displayed in the ERT test
;; runner buffer. 

;; B. By running (ert-run-tests-batch-and-exit), the tests will be executed in
;; batch mode, and the test results will be displayed in the *Messages*
;; buffer. If all tests pass, you will see the message All tests passed.

;;; Code:

(require 'ert)
(require 'gptai) ; assuming the provided code is saved in gptai.el

;;;###autoload
(defvar mock-api-response
  '((choices . [((text . "Mock API response text.") (index . 0))]))
  "A mock API response for testing purposes.")

;;;###autoload
(defun gptai-request-mock (gptai-prompt)
  "Simulates a GPTAI request without making an actual API call.
Argument GPTAI-PROMPT is the prompt to send to the API."
  mock-api-response)

;;;###autoload
(defun gptai-send-query-mock (gptai-prompt)
  "Sends a query to the mock GPTAI API and inserts the response text at the current point.
Argument GPTAI-PROMPT prompt."
  (interactive
   (list (read-string "Prompt: ")))
  (let ((response (gptai-request-mock gptai-prompt)))
    (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
      (if text
          (insert text)
        (error
         "Response doesn't contain text data")))))

;;;###autoload
(ert-deftest gptai-request-mock-test ()
  (should (equal (gptai-request-mock "test prompt") mock-api-response)))

;;;###autoload
(ert-deftest gptai-send-query-mock-test ()
  (with-temp-buffer
    (gptai-send-query-mock "test prompt")
    (should (equal (buffer-string) "Mock API response text."))))

(provide 'gptai-test)
;;; gptai-test.el ends here
