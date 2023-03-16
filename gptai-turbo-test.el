;;; gptai-turbo-test.el --- Test Suite for gptai-turbo.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hibl, Anton

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; URL: https://github.com/antonhibl/gptai
;; Keywords: comm, convenience
;; Version: 1.0.4
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

;; A suite of tests for the gptai-turbo functions to ensure they work as
;; expected.  To run these tests either:

;; A. M-x ert will open the ERT interactive test runner, where you can select
;; the tests you want to run.  Press t to run all the tests, or use r to run a
;; specific test by its name.  The test results will be displayed in the ERT test
;; runner buffer.

;; B. By running (ert-run-tests-batch-and-exit), the tests will be executed in
;; batch mode, and the test results will be displayed in the *Messages*
;; buffer.  If all tests pass, you will see the message All tests passed.

;; These tests use a mock API response and a test function that simulates
;; gptai-turbo-request without making an actual API call to ensure that the
;; tests do not depend on the availability of the API and to avoid rate-limiting
;; issues.

;;; Code:

(require 'ert)
(require 'gptai)
(require 'gptai-turbo)

(defvar mock-api-response)

;; Mock API response
(setq mock-api-response
      '((id . "chatcmpl-6uaMfmuqdi2R0Zj5HSgKvcUjQceeJ")
        (object . "chat.completion")
        (created . 1678944173)
        (model . "gpt-3.5-turbo-0301")
        (usage (prompt_tokens . 11) (completion_tokens . 41) (total_tokens . 52))
        (choices . [((message (role . "assistant") (content . "This is a test response.")) (finish_reason . "stop") (index . 0))])))

;; Test function that simulates gptai-turbo-request without making an API call
;;;###autoload
(defun gptai-turbo-test-request ()
  "Simulates gptai-turbo-request without making an actual API call."
  (let ((first-choice (elt (cdr (assoc 'choices mock-api-response)) 0)))
    (cdr (assoc 'content (cdr (assoc 'message first-choice))))))

;;;###autoload
(ert-deftest gptai-turbo-test-request ()
  "Test gptai-turbo-request with a mock response."
  (let* ((prompt "Test prompt")
         (gptai-turbo-request (symbol-function 'gptai-turbo-request)))
    (fset 'gptai-turbo-request (symbol-function 'gptai-turbo-test-request))
    (should (string= (gptai-turbo-request prompt) "This is a test response."))
    (fset 'gptai-turbo-request gptai-turbo-request)))

;;;###autoload
(ert-deftest gptai-turbo-test-response ()
  "Test gptai-turbo-response with a mock response."
  (let* ((prompt "Test prompt")
         (gptai-turbo-request (symbol-function 'gptai-turbo-request))
         (buffer (generate-new-buffer "*gptai-turbo-response-test*")))
    (fset 'gptai-turbo-request (symbol-function 'gptai-turbo-test-request))
    (with-current-buffer buffer
      (gptai-turbo-response prompt)
      (should (string= (buffer-string) "This is a test response.")))
    (fset 'gptai-turbo-request gptai-turbo-request)
    (kill-buffer buffer)))

(provide 'gptai-turbo-test)
;;; gptai-turbo-test.el ends here
