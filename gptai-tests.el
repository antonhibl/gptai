;;; gptai-test.el --- test suite for gptai -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Hibl, Anton

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; URL: https://github.com/antonhibl/gptai
;; Keywords: comm, convenience
;; Version: 0.1.0
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

(defun gptai-mock-request (gptai-prompt)
  "Mock GPTAI-request function to simulate API call without actually calling it.
Argument GPTAI-PROMPT is the prompt to send to the API."
  (let ((response `((id . "test-id")
                    (object . "test-object")
                    (created . 1234567890)
                    (model . ,gptai-model)
                    (usage . ((prompt_tokens . 10)
                              (completion_tokens . 20)
                              (total_tokens . 30)))
                    (choices . [((text . "Test response for prompt: ")
                                 (index . 0)
                                 (finish_reason . "stop")
                                 (logprobs . nil))]))))
    response))

(defun gptai-test-send-query (gptai-prompt)
  "Test function for gptai-send-query using mock API response.
Argument GPTAI-PROMPT prompt."
  (let ((response (gptai-mock-request gptai-prompt)))
    (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
      (if text
          (insert text)
        (error "Response doesn't contain text data")))))

(ert-deftest gptai-test-query-functions ()
  "Test gptai-send-query and other query functions using the mock request."
  (let ((gptai-prompt "Test prompt")
        (gptai-request-function #'gptai-mock-request))

    ;; Test gptai-send-query
    (with-temp-buffer
      (gptai-test-send-query gptai-prompt)
      (should (equal (buffer-string) "Test response for prompt: ")))

    ;; Test gptai-send-query-region
    (with-temp-buffer
      (insert gptai-prompt)
      (goto-char (point-min))
      (push-mark)
      (goto-char (point-max))
      (activate-mark)
      (gptai-send-query-region)
      (should (equal (buffer-string) "Test response for prompt: ")))

    ;; Test gptai-send-chat
    (with-temp-buffer
      (gptai-send-chat gptai-prompt)
      (should (equal (buffer-string) "Test response for prompt: ")))

    ;; Test gptai-open-chat
    (gptai-open-chat gptai-prompt)
    (should (equal (buffer-string) (concat gptai-prompt "\n\nChatGPT:Test response for prompt: ")))

    ;; Test gptai-continue-chat
    (with-current-buffer "*chat-gptai*"
      (gptai-continue-chat)
      (should (equal (buffer-string) (concat gptai-prompt "\n\nChatGPT:Test response for prompt: "
                                             "Test response for prompt: ")))

    ;; Test gptai-send-query-buffer
    (with-temp-buffer
      (insert gptai-prompt)
      (gptai-send-query-buffer)
      (should (equal (buffer-string) "Test response for prompt: "))))))

(provide 'gptai-test)
;;; gptai-test.el ends here
