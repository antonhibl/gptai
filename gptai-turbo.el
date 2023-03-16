;;; gptai-turbo.el --- Integrate with the gpt-3.5-turbo model -*- lexical-binding: t; -*-

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

;; A rough start to integrating the newer chat endpoints into emacs, working
;; towards a larger chat functionality for this package and the editor as a
;; whole.

;;; Code:

(defun gptai-turbo-request (gptai-prompt)
  "Sends a request to OpenAI API's gpt-3.5-turbo endpoint and returns the response.
Argument GPTAI-PROMPT is the prompt to send to the API."
  (when (null gptai-api-key)
    (error "OpenAI API key is not set"))

  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" gptai-api-key))))
         (url-request-data
          (json-encode `(("model" . "gpt-3.5-turbo")
                         ("messages" . [((role . "user") (content . ,gptai-prompt))])
                         ("temperature" . 0.7))))
         (url "https://api.openai.com/v1/chat/completions")
         (buffer (url-retrieve-synchronously url nil 'silent))
         response)

    (message "Sending request to OpenAI API using model 'gpt-3.5-turbo'")

    (if buffer
      (with-current-buffer buffer
        (goto-char url-http-end-of-headers)
        (condition-case gptai-err
            (progn
              (setq response (json-read))
              (if (assoc 'error response)
                  (error (cdr (assoc 'message (cdr (assoc 'error response)))))
                (let ((first-choice (elt (cdr (assoc 'choices response)) 0))) ; Extract the first choice
                  (cdr (assoc 'content (cdr (assoc 'message first-choice))))))) ; Get the 'content' field of the first choice
          (error (error "Error while parsing OpenAI API response: %s"
                        (error-message-string gptai-err)))))
      (error "Failed to send request to OpenAI API"))))

(defun gptai-turbo-response (gptai-prompt)
  "Sends a request to gpt-3.5-turbo and inserts response at the current point.
Argument GPTAI-PROMPT prompt to be sent."
  (interactive "sEnter your prompt: ")
  (let ((response (gptai-turbo-request gptai-prompt)))
    (insert response)))

(provide 'gptai-turbo)
;;; gptai-turbo.el ends here
