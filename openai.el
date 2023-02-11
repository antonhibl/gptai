;;; openai.el --- Integrate with the OpenAI API -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Anton Hibl <antonhibl11@gmail.com>
;; URL: https://github.com/antonhibl/openai
;; Keywords: AI
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is intended to allow for development and programming queries into the
;; OpenAI API.  This allows for sending queries stright from Emacs directly into
;; various models of OpenAI's platform.

;; See the accompanying Readme.org for configuration details.

;;; Code:

;;; Customization
(defgroup openai nil
  "OpenAI API."
  :group 'emacs)

;; dependencies
(require 'url)
(require 'json)

;; default values for local variables
(defvar openai-base-url "https://api.openai.com/v1/completions")
(defvar openai-model nil)
(defvar openai-api-key nil)

;; parse prompt into a request for the openai API
(defun openai-request (openai-prompt)
  "Sends a request to OpenAI API and return the response.
Argument OPENAI-PROMPT prompt."
  (when (null openai-api-key)
    (error "OpenAI API key is not set"))
  (let* ((url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" openai-api-key))))
         (url-request-data
          (json-encode `(("model" . ,openai-model)
                         ("prompt" . ,openai-prompt)
                         ("temperature" . 0.7)
                         ("max_tokens" . 1000)))))
    (message "Sending request to OpenAI API using model '%s'" openai-model)
    (condition-case openai-err
        (with-current-buffer
            (url-retrieve-synchronously openai-base-url nil 'silent)
          (goto-char url-http-end-of-headers)
          (let ((response (json-read)))
            (when (assoc 'error response)
              (error (cdr (assoc 'message (cdr (assoc 'error response))))))
            response))
      (error (error "Error while sending request to OpenAI API: %s"
                    (error-message-string openai-err))))))

;; standard way to send a query, interactively prompts user in emacs
(defun openai-send-query (openai-prompt)
  "Sends a query to OpenAI API and displays the response in a new buffer.
Argument OPENAI-PROMPT prompt."
  (interactive
   (list (read-string "Query: ")))
  (with-current-buffer (get-buffer-create "*openai*")
    (goto-char (point-max))
    (insert "===============\n")
    (insert (format "Request: %s\n" openai-prompt))
    (let ((response (openai-request openai-prompt)))
      (insert (format "Response: %s\n" response))
      (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
        (insert (format "Text: %s\n" text))))
    (insert "===============\n")
    (display-buffer (current-buffer) t)))

;; send selection text as prompt
(defun openai-send-query-from-selection ()
  "Sends query to OpenAI API using selected text as prompt."
  (interactive)
  (let ((openai-prompt (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Query: "))))
    (with-current-buffer (get-buffer-create "*openai*")
      (goto-char (point-max))
      (insert "===============\n")
      (insert (format "Request: %s\n" openai-prompt))
      (let ((response (openai-request openai-prompt)))
        (insert (format "Response: %s\n" response))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response)) 0)))))
          (insert (format "Text: %s\n" text))))
      (insert "===============\n")
      (display-buffer (current-buffer) t))))

;; send buffer's text as prompts
(defun openai-send-query-from-buffer (&optional buffer-name)
  "Sends a query to OpenAI API and displays the response in a new buffer.
Optional argument BUFFER-NAME buffer to prompt from."
  (interactive
   (list (read-buffer "Buffer: " (current-buffer))))
  (let ((openai-prompt (with-current-buffer buffer-name
                  (buffer-substring (point-min) (point-max)))))
    (with-current-buffer (get-buffer-create "*openai*")
      (goto-char (point-max))
      (insert "===============\n")
      (insert (format "Request: %s\n" openai-prompt))
      (let ((response (openai-request openai-prompt)))
        (insert (format "Response: %s\n" response))
        (let ((text (cdr (assoc 'text (elt (cdr (assoc 'choices response))
                                           0)))))
          (insert (format "Text: %s\n" text))))
      (insert "===============\n")
      (display-buffer (current-buffer) t))))

(defun openai-send-image-query (prompt n size filepath)
  "Sends a query to OpenAI Image Generation API and displays the response in a new buffer and downloads the generated images."
  (interactive
   (list (read-string "Prompt: ")
         (read-number "Number of images to generate: " 1)
         (read-string "Image size (e.g. 1024x1024): " "1024x1024")
         (read-directory-name "Enter output directory: " "~/Pictures")))
  (when (null openai-api-key)
    (error "OpenAI API key is not set"))
  (let* ((url "https://api.openai.com/v1/images/generations")
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(format "Bearer %s" openai-api-key))))
         (url-request-data
          (json-encode `(("prompt" . ,prompt)
                         ("n" . ,n)
                         ("size" . ,size)))))
    (message "Sending request to OpenAI Image Generation API")
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously url nil 'silent)
          (goto-char url-http-end-of-headers)
          (let ((response (json-read)))
            (when (assoc 'error response)
              (error (cdr (assoc 'message (cdr (assoc 'error response))))))
            (with-current-buffer (get-buffer-create "*openai*")
              (erase-buffer)
              (insert (format "Generated image URLs:\n"))
              (setq indn n)
              (setq image (let ((indn (length (cdr (assoc 'data response))))
                    (urls '()))
                (dotimes (x indn)
                  (push (cdr (assoc 'url (elt (cdr (assoc 'data response)) x))) urls))
                (reverse urls)))
              (insert (format "%s\n"
                              (let ((indn (length (cdr (assoc 'data response))))
                                    (urls '()))
                                (dotimes (x indn)
                                  (push (cdr (assoc 'url (elt (cdr (assoc 'data response)) x))) urls))
                                (reverse urls)))))
            (switch-to-buffer-other-window (current-buffer)))
          (setq index 0)
          (let ((images image))
              (dolist (image images)
                (async-shell-command (format "curl '%s' > %s/%s_%d.png" image filepath (format-time-string "%T") index))
                (sleep-for 2 500)
                (setq index (+ index 1))))
            (message "Finished downloading %d images to %s" n filepath))
      (error (error "Error while sending request to OpenAI Image Generation API: %s"
                    (error-message-string err))))))

;; list all currently available models from the list of current models at OpenAI
(defun openai-list-models ()
  "Retrieves a lsit of currently available GPT-3 models from OpenAI."
  (interactive)
  (let ((url "https://api.openai.com/v1/models"))
    (get-buffer-create "*openai*")
    (with-current-buffer (get-buffer-create "*openai*")
      (goto-char (point-max))
      (insert "========== List of OpenAI models ==========\n")
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (search-forward "\n\n")
        (setq openai-models (mapcar (lambda (x) (cdr (assoc 'id x))) (cdr (assoc 'data (json-read))))))
        (mapc (lambda (x) (insert (format "%s\n" x))) openai-models)
        (insert "=========================================\n"))
    (switch-to-buffer-other-window "*openai*")))

(provide 'openai)
;;; openai.el ends here
