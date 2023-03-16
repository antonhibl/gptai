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
  "Sends a request to OpenAI API's gpt-3.5-turbo endpoint with GPTAI-PROMPT and inserts the response content at the current point in the buffer."
  (interactive "sEnter your prompt: ")
  (let ((response (gptai-turbo-request gptai-prompt)))
    (insert response)))
