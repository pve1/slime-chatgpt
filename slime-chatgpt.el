;;; This file provides the function
;;; 'slime-chatgpt-comment-symbol-at-point' that generates lisp
;;; comments for functions using ChatGPT.

;;; IN ORDER TO GET REASONABLE RESULTS, THE ENTIRE BUFFER IS SENT TO
;;; CHATGPT AS CONTEXT. DO NOT USE THIS ON CODE YOU WANT TO
;;; KEEP PRIVATE.

;;; Requirements:

;;; 1. (setq slime-chatgpt-api-key "MY-SECRET-API-KEY")
;;;    - An OpenAI api key
;;;
;;; 2. curl (for connecting to ChatGPT)
;;;
;;; 3. jq (for parsing the response)

(require 'slime)
(require 'cl-lib)

(defgroup slime-chatgpt nil
  "Generate comments using ChatGPT."
  :prefix "slime-chatgpt-"
  :group 'slime)

(defvar slime-chatgpt-api-key nil)

(defun slime-chatgpt-api-key ()
  (if slime-chatgpt-api-key
      slime-chatgpt-api-key
    (error "No OpenAI API key defined. \
Please set it with \
(setq slime-chatgpt-api-key \"MY-SECRET-API-KEY\").")))

(defcustom slime-chatgpt-question-format-string "Can you produce a brief comment for the function '%s'?"
  "The question we want to ask ChatGPT as a format string."
  :type '(string)
  :group 'slime-chatgpt)

(defcustom slime-chatgpt-api-endpoint "https://api.openai.com/v1/chat/completions"
  "The ChatGPT api endpoint."
  :type '(string)
  :group 'slime-chatgpt)

(defcustom slime-chatgpt-prompt-format-string
  "Here is a file containing Common Lisp code. %s

File contents:

%s
"
"The complete prompt as a format string."
  :type '(string)
  :group 'slime-chatgpt)

(defun slime-chatgpt-generate-question ()
  (format slime-chatgpt-question-format-string (symbol-at-point)))

(defun slime-chatgpt-generate-prompt ()
  (format slime-chatgpt-prompt-format-string
          (slime-chatgpt-generate-question)
          (buffer-substring-no-properties 1 (buffer-end 1))))

(defvar slime-chatgpt-question-history nil)

(defun slime-chatgpt-generate-prompt-interactively ()
  (let* ((initial (slime-chatgpt-generate-question))
         (question 
         (read-from-minibuffer "Question: " 
                               initial
                               nil
                               nil 
                               'slime-chatgpt-question-history)))
    (format slime-chatgpt-prompt-format-string
            question
            (buffer-substring-no-properties 1 (buffer-end 1)))))

(defun slime-chatgpt-build-json-payload (prompt)
  (format "{ \"model\": \"gpt-3.5-turbo\", \"messages\": [{\"role\": \"user\", \"content\": %S}]}"
          prompt))

;; Single quote is not considered a separator.
(defun slime-chatgpt-single-quoted-string-for-shell (string)
  (replace-regexp-in-string "'" "'\\\\''"  string))
 
(defun slime-chatgpt-build-curl-command ()
  (format "curl '%s' \
-s \
-H 'Content-Type: application/json' \
-H 'Authorization: Bearer %s' \
-d @-"
          (slime-chatgpt-single-quoted-string-for-shell
           slime-chatgpt-api-endpoint)
          (slime-chatgpt-single-quoted-string-for-shell
           (slime-chatgpt-api-key))))

(defun slime-chatgpt-send-post-request (&optional dry-run)
  "Send ARGS to URL as a POST request."
  (save-excursion
    (let* ((url-request-method "POST")
           (url-request-extra-headers
            `(("Content-Type" . "application/json")
              ("Authorization" . ,slime-chatgpt-api-key))) 
           (curl-buffer (get-buffer-create "*slime-chatgpt curl*"))
           (prompt-buffer (get-buffer-create "*slime-chatgpt prompt*"))
           (response-buffer (get-buffer-create "*slime-chatgpt response*"))
           (prompt (slime-chatgpt-generate-prompt-interactively))
           (json (slime-chatgpt-build-json-payload prompt))
           (command (slime-chatgpt-build-curl-command)))
      (set-buffer response-buffer)
      (erase-buffer)
      (set-buffer curl-buffer)
      (erase-buffer)
      (insert command)
      (set-buffer prompt-buffer)
      (erase-buffer)
      (insert json)
      (message "Consulting ChatGPT, please wait.")
      (unless dry-run
        (shell-command-on-region 1 
                                 (buffer-end 1)
                                 command
                                 response-buffer
                                 nil
                                 "*slime-chatgpt error*"))    
      (message "Consulting ChatGPT, please wait. Done."))))

(defun slime-chatgpt-parse-response ()
  (save-excursion
    (set-buffer "*slime-chatgpt response*")
    (shell-command-on-region 1 
                             (buffer-end 1) 
                             "jq '.choices[0].message.content'")
    (set-buffer "*Shell Command Output*")
    (car 
     (read-from-string
      (buffer-substring-no-properties 1 (buffer-end 1))))))

(defun slime-chatgpt-format-response ()
  (save-excursion
    (let ((message (slime-chatgpt-parse-response))
          (buffer (get-buffer-create "*slime-chatgpt comment*")))
      (set-buffer buffer)
      (erase-buffer) 
      (setf message (replace-regexp-in-string "^\n*" "" message))
      (insert message)
      (replace-regexp "^" ";; " nil 1 (buffer-end 1)) 
      (buffer-substring-no-properties 1 (buffer-end 1)))))

(defun slime-chatgpt-insert-response ()
  (let ((contents (slime-chatgpt-format-response)))
    (beginning-of-line)
    (insert "\n")
    (previous-line)
    (beginning-of-line)
    (save-excursion  
      (insert contents))))

(defun slime-chatgpt-comment-symbol-at-point ()
  (interactive)
  (dolist (buf '("*slime-chatgpt error*"
                 "*slime-chatgpt response*"
                 "*slime-chatgpt curl*"
                 "*slime-chatgpt comment*"))
    (when (buffer-live-p (get-buffer buf))
      (kill-buffer buf)))
  (slime-chatgpt-send-post-request)
  (slime-chatgpt-insert-response))

(provide 'slime-chatgpt)
