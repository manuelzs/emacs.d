;; Post region to slack
(require 'url)
(require 'json)

(defvar slack-user-list)
(defvar slack-channel-list)
(setq slack-user-list '())
(setq slack-channel-list '())

(defun parse-users (response)
  (let* (
         (json-object-type 'hash-table)
         (data (json-read-from-string response)))
    (mapcar #'(lambda (arg) (gethash "name" arg)) (gethash "members" data))))

(defun parse-channels (response)
  (let* (
         (json-object-type 'hash-table)
         (data (json-read-from-string response)))
    (mapcar #'(lambda (arg) (gethash "name" arg)) (gethash "channels" data))))

(defun parse-response (parser store status)
        "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))
        (re-search-forward "^$")
        (delete-region (point) (point-min))
        (let* (
               (data (buffer-string))
               (names (funcall parser data)))
          (kill-buffer (current-buffer))
          (set store names)))

(defun slack-load-users ()
  ""
  (let* (
         (token (get-string-from-file "~/.slack_api_token"))
         (url (concat "https://slack.com/api/users.list?token=" token))
         (url-request-method "GET"))
    (message url)
    (url-retrieve url (apply-partially 'parse-response 'parse-users 'slack-user-list))))

(defun slack-load-channels ()
  ""
  (let* (
         (token (get-string-from-file "~/.slack_api_token"))
         (url (concat "https://slack.com/api/channels.list?token=" token))
         (url-request-method "GET"))
    (message url)
    (url-retrieve url (apply-partially 'parse-response 'parse-channels 'slack-channel-list))))

(defun slack-load-users-and-channels ()
  (slack-load-users)
  (slack-load-channels)
  (message "Loading slack channel and user lists"))


(defun slack-get-real-channel (channel)
  (if
      (member channel slack-user-list)
      (concat "@" channel)
    (concat "#" channel)))

(defun post-to-slack-channel (channel body)
  "Post body to slack channel"
  (let* (
         (token (get-string-from-file "~/.slack_api_token"))
         (real-channel (slack-get-real-channel channel))
         (url (concat
               "https://slack.com/api/chat.postMessage?token=" token
               "&channel=" (url-hexify-string real-channel)
               "&text=" (concat "%60%60%60" (url-hexify-string body) "%60%60%60")
               "&as_user=" "true"
               "&type=" "message")))
    (let ((url-request-method "GET"))
      (url-retrieve url #'(lambda (status) (kill-buffer (current-buffer)))))
    (message "Sent!")))

(defun post-region-to-slack (channel)
  (interactive (list (completing-read "Channel: " (append slack-user-list slack-channel-list))))
  (if mark-active
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (post-to-slack-channel channel selection)))
    (error "mark not active")))

(global-set-key (kbd "<f8>") 'post-region-to-slack)
