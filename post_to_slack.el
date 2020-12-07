;;; post_to_slack.el --- Mode for posting to slack from Emacs. -*- lexical-binding: t -*-

;; Copyright © 2017 Manuel Zapata <manuelzs@gmail.com>

;; Author: Manuel Zapata <manuelzs@gmail.com>
;; URL: https://github.com/manuelzs/emacs.d
;; Keywords: convenience, slack
;; Version: 0.1.0
;; Created: 2017-01-23

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides an easy way to post to a slack channel from
;; inside Emacs.  See the README for more details.
;;
;;; Code:

(require 'url)
(require 'json)

(defgroup post-to-slack nil
  "Post to slack from emacs"
  :group 'programming
  :prefix "post-to-slack-")

(defvar post-to-slack-user-list)
(defvar post-to-slack-channel-list)
(setq post-to-slack-user-list '())
(setq post-to-slack-channel-list '())

(setq post-to-slack-token
      (funcall
       (plist-get
        (nth 0 (auth-source-search
                :host "slack.com/api"
                :requires '(user secret)))
        :secret)))


(defun post-to-slack-parse-users (response)
  (let* (
         (json-object-type 'hash-table)
         (data (json-read-from-string response)))
    (mapcar #'(lambda (arg) (gethash "name" arg)) (gethash "members" data))))

(defun post-to-slack-parse-channels (response)
  (let* (
         (json-object-type 'hash-table)
         (data (json-read-from-string response)))
    (mapcar #'(lambda (arg) (gethash "name" arg)) (gethash "channels" data))))

(defun post-to-slack-parse-response (parser store status)
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
         (token post-to-slack-token)
         (url (concat "https://slack.com/api/users.list?token=" token))
         (url-request-method "GET"))
    (message url)
    (url-retrieve url (apply-partially 'post-to-slack-parse-response 'post-to-slack-parse-users 'post-to-slack-user-list))))

(defun slack-load-channels ()
  ""
  (let* (
         (token post-to-slack-token)
         (url (concat "https://slack.com/api/channels.list?token=" token))
         (url-request-method "GET"))
    (message url)
    (url-retrieve url (apply-partially 'post-to-slack-parse-response 'post-to-slack-parse-channels 'post-to-slack-channel-list))))

(defun slack-load-users-and-channels ()
  (slack-load-users)
  (slack-load-channels)
  (message "Loading slack channel and user lists"))


(defun slack-get-real-channel (channel)
  (if
      (member channel post-to-slack-user-list)
      (concat "@" channel)
    (concat "#" channel)))

(defun post-to-slack-channel (channel body)
  "Post body to slack channel"
  (let* (
         (token post-to-slack-token)
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
  (interactive (list (completing-read "Channel: " (append post-to-slack-user-list post-to-slack-channel-list))))
  (if mark-active
      (let (
            (selection (buffer-substring-no-properties (region-beginning) (region-end))))
        (if (= (length selection) 0)
            (message "empty string")
          (post-to-slack-channel channel selection)))
    (error "mark not active")))

(global-set-key (kbd "<f8>") 'post-region-to-slack)


(defun slack-write-token-to-file (token)
  (with-temp-buffer
    (insert token)
    (write-region (point-min) (point-max) post-to-slack-token-file t)))


(defun slack-start-authorization (client-id client-secret)
  (interactive "sClient ID: \nsClient Secret: ")
  (browse-url (concat
               "https://slack.com/oauth/authorize?client_id=" client-id
               "&scope=chat:write:user&scope=channels:read,users:read"))
  (let (
        (code (read-string "Code: ")))
    (browse-url (concat
                 "https://slack.com/api/oauth.access"
                 "?client_id=" client-id
                 "&client_secret=" client-secret
                 "&code=" code)))
  (let ((token (read-string "Token: ")))
    (slack-write-token-to-file token)))
