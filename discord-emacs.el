;;; discord-emacs.el --- Discord ipc for emacs -*- lexical-binding: t -*-
;; Author: Ben Simms <ben@bensimms.moe>
;; Version: 20180914
;; URL: https://github.com/nitros12/discord-emacs.el

(require 'json)
(require 'bindat)
(require 'cl-lib)

;;; Code:

(defvar discord-emacs--+handshake+ 0)
(defvar discord-emacs--+frame+ 1)
(defvar discord-emacs--+close+ 2)
(defvar discord-emacs--+ping+ 3)
(defvar discord-emacs--+pong+ 4)

(defvar discord-emacs--spec
  '((opcode u32r)
    (length u32r)
    (data str (length))))

(defvar discord-emacs--rich-presence-version 1)

(defvar discord-emacs--client-id nil)
(defvar discord-emacs--current-buffer nil)
(defvar discord-emacs--started nil)
(defvar discord-emacs--blacklisted-buffer-names
  '("^\\s *\\*"))

(defun discord-emacs--maybe (x r)
  "Shortcut for (if X X R)."
  (if x x r))

(defun discord-emacs--get-ipc-url ()
  "Get the socket address to make the ipc connection on."
  (format "/run/user/%i/discord-ipc-0" (user-uid)))

(defun discord-emacs--make-ipc-connection ()
  "Make a ipc socket connection."
  (make-network-process :name "discord-ipc-process"
                        :remote (discord-emacs--get-ipc-url)))

(defun discord-emacs--pack-data (opcode data)
  "Pack OPCODE and DATA."
  (let ((encoded-json (json-encode data)))
    (bindat-pack discord-emacs--spec `((opcode . ,opcode)
                                       (length . ,(length encoded-json))
                                       (data . ,encoded-json)))))

(defun discord-emacs--ipc-handshake (client-id)
  "Perform an ipc handshake with the client id CLIENT-ID."
  `((v .  ,discord-emacs--rich-presence-version)
    (client_id . ,client-id)))

(defun discord-emacs--rich-presence (&rest fields)
  "Build a rich presence payload with the fields FIELDS."
  `((cmd . "SET_ACTIVITY")
    (args . ((pid . ,(emacs-pid))
             (activity . ,fields)))
    (nonce . ,(number-to-string (random)))))

(defun discord-emacs--send-json (opcode data)
  "Send a JSON payload over the ipc connection with the opcode OPCODE and data DATA."
  (let ((process (get-process "discord-ipc-process")))
    (if (and process
             (process-live-p process))
        (process-send-string process (discord-emacs--pack-data opcode data))
      (discord-emacs--ipc-connect discord-emacs--client-id))))

(defun discord-emacs--ipc-connect (client-id)
  "Make an ipc connection to discord with the client id CLIENT-ID."
  (discord-emacs--make-ipc-connection)
  (discord-emacs--send-json discord-emacs--+handshake+ (discord-emacs--ipc-handshake client-id))
  (setq discord-emacs--started t))

(defun discord-emacs--count-buffers ()
  "Count the number of buffers."
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(defun discord-emacs--get-current-major-mode ()
  "Get the current major mode of the active buffer."
  (let ((mode (assq 'major-mode (buffer-local-variables))))
    (when mode
      (symbol-name (cdr mode)))))

(defun discord-emacs--start-time ()
  "Get the start time of this Emacs instance."
  (let* ((uptime (string-to-number (emacs-uptime "%s")))
         (current-time (string-to-number (format-time-string "%s" (current-time)))))
    (- current-time uptime)))

(defun discord-emacs--gather-data ()
  "Gather data for a rich presence payload."
  (discord-emacs--rich-presence
   :details (format "Editing buffer: %s" (buffer-name))
   :state (format "Buffers open: %d" (discord-emacs--count-buffers))
   :timestamps `(:start ,(discord-emacs--start-time))
   :assets `((large_image . ,(discord-emacs--maybe (file-name-extension (buffer-name)) "no-extension"))
             (large_text . ,(discord-emacs--get-current-major-mode))
             (small_image . "emacs")
             (small_text . "emacs"))))

(defun discord-emacs--some-pred (predicates val)
  "Apply all PREDICATES to VAL, return the first non-nil value or nil."
  (cl-some (lambda (pred) (funcall pred val))
           predicates))

(defun discord-emacs--test-buffer ()
  "Test if the current buffer is one that we should build a rich presence for."
  (discord-emacs--some-pred
   (cl-mapcar (lambda (regex)
                (lambda (s) (string-match regex s)))
              discord-emacs--blacklisted-buffer-names)
   (buffer-name)))

(defun discord-emacs--ipc-send-update ()
  "Send an ipc update to discord."
  (unless (or (string= discord-emacs--current-buffer (buffer-name))
              (discord-emacs--test-buffer))
    ;; dont send messages when we are in the same buffer or enter the minibuf
    (setq discord-emacs--current-buffer (buffer-name))
    (discord-emacs--send-json discord-emacs--+frame+ (discord-emacs--gather-data))))

(defun discord-emacs-run (client-id)
  "Run the rich presence with the client id CLIENT-ID."
  (unless discord-emacs--started
    (setq discord-emacs--client-id client-id)
    (add-hook 'post-command-hook #'discord-emacs--ipc-send-update)
    (ignore-errors ; if we fail here we'll just reconnect later
      (discord-emacs--ipc-connect client-id))))

(provide 'discord-emacs)
;;; discord-emacs.el ends here
