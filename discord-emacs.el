;;; discord-emacs.el --- Discord ipc for emacs
;; Author: Ben Simms <ben@bensimms.moe>
;; Version: 20170402
;; URL: https://github.com/nitros12/discord-emacs.el

(require 'json)
(require 'bindat)

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

(defun discord-emacs--maybe (x r)
  (if x x r))

(defun discord-emacs--get-ipc-url ()
  "Get the socket address to make the ipc connection on."
  (format "/run/user/%i/discord-ipc-0" (user-uid)))

(defun discord-emacs--make-ipc-connection ()
  "Make a ipc socket connection."
  (make-network-process :name "discord-ipc-process"
                        :remote (discord-emacs--get-ipc-url)))

(defun discord-emacs--handle-ipc-connection (process data)
  "Handle an ipc connection on a PROCESS with DATA."
  (let* ((decoded (discord-emacs--unpack-data data))
         (evt (cdr (assq 'evt (cdr decoded)))))
    (cond ((string= evt "READY")
           (setq discord-emacs--current-connection process)))))

(defun discord-emacs--pack-data (opcode data)
  "Pack OPCODE and DATA."
  (let ((encoded-json (json-encode data)))
    (bindat-pack discord-emacs--spec `((opcode . ,opcode)
                                       (length . ,(length encoded-json))
                                       (data . ,encoded-json)))))

(defun discord-emacs--unpack-data (data)
  (let ((unpacked (bindat-unpack discord-emacs--spec (string-as-unibyte data))))
    (cons (bindat-get-field unpacked 'opcode)
          (json-read-from-string (bindat-get-field unpacked 'data)))))

(defun discord-emacs--ipc-handshake (client-id)
  `((v .  ,discord-emacs--rich-presence-version)
    (client_id . ,client-id)))

(defun discord-emacs--rich-presence (&rest fields)
  `((cmd . "SET_ACTIVITY")
    (args . ((pid . ,(emacs-pid))
             (activity . ,fields)))
    (nonce . ,(number-to-string (random)))))

(defun discord-emacs--send-json (opcode data)
  (let ((process (get-process "discord-ipc-process")))
    (if (and process
             (process-live-p process))
        (process-send-string process (discord-emacs--pack-data opcode data))
      (discord-emacs--ipc-connect discord-emacs--client-id))))

(defun discord-emacs--ipc-connect (client-id)
  (let ((process (discord-emacs--make-ipc-connection)))
    (set-process-filter process #'discord-emacs--handle-ipc-connection)
    (discord-emacs--send-json discord-emacs--+handshake+ (discord-emacs--ipc-handshake client-id))
    (setq discord-emacs--started t)))

(defun discord-emacs--count-buffers ()
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(defun discord-emacs--get-current-major-mode ()
  (let ((mode (assq 'major-mode (buffer-local-variables))))
    (when mode
      (symbol-name (cdr mode)))))

(defun discord-emacs--start-time ()
  (let* ((uptime (string-to-number (emacs-uptime "%s")))
         (current-time (string-to-number (format-time-string "%s" (current-time)))))
    (- current-time uptime)))

(defun discord-emacs--gather-data ()
  (discord-emacs--rich-presence
   :details (format "Editing buffer: %s" (buffer-name))
   :state (format "Buffers open: %d" (discord-emacs--count-buffers))
   :timestamps `(:start ,(discord-emacs--start-time))
   :assets `((large_image . ,(discord-emacs--maybe (file-name-extension (buffer-name)) "no-extension"))
             (large_text . ,(discord-emacs--get-current-major-mode))
             (small_image . "emacs")
             (small_text . "emacs"))))

(defun discord-emacs--ipc-send-update ()
  (unless (or (string= discord-emacs--current-buffer (buffer-name))
              (string= (discord-emacs--get-current-major-mode) "minibuffer-inactive-mode"))
              ;; dont send messages when we are in the same buffer or enter the minibuf
    (setq discord-emacs--current-buffer (buffer-name))
    (discord-emacs--send-json discord-emacs--+frame+ (discord-emacs--gather-data))))

(defun discord-emacs-run (client-id)
  (unless discord-emacs--started
    (setq discord-emacs--client-id client-id)
    (add-hook 'post-command-hook #'discord-emacs--ipc-send-update)
    (ignore-errors ; if we fail here we'll just reconnect later
      (discord-emacs--ipc-connect client-id))))

(provide 'discord-emacs)
;;; discord-emacs.el ends here
