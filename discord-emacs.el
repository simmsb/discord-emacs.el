(require 'json)
(require 'bindat)

(defvar discord-ipc-+handshake+ 0)
(defvar discord-ipc-+frame+ 1)
(defvar discord-ipc-+close+ 2)
(defvar discord-ipc-+ping+ 3)
(defvar discord-ipc-+pong+ 4)

(defvar discord-ipc-spec
  '((opcode u32r)
    (length u32r)
    (data str (length))))

(defvar discord-ipc-rich-presence-version 1)

(defvar discord-ipc-process nil)
(defvar discord-ipc-client-id nil)
(defvar discord-ipc-current-buffer nil)
(defvar discord-ipc-started nil)

(defun get-ipc-url ()
  (format "/run/user/%i/discord-ipc-0" (user-uid)))

(defun make-ipc-connection ()
  "Make a ipc socket connection"
  (make-network-process :name "discord-emacs"
                        :remote (get-ipc-url)
                        :nowait nil))

(defun handle-ipc-connection (process data)
  (let* ((decoded (unpack-data data))
         (evt (cdr (assq 'evt (cdr decoded)))))
    (cond ((string= evt "READY")
           (setq discord-ipc-current-connection process)
           (send-json process discord-ipc-+frame+ (gather-data))))))

(defun handle-ipc-evt (process evt)
  (when (string= evt "connection broken by remote peer\n")
    (delete-process process)
    (setq discord-ipc-process nil)))

(defun pack-data (opcode data)
  (let ((encoded-json (json-encode data)))
    (bindat-pack discord-ipc-spec `((opcode . ,opcode)
                                    (length . ,(length encoded-json))
                                    (data . ,encoded-json)))))

(defun unpack-data (data)
  (let ((unpacked (bindat-unpack discord-ipc-spec (string-as-unibyte data))))
    (cons (bindat-get-field unpacked 'opcode)
          (json-read-from-string (bindat-get-field unpacked 'data)))))

(defun ipc-handshake (client-id)
  `((v .  ,discord-ipc-rich-presence-version)
    (client_id . ,client-id)))

(defun rich-presence (&rest fields)
  `((cmd . "SET_ACTIVITY")
    (args . ((pid . ,(emacs-pid))
             (activity . ,fields)))
    (nonce . ,(number-to-string (random)))))

(defun send-json (process opcode data)
  ;; (message "Sending discord-ipc op: %d data: %s" opcode data)
  (if process
      (process-send-string process (pack-data opcode data))
    (ipc-connect discord-ipc-client-id)))

(defun ipc-connect (client-id)
  (let ((process (make-ipc-connection)))
    (set-process-filter process 'handle-ipc-connection)
    (set-process-sentinel process 'handle-ipc-evt)
    (setq discord-ipc-process process)
    (send-json process discord-ipc-+handshake+ (ipc-handshake client-id))))

(defun count-buffers ()
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(defun get-current-major-mode ()
  (let ((mode (assq 'major-mode (buffer-local-variables))))
    (when mode
      (cdr mode))))

(defun start-time ()
  (let* ((uptime (string-to-number (emacs-uptime "%s")))
         (current-time (string-to-number (format-time-string "%s" (current-time)))))
    (- current-time uptime)))

(defun gather-data ()
  (rich-presence :details (format "Editing buffer: %s" (buffer-name))
                 :state (format "Buffers open: %d" (count-buffers))
                 :timestamps `(:start ,(start-time))
                 :assets `((large_image . ,(file-name-extension (buffer-name)))
                           (large_text . ,(get-current-major-mode))
                           (small_image . emacs)
                           (small_text . emacs))))

(defun ipc-send-update ()
  (unless (or (string= discord-ipc-current-buffer (buffer-name))
              (string= (get-current-major-mode) "minbuffer-inactive-mode")) ; dont send messages when we enter the minibuffer
    (setq discord-ipc-current-buffer (buffer-name))
    (send-json discord-ipc-process discord-ipc-+frame+ (gather-data))))

(defun discord-ipc-run (client-id)
  (unless discord-ipc-started
    (setq discord-ipc-client-id client-id)
    (setq discord-ipc-started t)
    (ipc-connect client-id)
    (add-hook 'post-command-hook 'ipc-send-update)))

(provide 'discord-ipc)
