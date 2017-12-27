;;; discord-ipc.el --- Discord ipc for emacs
;; Author: Ben Simms <ben@bensimms.moe>
;; Version: 20171212
;; URL: https://github.com/nitros12/discord-ipc.el

(require 'json)
(require 'bindat)

(defmacro maybe (x r)
  `(if ,x ,x ,r))

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

(defvar discord-ipc-client-id nil)
(defvar discord-ipc-current-buffer nil)
(defvar discord-ipc-started nil)

(defun get-ipc-url ()
  "Get the socket address to make the ipc connection on."
  (format "/run/user/%i/discord-ipc-0" (user-uid)))

(defun make-ipc-connection ()
  "Make a ipc socket connection."
  (make-network-process :name "discord-ipc-process"
                        :remote (get-ipc-url)))

(defun handle-ipc-connection (process data)
  "Handle an ipc connection on a PROCESS with DATA."
  (let* ((decoded (unpack-data data))
         (evt (cdr (assq 'evt (cdr decoded)))))
    (cond ((string= evt "READY")
           (setq discord-ipc-current-connection process)))))

(defun pack-data (opcode data)
  "Pack OPCODE and DATA."
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

(defun send-json (opcode data)
  (let ((process (get-process "discord-ipc-process")))
    (if (and process
             (process-live-p process))
        (process-send-string process (pack-data opcode data))
      (ipc-connect discord-ipc-client-id))))

(defun ipc-connect (client-id)
  (let ((process (make-ipc-connection)))
    (set-process-filter process #'handle-ipc-connection)
    (send-json discord-ipc-+handshake+ (ipc-handshake client-id))
    (setq discord-ipc-started t)))

(defun count-buffers ()
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(defun get-current-major-mode ()
  (let ((mode (assq 'major-mode (buffer-local-variables))))
    (when mode
      (symbol-name (cdr mode)))))

(defun start-time ()
  (let* ((uptime (string-to-number (emacs-uptime "%s")))
         (current-time (string-to-number (format-time-string "%s" (current-time)))))
    (- current-time uptime)))

(defun gather-data ()
  (rich-presence :details (format "Editing buffer: %s" (buffer-name))
                 :state (format "Buffers open: %d" (count-buffers))
                 :timestamps `(:start ,(start-time))
                 :assets `((large_image . ,(maybe (file-name-extension (buffer-name)) "no-extension"))
                           (large_text . ,(get-current-major-mode))
                           (small_image . "emacs")
                           (small_text . "emacs"))))

(defun ipc-send-update ()
  (unless (or (string= discord-ipc-current-buffer (buffer-name))
              (string= (get-current-major-mode) "minbuffer-inactive-mode")) ; dont send messages when we are in the same buffer or enter the minibuf
      (setq discord-ipc-current-buffer (buffer-name))
      (send-json discord-ipc-+frame+ (gather-data))))

(defun discord-ipc-run (client-id)
  (unless discord-ipc-started
    (setq discord-ipc-client-id client-id)
    (add-hook 'post-command-hook #'ipc-send-update)
    (ignore-errors ; if we fail here we'll just reconnect later
      (ipc-connect client-id))))

(provide 'discord-ipc)
;;; discord-ipc.el ends here
