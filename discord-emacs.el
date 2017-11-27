(require 'json)
(require 'bindat)
(require 'timer)

(defvar +handshake+ 0)
(defvar +frame+ 1)
(defvar +close+ 2)
(defvar +ping+ 3)
(defvar +pong+ 4)

(setq discord-ipc-spec
  '((opcode u32r)
    (length u32r)
    (data str (length))))

(setq discord-rich-presence-version 1)

(setq discord-ipc-client-id nil)
(setq discord-ipc-client-timer nil)

(defun get-ipc-url ()
  (format "/run/user/%i/discord-ipc-0" (user-uid)))

(defun make-ipc-connection ()
  "Make a ipc socket connection"
  (make-network-process :name "discord-emacs"
                        :buffer "*discord-rich-presence*"
                        :remote (get-ipc-url)
                        :nowait nil))

(defun handle-ipc-connection (process data)
  (let* ((decoded (unpack-data data))
         (evt (cdr (assq 'evt (cdr decoded)))))
    (cond ((string= evt "READY")
           (setq ipc-ready t)
           (setq current-connection process)
           (send-json process +frame+ (gather-data))
           (run-at-time "1 min" nil (lambda () (delete-process process)))))))
          ;;((string= evt "ERROR")
          ;; (print decoded)))))

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
  `((v .  ,discord-rich-presence-version)
    (client_id . ,client-id)))

(defun rich-presence (&rest fields)
  `((cmd . "SET_ACTIVITY")
    (args . ((pid . ,(emacs-pid))
             (activity . ,fields)))
    (nonce . ,(number-to-string (random)))))

(defun send-json (process opcode data)
  (process-send-string process (pack-data opcode data)))

(defun ipc-update ()
  (let ((process (make-ipc-connection)))
    (set-process-filter process 'handle-ipc-connection)
    (send-json process +handshake+ (ipc-handshake discord-ipc-client-id))))

(defun count-buffers ()
  (cl-count-if
   (lambda (b)
     (or (buffer-file-name b)
         (not (string-match "^ " (buffer-name b)))))
   (buffer-list)))

(defun start-time ()
  (let* ((uptime (string-to-number (emacs-uptime "%s")))
         (current-time (string-to-number (format-time-string "%s" (current-time)))))
    (- current-time uptime)))

(defun gather-data ()
  (rich-presence :details (format "Editing buffer: %s" (buffer-name))
                 :state (format "Buffers open: %d" (count-buffers))))
                 ;;:startTimestamp (start-time)))

(defun discord-ipc-run (client-id &optional period) ;; TODO: update on emacs events
  (let ((period (if period period (* 5 60))))
    (when discord-ipc-client-timer
      (cancel-timer discord-ipc-client-timer))
    (setq discord-ipc-client-id client-id)
    (setq discord-ipc-client-timer (run-at-time "1 sec" period 'ipc-update))))

(provide 'discord-ipc-run)
