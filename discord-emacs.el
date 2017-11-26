(require 'json)
(require 'bindat)

(defvar discord-ipc-spec
  '((opcode u32r)
    (length u32r)
    (data str (length))))

(defvar discord-rich-presence-version "0.0.1")

(defun get-ipc-url ()
  (format "/run/user/%u/discord-ipc-0" (user-uid)))

(defun make-ipc-connection ()
  "Make a ipc socket connection"
  (make-network-process :name "discord-emacs"
                        :remote (get-ipc-url)
                        :buffer "*discord-rich-presence*"
                        :sentinel 'ipc-senteniel))

(defun ipc-senteniel (sock msg)
  (print msg))

(defun pack-date (opcode data)
  (let ((encoded-json (json-encode data)))
    (bindat-pack discord-ipc-spec `((opcode . ,opcode)
                                    (length . ,(length encoded-json))
                                    (data . ,encoded-json)))))

(defun ipc-handshake (client-id)
  `((v . ,discord-rich-presence-version)
    (client_id . ,client-id)))

; (pack-date 0 (ipc-handshake 1234567))
