(in-package #:alice)

(defun notify-kdbot-down ()
  (send-email *kdbot-notification-email* "Hej, kdbot padł :(. Pozdrawiam!"))
