(in-package :alice)

(defparameter *blueline-answers*
  '("heh"
    "lol"
    "omg"
    "nieźle..."
    "no?"))

(defparameter *blueline-eligible* '("rhqq"))

(defvar *consecutive-blueline-msgs* 0)

(defparameter *min-blueline-msgs-required* 3)

(defun handle-blueline (destination is-private is-public is-directed from-who message-body)
  (declare (ignore message-body is-directed is-private))
  (if (and is-public
           (equalp destination "#hackerspace-krk")
           (position from-who *blueline-eligible* :test #'equal))
      (progn
        (incf *consecutive-blueline-msgs*)
        (if (and (> *consecutive-blueline-msgs* *min-blueline-msgs-required*)
                 (= 0 (random 3)))
            (say destination (random-elt *blueline-answers*))))
      (if (= 0 (random 2)) (setf *consecutive-blueline-msgs* 0))))

