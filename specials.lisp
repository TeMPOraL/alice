;; A file to separate out special situations
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


(defparameter *comments-image-macros*
  '((:szynka-z-kotka . "https://dl.dropboxusercontent.com/u/216352/comment/szynka_z_kotka.jpg")
    (:disagree . "https://dl.dropboxusercontent.com/u/216352/comment/i-respectfully-disagree.jpg")
    (:memetic-hazard . "https://dl.dropboxusercontent.com/u/216352/comment/memetic_hazard.png")
    (:stupidity . "https://dl.dropboxusercontent.com/u/216352/comment/stupidity.png")
    (:reference . "https://dl.dropboxusercontent.com/u/216352/comment/reference.gif")
    (:cool . "https://dl.dropboxusercontent.com/u/216352/comment/cool.jpg")
    (:oops . "https://dl.dropboxusercontent.com/u/216352/comment/oops.jpg")
    (:noidea . "https://dl.dropboxusercontent.com/u/216352/comment/noidea.jpg")
    (:suitability . "https://dl.dropboxusercontent.com/u/216352/comment/suitability.jpg")
    (:strong-upvote . "https://dl.dropboxusercontent.com/u/216352/comment/strong-upvote.gif")
    (:fuck-science . "https://dl.dropboxusercontent.com/u/216352/comment/fuck_science.jpg")))

(defun handle-specials (destination is-private is-public is-directed from-who message-body)
  ;; (handle-blueline destination is-private is-public is-directed from-who message-body)
  (handle-comments destination is-private is-public is-directed from-who message-body)
  (handle-marchewa-presentation destination is-private is-public is-directed from-who message-body))


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

(defun handle-marchewa-presentation (destination is-private is-public is-directed from-who message-body)

  )


(defun handle-comments (destination is-private is-public is-directed from-who message-body)
  (declare (ignore from-who is-directed is-private))
  (flet ((say-image-macro (destination symbol)
           (say destination (cdr (assoc symbol *comments-image-macros*)))))
    (if is-public
        (cond
          ((and (or (mentions "szynka" message-body)
                    (mentions "szynkę" message-body)
                    (mentions "szynke" message-body))
                (mentions "z kotka" message-body))
           (say-image-macro destination :szynka-z-kotka))

          ((mentions "respectfully disagree" message-body)
           (say-image-macro destination :disagree))

          ((mentions "memetic hazard" message-body)
           (say-image-macro destination :memetic-hazard))

          ((mentions-regexp "^stupidity\\." message-body)
           (say-image-macro destination :stupidity))

          ((mentions-regexp "^oops\\." message-body)
           (say-image-macro destination :oops))

          ((mentions "just because you can" message-body)
           (say-image-macro destination :suitability))

          ((mentions "i understood that reference" message-body)
           (say-image-macro destination :reference))

          ((or (mentions-regexp "^strong upvote" message-body)
               (mentions-regexp "[^ \\+]\\+\\+\\+\\+$" message-body))
           (say-image-macro destination :strong-upvote))

          ((or (mentions "i have no idea what i'm doing" message-body)
               (mentions "i have no idea what i am doing" message-body))
           (say-image-macro destination :noidea))

          ((mentions "fuck science" message-body)
           (say-image-macro destination :fuck-science))

          ((mentions-regexp "^cool\\." message-body)
           (say-image-macro destination :cool))))))
