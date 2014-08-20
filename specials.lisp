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
    (:fuck-science . "https://dl.dropboxusercontent.com/u/216352/comment/fuck_science.jpg")
    (:congratulations-song . "https://www.youtube.com/watch?v=AteCdXvZOZc") ; well, it's not an image macro, but stil....
))

(defparameter *standard-answers*
  '(("SOA#1" . "SOA#1: U mnie działa." )
    ("SOA#2" . "SOA#2: U mnie też nie działa.")
    ("SOA#4" . ("SOA#4: Się naprawi."
                "SOA#4: Working on it..."))
    ("SOA#8" . "SOA#8: RTFM!")
    ("SOA#16" . ("SOA#16: Google Is Your Friend!"
                 "SOA#16: UTFG!"))
    ("SOA#32" . "SOA#32: Masz niekompatybilną podkładkę pod mysz.")
    ("SOA#512" . "SOA#512: Dziwne... u mnie niedawno działało.")
    ("SOA#1024" . "SOA#1024: U mnie ZAWSZE wszystko działa.")
    ("SOA#2048" . "SOA#2048: Dzięki za info.")
    ("SOA#4096" . ("SOA#4096: It's not a bug, it's a feature!"
                   "SOA#4096: It's not a bug. It's a feature!"))
    ("SOA#R" . ("SOA#R: Próbowałeś zrestartować komputer?"
                "SOA#R: Have you tried to turn it off and on again?"))
    ("SOA#Z" . ("SOA#Z: Zrobione."
                "SOA#Z: Done."))
    ("SOD#1" . "SOD#1: Zrób sobie sam.")
    ("SOM#1" . "SOM#1: Dajcie wy mi święty spokój.")
    ("SOM#2" . ("SOM#2: Please, keep me out of this."
		"SOM#2: Proszę, dyskutujcie o tym beze mnie."))
    ("SOM#3" . "SOM#3: Ale ja nic nie wiem!")
    ("SOM#4" . "SOM#4: Cooo? I tak Cię nie słyszę.")

    ("SOW#1" . "SOW#1: Fuck off.")))


(defun handle-specials (destination is-private is-public is-directed from-who message-body) 
  ;; (handle-blueline destination is-private is-public is-directed from-who message-body)
  (handle-comments destination is-private is-public is-directed from-who message-body)
  (handle-standard-answers destination is-private is-public is-directed from-who message-body)
  (handle-general-terms destination is-private is-public is-directed from-who message-body)
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

(defun handle-standard-answers (destination is-private is-public is-directed from-who message-body)
  (declare (ignore from-who is-private))

  (if (or is-public
          is-directed)
      (let ((match (cl-ppcre:scan-to-strings "(?i)(SO(A|D|M|W)#[0-9A-Z]+)" message-body)))
        (if match
            (say destination (cdr (assoc match *standard-answers* :test #'equalp)))))))

(defun handle-general-terms (destination is-private is-public is-directed from-who message-body)
  (declare (ignore from-who is-private))
  (if (or is-public
          is-directed)
      (cond
        ((or (mentions "poes law" message-body)
            (mentions "poe's law" message-body))
         (say destination #("Poe's Law: Without a winking smiley or other blatant display of humor, it is utterly impossible to parody anything in such a way that someone won't mistake for the genuine article."
                            "See: http://en.wikipedia.org/wiki/Poe's_law for more information."))))))

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

          ((mentions "congratulations!!" message-body)
           (say-image-macro destination :congratulations-song))

          ((mentions-regexp "^cool\\." message-body)
           (say-image-macro destination :cool))))))
