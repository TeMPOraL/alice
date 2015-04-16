(in-package :alice)

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

    ("SOW#1" . "SOW#1: Fuck off.")

    ("SOT#1" . "SOT#1: Chill the fuck out.")))

(defun handle-standard-answers (destination is-private is-public is-directed from-who message-body)
  (declare (ignore from-who is-private))

  (if (or is-public
          is-directed)
      (let ((match (cl-ppcre:scan-to-strings "(?i)(SO(A|D|M|T|W)#[0-9A-Z]+)" message-body)))
        (if match
            (say destination (cdr (assoc match *standard-answers* :test #'equalp)))))))
