(in-package #:alice)

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
    (:trying-to-think . "http://weknowmemes.com/wp-content/uploads/2011/12/everyone-shut-the-fuck-up-im-trying-to-think.jpg")
    (:congratulations-song . "https://www.youtube.com/watch?v=AteCdXvZOZc") ; well, it's not an image macro, but stil....
))

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

          ((mentions-regexp "^(?i)STFU.*think" message-body)
           (say-image-macro destination :trying-to-think))

          ((or (mentions "i have no idea what i'm doing" message-body)
               (mentions "i have no idea what i am doing" message-body))
           (say-image-macro destination :noidea))

          ((mentions "fuck science" message-body)
           (say-image-macro destination :fuck-science))

          ((mentions "congratulations!!" message-body)
           (say-image-macro destination :congratulations-song))

          ((mentions-regexp "^cool\\." message-body)
           (say-image-macro destination :cool))))))
