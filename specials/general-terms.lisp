(in-package #:alice)

(defun handle-general-terms (destination is-private is-public is-directed from-who message-body)
  (declare (ignore from-who is-private))
  (if (or is-public
          is-directed)
      (cond
        ((or (mentions "poes law" message-body)
            (mentions "poe's law" message-body))
         (say destination #("Poe's Law: Without a winking smiley or other blatant display of humor, it is utterly impossible to parody anything in such a way that someone won't mistake for the genuine article."
                            "See: http://en.wikipedia.org/wiki/Poe's_law for more information."))))))
