(in-package #:alice)


(defclass sentence-features ()
  ;; sentence-understanding-related
  ((words
    :initarg :words
    :accessor words
    :documentation "Sentence split to particular words.")

   (nicks-present
    :initarg :nicks-present
    :accessor nicks-present
    :documentation "All the nicknames identified in the sentence that are present on channel.")

   (nicks-known
    :initarg :nicks-known
    :accessor nicks-known
    :documentation "All the nicknames identified in the sentence that the bot ever heard, even if not present on particular channel.")

   (tone
    :initarg :tone
    :accessor tone
    :documentation "Tone of the sentence; i.e. whether it is recognized as nice, angry, etc.")


   ;; IRC-related
   (channel
    :initarg :channel
    :accessor channel
    :documentation "Name of channel on which this sentence was spoken.")

   (source
    :initarg :source
    :accessor source
    :documentation "Source person (nickname) who spoke the message.")

   (directedp
    :initarg :directedp
    :accessor directedp
    :documentation "Was this message directed at the bot? (in a form of 'Bot: ...' message)")

   (mentionsp
    :initarg :mentionsp
    :accessor mentionsp
    :documentation "Does this sentence mentions bot by nickname?")

   (publicp
    :initarg :publicp
    :accessor publicp
    :documentation "Is this message send publicly on the channel (as opposed to direct messages)?")
))

(defun extract-words (text)
  (delete-duplicates
   (cl-ppcre:all-matches-as-strings "\\w{2,}" text)
   :test #'string=))

(defun extract-message-features (from message)
  (make-instance 'sentence-features
                 :words (extract-words message)
                 :nicks-present nil
                 :nicks-known nil
                 :tone nil
                 :channel nil
                 :source nil
                 :directedp nil
                 :mentionsp nil
                 :publicp nil))
