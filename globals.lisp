(in-package #:alice)


;;; phrases
;; phrases are defined as an alist of label and possible phrases to speak
;; plain-text value will be said directly
;; list of values -> bot will pick one at random
;; vector of values -> bot will output all of them sequencially, possibly throttling output to avoid flooding the channel
;; 
;; values are read recursively, i.e. encoding a symbol will cause bot to find the proper phrase in
;; this alist, and e.g. list of vector means "pick a sequence of things to say at random"




