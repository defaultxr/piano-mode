;;; cl-collider-example.el --- example of piano-mode functions for use with cl-collider  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021  
;;
;; Author: modula t. <defaulxr at gmail dot com>
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; An example of how to use Piano-Mode to control synths via
;; cl-collider.  You can simply load this file and then run the
;; following to set it as your Piano-Mode key up/key down functions:
;;
;; (setq piano-key-down-function 'piano-cl-collider-play-note
;;       piano-key-up-function 'piano-cl-collider-release-note)
;;
;; You can also change the synth used by customizing
;; `piano-cl-collider-synth' and supply additional parameters to it
;; with `piano-cl-collider-synth-parameters'.

;;; Code:

(defvar piano-cl-collider-note-map (make-hash-table)
  "Hash mapping note numbers to the ID of the synth playing them.")

(defcustom piano-cl-collider-synth 'default
  "The synth to use to play notes."
  :type '(symbol))

(defcustom piano-cl-collider-synth-parameters (list)
  "Any additional parameters you want to send to the synth."
  :type '(list))

(defun piano-cl-collider-lisp-eval (sexp)
  "Evaluate SEXP via slime or sly."
  (if (fboundp 'slime-eval)
      (slime-eval sexp)
    (sly-eval sexp)))

(defun piano-cl-collider-play-note (&optional note)
  "Start playing a note with cl-collider."
  (setf (gethash note piano-cl-collider-note-map)
        (piano-cl-collider-lisp-eval
         `(cl:slot-value (cl-collider:synth ',piano-cl-collider-synth
                                            :freq (cl-collider:midicps ,(or note 69))
                                            ,@piano-cl-collider-synth-parameters)
                         'cl-collider::id))))

(defun piano-cl-collider-release-note (&optional note)
  "Release a note being played with cl-collider."
  (let ((id (gethash note piano-cl-collider-note-map)))
    (when id
      (piano-cl-collider-lisp-eval `(cl-collider:release ,id)))
    (setf (gethash note piano-cl-collider-note-map) nil)))

(provide 'piano-cl-collider-example)
;;; piano-cl-collider-example.el ends here
