;;; piano-mode-cl-collider.el --- Piano-Mode functions for use with cl-collider  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 modula t.
;;
;; Author: modula t. <defaultxr at gmail dot com>
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

;; Piano-Mode functionality to control synths via cl-collider; also
;; works as an example of how to implement your own Piano-Mode
;; functionality.  You can simply load this file and then run the
;; following to set it as your Piano-Mode key up/key down functions:
;;
;; (setq piano-key-down-function 'piano-cl-collider-play-note
;;       piano-key-up-function 'piano-cl-collider-release-note)
;;
;; You can change the instrument used by customizing
;; `piano-cl-collider-instrument', and can supply additional
;; parameters to it with `piano-cl-collider-instrument-parameters'.
;; These variables are more easily modified with the
;; `piano-cl-collider-change-instrument' and
;; `piano-cl-collider-change-parameters' functions, respectively,
;; which I usually like to bind to backslash (\) and pipe (|) in
;; `piano-mode-map', like so:
;;
;; (let ((m piano-mode-map))
;;   (define-key m (kbd "\\") 'piano-cl-collider-change-instrument)
;;   (define-key m (kbd "|") 'piano-cl-collider-change-parameters))

;;; Code:

(defvar piano-cl-collider-note-map (make-hash-table)
  "Hash mapping note numbers to the ID of the synth playing them.")

(defcustom piano-cl-collider-instrument 'default
  "The instrument to use to play notes."
  :type '(symbol))

(defcustom piano-cl-collider-instrument-parameters (list)
  "Any additional parameters you want to send to the instrument."
  :type '(list))

(defun piano-cl-collider-lisp-eval (sexp)
  "Evaluate SEXP via slime or sly."
  (if (fboundp 'slime-eval)
      (slime-eval sexp)
    (sly-eval sexp)))

(defun piano-cl-collider-change-instrument ()
  "Change the active instrument."
  (interactive)
  (setf piano-cl-collider-instrument
        (intern (if (fboundp 'cl-patterns-select-instrument)
                    (cl-patterns-select-instrument)
                  (let ((res (read-string "Instrument? " nil 'piano-cl-collider-instrument-history)))
                    (unless (member (elt res 0) (list ?: ?'))
                      (concat ":" res)))))))

(defun piano-cl-collider-change-parameters ()
  "Change the additional parameters to be sent to the active instrument."
  (interactive)
  (setf piano-cl-collider-instrument-parameters
        (read--expression (concat "Parameters for " (symbol-name piano-cl-collider-instrument) "? ")
                          (prin1-to-string piano-cl-collider-instrument-parameters))))

(defun piano-cl-collider-play-note (&optional note)
  "Start playing a note with cl-collider."
  (setf (gethash note piano-cl-collider-note-map)
        (piano-cl-collider-lisp-eval
         `(cl:slot-value (cl-collider:synth ',piano-cl-collider-instrument
                                            :freq (cl-collider:midicps ,(or note 69))
                                            ,@piano-cl-collider-instrument-parameters)
                         'cl-collider::id))))

(defun piano-cl-collider-release-note (&optional note)
  "Release a note being played with cl-collider."
  (let ((id (gethash note piano-cl-collider-note-map)))
    (when id
      (piano-cl-collider-lisp-eval `(cl-collider:release ,id)))
    (setf (gethash note piano-cl-collider-note-map) nil)))

(provide 'piano-mode-cl-collider)
;;; piano-mode-cl-collider.el ends here
