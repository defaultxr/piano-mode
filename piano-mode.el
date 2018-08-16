;;; piano-mode.el --- Minor mode to turn your keyboard into a pseudo-piano

;; Copyright (C) 2016 modula t.

;; Author: modula t. <defaultxr@gmail.com>
;; Homepage: https://github.com/defaultxr/piano-mode
;; Version: 0.5
;; Package-Requires: ((emacs "24.4"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; Piano-Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Foobar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Piano-Mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Piano-Mode is a minor mode for Emacs that turns your keyboard into a "piano"
;; keyboard. It is modeled after the behavior of music trackers or digital
;; audio workstations.
;;
;; Since (to the best of my knowledge) Emacs does not provide any way to
;; differentiate between a user's keystrokes and OS-generated key repeat
;; events, and doesn't expose key release events to Elisp at all, this mode
;; uses the time between events to determine whether a key is still "pressed".
;; Obviously, this is hackish, so expect glitchy behavior to occur sometimes.
;; You may also want to tune the `piano-key-repeat-delay' and
;; `piano-key-repeat-rate' variables to be based on your OS settings.
;;
;; Additionally, this will obviously only work if you're using a QWERTY
;; keyboard layout.

;; TODO:
;; * ability to set key-down and key-up functions on a per-major mode basis

;;; Code:

(defgroup piano-mode nil
  "Piano-Mode for Emacs."
  :group 'external
  :prefix "piano-")

(defcustom piano-key-repeat-delay 0.19
  "The initial delay, in seconds, before the OS's key repeat functionality starts."
  :type '(float))

(defcustom piano-key-repeat-rate 0.05
  "The time in seconds between the OS's key repeat events."
  :type '(float))

(defcustom piano-key-down-function 'piano-diagnostic-press
  "The function to run when a key on the piano is pressed.

The function is passed one argument: a MIDI note number."
  :type '(function))

(defcustom piano-key-up-function 'piano-diagnostic-release
  "The function to run when a key on the piano is lifted.

The function is passed one argument: a MIDI note number."
  :type '(function))

(defvar piano-key nil
  "The currently-playing piano key, or nil if none.")

(defun piano-ascii-to-note-number (ascii)
  "Convert an ASCII number to a relative note number."
  (cadr (assoc ascii '((122 0)
                       (115 1)
                       (120 2)
                       (100 3)
                       (99 4)
                       (118 5)
                       (103 6)
                       (98 7)
                       (104 8)
                       (110 9)
                       (106 10)
                       (109 11)
                       (44 12)
                       (108 13)
                       (46 14)
                       (59 15)
                       (47 16)
                       (113 12)
                       (50 13)
                       (119 14)
                       (51 15)
                       (101 16)
                       (114 17)
                       (53 18)
                       (116 19)
                       (54 20)
                       (121 21)
                       (55 22)
                       (117 23)
                       (105 24)
                       (57 25)
                       (111 26)
                       (48 27)
                       (112 28)
                       (91 29)
                       (61 30)
                       (93 31)))))

(defvar piano-base-note 60
  "The base note; the MIDI note number of the z key.")

(defun piano-set-base-note (note)
  "Set the `piano-base-note'."
  (setq piano-base-note (min (max note 0) 127)))

(defun piano-set-octave (octave)
  "Set the octave of the piano."
  (interactive "NOctave: ")
  (let ((octave (min (max octave 0) 8)))
    (setq piano-base-note (+ 12 (* 12 octave)))
    (message (concat "Octave: " (number-to-string (piano-octave))))
    (piano-octave)))

(defun piano-octave ()
  "Get the octave of the piano."
  (/ (- piano-base-note 12) 12))

(defun piano-octave-up ()
  "Go up an octave."
  (interactive)
  (piano-set-octave (1+ (piano-octave))))

(defun piano-octave-down ()
  "Go down an octave."
  (interactive)
  (piano-set-octave (1- (piano-octave))))

(defun piano-press (note)
  (setq piano-key note)
  (funcall piano-key-down-function note))

(defun piano-release (&optional note)
  (let ((note (or note piano-key)))
    (when note
      (funcall piano-key-up-function note))))

(defun piano-diagnostic-press (note)
  "Diagnostic function for piano-mode; simply produces a message with the note number."
  (message (concat "Piano key press: " (number-to-string note))))

(defun piano-diagnostic-release (note)
  "Diagnostic function for piano-mode; simply produces a message with the note number."
  (message (concat "Piano key release: " (number-to-string note))))

(defun piano-play-key () ;; FIX
  ""
  (interactive)
  (let ((note (+ piano-base-note (piano-ascii-to-note-number last-input-event))))
    (when (not (eq note piano-key))
      (unless (null piano-key)
        (piano-release piano-key))
      (piano-press note))))

(defvar piano-mode-previous-cursor-color nil
  "The original cursor color, before piano-mode was started.")

(defun piano-set-cursor-color ()
  "Save the previous cursor color and set the cursor to green."
  (setq piano-mode-previous-cursor-color (frame-parameter (selected-frame) 'cursor-color))
  (set-cursor-color "#00AA00"))

(defun piano-restore-cursor-color ()
  "Restore the previous cursor color."
  (set-cursor-color piano-mode-previous-cursor-color)
  (setq piano-mode-previous-cursor-color nil))

(define-minor-mode piano-mode
  "Toggle SCLang Piano Mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When SCLang Piano Mode is enabled, your computer keyboard acts as a piano
keyboard."
  nil
  " Piano"
  :keymap (let ((m (make-keymap)))
            (mapc (lambda (x) (define-key m x 'piano-play-key)) '("z" "s" "x" "d" "c" "v" "g" "b" "h" "n" "j" "m" "," "l" "." ";" "/" "q" "2" "w" "3" "e" "r" "5" "t" "6" "y" "7" "u" "i" "9" "o" "0" "p" "[" "=" "]"))
            (mapc (lambda (x) (define-key m x 'ignore)) '("a" "f" "k" "'" "1" "4" "8" "-"))
            (define-key m (kbd "<f1>") (lambda () (interactive) (piano-set-octave 0)))
            (define-key m (kbd "<f2>") (lambda () (interactive) (piano-set-octave 1)))
            (define-key m (kbd "<f3>") (lambda () (interactive) (piano-set-octave 2)))
            (define-key m (kbd "<f4>") (lambda () (interactive) (piano-set-octave 3)))
            (define-key m (kbd "<f5>") (lambda () (interactive) (piano-set-octave 4)))
            (define-key m (kbd "<f6>") (lambda () (interactive) (piano-set-octave 5)))
            (define-key m (kbd "<f7>") (lambda () (interactive) (piano-set-octave 6)))
            (define-key m (kbd "<f8>") (lambda () (interactive) (piano-set-octave 7)))
            (define-key m (kbd "<f9>") (lambda () (interactive) (piano-set-octave 8)))
            (define-key m (kbd "M-[") 'piano-octave-down)
            (define-key m (kbd "M-]") 'piano-octave-up)
            ;; (define-key m (kbd "\\") 'piano-select-synth)
            ;; (define-key m (kbd "|") 'piano-set-args)
            ;; (define-key m (kbd "\"") 'piano-set-args)
            (define-key m (kbd "C-g") 'piano-mode)
            ;; (define-key m (kbd "<tab>") 'piano-guess-synth)
            (define-key m (kbd "<return>") 'piano-mode)
            ;; (define-key m (kbd "`") 'piano-record)
            ;; (define-key m (kbd "C-t") 'piano-trace-mode)
            m)
  ;; (when piano-recording
  ;;   (piano-record))
  ;; (sclang-eval-string
  ;;  "if(~emacsPianoModeControlSurface.isNil, {~emacsPianoModeControlSurface = ControlSurface().xKeyQuirkDetect_(true);});"
  ;;  nil)
  (if (null piano-mode-previous-cursor-color)
      (piano-set-cursor-color)
    (progn
      (piano-release)
      (piano-restore-cursor-color))))

