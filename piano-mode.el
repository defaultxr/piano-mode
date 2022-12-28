;;; piano-mode.el --- Minor mode to turn your keyboard into a pseudo-piano -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2021 modula t.

;; Author: modula t. <defaultxr at gmail dot com>
;; Homepage: https://github.com/defaultxr/piano-mode
;; Version: 0.7
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages

;; This file is not part of GNU Emacs.

;; Piano-Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Piano-Mode is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Piano-Mode.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Piano-Mode is a minor mode for Emacs that turns your keyboard into
;; a "piano" keyboard.  It is modeled after the same behavior commonly
;; included in music trackers or digital audio workstations.  Each
;; "note on" (piano key press) event is sent to the function specified
;; by the `piano-key-down-function' variable, and each "note off" is
;; sent to the function specified by `piano-key-up-function'.  See the
;; included "piano-mode-cl-collider.el" for an example of functions
;; that could be set for these variables.

;; Since (to the best of my knowledge) Emacs does not provide any way
;; to differentiate between a user's keystrokes and OS-generated key
;; repeat events, and doesn't expose key release events to Elisp at
;; all, this mode uses the time between events to determine whether a
;; key is still "pressed".  Obviously, this is a very hacky solution,
;; so expect glitchy behavior to occur sometimes.  You may want to tune
;; the `piano-key-repeat-delay' and `piano-key-repeat-rate' variables
;; based on your OS settings.

;; Right now, this will only work if you're using a QWERTY keyboard
;; layout, but you can edit `piano-ascii-to-note-translation-table' to
;; match your keyboard layout.  (Pull requests for alternate layouts
;; are welcome!)

;; TODO:
;; - "mono mode"
;; - record mode
;; - better heuristic for note auto-release; take into account whether
;;   the key is "repeating" or not.

;;; Code:

(defgroup piano nil
  "Piano-Mode for Emacs."
  :group 'external
  :prefix "piano-")

(defcustom piano-key-repeat-delay 0.19
  "The initial delay, in seconds, before the OS's key repeat functionality starts."
  :type '(float))

(defcustom piano-key-repeat-rate 0.005
  "The time in seconds between the OS's key repeat events."
  :type '(float))

(defcustom piano-key-down-function 'piano-diagnostic-press
  "The function to run when a key on the piano is pressed.

The function is passed one argument: a MIDI note number."
  :type '(function))

(defcustom piano-key-up-function 'piano-diagnostic-release
  "The function to run when a key on the piano is released.

The function is passed one argument: a MIDI note number."
  :type '(function))

(defun piano-diagnostic-press (note)
  "Diagnostic function for Piano-Mode; simply produces a message that NOTE has been pressed."
  (message (concat "Piano key press: " (number-to-string note))))

(defun piano-diagnostic-release (note)
  "Diagnostic function for Piano-Mode; simply produces a message that NOTE has been released."
  (message (concat "Piano key release: " (number-to-string note))))

(defvar piano-key nil
  "The currently-playing piano key, or nil if none.")

(defvar piano-ascii-to-note-translation-table
  '((122 0) (115 1) (120 2) (100 3) (99 4) (118 5) (103 6) (98 7) (104 8)
    (110 9) (106 10) (109 11) (44 12) (108 13) (46 14) (59 15) (47 16) (113 12)
    (50 13) (119 14) (51 15) (101 16) (114 17) (53 18) (116 19) (54 20) (121 21)
    (55 22) (117 23) (105 24) (57 25) (111 26) (48 27) (112 28) (91 29) (61 30)
    (93 31))
  "Table mapping ASCII character numbers to relative note numbers.  For non-QWERTY layouts, this table would need to be changed.")

(defun piano-ascii-to-note-number (ascii)
  "Convert an ASCII number to a relative note number using `piano-ascii-to-note-translation-table'."
  (cadr (assoc ascii piano-ascii-to-note-translation-table)))

(defvar piano-base-note 60
  "The base note; the current MIDI note number of the z key.  This is changed when you switch octaves, i.e. with `piano-octave-up' or `piano-octave-down'.")

(defun piano-set-octave (octave)
  "Transpose the piano to OCTAVE."
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

(defun piano-fn-octaves (&optional arg)
  "Enable or disable bindings in `piano-mode-map' to allow changing the octave with the F1-F9 keys.  If ARG is positive, enable octave keys; if negative, disable; and if omitted, toggle."
  (interactive "P")
  (let ((enable (if arg
                    (> (prefix-numeric-value arg) 0)
                  (not (lookup-key piano-mode-map (kbd "<f1>"))))))
    (dotimes (n 9)
      (define-key piano-mode-map (kbd (concat "<f" (number-to-string (1+ n)) ">"))
        (if enable
            (lambda () (interactive) (piano-set-octave n))
          nil)))
    (message "Piano-Mode Fn keys %s." (if enable "enabled" "disabled"))))

(defun piano-press (note)
  "\"Press the NOTE key\" on the piano to start playing it."
  (setq piano-key note)
  (funcall piano-key-down-function note))

(defun piano-release (&optional note)
  "\"Release the NOTE key\" on the piano to stop playing it."
  (when-let ((note (or note piano-key)))
    (funcall piano-key-up-function note)))

(defvar piano-timer nil
  "The timer that Piano-Mode uses to detect when keys are released.")

(defvar piano-last-key-press-time nil
  "The time in milliseconds when the last key was pressed.")

(defun piano-auto-release ()
  "Automatically release the currently-held note if the last keypress was longer than `piano-key-repeat-delay' seconds ago."
  (when piano-last-key-press-time
    (when (> (/ (- (car (time-convert nil 65536)) piano-last-key-press-time) 65536.0)
             piano-key-repeat-delay)
      (when piano-timer
        (cancel-timer piano-timer))
      (piano-release)
      (setq piano-timer nil
            piano-key nil))))

(defun piano-run-timer ()
  "Cancel the current timer if it exists, then run a new timer."
  (when piano-timer
    (cancel-timer piano-timer))
  (setq piano-timer (run-with-timer piano-key-repeat-delay piano-key-repeat-rate 'piano-auto-release)))

(defun piano-play-key ()
  "Parse the most recent input event and forward it to `piano-key-down-function' or `piano-key-up-function'."
  (interactive)
  (let* ((note (+ piano-base-note (piano-ascii-to-note-number last-input-event)))
         (same-note (eql note piano-key)))
    (unless same-note
      (when piano-key
        (piano-release piano-key))
      (piano-press note))
    (setq piano-last-key-press-time (car (time-convert nil 65536)))
    (unless same-note
      (piano-run-timer))))

(defun piano-panic ()
  "Release all notes (useful in case of stuck notes)."
  (interactive)
  (dotimes (note 128)
    (piano-release note)))

(defvar piano-mode-map
  (let ((m (make-keymap)))
    (mapc (lambda (x) (define-key m x 'piano-play-key)) '("z" "s" "x" "d" "c" "v" "g" "b" "h" "n" "j" "m" "," "l" "." ";" "/" "q" "2" "w" "3" "e" "r" "5" "t" "6" "y" "7" "u" "i" "9" "o" "0" "p" "[" "=" "]"))
    (mapc (lambda (x) (define-key m x 'ignore)) '("a" "f" "k" "'" "1" "4" "8" "-"))
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
  "Keymap for `piano-mode'.")

;;;###autoload
(define-minor-mode piano-mode
  "Toggle Piano Mode.
Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When Piano Mode is enabled, your computer keyboard acts as a
piano keyboard.  Each key is mapped to `piano-play-key', which
attempts to convert keypress events into key down and key up
events, which are sent to `piano-key-down-function' and
`piano-key-up-function', respectively."
  :global nil
  :lighter " Piano"
  (when piano-mode
    (piano-release)))

(provide 'piano-mode)
;;; piano-mode.el ends here
