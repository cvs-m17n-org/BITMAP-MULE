;; bm-utils.el -- utility functions using bitmap-mule.

;; Copyright (C) 2000, 2001 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;;         Yoshitsugu Mito <mit@nines.nec.co.jp>
;; Created: 2000/03/28
;; Revised: 2000/12/14
;; Keywords: bitmap, progress-feedback-with-label, display-time

;; This file is part of bitmap-mule.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; If you would like to use `display-time' with XEmacs style,
;; for example, put the following lines in your .emacs file.
;;
;;(if (and (not (featurep 'xemacs))
;;	 window-system
;;	 (>= emacs-major-version 20))
;;    (progn
;;      (require 'bm-utils)
;;      (setq display-time-interval 1)
;;      (setq display-time-string-forms
;;	    '((bitmap-string-to-special-symbols
;;	       (format-time-string "%T" now))
;;	      (bitmap-load-average-to-bitmap load)
;;	      ;; If you have another biff, remove the following 3 lines.
;;	      (if mail
;;		  bitmap-full-mailbox-data
;;		bitmap-empty-mailbox-data)
;;	      ))
;;      (display-time-mode 1)))

;;; Code:

(eval-when-compile
  (require 'cl)
  ;; `dolist' may be defined in egg.el, we should use the proper one.
  (if (<= emacs-major-version 20)
      (load "cl-macs" nil t)))
(eval-when-compile (require 'static))
(require 'pcustom)
(require 'bitmap)

(defcustom bitmap-progress-feedback-textual nil
  "If it is non-nil, progress display will be textual."
  :type 'boolean
  :group 'bitmap-mule)

(defvar bitmap-special-symbol-alist
  (list '(?  . " ")
	(cons ?% (bitmap-compose "00000000007A4A4C4C781E3232525E00"))
	(cons ?- (bitmap-compose "000000000000003C7800000000000000"))
	(cons ?0 (bitmap-compose "007CBAC6C6C6C2800286C6C6C6BA7C00"))
	(cons ?1 (bitmap-compose "00000206060602000206060606020000"))
	(cons ?2 (bitmap-compose "007C3A060606023C7880C0C0C0B87C00"))
	(cons ?3 (bitmap-compose "007C3A060606023C7A060606063A7C00"))
	(cons ?4 (bitmap-compose "000082C6C6C6C2BC7A06060606020000"))
	(cons ?5 (bitmap-compose "007CB8C0C0C0C0BC7A060606063A7C00"))
	(cons ?6 (bitmap-compose "007CB8C0C0C0C0BC7A86C6C6C6BA7C00"))
	(cons ?7 (bitmap-compose "007CBAC6C6C6C2800206060606020000"))
	(cons ?8 (bitmap-compose "007CBAC6C6C6C2BC7A86C6C6C6BA7C00"))
	(cons ?9 (bitmap-compose "007CBAC6C6C6C2BC7A060606063A7C00")))
  "Alist of char and special symbol bitmap.")

(defvar bitmap-progress-data-for-clear-bar
  (list (bitmap-compose "55AA552A152A152A152A152A15AA55AA")
	(bitmap-compose "55AA550A050A050A050A050A05AA55AA")
	(bitmap-compose "55AA5502010201020102010201AA55AA"))
  "Bitmaps for progress guage with clear bar.")

(defvar bitmap-progress-backgrounds-for-clear-bar
  (list (bitmap-compose "05020502050205020502050205020502")
	(bitmap-compose "55AA5500000000000000000000AA55AA")
	(bitmap-compose "55AA55AA55AA55AA55AA55AA55AA55AA")
	(bitmap-compose "40A040A040A040A040A040A040A040A0"))
  "Bitmaps for progress guage background with clear bar.")

(defvar bitmap-progress-data-for-opaque-bar
  (list (bitmap-compose "55AA55EAF5EAF5EAF5EAF5EAF5AA55AA")
	(bitmap-compose "55AA55FAF5FAF5FAF5FAF5FAF5AA55AA")
	(bitmap-compose "55AA55FEFDFEFDFEFDFEFDFEFDAA55AA"))
  "Bitmaps for progress guage with opaque bar.")

(defvar bitmap-progress-backgrounds-for-opaque-bar
  (list (bitmap-compose "05020502050205020502050205020502")
	(bitmap-compose "55AA55FFFFFFFFFFFFFFFFFFFFAA55AA")
	(bitmap-compose "55AA55AA55AA55AA55AA55AA55AA55AA")
	(bitmap-compose "40A040A040A040A040A040A040A040A0"))
  "Bitmaps for progress guage background with opaque bar.")

(defcustom bitmap-progress-feedback-use-clear-bar t
  "Non-nil means progress bar will be displayed clearly, otherwise opaquely."
  :type (` (radio
	    (const
	     :format
	     (, (concat
		 "%{"
		 (car bitmap-progress-backgrounds-for-clear-bar)
		 (bitmap-make-string
		  20 (nth 1 bitmap-progress-backgrounds-for-clear-bar))
		 (bitmap-make-string
		  5 (nth 2 bitmap-progress-backgrounds-for-clear-bar))
		 (nth 3 bitmap-progress-backgrounds-for-clear-bar)
		 "%}  "))
	     t)
	    (const
	     :tag
	     (, (concat
		 (car bitmap-progress-backgrounds-for-opaque-bar)
		 (bitmap-make-string
		  20 (nth 1 bitmap-progress-backgrounds-for-opaque-bar))
		 (bitmap-make-string
		  5 (nth 2 bitmap-progress-backgrounds-for-opaque-bar))
		 (nth 3 bitmap-progress-backgrounds-for-opaque-bar)))
	     nil)))
  :group 'bitmap-mule)

(eval-when-compile
  (defmacro bitmap-progress-data ()
    '(if bitmap-progress-feedback-use-clear-bar
	 bitmap-progress-data-for-clear-bar
       bitmap-progress-data-for-opaque-bar))

  (defmacro bitmap-progress-backgrounds ()
    '(if bitmap-progress-feedback-use-clear-bar
	 bitmap-progress-backgrounds-for-clear-bar
       bitmap-progress-backgrounds-for-opaque-bar)))

(defvar bitmap-load-average-data
  (let (rest)
    (dolist
	(data
	 '("00000000000101030307070F0F1F0000007878F8F8F8F8F8F8F8F8F8F8F80000"
	   "00000000000101030307070F001F0000007878F8F8F8F8F8F8F8F8F800F80000"
	   "00000000000101030307000F001F0000007878F8F8F8F8F8F8F800F800F80000"
	   "00000000000101030007000F001F0000007878F8F8F8F8F800F800F800F80000"
	   "00000000000100030007000F001F0000007878F8F8F800F800F800F800F80000"
	   "00000000000100030007000F001F0000007878F800F800F800F800F800F80000"
	   "00000000000100030007000F001F0000007800F800F800F800F800F800F80000")
	 rest)
      (setq rest (nconc rest (list (bitmap-compose data))))))
  "Bitmaps for load average 0.0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0.")

(defvar bitmap-empty-mailbox-data
  (bitmap-compose
   "000000003F202824222528203F00000000000000FE020A1222528A02FE000000")
  "Bitmap for empty mailbox.")

(defvar bitmap-full-mailbox-data
  (bitmap-compose
   "000000003F2F373B3D3A372F3F00000000000000FEFAF6EEDEAE76FAFE000000")
  "Bitmap for full mailbox.")

(defun bitmap-string-to-special-symbols (string)
  "Convert STRING to a special symbols."
  (let ((rest ""))
    (dolist (char (append string nil) rest)
      (setq rest (concat rest (or (cdr (assq char bitmap-special-symbol-alist))
				  (char-to-string char)))))))

(defun bitmap-progress-feedback-with-label (label fmt &optional value
						  &rest args)
  "Print a progress gauge and message in the echo area.
First argument LABEL is ignored.  The rest of the arguments are
the same as to `format'.  [XEmacs 21.2.36 emulating function]"
  (if (and (integerp value) fmt)
      (let ((msg (apply 'format fmt args))
	    (val (abs value)))
	(if (> val 100)
	    (if (zerop (setq val (% val 100)))
		(setq val 100)))
	(if (or (not window-system)
		(< emacs-major-version 20)
		bitmap-progress-feedback-textual)
	    (message "%s%s%s"
		     msg (make-string (/ val 5) ?.)
		     (if (eq 100 value) "done" ""))
	  (let ((msgmax
		 (max 0
		      (- (window-width (minibuffer-window))
			 29 (max 3 (string-width (number-to-string value))))))
		(cursor-in-echo-area t))
	    (message
	     (concat "%-" (number-to-string msgmax) "s%s%s%s%s%s%s")
	     (cond ((zerop msgmax) "")
		   ((> (string-width msg) msgmax)
		    (if (<= msgmax 3)
			""
		      (concat
		       (static-if (fboundp 'truncate-string-to-width)
			   (truncate-string-to-width msg (- msgmax 3))
			 (truncate-string msg (- msgmax 3)))
		       "...")))
		   (t msg))
	     (car (bitmap-progress-backgrounds))
	     (bitmap-make-string (/ val 4)
				 (nth 1 (bitmap-progress-backgrounds)))
	     (if (zerop (% val 4))
		 ""
	       (nth (1- (% val 4)) (bitmap-progress-data)))
	     (bitmap-make-string (- 25 (/ (+ 3 val) 4))
				 (nth 2 (bitmap-progress-backgrounds)))
	     (nth 3 (bitmap-progress-backgrounds))
	     (bitmap-string-to-special-symbols (format "%3d%%" value))))))
    (message "")))

(defalias 'progress-feedback-with-label 'bitmap-progress-feedback-with-label)
(defalias 'lprogress-display 'bitmap-progress-feedback-with-label)
(make-obsolete 'lprogress-display 'progress-feedback-with-label)

;;(defun progress-feedback-with-label-test ()
;;  (interactive)
;;  (let ((bitmap-progress-feedback-textual nil)
;;	(n -20)
;;	(textual (not (and window-system (>= emacs-major-version 20)))))
;;    (while (< n 120)
;;      (if textual
;;	  (progress-feedback-with-label nil "Processing" n)
;;	(progress-feedback-with-label nil "Processing..." n))
;;      (sleep-for 0.1)
;;      (setq n (1+ n)))
;;    (if textual
;;	(progress-feedback-with-label nil "Processing" n)
;;      (progress-feedback-with-label nil "Processing...done" n))))

(defun bitmap-load-average-to-bitmap (load)
  "Convert load average data to bitmap."
  (nth (min 6 (round (* 2 (string-to-number load))))
       bitmap-load-average-data))

(provide 'bm-utils)

;;; bm-utiles.el ends here
