;; bm-utils.el -- utility functions using bitmap-mule.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2000/03/28
;; Revised: 2000/03/30
;; Keywords: bitmap, lprogress-display

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

;;; Code:

(require 'bitmap)

(defvar bitmap-lprogress-display-textual nil
  "*If it is non-nil, progress display will be textual.")

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
	(cons ?7 (bitmap-compose "007C3A06060602000206060606020000"))
	(cons ?8 (bitmap-compose "007CBAC6C6C6C2BC7A86C6C6C6BA7C00"))
	(cons ?9 (bitmap-compose "007CBAC6C6C6C2BC7A060606063A7C00"))))

(defvar bitmap-lprogress-data
  (mapcar 'bitmap-compose '("55AA552A152A152A152A152A15AA55AA"
			    "55AA550A050A050A050A050A05AA55AA"
			    "55AA5502010201020102010201AA55AA")))

(defvar bitmap-lprogress-backgrounds
  (list (bitmap-compose "05020502050205020502050205020502")
	(string-to-char (bitmap-compose "55AA5500000000000000000000AA55AA"))
	(string-to-char (bitmap-compose "55AA55AA55AA55AA55AA55AA55AA55AA"))
	(bitmap-compose "40A040A040A040A040A040A040A040A0")))

(defun bitmap-string-to-special-symbols (string)
  "Convert STRING to a special symbols."
  (mapconcat (function (lambda (char)
			 (or (cdr (assq char bitmap-special-symbol-alist))
			     (char-to-string char))))
	     string ""))

(defun bitmap-lprogress-display (label fmt &optional value &rest args)
  "Print a progress gauge and message in the echo area.
First argument LABEL is ignored.  The rest of the arguments are
the same as to `format'.  [XEmacs 21.2.32 emulating function]"
  (if (and (integerp value) fmt)
      (let ((msg (apply 'format fmt args))
	    (val (abs value)))
	(if (> val 100)
	    (if (zerop (setq val (% val 100)))
		(setq vall 100)))
	(if (or (not window-system)
		(< emacs-major-version 20)
		bitmap-lprogress-display-textual)
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
		      (concat (truncate-string msg (- msgmax 3)) "...")))
		   (t msg))
	     (car bitmap-lprogress-backgrounds)
	     (make-string (/ val 4) (nth 1 bitmap-lprogress-backgrounds))
	     (if (zerop (% val 4))
		 ""
	       (nth (1- (% val 4)) bitmap-lprogress-data))
	     (make-string (- 25 (/ (+ 3 val) 4))
			  (nth 2 bitmap-lprogress-backgrounds))
	     (nth 3 bitmap-lprogress-backgrounds)
	     (bitmap-string-to-special-symbols (format "%3d%%" value))))))
    (message "")))

(defalias 'lprogress-display 'bitmap-lprogress-display)

;;(defun lprogress-display-test ()
;;  (interactive)
;;  (let ((bitmap-lprogress-display-textual nil)
;;	(n -20)
;;	(textual (not (and window-system (>= emacs-major-version 20)))))
;;    (while (< n 120)
;;      (if textual
;;	  (lprogress-display nil "Processing" n)
;;	(lprogress-display nil "Processing..." n))
;;      (sleep-for 0.1)
;;      (setq n (1+ n)))
;;    (if textual
;;	(lprogress-display nil "Processing" n)
;;      (lprogress-display nil "Processing...done" n))))

(provide 'bm-utils)

;;; bm-utiles.el ends here
