;; bm-utils.el -- utility functions using bitmap-mule.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2000/03/28
;; Revised: 2000/03/29
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
  (list (cons ?- (bitmap-compose "000000000000003C7800000000000000"))
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
  (if (or (not (and (natnump value) (<= value 100)))
	  (and (null fmt) (null args)))
      (message "")
    (let ((msg (apply 'format fmt args)))
      (if (or (not window-system)
	      (< emacs-major-version 20)
	      bitmap-lprogress-display-textual)
	  (message "%s%s%s"
		   msg (make-string (/ value 5) ?.)
		   (if (eq 100 value) "done" ""))
	(let ((msgmax (- (window-width (minibuffer-window)) 35)))
	  (cond ((< msgmax 0)
		 (setq msg (make-string (max 0 (+ 3 msgmax)) ? )))
		((> (string-width msg) msgmax)
		 (setq msg (truncate-string msg msgmax)
		       msg (concat
			    msg "..."
			    (make-string (- msgmax (string-width msg)) ? ))))
		(t
		 (setq msg (concat
			    msg
			    (make-string (+ 3 (- msgmax (string-width msg)))
					 ? ))))))
	(let ((cursor-in-echo-area t))
	  (message "%s%s%s%s%s%s%s"
		   msg
		   (car bitmap-lprogress-backgrounds)
		   (make-string (/ value 4)
				(nth 1 bitmap-lprogress-backgrounds))
		   (if (zerop (% value 4))
		       ""
		     (nth (1- (% value 4)) bitmap-lprogress-data))
		   (make-string (- 25 (/ (+ 3 value) 4))
				(nth 2 bitmap-lprogress-backgrounds))
		   (nth 3 bitmap-lprogress-backgrounds)
		   (bitmap-string-to-special-symbols
		    (format "%3d%%" value))))))))

(defalias 'lprogress-display 'bitmap-lprogress-display)

;;(defun lprogress-display-test ()
;;  (interactive)
;;  (let ((bitmap-lprogress-display-textual)
;;	(n 0)
;;	(textual (not (and window-system (>= emacs-major-version 20)))))
;;    (while (< n 100)
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
