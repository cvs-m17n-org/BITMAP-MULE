;; bm-utils.el -- utility functions using bitmap-mule.

;; Copyright (C) 2000 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 2000/03/28
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

(defvar bitmap-lprogress-data
  (mapcar 'bitmap-compose '("55AA552A152A152A152A152A15AA55AA"
			    "55AA550A050A050A050A050A05AA55AA"
			    "55AA5502010201020102010201AA55AA")))

(defvar bitmap-lprogress-backgrounds
  (list (bitmap-compose "05020502050205020502050205020502")
	(string-to-char (bitmap-compose "55AA5500000000000000000000AA55AA"))
	(string-to-char (bitmap-compose "55AA55AA55AA55AA55AA55AA55AA55AA"))
	(bitmap-compose "40A040A040A040A040A040A040A040A0")))

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
	(let ((msgmax (- (window-width (minibuffer-window)) 27)))
	  (cond ((< msgmax 3)
		 (setq msg ""))
		((> (string-width msg) msgmax)
		 (setq msg (concat (truncate-string msg (- msgmax 3))
				   "...")))))
	(message "%s%s%s%s%s%s"
		 msg
		 (car bitmap-lprogress-backgrounds)
		 (make-string (/ value 4) (nth 1 bitmap-lprogress-backgrounds))
		 (if (zerop (% value 4))
		     ""
		   (nth (1- (% value 4)) bitmap-lprogress-data))
		 (make-string (- 25 (/ (+ 3 value) 4))
			      (nth 2 bitmap-lprogress-backgrounds))
		 (nth 3 bitmap-lprogress-backgrounds))))))

(defalias 'lprogress-display 'bitmap-lprogress-display)

;;(defun lprogress-display-test ()
;;  (interactive)
;;  (let ((bitmap-lprogress-display-textual)
;;	(n 0)
;;	(textual (not (and window-system (>= emacs-major-version 20)))))
;;    (while (< n 100)
;;      (if textual
;;	  (lprogress-display nil "Processing" n)
;;	(lprogress-display nil "Processing...% 3d%%" n n))
;;      (sleep-for 0.1)
;;      (setq n (1+ n)))
;;    (if textual
;;	(lprogress-display nil "Processing" n)
;;      (lprogress-display nil "Processing...done" n))))

(provide 'bm-utils)

;;; bm-utiles.el ends here
