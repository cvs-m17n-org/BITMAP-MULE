;; x-face-18.el -- X-Face decoder for MULE 1.

;; Copyright (C) 1996, 1999, 2001 Free Software Foundation, Inc.

;; Author: Hiroshi Ueno <jl07715@yamato.ibm.co.jp>
;;	   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: X-Face, bitmap, MULE

;; This file is part of bitmap-mule.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code:

(defun x-face-decode-message-header ()
  (save-restriction
    (narrow-to-region (goto-char (point-min))
		      (if (search-forward "\n\n" nil t)
			  (1+ (match-beginning 0))
			(point-max)))
    (goto-char (point-min))
    (if (re-search-forward "^X-Face:[ \t]*" nil t)
	(let ((p (match-beginning 0))
	      (beg (match-end 0))
	      (end (progn
		     (while (and (not (eobp))
				 (progn
				   (forward-line)
				   (looking-at "[\t ]"))))
		     (point)))
	      (cur-buf (current-buffer))
	      )
	  (save-restriction
	    (narrow-to-region p end)
	    (delete-region p beg)
	    (call-process-region p (point-max)
				 uncompface-program t t nil)
	    (let (i k k+6 cmp temp)
	      (goto-char (point-min))
	      (search-forward "0x" nil t)
	      (setq cmp (make-vector 18 nil))
	      (setq i 0)
	      (while (< i 48)
		(setq k (* (/ i 16) 6))
		(setq k+6 (+ k 6))
		(while (< k k+6)
		  (setq temp (buffer-substring (point) (+ (point) 2)))
		  (aset cmp k (concat (aref cmp k) temp))
		  (setq k (1+ k))
		  (setq temp (buffer-substring (+ (point) 2) (+ (point) 4)))
		  (aset cmp k (concat (aref cmp k) temp))
		  (setq k (1+ k))
		  (search-forward "0x" nil t)
		  )
		(setq i (1+ i)))
	      (delete-region (point-min)(point-max))
	      (insert "X-Face: ")
	      (setq k 0)
	      (while (< k 6)
		(insert (bitmap-compose (aref cmp k)))
		(setq k (1+ k))
		)
	      (insert ?\n)
	      (setq i 1)
	      (while (< i 3)
		(insert "        ")
		(setq k (* i 6)
		      k+6 (+ k 6))
		(while (< k k+6)
		  (insert (bitmap-compose (aref cmp k)))
		  (setq k (1+ k))
		  )
		(insert ?\n)
		(setq i (1+ i))
		)))))))


;;; @ end
;;;

(provide 'x-face-18)

;;; x-face-18.el ends here
