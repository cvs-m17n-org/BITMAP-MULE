;; bitmap-bi.el -- bitmap handler for MULE 1.*, 2.*, Emacs 20.1 and 20.2.

;; Copyright (C) 1992 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1996,1997,1998 MORIOKA Tomohiko

;; Author: Ken'ichi HANDA <handa@etl.go.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: bitmap, xbm, MULE

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code:

(require 'emu)
(eval-when-compile (require 'static))

(static-cond
 ((and (boundp 'MULE) (<= emacs-major-version 18))
  (defvar lc-bitmap
    (new-private-character-set 2 1 3 0 ?0 0 "BITMAP 8x16")
    "Leading character for BITMAP.8x16."))
 ((boundp 'MULE)
  (defvar lc-bitmap
    (new-private-character-set 2 1 3 0 ?0 0 "BITMAP 8x16" "bitmap")
    "Leading character for BITMAP.8x16."))
 (t
  (if (not (memq 'bitmap charset-list))
      (define-charset nil 'bitmap
	[2 96 1 0 ?0 0 "BITMAP" "BITMAP.8x16" "8x16 bitmap elements"]))
  (defconst lc-bitmap 'bitmap)))

;; Avoid byte compile warning
(eval-when-compile
  (autoload 'fontset-list "fontset");; for Emacs 20.1 or later
  (autoload 'read-hexa "bitmap"))

(if window-system
    (static-if (<= emacs-major-version 18)
	(x-set-font "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0"
		    lc-bitmap 0)
      (let ((fontsets (fontset-list))
	    fontset size)
	(while fontsets
	  (setq fontset (car fontsets)
		fontsets (cdr fontsets)
		size (fontset-pixel-size fontset))
	  (cond
	   ((= size 12)
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--12-*-100-100-m-*-bitmap.6x12-0"))
	   ((= size 14)
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--14-*-100-100-m-*-bitmap.7x14-0"))
	   ((= size 16)
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0"))
	   ((= size 20)
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--20-*-100-100-m-*-bitmap.10x20-0"))
	   ((= size 24)
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--24-*-100-100-m-*-bitmap.12x24-0"))
	   (t
	    (set-fontset-font
	     fontset lc-bitmap
	     "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0")))))))

;; Block (all bits set) character
(defvar bitmap-block (make-char lc-bitmap 32 33))

;; Space (all bits cleared) character
(defvar bitmap-space (make-char lc-bitmap 32 32))

(eval-when-compile
  (defmacro bitmap-pad-string (string padding right)
    (` (let ((padding (mapconcat (function char-to-string)
				 (make-list (, padding) bitmap-space) "")))
	 (if (, right)
	     (concat (, string) padding)
	   (concat padding (, string)))))))

(eval-when-compile
  (defmacro bitmap-rotate-string (string length rotation left bytes)
    (` (let ((length (/ (, length) (, bytes))))
	 (if (<= (+ length (, rotation)) 16)
	     (bitmap-pad-string (, string) (, rotation) (, left))
	   (let ((string (, string))
		 (rotation (, rotation)))
	     (if (< length 16)
		 (setq string (bitmap-pad-string string
						 (- 16 length) (, left))
		       rotation (+ rotation length -16)))
	     (if (, left)
		 (progn
		   (setq rotation (* rotation (, bytes)))
		   (concat (substring string rotation)
			   (substring string 0 rotation)))
	       (setq rotation (* (- 16 rotation) (, bytes)))
	       (concat (substring string rotation)
		       (substring string 0 rotation)))))))))

(defun bitmap-compose (hex &optional rotation)
  "Return a string of composite characters which represents the bitmap-
pattern HEX.  HEX is a string of hexa decimal for 8x16 dot-pattern(s).
For example, the pattern \"00818142422424181824244242818100\" is for
a bitmap of shape something like 'X' character.

Elements of each character will be rotated left by the number ROTATION,
if it is specified, before composing.  If ROTATION is negative, rotating
is actually to the right.  It is useful to make a different string even
though which has the same representation."
  (let* ((len (/ (length hex) 2))
	 (bytes (charset-bytes lc-bitmap))
	 (cmpstr "")
	 (buf (make-string (* 16 bytes) 0))
	 (block-flag t)
	 (i 0)
	 (j 0)
	 left row code)
    (if (numberp rotation)
	(progn
	  (setq left (natnump rotation)
		rotation (% (abs rotation) 16))
	  (if (zerop rotation)
	      (setq rotation nil))))
    (while (< i len)
      (setq row (read-hexa (substring hex (* i 2) (+ (* i 2) 2))))
      (if block-flag
	  (setq block-flag (eq row 255)))
      (if (zerop row)
	  nil
	(setq code (+ (* (% i 16) 255) row -1))
	(sset buf j (make-char lc-bitmap
			       (+ (/ code 96) 33) (+ (% code 96) 32)))
	(setq j (+ j bytes)))
      (setq i (1+ i))
      (if (or (zerop (% i 16))
	      (>= i len))
	  (setq cmpstr
		(concat
		 cmpstr
		 (if rotation
		     (compose-string
		      (cond ((and block-flag (eq j (* 16 bytes)))
			     (bitmap-pad-string (char-to-string bitmap-block)
						rotation left))
			    ((zerop j)
			     (bitmap-pad-string (char-to-string bitmap-space)
						rotation left))
			    ((eq j bytes)
			     (bitmap-pad-string (substring buf 0 bytes)
						rotation left))
			    (t
			     (bitmap-rotate-string (substring buf 0 j)
						   j rotation left bytes))))
		   (cond ((and block-flag (eq j (* 16 bytes)))
			  (char-to-string bitmap-block))
			 ((zerop j)
			  " ")
			 ((eq j bytes)
			  (substring buf 0 bytes))
			 (t
			  (compose-string (substring buf 0 j))))))
		block-flag t
		j 0)))
    cmpstr))


;;; @ end
;;;

(provide 'bitmap-bi)

;;; bitmap-bi.el ends here
