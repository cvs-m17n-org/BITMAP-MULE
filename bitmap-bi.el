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

(cond ((boundp 'MULE)
       (defvar lc-bitmap
	 (new-private-character-set 2 1 3 0 ?0 0 "BITMAP 8x16" "bitmap")
	 "Leading character for BITMAP.8x16.")
       )
      (t
       (define-charset nil 'bitmap
	 [2 96 1 0 ?0 0 "BITMAP" "BITMAP.8x16" "8x16 bitmap elements"])
       (defconst lc-bitmap 'bitmap)
       ))

(if window-system
    (mapcar (lambda (fontset)
	      (if (= (fontset-pixel-size fontset) 16)
		  (set-fontset-font
		   fontset lc-bitmap
		   "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0")
		))
	    (fontset-list))
  )

;; Block (all bits set) character
(defvar bitmap-block (make-char lc-bitmap 32 33))

(defun bitmap-compose (hex)
  "Return a string of composite characters which represents BITMAP-PATTERN.
BITMAP-PATTERN is a string of hexa decimal for 8x16 dot-pattern.
For example the pattern \"0081814242242442111124244242818100\" is
 for a bitmap of shape something like 'X' character."
  (let* ((len (/ (length hex) 2))
	 (bytes (charset-bytes lc-bitmap))
	 (cmpstr "")
	 (buf (make-string 64 0))
	 block-flag i j row code c1 c2)
    (setq i 0 j 0 block-flag t)
    (while (< i len)
      (setq row (read-hexa (substring hex (* i 2) (+ (* i 2) 2))))
      (if block-flag
	  (setq block-flag (= row 255)))
      (if (/= row 0)
	  (progn
	    (setq code (+ (* (% i 16) 255) row -1))
	    (setq c1 (+ (/ code 96) 33)
		  c2 (+ (% code 96) 32))
	    (sset buf j (make-char lc-bitmap c1 c2))
	    (setq j (+ j bytes))))
      (setq i (1+ i))
      (if (or (= (% i 16) 0) (>= i len))
	  (setq cmpstr
		(concat cmpstr
			(cond ((and block-flag (= j 64))
			       (char-to-string bitmap-block)
			       )
			      ((= j 0)
			       " ")
			      ((= j 4)
			       (substring buf 0 4)
			       )
			      (t
			       (compose-string (substring buf 0 j))
			       )))
		block-flag t
		j 0)))
    cmpstr))


;;; @ end
;;;

(provide 'bitmap-bi)

;;; bitmap-bi.el ends here
