;; bitmap.el -- bitmap file handler on MULE.

;; Copyright (C) 1992 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: Ken'ichi HANDA <handa@etl.go.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id: bitmap.el,v 7.15 1996/11/27 13:44:46 morioka Exp $
;; Keywords: bitmap, xbm, MULE

;; This file is part of tl (Tiny Library).

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
       (defvar charset-bitmap
	 (new-private-character-set 2 1 3 0 ?0 0 "BITMAP 8x16" "bitmap")
	 "Leading character for BITMAP.8x16.")
       )
      (t
       (let ((info
	      ;; [ WIDTH DIRECTION ISO-CLASS ISO-FINAL-CHAR ISO_GRAPHIC_PLANE
	      ;;   SHORT-NAME LONG-NAME DESCRIPTION ])
	      [1 0 3 ?0 0 "bitmap" "BITMAP 8x16"
		 "bitmap 8x16"])
	     )
	 (setq charset-bitmap
	       (get-undefined-private-charset (aref info 2) (aref info 0)))
	 (define-charset charset-bitmap info)
	 )
       ))

(if window-system
    (mapcar (lambda (fontset)
	      (if (= (fontset-pixel-size fontset) 16)
		  (set-fontset-font
		   fontset charset-bitmap
		   "-etl-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0")
		))
	    (fontset-list))
  )

;; Block (all bits set) character
(defvar bitmap-block (make-character charset-bitmap 32 33))

;; Simple examples:
;;	(bitmap-compose "00FF00FF00FF00FF00FF00FF00FF00FF")
;;	(bitmap-compose
;;	  "FF00FF00FF00FF00FF00FF00FF00FF00AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")

(defun read-hexa (str)
  (let ((result 0) (i 0) (max (length str)))
    (while (< i max)
      (let ((ch (aref str i)))
	(cond((and (<= ?0 ch) (<= ch ?9))
	      (setq result (+ (* result 16) (- ch ?0))))
	     ((and (<= ?a ch) (<= ch ?f))
	      (setq result (+ (* result 16) (+ (- ch ?a) 10))))
  	     ((and (<= ?A ch) (<= ch ?F))
	      (setq result (+ (* result 16) (+ (- ch ?A) 10)))))
	(setq i (1+ i))))
    result))

(defun bitmap-compose (hex)
  "Return a string of composite characters which represents BITMAP-PATTERN.
BITMAP-PATTERN is a string of hexa decimal for 8x16 dot-pattern.
For example the pattern \"0081814242242442111124244242818100\" is
 for a bitmap of shape something like 'X' character."
  (let* ((len (/ (length hex) 2))
	 (bytes (charset-bytes charset-bitmap))
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
	    (sset buf j (make-character charset-bitmap c1 c2))
	    (setq j (+ j bytes))))
      (setq i (1+ i))
      (if (or (= (% i 16) 0) (>= i len))
	  (setq cmpstr
		(concat cmpstr
			(cond ((and block-flag (= j 64))
			       (char-to-string bitmap-block))
			      ((= j 0)
			       " ")
			      ((= j 1)
			       (substring buf 0 1))
			      (t
			       (compose-string (substring buf 0 j)))))
		block-flag t
		j 0)))
    cmpstr))


;;; @ BDF
;;;

;; Internal variables  -- declared here to reduce garbage collection.
(defconst *hex* (vector (make-string 96 0) (make-string 96 0)))
(defconst *hex-len* (length *hex*))
(defconst *cmp* (make-vector *hex-len* nil))

(defun bdf-to-bitmap (bdf)
  "Set *cmp* a vector of string for BDF.
BDF is a vector of string, each elements corresponds to a line of bitmap
of difinition of a character glyph in bdf file."
  (let ((width (length (aref bdf 0)))
	(height (length bdf))
	i j)
    (if (or (/= (/ (+ height 15) 16) *hex-len*)
	    (/= width (length (aref *hex* 0))))
	(progn
	  (setq *hex-len* (/ (+ height 15) 16))
	  (setq *hex* (make-vector *hex-len* nil))
	  (setq *cmp* (make-vector *hex-len* nil))
	  (setq i 0)
	  (while (< i *hex-len*)
	    (aset *hex* i (make-string 96 0))
	    (setq i (1+ i)))))
    (setq j 0)
    (while (< j width)
      (setq i 0)
      (while (< i (* *hex-len* 16))
	(aset (aref *hex* (/ i 16))
	      (+ (* (/ j 2) 32) (* (% i 16) 2) (% j 2))
	      (if (< i height) (aref (aref bdf i) j) 0))
	(setq i (1+ i)))
      (setq j (1+ j)))
    (setq i 0)
    (while (< i *hex-len*)
      (aset *cmp* i (bitmap-compose (aref *hex* i)))
      (setq i (1+ i)))
    *cmp*
    ))


;;; @ XBM
;;;

(defun bitmap-decode-xbm (xbm)
  (let ((hexa-string "0123456789ABCDEF")
	(reverse-bit '[0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15])
	w h i j i2 i2+2 bitmap cmp c temp line)
    (setq w (car xbm)
	  xbm (cdr xbm)
	  h (car xbm)
	  xbm (cdr xbm))
    (setq bitmap (make-vector h 0))
    (setq cmp (make-vector (/ (+ h 15) 16) nil))
    (setq w (/ (+ w 7) 8))
    (setq j 0)
    (while (< j h)
      (setq i 0
	    line (car xbm)
	    i2 0
	    i2+2 2)
      (aset bitmap j (make-vector w 0))
      (while (< i w)
	(setq temp (substring line i2 i2+2))
	(aset (aref bitmap j) i temp)
	(setq c (read-hexa temp))
	(aset temp 0 (aref hexa-string (aref reverse-bit (% c 16))))
	(aset temp 1 (aref hexa-string (aref reverse-bit (/ c 16))))
	(setq i (1+ i)
	      i2 i2+2
	      i2+2 (+ i2+2 2))
	)
      (setq j (1+ j)
	    xbm (cdr xbm)))
    (setq i 0)
    (while (< i w)
      (setq j 0)
      (while (< j h)
	(aset cmp (/ j 16)
	      (concat (aref cmp (/ j 16))
		      (aref (aref bitmap j) i)))
	(setq j (1+ j)))
      (if (> (% h 16) 0)
	  (aset cmp (/ h 16)
		(concat (aref cmp (/ h 16))
			(make-string (* (- 16 (% h 16)) 2) ?0))))
      (setq i (1+ i)))
    cmp))

(defun bitmap-read-xbm-buffer (buf)
  (save-excursion
    (let (width height dest)
      (save-excursion
	(set-buffer buf)
	(goto-char (point-min))
	(search-forward "width ")
	(setq width (read (current-buffer)))
	(goto-char (point-min))
	(search-forward "height ")
	(setq height (read (current-buffer)))
	(let ((w (/ (+ width 7) 8))
	      i (j 0)
	      line)
	  (while (< j height)
	    (setq i 0
		  line "")
	    (while (< i w)
	      (search-forward "0x")
	      (setq line
		    (concat line (buffer-substring (point) (+ (point) 2))))
	      (setq i (1+ i))
	      )
	    (setq dest (cons line dest)
		  j (1+ j))
	    ))
	(cons width (cons height (nreverse dest)))
	))))

(defun bitmap-read-xbm-file (file)
  (bitmap-read-xbm-buffer (find-file-noselect (expand-file-name file)))
  )

(defun bitmap-insert-xbm-buffer (buffer)
  "Insert xbm bitmap from BUFFER. Very slow! [bitmap.el]"
  (let* ((cmp (bitmap-decode-xbm (bitmap-read-xbm-buffer buffer)))
	 (len (length cmp))
	 (i 0))
    (while (< i len)
      (insert (bitmap-compose (aref cmp i)) ?\n)
      (setq i (1+ i)))))

(defun bitmap-insert-xbm-file (file &optional no-kill)
  "Insert xbm bitmap from FILE. Very slow! [bitmap.el]"
  (interactive "fxbm file: ")
  (let ((buf (find-file-noselect (expand-file-name file))))
    (bitmap-insert-xbm-buffer buf)
    (or no-kill
	(kill-buffer buf)
	)
    ))


;;; @ end
;;;

(provide 'bitmap)

;;; bitmap.el ends here
