;; bitmap-ci.el -- bitmap handler for Emacs 20.3 and later

;; Copyright (C) 1992 Electrotechnical Laboratory, JAPAN.
;; Copyright (C) 1996,1997,1998 MORIOKA Tomohiko

;; Author: Ken'ichi HANDA <handa@etl.go.jp>
;;         MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: bitmap, xbm, MULE

;; This file is part of BITMAP-MULE.

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

(require 'poem)

(defvar bitmap-alterable-charset nil
  "*A symbol of a charset which will be used to a substitute for `bitmap'
only when there is no room for a new private charset.  It is useless for
Emacs 21.  The valid alterable charsets for Emacs 20 are `indian-1-column'
and `tibetan-1-column'.")

(eval-when-compile
  ;; For picking up the macro `dolist'.
  (if (<= emacs-major-version 20)
      (progn
	(require 'cl)
	;; `dolist' may be defined in egg.el, we should use the proper one.
	(load "cl-macs" nil t))))

(if (not (charsetp 'bitmap))
    (condition-case code
	(define-charset nil 'bitmap
	  [2 96 1 0 ?0 0 "BITMAP" "BITMAP.8x16" "8x16 bitmap elements"])
      (error
       ;; Attempt to divert `bitmap-alterable-charset' to `bitmap'.
       (if (and (charsetp bitmap-alterable-charset)
		(eq 2 (charset-dimension bitmap-alterable-charset))
		(eq 1 (charset-width bitmap-alterable-charset)))
	   (progn
	     (setcar (memq bitmap-alterable-charset charset-list) 'bitmap)
	     (let ((info (charset-info bitmap-alterable-charset)))
	       (aset info 3 96);; chars
	       (aset info 8 ?0);; iso-final-char
	       (aset info 11 "BITMAP");; short-name
	       (aset info 12 "BITMAP.8x16");; long-name
	       (aset info 13 "8x16 bitmap elements");; description
	       (aset info 14 nil);; plist
	       (put 'bitmap 'charset info))
	     (put bitmap-alterable-charset 'charset nil)
	     (declare-equiv-charset 2 96 ?0 'bitmap)
	     (if (and window-system
		      (boundp 'global-fontset-alist))
		 (dolist (elem (symbol-value 'global-fontset-alist))
		   (setcdr elem (delete (assq bitmap-alterable-charset
					      (cdr elem))
					(cdr elem))))))
	 (error "%s" (car (cdr code)))))))

;; Avoid byte compile warning
(eval-when-compile
  (autoload 'fontset-list "fontset");; for Emacs 20
  (autoload 'read-hexa "bitmap"))

(if window-system
    (let ((fontsets (fontset-list))
	  fontset size)
      (while fontsets
	(setq fontset (car fontsets)
	      fontsets (cdr fontsets)
	      size (fontset-pixel-size fontset))
	(cond
	 ((eq size 12)
	  (set-fontset-font
	   fontset 'bitmap
	   "-*-fixed-medium-r-*--12-*-100-100-m-*-bitmap.6x12-0"))
	 ((eq size 14)
	  (set-fontset-font
	   fontset 'bitmap
	   "-*-fixed-medium-r-*--14-*-100-100-m-*-bitmap.7x14-0"))
	 ((eq size 20)
	  (set-fontset-font
	   fontset 'bitmap
	   "-*-fixed-medium-r-*--20-*-100-100-m-*-bitmap.10x20-0"))
	 ((eq size 24)
	  (set-fontset-font
	   fontset 'bitmap
	   "-*-fixed-medium-r-*--24-*-100-100-m-*-bitmap.12x24-0"))
	 (t
	  (set-fontset-font
	   fontset 'bitmap
	   "-*-fixed-medium-r-*--16-*-100-100-m-*-bitmap.8x16-0"))))))

;; Block (all bits set) character
(defvar bitmap-block (make-char 'bitmap 32 33))

;; Space (all bits cleared) character
(defvar bitmap-space (make-char 'bitmap 32 32))

(eval-when-compile
  (defmacro bitmap-pad-string (string padding right)
    `(if ,right
	 (concat ,string (make-string ,padding ?\ ))
       (concat (make-string ,padding ?\ ) ,string))))

(eval-when-compile
  (defmacro bitmap-rotate-string (string length rotation left)
    `(if (<= (+ ,length ,rotation) 16)
	 (bitmap-pad-string ,string ,rotation ,left)
       (let ((string ,string)
	     (rotation ,rotation))
	 (if (< ,length 16)
	     (setq string (bitmap-pad-string string (- 16 ,length) ,left)
		   rotation (+ rotation ,length -16)))
	 (if ,left
	     (concat (substring string rotation)
		     (substring string 0 rotation))
	   (setq rotation (- 16 rotation))
	   (concat (substring string rotation)
		   (substring string 0 rotation)))))))

(defun bitmap-compose (hex &optional rotation)
  "Return a string of composite characters which represents the bitmap-
pattern HEX.  HEX is a string of hexa decimal for 8x16 dot-pattern(s).
For example, the pattern \"00818142422424181824244242818100\" is for
a bitmap of shape something like 'X' character.

Elements of each character will be rotated left by the number ROTATION,
if it is specified, before composing.  If ROTATION is negative, rotating
is actually to the right.  It is useful to make a different string even
though which has the same representation."
  (let ((len (/ (length hex) 2))
	(cmpstr "")
	(buf (make-string 16 bitmap-block))
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
	(sset buf j (make-char 'bitmap (+ (/ code 96) 33) (+ (% code 96) 32)))
	(setq j (1+ j)))
      (setq i (1+ i))
      (if (or (zerop (% i 16))
	      (>= i len))
	  (setq cmpstr
		(concat
		 cmpstr
		 (if rotation
		     (compose-string
		      (cond ((and block-flag (eq j 16))
			     (bitmap-pad-string (char-to-string bitmap-block)
						rotation left))
			    ((zerop j)
			     (bitmap-pad-string (char-to-string bitmap-space)
						rotation left))
			    ((eq j 1)
			     (bitmap-pad-string (substring buf 0 1)
						rotation left))
			    (t
			     (bitmap-rotate-string (substring buf 0 j)
						   j rotation left))))
		   (cond ((and block-flag (eq j 16))
			  (char-to-string bitmap-block))
			 ((zerop j)
			  " ")
			 ((eq j 1)
			  (substring buf 0 1))
			 (t
			  (compose-string (substring buf 0 j))))))
		block-flag t
		j 0)))
    cmpstr))


;;; @ end
;;;

(provide 'bitmap-ci)

;;; bitmap-ci.el ends here
