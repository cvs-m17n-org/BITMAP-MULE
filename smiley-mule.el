;;; smiley-mule.el --- smiley faces decoder for MULE

;; Copyright (C) 1996, 1998, 1999 MORIOKA Tomohiko
;; Copyright (C) 1996, 1999, 2000, 2001 Katsumi Yamaoka

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
;; Created: 1996/7/26
;; Revised: 2001/02/02
;; Keywords: smiley, face-mark, MULE, bitmap, xbm, fun

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
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - How to use.
;;   If you use tm, put the following lines in your .emacs file:
;;	(if window-system
;;	    (progn
;;	      (autoload 'smiley-buffer "smiley-mule" nil t)
;;	      (autoload 'smiley-encode-buffer "smiley-mule" nil t)
;;	      (add-hook 'mime-viewer/plain-text-preview-hook 'smiley-buffer)
;;	      (add-hook 'mime-editor/translate-hook 'smiley-encode-buffer)
;;	      ))
;;   Or if you use SEMI, use the following lines instead:
;;	(if window-system
;;	    (progn
;;	      (autoload 'smiley-buffer "smiley-mule" nil t)
;;	      (autoload 'smiley-encode-buffer "smiley-mule" nil t)
;;	      (add-hook 'mime-display-text/plain-hook 'smiley-buffer)
;;	      (add-hook 'mime-edit-translate-hook 'smiley-encode-buffer)
;;	      ))
;;   Of course, it may be available for other hooks to filter messages.

;;; Code:

(require 'bitmap)
(require 'pcustom)

(defface smiley-face '((t (:background "Yellow" :foreground "Black")))
  "Face used for displaying smiley faces."
  :group 'bitmap-mule
  :group 'faces)

(defface smiley-manga-face '((t (:background "White" :foreground "Black")))
  "Face used for displaying smiley manga faces."
  :group 'bitmap-mule
  :group 'faces)

(defvar smiley-bitmap-for-smile
  (bitmap-decode-xbm
   '(24 16
	"000000"
	"f8c10f"
	"042210"
	"000000"
	"f0c007"
	"68a109"
	"68a109"
	"68a109"
	"f0c007"
	"000000"
	"000000"
	"508004"
	"000000"
	"202201"
	"001c00"
	"000000"))
  "
   oooooo     oooooo
  '      '   '      '
   o'MM'o    o'MM''o
   M MM M    M MM  M
    ''''      '''''
    o o        o  o
     o   o   o  o
          '''
")

(defvar smiley-bitmap-for-winking
  (bitmap-decode-xbm
   '(24 16
	"000000"
	"f8c10f"
	"042210"
	"000000"
	"00c007"
	"f0a009"
	"08a109"
	"00a009"
	"00c007"
	"000000"
	"000000"
	"508004"
	"000000"
	"202201"
	"001c00"
	"000000"))
  "
   oooooo     oooooo
  '      '   '      '
    oooo     o'MM''o
   '    '    M MM  M
              '''''
    o o        o  o
     o   o   o  o
          '''
")

(defvar smiley-bitmap-for-ase
  (bitmap-decode-xbm
   '(24 16
	"000000"
	"f8c10f"
	"042210"
	"000000"
	"f0c003"
	"68a105"
	"68a105"
	"68a105"
	"f0c023"
	"000020"
	"000050"
	"504052"
	"000050"
	"209120"
	"000e00"
	"000000"))
  "
   oooooo     oooooo
  '      '   '      '
   o'MM'o    o'MM'o
   M MM M    M MM M
    ''''      ''''   M
    o o       o  o  M M
     o  o   o  o    'o'
         '''
")

(defvar smiley-bitmap-for-ase2
  (bitmap-decode-xbm
   '(32 16
	"00000000"
	"f0831f00"
	"08442000"
	"00000000"
	"e0810700"
	"d0420b00"
	"d0420b00"
	"d0420b00"
	"e0818710"
	"00008010"
	"00004029"
	"a0804429"
	"00004029"
	"40228110"
	"001c0000"
	"00000000"))
  "
    oooooo     oooooo
   '      '   '      '
    o'MM'o    o'MM'o
    M MM M    M MM M
     ''''      ''''    M    M
     o o       o  o   M M  M M
      o  o   o  o     'o'  'o'
          '''
")

(defvar smiley-bitmap-for-ase3
  (bitmap-decode-xbm
   '(24 16
	"000000"
	"f8c10f"
	"042210"
	"000000"
	"180007"
	"e0e000"
	"fcf30f"
	"c0e100"
	"380027"
	"000020"
	"000050"
	"504052"
	"000050"
	"209120"
	"000e00"
	"000000"))
  "
   oooooo     oooooo
  '      '   '      '
   ''ooo     ooo'''
  ''''MMM'  'MMM''''
   '''          '''  M
    o o       o  o  M M
     o  o   o  o    'o'
         '''
")

(defvar smiley-bitmap-for-weep
  (bitmap-decode-xbm
   '(24 16
	"1c003c"
	"e28043"
	"006300"
	"180018"
	"600007"
	"80e300"
	"fcf73f"
	"80e300"
	"600007"
	"58001a"
	"400002"
	"a00005"
	"a00005"
	"a01c05"
	"402202"
	"000000"))
  "
 o'''ooo       ooo''''o
   oo   ''   ''    oo
     ''ooo   ooo'''
  '''''MMM' 'MMM''''''
   oo'M         'M'oo
     o'o        o'o
     M M  ooo   M M
      '  '   '   '
")

(defvar smiley-bitmap-FaceAngry
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"17e8"
	"3ffc"
	"73ce"
	"6186"
	"6186"
	"0180"
	"e187"
	"f3cf"
	"3ffc"
	"17e8"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM'M      M'MMM
MM''MMo  oMM''MM
M    MM  MM    M
M    oooooo    M
MMooMM''''MMooMM
MMMoM      MoMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceGoofy
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0db0"
	"fdbf"
	"ffff"
	"07e0"
	"07e0"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M MMooooooooMM M
MMM''''''''''MMM
MMMoo      ooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceGrinning
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"f99f"
	"f99f"
	"33cc"
	"67e6"
	"c7e3"
	"dffb"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M  MMMMMMMMMM  M
MMo 'Mo  oM' oMM
MMMoo MMMM ooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceHappy
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0db0"
	"fdbf"
	"f3cf"
	"e7e7"
	"c7e3"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M MMooooooooMM M
MMo 'MMMMMM' oMM
MMMoo '''' ooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceIronic
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"01b0"
	"e1bf"
	"f3cf"
	"3fe0"
	"17e0"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M    oooooooMM M
MMooMM'''''' oMM
MMMoM      ooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceKOed
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"1ff8"
	"1ff8"
	"07e0"
	"8ff1"
	"d99b"
	"718e"
	"d99b"
	"8db1"
	"0180"
	"f18f"
	"f7ef"
	"07e0"
	"1ff8"
	"1ff8"
	"ffff"
	))
  "
MMMMM''''''MMMMM
MMM''      ''MMM
M''Mo oMMo oM''M
M  oM'MooM'Mo  M
M ''   ''   '' M
Moo MMMMMMMM ooM
MMMoo      ooMMM
MMMMMooooooMMMMM
")

(defvar smiley-bitmap-FaceNyah
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"07e0"
	"73ce"
	"718e"
	"0180"
	"0180"
	"e187"
	"e3c7"
	"e7e7"
	"c7e3"
	"dffb"
	"ffff"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM' ooo  ooo 'MM
M   '''  '''   M
M    oooooo    M
MMo  MMMMMM  oMM
MMMoo MMMM ooMMM
MMMMMMMMMMMMMMMM
")

(defvar smiley-bitmap-FaceSad
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0180"
	"e187"
	"f3cf"
	"3ffc"
	"1ff8"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M    oooooo    M
MMooMM''''MMooMM
MMMMM      MMMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceStartled
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"e187"
	"f18f"
	"3bdc"
	"3bdc"
	"f7ef"
	"eff7"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M   oMMMMMMo   M
MM MMM    MMM MM
MMMo'MMMMMM'oMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceStraight
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0180"
	"e187"
	"e3c7"
	"07e0"
	"07e0"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M    oooooo    M
MMo  ''''''  oMM
MMMoo      ooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceTalking
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0180"
	"e19f"
	"e3df"
	"07df"
	"07ef"
	"1ff6"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M    oooooooo  M
MMo  '''MMMMM MM
MMMoo   'MM'oMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceTasty
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"27e4"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"6180"
	"f180"
	"fbc0"
	"fbef"
	"f7ef"
	"1ff8"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''o    o''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M   oMMo       M
MM MMMMMoooo oMM
MMMoM''''''MoMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceWry
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"07e0"
	"67e6"
	"63c6"
	"6186"
	"0180"
	"0180"
	"018f"
	"c3cf"
	"e7e0"
	"c7e7"
	"1fff"
	"3ffc"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM''      ''MMM
MM'  MM  MM  'MM
M    ''  ''    M
M       oooo   M
MMo  oMM'''' oMM
MMMoo ''MMMooMMM
MMMMMMooooMMMMMM
")

(defvar smiley-bitmap-FaceYukky
  (bitmap-decode-xbm
   '(16 16
	"ffff"
	"3ffc"
	"1ff8"
	"17e8"
	"3ffc"
	"73ce"
	"6186"
	"0180"
	"0180"
	"f187"
	"f3c7"
	"e7e7"
	"c7e3"
	"dffb"
	"ffff"
	"ffff"
	))
  "
MMMMMM''''MMMMMM
MMM'M      M'MMM
MM''MMo  oMM''MM
M    ''  ''    M
M   ooooooo    M
MMo 'MMMMMM  oMM
MMMoo MMMM ooMMM
MMMMMMMMMMMMMMMM
")

(defvar smiley-face-bitmap-list
  (list
   (vector "^_^;;;"
	   (bitmap-compose (aref smiley-bitmap-for-ase3 0))
	   'smiley-manga-face)
   (vector "^^;;;"
	   (bitmap-compose (aref smiley-bitmap-for-ase3 0) 1)
	   'smiley-manga-face)
   (vector "^_^;;"
	   (bitmap-compose (aref smiley-bitmap-for-ase2 0))
	   'smiley-manga-face)
   (vector "^^;;"
	   (bitmap-compose (aref smiley-bitmap-for-ase2 0) 1)
	   'smiley-manga-face)
   (vector "^_^;"
	   (bitmap-compose (aref smiley-bitmap-for-ase 0))
	   'smiley-manga-face)
   (vector "^^;"
	   (bitmap-compose (aref smiley-bitmap-for-ase 0) 1)
	   'smiley-manga-face)
   (vector ";_;"
	   (bitmap-compose (aref smiley-bitmap-for-weep 0))
	   'smiley-manga-face)
   (vector "T_T"
	   (bitmap-compose (aref smiley-bitmap-for-weep 0) 1)
	   'smiley-manga-face)
   (vector ">_<"
	   (bitmap-compose (aref smiley-bitmap-for-weep 0) 2)
	   'smiley-manga-face)
   (vector "^_^"
	   (bitmap-compose (aref smiley-bitmap-for-smile 0))
	   'smiley-manga-face)
   (vector "(^^)"
	   (bitmap-compose (aref smiley-bitmap-for-smile 0) 1)
	   'smiley-manga-face)
   (vector ":-<"
	   (bitmap-compose (aref smiley-bitmap-FaceAngry 0))
	   'smiley-face)
   (vector ":-]"
	   (bitmap-compose (aref smiley-bitmap-FaceGoofy 0))
	   'smiley-face)
   (vector ":-D"
	   (bitmap-compose (aref smiley-bitmap-FaceGoofy 0) 1)
	   'smiley-face)
   (vector ":-)"
	   (bitmap-compose (aref smiley-bitmap-FaceHappy 0))
	   'smiley-face)
   (vector ":-/"
	   (bitmap-compose (aref smiley-bitmap-FaceIronic 0))
	   'smiley-face)
   (vector "8-|"
	   (bitmap-compose (aref smiley-bitmap-FaceKOed 0))
	   'smiley-face)
   (vector ":-#"
	   (bitmap-compose (aref smiley-bitmap-FaceNyah 0))
	   'smiley-face)
   (vector ":-("
	   (bitmap-compose (aref smiley-bitmap-FaceSad 0))
	   'smiley-face)
   (vector ":-O"
	   (bitmap-compose (aref smiley-bitmap-FaceStartled 0))
	   'smiley-face)
   (vector ":-|"
	   (bitmap-compose (aref smiley-bitmap-FaceStraight 0))
	   'smiley-face)
   (vector ":-p"
	   (bitmap-compose (aref smiley-bitmap-FaceTalking 0))
	   'smiley-face)
   (vector ":-d"
	   (bitmap-compose (aref smiley-bitmap-FaceTasty 0))
	   'smiley-face)
   (vector ";-)"
	   (bitmap-compose (aref smiley-bitmap-for-winking 0))
	   'smiley-manga-face)
   (vector ":-V"
	   (bitmap-compose (aref smiley-bitmap-FaceWry 0))
	   'smiley-face)
   (vector ":-P"
	   (bitmap-compose (aref smiley-bitmap-FaceYukky 0))
	   'smiley-face))
  "List of smiley face data.  Each element looks like [TEXT BITMAP FACE],
where TEXT is a smiley face represented by the ordinary text, BITMAP is a
string consists of composite bitmap characters and FACE is a face used for
highlighting the inline image.")

;;;###autoload
(defun smiley-buffer (&optional buffer start end)
  "Display smiley faces in the buffer."
  (interactive "*")
  (if window-system
      (save-excursion
	(if buffer
	    (set-buffer buffer))
	(save-restriction
	  (narrow-to-region (or start (point-min)) (or end (point-max)))
	  (let ((rest smiley-face-bitmap-list)
		elem overlay case-fold-search)
	    (while rest
	      (setq elem (car rest)
		    rest (cdr rest))
	      (goto-char (point-min))
	      (while (search-forward (aref elem 0) nil t)
		(replace-match "")
		(setq overlay (make-overlay (point) (progn
						      (insert (aref elem 1))
						      (point))))
		(overlay-put overlay 'face (aref elem 2))
		(overlay-put overlay 'evaporate t))))))
    (if (interactive-p)
	(message "You're not under window system."))))

;;;###autoload
(defun smiley-region (beg end)
  "Display smiley faces in the region between BEG and END."
  (interactive "*r")
  (if window-system
      (save-restriction
	(narrow-to-region beg end)
	(smiley-buffer))
    (if (interactive-p)
	(message "You're not under window system."))))

;;;###autoload
(defun smiley-toggle-buffer (&optional arg buffer start end)
  "Toggle displaying smiley faces.
With arg, turn displaying on if and only if arg is positive."
  (interactive "*P")
  (if window-system
      (save-excursion
	(if buffer
	    (set-buffer buffer))
	(save-restriction
	  (narrow-to-region (or start (point-min)) (or end (point-max)))
	  (goto-char (point-min))
	  (if (and (not (and (numberp arg) (< arg 0)))
		   (or (and (numberp arg) (> arg 0))
		       (let (case-fold-search)
			 (re-search-forward
			  (mapconcat
			   (function
			    (lambda (elem)
			      (regexp-quote (aref elem 0))))
			   smiley-face-bitmap-list "\\|")
			  nil t))))
	      (smiley-buffer)
	    (smiley-encode-buffer))))
    (if (interactive-p)
	(message "You're not under window system."))))

;;;###autoload
(defun smiley-encode-buffer ()
  "Encode smiley faces to ordinary text in the buffer."
  (interactive "*")
  (if window-system
      (save-excursion
	(let ((rest smiley-face-bitmap-list)
	      elem)
	  (while rest
	    (setq elem (car rest)
		  rest (cdr rest))
	    (goto-char (point-min))
	    (while (search-forward (aref elem 1) nil t)
	      (replace-match "")
	      (insert (aref elem 0))))))
    (if (interactive-p)
	(message "You're not under window system."))))


;;; @ end
;;;

(provide 'smiley-mule)

(run-hooks 'smiley-mule-load-hook)

;;; smiley-mule.el ends here
