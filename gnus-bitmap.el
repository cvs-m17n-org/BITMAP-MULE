;; gnus-bitmap.el -- bitmap utilities for gnus.

;; Copyright (C) 1999,2000 Free Software Foundation, Inc.

;; Author: KORIYAMA Naohiro <kory@ba2.so-net.ne.jp>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
;;         Tatsuya Ichikawa <ichikawa@erc.epson.com>
;; Created: 1999/08/20
;; Revised: 2000/05/18
;; Keywords: bitmap, x-face, splash, gnus

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

;; How to use:
;;   If you would like to splash the startup screen with bitmap image,
;;   put the following lines in your .emacs file before gnus is loaded.
;;
;;	;; It must be placed in .emacs file before gnus.el(c) is loaded.
;;	(if window-system
;;	    (progn
;;	      (autoload 'gnus-bitmap-splash "gnus-bitmap")
;;	      (add-hook 'gnus-load-hook 'gnus-bitmap-splash)))
;;
;;   Or you can inhibit the use of bitmap images (except for X-Face) as
;;   follows.
;;
;;	(setq gnus-bitmap-splash-image nil
;;	      gnus-bitmap-modeline-image nil)

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'static))
(require 'pcustom)
(require 'alist)
(require 'bitmap)
(require 'x-face-mule)


;;; Internal variables and the related functions.
;;

(eval-when-compile
  (defmacro gnus-bitmap-splash-image-internal ()
    (let ((file (expand-file-name "gnus.xbm"
				  (if (string-match "/tm/$" default-directory)
				      "../bitmap-mule/";; tm-8
				    "./"))))
      (if (file-exists-p file)
	  (progn
	    (message "  ++ decoding internal image for splashing...")
	    (bitmap-decode-xbm (bitmap-read-xbm-file file)))
	(byte-compile-warn "Warning: file \"gnus.xbm\" not found.")
	nil)))

  (defmacro gnus-bitmap-modeline-image-internal ()
    (let ((file (expand-file-name "gnus-pointer.xbm"
				  (if (string-match "/tm/$" default-directory)
				      "../bitmap-mule/";; tm-8
				    "./"))))
      (if (file-exists-p file)
	  (progn
	    (message
	     "  ++ decoding internal image for modeline identifier...")
	    (bitmap-decode-xbm (bitmap-read-xbm-file file)))
	(byte-compile-warn "Warning: file \"gnus-pointer.xbm\" not found.")
	nil)))

  (defmacro gnus-bitmap-xbm-to-bitmap (cmp)
    (` (let ((cmp (, cmp)))
	 (if cmp
	     (let ((len (length cmp))
		   (bitmap (bitmap-compose (aref cmp 0)))
		   (i 1))
	       (while (< i len)
		 (setq bitmap (concat bitmap "\n"
				      (bitmap-compose (aref cmp i)))
		       i (1+ i)))
	       bitmap)))))
  )

(defconst gnus-bitmap-splash-image-internal
  (gnus-bitmap-xbm-to-bitmap (gnus-bitmap-splash-image-internal)))

(defconst gnus-bitmap-modeline-image-internal
  (gnus-bitmap-xbm-to-bitmap (gnus-bitmap-modeline-image-internal)))

(defvar gnus-bitmap-splash-image-data nil)

(defvar gnus-bitmap-modeline-identifier nil)

(eval-when-compile
  (makunbound 'gnus-bitmap-broken-facility-check)
  (defcustom gnus-bitmap-broken-facility-check t
    "The value t means `:set' does not work at the loading time.  For instance
it will be occurred under Mule 2.3 based on Emacs 19.34 using custom-1.9962."
    :set (function (lambda (symbol value) (set-default symbol nil)))))

(static-if gnus-bitmap-broken-facility-check
    (progn
      (defun gnus-bitmap-set-splash-image-data (arg)
	(setq gnus-bitmap-splash-image-data
	      (cond ((eq 'internal arg)
		     gnus-bitmap-splash-image-internal)
		    ((and (stringp arg) (file-exists-p arg))
		     (gnus-bitmap-xbm-to-bitmap arg)))))
      (defun gnus-bitmap-set-modeline-image-data (arg)
	(setq gnus-bitmap-modeline-identifier
	      (cond ((eq 'internal arg)
		     gnus-bitmap-modeline-image-internal)
		    ((and (stringp arg) (file-exists-p arg))
		     (gnus-bitmap-xbm-to-bitmap arg)))))
      )
  (defmacro gnus-bitmap-set-splash-image-data (arg)
    (` (setq gnus-bitmap-splash-image-data
	     (cond ((eq 'internal (, arg))
		    gnus-bitmap-splash-image-internal)
		   ((and (stringp (, arg)) (file-exists-p (, arg)))
		    (gnus-bitmap-xbm-to-bitmap (, arg)))))))
  (defmacro gnus-bitmap-set-modeline-image-data (arg)
    (` (setq gnus-bitmap-modeline-identifier
	     (cond ((eq 'internal (, arg))
		    gnus-bitmap-modeline-image-internal)
		   ((and (stringp (, arg)) (file-exists-p (, arg)))
		    (gnus-bitmap-xbm-to-bitmap (, arg)))))))
  )


;;; Options.
;;

(defcustom x-face-mule-gnus-force-decode-headers nil
  "If non-nil, display X-Face even if `gnus-show-mime' is nil."
  :type 'boolean
  :group 'x-face-mule)

(defcustom gnus-bitmap-splash-image 'internal
  "If it is the symbol `internal', use the internal image for splashing
the startup screen.  It can also be a name of XBM file.  If it is nil,
bitmap image is not used."
  :type (list 'radio
	      (list 'const :tag
		    (concat "Internal image\n"
			    gnus-bitmap-splash-image-internal)
		    'internal)
	      '(file  :tag "XBM file")
	      '(const :tag "Off" nil))
  :set (function
	(lambda (symbol value)
	  (gnus-bitmap-set-splash-image-data value)
	  (set-default symbol value)))
  :group 'bitmap-mule
  :group 'gnus-start)

(defcustom gnus-bitmap-modeline-image 'internal
  "If it is the symbol `internal', use the internal image for displaying
a nifty image in the modeline.  It can also be a name of XBM file.  If it
is nil or the value of `emacs-major-version' is less than 20, bitmap image
is not used."
  :type (list 'radio
	      (list 'const :tag
		    (concat "Internal image ["
			    gnus-bitmap-modeline-image-internal "]")
		    'internal)
	      '(file  :tag "XBM file")
	      '(const :tag "Off" nil))
  :set (function
	(lambda (symbol value)
	  (gnus-bitmap-set-modeline-image-data value)
	  (set-default symbol value)))
  :group 'bitmap-mule
  :group 'gnus-start)

(static-when gnus-bitmap-broken-facility-check
  (gnus-bitmap-set-splash-image-data gnus-bitmap-splash-image)
  (gnus-bitmap-set-modeline-image-data gnus-bitmap-modeline-image))


;;; Avoid byte compile warnings.
;;

(defvar gnus-article-buffer)
(defvar gnus-article-display-hook)
(defvar gnus-article-x-face-command)
(defvar gnus-ignored-headers)
(defvar gnus-mule-bitmap-image-file)
(defvar gnus-simple-splash)
(defvar gnus-strict-mime)
(defvar gnus-treat-display-xface)
(defvar gnus-treat-hide-headers)
(defvar gnus-version)
(defvar gnus-visible-headers)
(defvar last)
(eval-when-compile
  (fset 'gnus-bitmap-original-gnus-group-startup-message 'ignore)
  (autoload 'article-goto-body "gnus-art")
  (autoload 'gnus-indent-rigidly "gnus")
  (autoload 'gnus-splash "gnus")
  (autoload 'gnus-summary-select-article "gnus-sum"))


;;; X-Face functions.
;;

;;;###autoload
(defun x-face-mule-gnus-article-display-x-face (&rest args)
  "Decode and show X-Face.  The buffer is expected to be narrowed to just the
headers of the article.  If `gnus-article-x-face-command' is set to the
symbol of this function, gnus will call it for each X-Face fields.  So we
set the bound variable `last' to T in order to make good use of time."
  (when window-system
    (let ((x-face-mule-highlight-x-face-style
	   (if (and (eq x-face-mule-highlight-x-face-style 'xmas)
		    (memq this-command
			  '(gnus-summary-toggle-mime)))
	       'default
	     x-face-mule-highlight-x-face-style))
	  x-face-mule-preserve-original-from-field)
      (x-face-mule-x-face-decode-message-header-1)
      (x-face-mule-highlight-header)))
  (setq last t))

(defun x-face-mule-gnus-highlight-headers-if-no-mime ()
  "Decode and show X-Face even if `gnus-show-mime' is nil.  It is
controlled by the value of `x-face-mule-gnus-force-decode-headers'."
  (when (and x-face-mule-gnus-force-decode-headers
	     (eq major-mode 'gnus-article-mode)
	     (memq x-face-mule-highlight-x-face-position '(from x-face)))
    (x-face-decode-message-header)))


;;; Splash the startup screen.
;;

(defun gnus-bitmap-sit-for-0 ()
  (remove-hook 'gnus-startup-hook 'gnus-bitmap-sit-for-0)
  (sit-for 0))

(defun gnus-bitmap-startup-message (&optional x y)
  "Insert startup message in current buffer."
  (if (null gnus-bitmap-splash-image-data)
      (gnus-bitmap-original-gnus-group-startup-message x y)
    (erase-buffer)
    (insert gnus-version "\n")
    (let ((fill-column (1- (window-width))))
      (center-region (point-min) (point-max)))
    (let ((start (point))
	  width)
      ;; Insert an image.
      (insert gnus-bitmap-splash-image-data)
      (setq width (current-column))
      (insert "\n")
      ;; And then hack it.
      (gnus-indent-rigidly start (point)
			   (/ (max (- (window-width) (or x (1- width))) 0) 2))
      (goto-char start)
      (insert-char
       ?\n (/ (max 0 (- (window-height) (count-lines start (point-max)))) 2)))
    ;; Fontify some.
    (put-text-property (point-min) (point-max) 'face 'gnus-splash-face)
    (goto-char (point-min))
    (setq mode-line-buffer-identification gnus-version)
    (setq gnus-simple-splash nil)
    (set-buffer-modified-p t)
    ;; Redisplay after `gnus-startup-hook' is evaluated.
    (add-hook 'gnus-startup-hook 'gnus-bitmap-sit-for-0 'append)))


;;; Modeline identification.
;;

(defun gnus-bitmap-mode-line-buffer-identification (line)
  (let ((l (car line)))
    (if (and (stringp l) gnus-bitmap-modeline-identifier)
	(list gnus-bitmap-modeline-identifier
	      (if (string-match "^Gnus: *" l)
		  (substring l (match-end 0))
		l))
      line)))


;;; Setup.
;;

(set-alist 'x-face-mule-highlight-x-face-refresh-method-alist
	   'gnus-summary-mode
	   (function (lambda () (gnus-summary-select-article nil t))))

(when window-system
  (add-hook 'gnus-exit-gnus-hook 'x-face-mule-save-cache-file))

(defun gnus-bitmap-redefine (&optional splash)
  "Redifine variables and functions for the use of bitmap-mule.  If SPLASH
is non-nil, the function `gnus-group-startup-message' is also redefined
and splashing the startup screen with a bitmap image."
  (require 'gnus-art)
  (setq gnus-strict-mime nil)

  ;; Modify `gnus-visible-headers' and `gnus-ignored-headers'.
  (cond ((stringp gnus-visible-headers)
	 (let ((xf (string-match gnus-visible-headers "X-Face:"))
	       (xfi (string-match gnus-visible-headers "X-Face-Img:")))
	   (cond ((not (or xf xfi))
		  (setq gnus-visible-headers
			(format "\\(%s\\)\\|^X-Face:\\|^X-Face-Img:"
				gnus-visible-headers)))
		 ((not xf)
		  (setq gnus-visible-headers
			(format "\\(%s\\)\\|^X-Face:"
				gnus-visible-headers)))
		 ((not xfi)
		  (setq gnus-visible-headers
			(format "\\(%s\\)\\|^X-Face-Img:"
				gnus-visible-headers))))))
	((consp gnus-visible-headers)
	 (unless (member "^X-Face:" gnus-visible-headers)
	   (setq gnus-visible-headers
		 (append gnus-visible-headers '("^X-Face:"))))
	 (unless (member "^X-Face-Img:" gnus-visible-headers)
	   (setq gnus-visible-headers
		 (append gnus-visible-headers '("^X-Face-Img:")))))
	((stringp gnus-ignored-headers)
	 (let ((regexp (format  "\\\\|%s\\|%s\\\\|%s"
				"\\^?X-Face\\(-Img\\)?:"
				"\\^?X-Face\\(-Img\\)?:"
				"\\^?X-Face\\(-Img\\)?:")))
	   (while (string-match regexp gnus-ignored-headers)
	     (setq gnus-ignored-headers
		   (concat (substring gnus-ignored-headers 0
				      (match-beginning 0))
			   (substring gnus-ignored-headers
				      (match-end 0)))))))
	((consp gnus-ignored-headers)
	 (when (member "^X-Face:" gnus-ignored-headers)
	   (setq gnus-ignored-headers
		 (delete "^X-Face:" gnus-ignored-headers)))
	 (when (member "^X-Face-Img:" gnus-ignored-headers)
	   (setq gnus-ignored-headers
		 (delete "^X-Face-Img:" gnus-ignored-headers)))
	 ))

  (when (boundp 'gnus-treat-display-xface)
    (if (not (stringp gnus-article-x-face-command))
	;; Don't touch any user options.
	nil
      ;; Pterodactyl Gnus, T-gnus 6.10.064 or later.
      (setq
       gnus-treat-display-xface
       (if (and (featurep 'running-pterodactyl-gnus-0_73-or-later); T-gnus
		(not x-face-mule-gnus-force-decode-headers))
	   '(and mime head)
	 'head)
       gnus-treat-hide-headers 'head
       gnus-article-x-face-command 'x-face-mule-gnus-article-display-x-face)))

  ;; Advise the hiding function.
  (defadvice article-hide-headers (after x-face-mule-highlight-header
					 activate)
    "Highlight X-Face field(s) after `article-hide-headers' is done."
    (x-face-mule-highlight-header))

  ;; Gnus 5.7, Semi-gnus 6.10, Nana-gnus 6.13 or earlier.
  (when (and (boundp 'gnus-article-display-hook)
	     (or (memq 'gnus-article-hide-headers-if-wanted
		       gnus-article-display-hook)
		 (memq 'gnus-article-maybe-highlight
		       gnus-article-display-hook)))
    (remove-hook 'gnus-article-display-hook 'gnus-article-display-x-face)
    (add-hook 'gnus-article-display-hook
	      'x-face-mule-gnus-highlight-headers-if-no-mime 'append)
    )

  ;; Redifine the splash function.
  (when splash
    (unless (fboundp 'gnus-bitmap-original-gnus-group-startup-message)
      (fset 'gnus-bitmap-original-gnus-group-startup-message
	    (symbol-function 'gnus-group-startup-message)))
    (fset 'gnus-group-startup-message 'gnus-bitmap-startup-message)
    ;; Splash the screen when `gnus' is being loaded.
    (unless (featurep 'gnus)
      (gnus-splash)))

  ;; Invalidate the old style splashing feature.
  (when (boundp 'gnus-mule-bitmap-image-file)
    (setq gnus-mule-bitmap-image-file nil))

  (when (eq emacs-major-version 20)
    ;; Redifine the modeline identify function.
    (fset 'gnus-mode-line-buffer-identification
	  'gnus-bitmap-mode-line-buffer-identification))
  )

;;;###autoload
(defun gnus-bitmap-splash ()
  "Splash the startup screen.  This function has only limited use for
`gnus-load-hook' which should be set before `gnus' is loaded."
  (when (and window-system
	     (not (featurep 'gnus))
	     gnus-bitmap-splash-image-data)
    ;; Suppress splashing with an ascii picture.
    (setq this-command nil)
    (let ((fns (cdr (assoc "gnus" after-load-alist))))
      (if fns
	  (put-alist 'gnus-bitmap-redefine '(t) fns)
	(eval-after-load "gnus" '(gnus-bitmap-redefine 'splash))))))

(eval-and-compile (autoload 'smiley-toggle-buffer "smiley-mule"))

;;;###autoload
(defun gnus-smiley-display (&optional arg)
  "Display \"smileys\" as small graphical icons.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (if window-system
      (save-excursion
	(set-buffer gnus-article-buffer)
	(save-restriction
	  (widen)
	  (article-goto-body)
	  (narrow-to-region (point) (point-max))
	  (let ((inhibit-read-only t)
		buffer-read-only)
	    (smiley-toggle-buffer arg))))
    (when (interactive-p)
      (message "You're not under window system."))))


(provide 'gnus-bitmap)

;; gnus-bitmap.el ends here
