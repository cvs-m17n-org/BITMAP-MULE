;; x-face-mule.el --  X-Face decoder for MULE.

;; Copyright (C) 1997,1998,1999 Free Software Foundation, Inc.

;; Honorary-Author: Hiroshi Ueno <jl07715@yamato.ibm.co.jp>
;;                  MORIOKA Tomohiko <morioka@jaist.ac.jp>

;; Author: KORIYAMA Naohiro <kory@ba2.so-net.ne.jp>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
;; Maintainer: Katsumi Yamaoka <yamaoka@jpl.org>
;; Version: 0.27
;; Created: 1997/10/24
;; Revised: 1999/08/31
;; Keywords: X-Face, bitmap, Emacs, MULE

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - How to use

;; 1. Build and install `uncompface' program which is available from:
;;
;;	ftp://ftp.xemacs.org/pub/xemacs/aux/compface.tar.gz
;;
;;    It is one example of many.

;; 2. Setting up
;;
;;    [SEMI-MUA, tm-MUA] i.e. cmail, gnus, mh-e, VM, etc.
;;	There is nothing to be done.
;;
;;    [Wanderlust]
;;	Add the following code in your ~/.emacs:
;;
;;	(setq wl-highlight-x-face-func
;;	      'x-face-mule-x-face-decode-message-header)
;;	(require 'x-face-mule)
;;
;;    [before Mew 1.90]
;;	Add the following code in your ~/.emacs:
;;
;;	(add-hook 'mew-summary-display-message-filter-hook ;; 1.90
;;		  'x-face-mule-x-face-decode-message-header)
;;	(add-hook 'mew-message-hook ;; 1.70 or older
;;		  'x-face-mule-x-face-decode-message-header)
;;	(require 'x-face-mule)
;;
;;    [Mew 1.92]
;;	Add the following code in your ~/.emacs:
;;
;;	(setq mew-opt-highlight-x-face t)
;;	(setq mew-opt-highlight-x-face-function
;;	      'x-face-mule-x-face-decode-message-header)
;;	(require 'x-face-mule)
;;
;;    [Mew 1.93 or later]
;;	Add the following code in your ~/.emacs:
;;
;;	(setq mew-use-highlight-x-face t)
;;	(setq mew-use-highlight-x-face-function
;;	      'x-face-mule-x-face-decode-message-header)
;;	(require 'x-face-mule)

;; 3. Customization
;;
;;    * If you don't want to show X-Face at "From:" field,
;;	add the following code in your ~/.emacs:
;;
;;	(setq x-face-mule-highlight-x-face-position 'x-face)
;;
;;	and you can show X-Face at "X-Face:" field.
;;
;;    * If you want to show X-Face like XEmacs style,
;;	add the following code in ~/.eamcs:
;;
;;	(setq x-face-mule-highlight-x-face-style 'xmas)
;;
;;	But, this feature won't work well on some MUA...
;;
;;    * Other features, please read this file...

;; 4. User commands
;;
;; `x-face-mule-toggle-x-face-position'
;;	toggle show position.  from->x-face->nil->from...
;;
;; `x-face-mule-toggle-x-face-style'
;;	toggle show style.  default->xmas->default->...

;; 5. Known bugs & todo
;;
;;    * same x-face saved in cache file.

;; 6. Thanks to the following people have contributed many patches
;;    and suggestions:
;;
;; OKUNISHI Fujikazu   <fuji0924@mbox.kyoto-inet.or.jp>
;; Yuuichi Teranishi   <teranisi@gohome.org>
;; TSUMURA Tomoaki     <tsumura@kuis.kyoto-u.ac.jp>
;; Shiono@FSC          <jun@fsc.fsas.fujitsu.co.jp>
;; Kazuhiro Ohta       <ohta@ele.cst.nihon-u.ac.jp>
;; Tatsuya Ichikawa    <ichikawa@erc.epson.com>
;; Shigeyuki FUKUSHIMA <shige@kuis.kyoto-u.ac.jp>
;; Hideyuki SHIRAI     <Shirai@rdmg.mgcs.mei.co.jp>
;; Koichiro Ohba       <Koichiro.Ohba@nf-system.co.jp>


;;; Code:

(defconst x-face-mule-version-number "0.27")
(defconst x-face-mule-version
  (format "X-Face-Mule v%s" x-face-mule-version-number))

(require 'bitmap)
(eval-when-compile (require 'cl))
(require 'poem)
(require 'pcustom)

(eval-when-compile
  (when (<= emacs-major-version 19)
    ;; The following procedure is imported from Emacs 20.2, bytecomp.el.
    (put 'custom-declare-variable 'byte-hunk-handler
	 'byte-compile-file-form-custom-declare-variable)
    (defun byte-compile-file-form-custom-declare-variable (form)
      (if (memq 'free-vars byte-compile-warnings)
	  (setq byte-compile-bound-variables
		(cons (nth 1 (nth 1 form)) byte-compile-bound-variables)))
      form)
    )

  ;; Bind functions defined by `defun-maybe'.
  (put 'defun-maybe 'byte-hunk-handler 'byte-compile-file-form-defun-maybe)
  (defun byte-compile-file-form-defun-maybe (form)
    (if (and (not (fboundp (nth 1 form)))
	     (memq 'unresolved byte-compile-warnings))
	(setq byte-compile-function-environment
	      (cons (cons (nth 1 form)
			  (cons 'lambda (cdr (cdr form))))
		    byte-compile-function-environment)))
    form)
  )

(defun-maybe match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
\[Emacs 20.3 emulating function]"
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

;; Avoid byte compile warnings.
(defvar gnus-article-buffer)
(defvar vm-message-pointer)

(eval-when-compile
  (mapcar
   (function (lambda (name) (or (fboundp name) (fset name 'ignore))))
   '(cmail-get-page-number-from-summary
     cmail-show-contents
     emh-show
     mew-summary-display
     mh-show-msg
     mime-entity-header-buffer
     tm-mh-e/show)))

(defgroup x-face-mule nil
  "Show X-Face inline for Emacs/Mule."
  :prefix 'x-face-mule
  :group 'news
  :group 'mail)

(defcustom x-face-mule-uncompface-program
  (cond ((eq system-type 'windows-nt)	"uncompface.exe")
	(t				"uncompface"))
  "Program for decoding X-Face string to UNIX Icon."
  :group 'x-face-mule
  :type 'string)

;; Compatibility.
(defvar uncompface-program (symbol-value 'x-face-mule-uncompface-program))
(make-obsolete-variable 'uncompface-program 'x-face-mule-uncompface-program)

(defcustom x-face-mule-highlight-x-face-position 'from
  "This variable says where X-Face is shown.
`from' at From: field, `x-face' at X-Face: field, `off' don't show."
  :group 'x-face-mule
  :type '(radio (const :format "%v " from)
		(const :format "%v " x-face)
		(const off)))

(defcustom x-face-mule-highlight-x-face-style 'default
  "Variable used for setting style for showing X-Face.
When `default' is set, show like this:
From:xxxxxx
     xxxxxx
     xxxxxx Foo Bar <foobar@someware.org>

When `xmas' is set, show like this:
     xxxxxx
     xxxxxx
From:xxxxxx Foo Bar <foobar@someware.org>"
  :set (function
	(lambda (symbol value)
	  (set-default symbol value)
	  (if (eq (symbol-value symbol) 'xmas)
	      (set-default 'x-face-mule-highlight-x-face-position 'from))))
  :group 'x-face-mule
  :type '(radio (const :format "%v " default)
		(const xmas)))

(defcustom x-face-mule-highlight-x-face-position-alist nil
  "If non-nil, x-face-position is changed by major-mode.
A format is like this:

\(setq x-face-mule-highlight-x-face-position-alist
      '((gnus-article-mode . from)
	(gnus-original-article-mode . off)))"
  :group 'x-face-mule
  :type '(repeat (cons :format "%v" (symbol :tag "MAJOR-MODE")
		       (radio :format "POSITION: %v "
			      (const :format "%v " from)
			      (const :format "%v " x-face)
			      (const off)))))

(defcustom x-face-mule-highlight-x-face-style-alist nil
  "If non-nil, x-face-style is changed by major-mode.
A format is like this:

\(setq x-face-mule-highlight-x-face-style-alist
      '((mew-message-mode . xmas)
	(gnus-article-mode . default)))"
  :group 'x-face-mule
  :type '(repeat (cons :format "%v" (symbol :tag "MAJOR-MODE")
		       (radio :format "STYLE: %v"
			      (const :format "%v " default)
			      (const xmas)))))

(defcustom x-face-mule-highlight-x-face-refresh-method-alist
  (list
   (cons 'cmail-summary-mode	(function
				 (lambda ()
				   (cmail-show-contents
				    (cmail-get-page-number-from-summary)))))
   (cons 'wl-summary-mode	(function wl-summary-redisplay))
   (cons 'mew-summary-mode	(function
				 (lambda ()
				   (condition-case nil
				       ;; 1.94b19 or eralier.
				       (mew-summary-display)
				     (wrong-number-of-arguments
				      ;; 1.94b20 or later.
				      (mew-summary-display t))))))
   (cons 'mew-virtual-mode	(function
				 (lambda ()
				   (condition-case nil
				       ;; 1.94b19 or eralier.
				       (mew-summary-display)
				     (wrong-number-of-arguments
				      ;; 1.94b20 or later.
				      (mew-summary-display t))))))
   (cons 'mh-folder-mode	(function
				 (lambda ()
				   (cond ((fboundp (quote emh-show))
					  (emh-show))
					 ((fboundp (quote tm-mh-e/show))
					  (tm-mh-e/show))
					 (t (mh-show-msg nil))))))
   )
  "Alist for refreshing message-buffer.
Format:
\    '((MAJOR-MODE . (FUNCTION ARGS)))"
  :group 'x-face-mule
  :type '(repeat (cons :format "%v" (symbol :tag "MAJOR-MODE")
		       (function :tag "FUNCTION"))))

(defcustom x-face-mule-x-face-decode-message-header-hook nil
  "A hook called before decoding X-Face field."
  :group 'x-face-mule
  :type 'hook)

(defcustom x-face-mule-delete-x-face-field 'always
  "A Valiable says When delete X-Face field. (for NOT Gnus or Mew...)
`always' delete always, `color' delete when color X-Face comes, `mono'
delete when monochrome X-Face comes, `never' never delete."
  :group 'x-face-mule
  :type '(radio (const :format "%v " always)
		(const :format "%v " color)
		(const :format "%v " mono)
		(const never)))

(defcustom x-face-mule-use-cache-file t
  "If non-nil, use X-Face cache file."
  :group 'x-face-mule
  :type 'boolean)

(defcustom x-face-mule-cache-file "~/.x-face-cache"
  "Filename of saving X-Face bitmap-char cache."
  :group 'x-face-mule
  :type 'file)

(defcustom x-face-mule-force-save-cache-file nil
  "If non-nil, save X-Face cache file without asking."
  :group 'x-face-mule
  :type 'boolean)

(defcustom x-face-mule-preserve-original-from-field t
  "If non-nil, preserve original From: field."
  :group 'x-face-mule
  :type 'boolean)

(defcustom x-face-mule-hidden-properties '(invisible t)
  "Property list to use for hiding text.  Is is not recommended that the
value includes `intangible' property because some MUAs, e.g. VM, might
get hung up with it."
  :group 'x-face-mule
  :type 'sexp)

(defcustom x-face-mule-highlight-x-face-face
  'x-face-mule-highlight-x-face-face
  "Face used for highlighting X-Face in the article buffer."
  :group 'x-face-mule
  :type 'face)

(defface x-face-mule-highlight-x-face-face
  '((((class color)
      (background dark))
     (:foreground "Black"  :background "White"))
    (((class color)
      (background light))
     (:foreground "Black"))
    (t
     ()))
  "X-Face face."
  :group 'x-face-mule)

(unless (or (face-foreground 'x-face-mule-highlight-x-face-face)
	    (face-background 'x-face-mule-highlight-x-face-face))
  (set-face-foreground 'x-face-mule-highlight-x-face-face "Black")
  (set-face-background 'x-face-mule-highlight-x-face-face "White"))


;;; Internal variables.
;;

(defvar x-face-mule-cache-file-loaded-p nil)
(defvar x-face-mule-cache-modified-p nil)
(defvar x-face-mule-original-from-field nil)
(defvar x-face-mule-original-x-face-fields nil)
(defvar x-face-mule-x-face-to-rectangle-cache nil)
(defconst x-face-mule-image-file-coding-system
  (if (> emacs-major-version 19)
      'iso-2022-7bit-unix
    '*junet*unix))


;;; Internal functions for cache.
;;

(defun x-face-mule-load-cache-file ()
  "Load X-Face cache file."
  (when (and x-face-mule-use-cache-file
	     (file-exists-p x-face-mule-cache-file))
    (setq x-face-mule-x-face-to-rectangle-cache
	  (mapcar
	   (function (lambda (data)
		       (let ((string (car data)))
			 (set-text-properties 0 (length string) nil string)
			 (cons string (cdr data)))))
	   (with-temp-buffer
	     (insert-file-contents-as-coding-system
	      x-face-mule-image-file-coding-system x-face-mule-cache-file)
	     (condition-case nil
		 (read (current-buffer))
	       (error nil)))))))

(defun x-face-mule-save-cache-file ()
  "Save X-Face cache file."
  (if (and x-face-mule-cache-modified-p
	   x-face-mule-x-face-to-rectangle-cache
	   x-face-mule-use-cache-file
	   x-face-mule-cache-file
	   (or x-face-mule-force-save-cache-file
	       (y-or-n-p "Save X-Face cache now? ")))
      (let ((name (file-name-nondirectory x-face-mule-cache-file))
	    (cache x-face-mule-x-face-to-rectangle-cache)
	    eol data)
	(with-temp-buffer
	  (insert ";; This file is generated automatically by X-Face-Mule "
		  x-face-mule-version-number ".")
	  (setq eol (current-column))
	  (goto-char (point-min))
	  (insert ";; " name)
	  (insert-char ?\  (max 1 (- eol 29 (current-column))))
	  (insert "-*- coding: iso-2022-7bit -*-\n")
	  (goto-char (point-max))
	  (insert "\n\n(\n ")
	  (while (setq data (pop cache))
	    (insert "(" (prin1-to-string (car data)) "\n")
	    (while (setq data (cdr data))
	      (insert "  " (prin1-to-string (car data))
		      (if (> (length data) 1)
			  "\n"
			")\n "))))
	  (insert ")\n\n;; " name " ends here\n")
	  (write-region-as-coding-system x-face-mule-image-file-coding-system
					 (point-min) (point-max)
					 x-face-mule-cache-file)
	  (setq x-face-mule-cache-modified-p nil)))
    (message "")))

;; hooks for saving cache (for Mew, cmail, mh-e, VM, Wanderlust).
(mapcar
 (function (lambda (hook) (add-hook hook 'x-face-mule-save-cache-file)))
 '(mew-quit-hook cmail-quit-hook mh-quit-hook
		 wl-folder-exit-hook wl-exit-hook kill-emacs-hook))


;;; Internal functions for decoding and displaying X-Face.
;;

(defun x-face-mule-convert-x-face-to-rectangle (string)
  "Convert x-face string to rectangle using cache."
  (unless x-face-mule-cache-file-loaded-p
    (x-face-mule-load-cache-file)
    (setq x-face-mule-cache-file-loaded-p t))
  (let ((data (assoc string x-face-mule-x-face-to-rectangle-cache)))
    (unless data
      (setq data
	    (cons
	     string
	     (x-face-mule-convert-icon-to-rectangle
	      (x-face-mule-convert-x-face-to-icon string))))
      (setq x-face-mule-cache-modified-p t)
      (setq x-face-mule-x-face-to-rectangle-cache
	    (cons data x-face-mule-x-face-to-rectangle-cache)))
    (cdr data)))

(defun x-face-mule-convert-x-face-to-icon (string)
  "Decode x-face string to UNIX ICON."
  (with-temp-buffer
    (insert string)
    (as-binary-process
     (call-process-region (point-min) (point-max)
			  x-face-mule-uncompface-program t t nil))
    (buffer-string)))

(defun x-face-mule-convert-vector-to-rectangle (vector)
  "Make x-face rectangle from vector."
  (let ((ret nil) (i 0))
    (while (< i 3)
      (let* ((line "") (k (* i 6)) (k+6 (+ k 6)))
	(while (< k k+6)
	  (setq line (concat line (bitmap-compose (aref vector k))))
	  (incf k))
	(setq ret (append ret (list line))))
      (incf i))
    ret))

(defun x-face-mule-convert-icon-to-rectangle (icon)
  "Decode UNIX ICON to rectangle."
  (let ((i 0) (cmp (make-vector 18 nil)))
    (with-temp-buffer
      (insert icon)
      (goto-char (point-min))
      (search-forward "0x" nil t)
      (while (< i 48)
	(let* (temp (k (* (/ i 16) 6)) (k+6 (+ k 6)))
	  (while (< k k+6)
	    (setq temp (buffer-substring (point) (+ (point) 2)))
	    (aset cmp k (concat (aref cmp k) temp))
	    (incf k)
	    (setq temp (buffer-substring (+ (point) 2) (+ (point) 4)))
	    (aset cmp k (concat (aref cmp k) temp))
	    (incf k)
	    (search-forward "0x" nil t)))
	(incf i)))
    (x-face-mule-convert-vector-to-rectangle cmp)))

(defun x-face-mule-x-face-insert-at-point (rectangles last)
  (let ((beg-point (point))
	insertcolumn rectangle)
    (beginning-of-line)
    (setq insertcolumn (- beg-point (point)))
    (while (progn
	     (setq rectangle (pop rectangles))
	     rectangles)
      (goto-char (setq beg-point (+ insertcolumn (point))))
      (insert rectangle)
      (put-text-property beg-point (point) 'x-face-mule-bitmap-image t)
      (forward-line 1))
    (if (and last
	     (eq x-face-mule-highlight-x-face-position 'from)
	     (eq x-face-mule-highlight-x-face-style 'xmas))
	(goto-char (setq beg-point (+ insertcolumn (point) -5)))
      (goto-char (setq beg-point (+ insertcolumn (point)))))
    (insert rectangle)
    (put-text-property beg-point (point) 'x-face-mule-bitmap-image t)))

(defsubst x-face-mule-insert-invisible-from ()
  (insert "From:")
  (put-text-property (- (point) 5) (point) 'x-face-mule-hidden-from t)
  (insert "     "))

(defun x-face-mule-x-face-allocate-lines (beg end height)
  "Allocate new lines according to x-face-mule-highlight-x-face-position.
returns the begin-point of the x-face rectangle."
  (let ((n 1)
	from
	(mime-entity (get-text-property (point-min) 'mime-view-entity)))
    (cond
     ((or (eq x-face-mule-highlight-x-face-position 'x-face)
	  (and (eq x-face-mule-highlight-x-face-position 'from)
	       (progn
		 (goto-char beg)
		 (not (setq from (and (re-search-forward  "^From:" end t)
				      (point)))))))
      (goto-char beg)
      (insert "X-Face-Img:")
      (prog1
	  (point); return the begin-point
	(insert "\n           \n           ")
	(while (< n height)
	  (insert "\n           \n           \n           ")
	  (incf n))
	(insert "\n")
	(put-text-property beg (point) 'mime-view-entity mime-entity)))
     ((and (eq x-face-mule-highlight-x-face-position 'from)
	   (if from
	       (goto-char from)
	     (goto-char beg)
	     (re-search-forward  "^From:" end t)))
      (cond
       ((eq x-face-mule-highlight-x-face-style 'xmas)
	(beginning-of-line)
	(setq beg (point))
	(x-face-mule-insert-invisible-from)
	(prog1
	    (point); return the begin-point
	  (insert "\n")
	  (x-face-mule-insert-invisible-from)
	  (insert "\n")
	  (while (< n height)
	    (x-face-mule-insert-invisible-from)
	    (insert "\n")
	    (x-face-mule-insert-invisible-from)
	    (insert "\n")
	    (x-face-mule-insert-invisible-from)
	    (insert "\n")
	    (incf n))
	  (put-text-property beg (point) 'mime-view-entity mime-entity)))
       (t
	(prog1
	    (point); return the begin-point
	  (setq beg (- (point) 5))
	  (insert "\n     \n     ")
	  (while (< n height)
	    (insert "\n     \n     \n     ")
	    (incf n))
	  (put-text-property beg (point) 'mime-view-entity mime-entity))))))))

(defun x-face-mule-change-highlight-x-face-method-by-alist ()
  (when x-face-mule-highlight-x-face-position-alist
    (let ((position (cdr
		     (assq major-mode
			   x-face-mule-highlight-x-face-position-alist))))
      (when (memq position '(from x-face off))
	(setq x-face-mule-highlight-x-face-position position))))
  (when x-face-mule-highlight-x-face-style-alist
    (let ((style (cdr
		  (assq major-mode
			x-face-mule-highlight-x-face-style-alist))))
      (when (memq style '(default xmas))
	(setq x-face-mule-highlight-x-face-style style)))))

(defun x-face-mule-analyze-x-face-type (beg end)
  "Analyze type of X-Face."
  (goto-char beg)
  (if (re-search-forward
       "^X-Face-Type: \\(RGB; \\)?geometry=\\([0-9]+\\)x\\([0-9]+\\)" end t)
      (list
       (if (match-string-no-properties 1) 'color 'mono)
       (string-to-number (match-string-no-properties 2))
       (string-to-number (match-string-no-properties 3)))
    (let ((i 0) (j 0))
      (goto-char beg)
      (while (re-search-forward
	      "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n" end t)
	(incf i))
      (setq j (if (> i 0) 1 0))
      (list 'mono i j))))

(defun x-face-mule-save-original-x-face-fields (beg end)
  "Save original X-Face field."
  (let ((x-face-type nil) (x-faces nil))
    (goto-char beg)
    (when (re-search-forward
	   "^X-Face-Type: \\(RGB; \\)?geometry=\\([0-9]+\\)x\\([0-9]+\\)"
	   end t)
      (setq x-face-type (match-string-no-properties 0)))
    (goto-char beg)
    (while (re-search-forward
	    "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n" end t)
      (setq x-faces (append x-faces (list (match-string-no-properties 0)))))
    (make-local-variable 'x-face-mule-original-x-face-fields)
    (setq x-face-mule-original-x-face-fields (list x-face-type x-faces))))

(defun x-face-mule-save-original-from-field (beg end)
  "Save original From field."
  (goto-char beg)
  (set (make-local-variable 'x-face-mule-original-from-field)
       (when (re-search-forward "^From: *\\(.*\\(\n[ \t].*\\)*\\)\n" end t)
	 (match-string-no-properties 0))))

(defun x-face-mule-insert-original-from-field ()
  "Insert original From field and make it to be invisible."
  (when (and x-face-mule-original-from-field
	     x-face-mule-preserve-original-from-field)
    (goto-char (point-min))
    (when (re-search-forward "^From:" nil t)
      (goto-char (match-beginning 0)))
    (let ((beg-point (point)))
      (insert x-face-mule-original-from-field)
      (put-text-property beg-point
			 ;; Don't include the last newline, if so,
			 ;; gnus will break it after sorting headers.
			 (1- (point))
			 'x-face-mule-original-from t))))

(defsubst x-face-mule-x-face-decode-message-header-1 ()
  "Decode and show X-Face.  The buffer is expected to be narrowed to
just the headers of the article."
  (run-hooks 'x-face-mule-x-face-decode-message-header-hook)
  (when (and window-system
	     (memq x-face-mule-highlight-x-face-position '(from x-face)))
    (x-face-mule-save-original-x-face-fields (point-min) (point-max))
    (x-face-mule-save-original-from-field (point-min) (point-max))
    (let* ((inhibit-read-only t)
	   buffer-read-only
	   faces faces-s (first t)
	   (x-face-type (x-face-mule-analyze-x-face-type (point-min)
							 (point-max)))
	   (col-type (nth 0 x-face-type))
	   (width (* (nth 1 x-face-type) (if (eq col-type 'color) 3 1)))
	   (height (nth 2 x-face-type))
	   w)
      (goto-char (point-min))
      (let ((h 0))
	(while (<= (incf h) height)
	  (setq faces-s nil)
	  (setq w 0)
	  (while (<= (incf w) width)
	    (re-search-forward "^X-Face: *\\(.*\\(\n[ \t].*\\)*\\)\n" nil t)
	    (setq faces-s (cons (match-string-no-properties 1) faces-s))
	    (when (or (eq x-face-mule-delete-x-face-field 'always)
		      (eq x-face-mule-delete-x-face-field col-type))
	      (delete-region (match-beginning 0) (match-end 0))))
	  (setq faces (append faces faces-s))))
      (x-face-mule-change-highlight-x-face-method-by-alist)
      (when faces
	(while faces
	  (let ((begin-point
		 (if first
		     (progn
		       (setq first nil)
		       (x-face-mule-x-face-allocate-lines (point-min)
							  (point-max) height))
		   (end-of-line 2) (point))))
	    (setq w 0)
	    (while (<= (incf w) width)
	      (goto-char begin-point)
	      (x-face-mule-x-face-insert-at-point
	       (x-face-mule-convert-x-face-to-rectangle (pop faces))
	       (< (length faces) width)))))
	(x-face-mule-insert-original-from-field)
	))))

(defun x-face-mule-highlight-header ()
  (let ((inhibit-read-only t) buffer-read-only
	start (end (point-min)))
    (while (and (setq start (text-property-any
			     end (point-max) 'x-face-mule-bitmap-image t))
		(setq end (text-property-not-all
			   start (point-max) 'x-face-mule-bitmap-image t)))
      (overlay-put (make-overlay start end)
		   'face x-face-mule-highlight-x-face-face))
    (setq end (point-min))
    (while (and (setq start (text-property-any
			     end (point-max) 'x-face-mule-hidden-from t))
		(setq end (text-property-not-all
			   start (point-max) 'x-face-mule-hidden-from t)))
      (add-text-properties start end x-face-mule-hidden-properties))
    (setq end (point-min))
    (while (and (setq start (text-property-any
			     end (point-max) 'x-face-mule-original-from t))
		(setq end (text-property-not-all
			   start (point-max) 'x-face-mule-original-from t)))
      (add-text-properties start
			   (1+ end);; Include the last newline.
			   x-face-mule-hidden-properties))))

;; Compatibility.
(define-obsolete-function-alias
  (function x-face-decode-message-header)
  (function x-face-mule-x-face-decode-message-header))

(defun x-face-mule-x-face-decode-message-header (&optional beg end)
  "Decode and show X-Face."
  (save-restriction
    (narrow-to-region (goto-char (or beg (point-min)))
		      (or end (if (search-forward "\n\n" nil t)
				  (point)
				(point-max))))
    (x-face-mule-x-face-decode-message-header-1)
    (unless (or (and (boundp 'gnus-article-buffer)
		     (eq (get-buffer gnus-article-buffer) (current-buffer)))
		(and (boundp 'vm-message-pointer)
		     (fboundp
		      'x-face-mule-original-vm-energize-headers-and-xfaces)
		     vm-message-pointer)
		)
      (x-face-mule-highlight-header))))


;;; Commands.
;;

;;;###autoload
(defun x-face-mule-version ()
  "Version number of this version of X-Face-Mule."
  (interactive)
  (message x-face-mule-version))

;;;###autoload
(defun x-face-mule-toggle-x-face-position (&optional arg)
  "Toggle position of showing X-Face."
  (interactive "P")
  (let ((pos (cond ((eq x-face-mule-highlight-x-face-position 'from)
		    'x-face)
		   ((eq x-face-mule-highlight-x-face-position 'x-face)
		    'off)
		   (t 'from)))
	(table '(("From:" . from) ("X-Face:" . x-face) ("Off" . off)))
	(method (cdr
		 (assq major-mode
		       x-face-mule-highlight-x-face-refresh-method-alist)))
	msg)
    (setq x-face-mule-highlight-x-face-position
	  (if arg
	      (cdr (assoc (completing-read
			   "Show X-Face at> " table nil t
			   (concat (capitalize (symbol-name pos))
				   (and (memq pos '(from x-face)) ":")))
			  table))
	    pos))
    (when (interactive-p)
      (setq msg (cond
		 ((eq x-face-mule-highlight-x-face-position 'from)
		  "Show X-Face at From:")
		 ((eq x-face-mule-highlight-x-face-position 'x-face)
		  "Show X-Face at X-Face-Img:")
		 (t "Don't Show X-Face")))
      (message "%s..." msg))
    (when method
      (funcall method))
    (when (interactive-p)
      (message "%s...done" msg))))

;;;###autoload
(defun x-face-mule-toggle-x-face-style (&optional arg)
  "Toggle style of showing X-Face."
  (interactive "P")
  (let ((style (cond ((eq x-face-mule-highlight-x-face-style 'default)
		      'xmas)
		     ((eq x-face-mule-highlight-x-face-style 'xmas)
		      'default)))
	(table '(("Xmas" . xmas) ("Default" . default)))
	(method (cdr
		 (assq major-mode
		       x-face-mule-highlight-x-face-refresh-method-alist)))
	msg)
    (setq x-face-mule-highlight-x-face-style
	  (if arg
	      (cdr (assoc (completing-read
			   "Show X-Face style> " table nil t
			   (capitalize (symbol-name style)))
			  table))
	    style))
    (when (interactive-p)
      (setq msg (cond
		 ((eq x-face-mule-highlight-x-face-style 'xmas)
		  "Show X-Face in XEmacs style")
		 ((eq x-face-mule-highlight-x-face-style 'default)
		  "Show X-Face in default style")))
      (message "%s..." msg))
    (when method
      (funcall method))
    (when (interactive-p)
      (message "%s...done" msg))))


;;; MUA dependencies.
;;

;; gnus
(unless (boundp 'gnus-bitmap-redefine-will-be-evaluated-after-gnus-is-loaded)
  (autoload 'gnus-bitmap-redefine "gnus-bitmap")
  (eval-after-load "gnus" '(gnus-bitmap-redefine))
  (set 'gnus-bitmap-redefine-will-be-evaluated-after-gnus-is-loaded t))

;; VM
(autoload 'vm-bitmap-redefine "vm-bitmap")
(eval-after-load "vm" '(vm-bitmap-redefine))


(provide 'x-face-mule)

;; x-face-mule.el ends here
