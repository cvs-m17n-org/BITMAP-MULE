;; po.el -- a BDF font editor

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id: po.el,v 1.2 1998-03-23 02:14:44 morioka Exp $
;; Keywords: bitmap, bdf, MULE

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

(require 'bitmap)

(defconst po-block-alist
  '((?0 . "・・・・")
    (?1 . "・・・■")
    (?2 . "・・■・")
    (?3 . "・・■■")
    (?4 . "・■・・")
    (?5 . "・■・■")
    (?6 . "・■■・")
    (?7 . "・■■■")
    (?8 . "■・・・")
    (?9 . "■・・■")
    (?a . "■・■・")
    (?b . "■・■■")
    (?c . "■■・・")
    (?d . "■■・■")
    (?e . "■■■・")
    (?f . "■■■■")
    (?A . "■・■・")
    (?B . "■・■■")
    (?C . "■■・・")
    (?D . "■■・■")
    (?E . "■■■・")
    (?F . "■■■■")
    ))

(defun po-hex-list-to-block (hex-list)
  (mapconcat (function
	      (lambda (hex)
		(mapconcat (function
			    (lambda (chr)
			      (cdr (assq chr po-block-alist))
			      ))
			   hex "")
		))
	     hex-list "\n")
  )

(defun po-bdf-display-block (encoding buffer)
  (let (dest char)
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (if (re-search-forward (format "\nENCODING %d\n" encoding) nil t)
	  (progn
	    (search-forward "\nBITMAP\n")
	    (save-restriction
	      (narrow-to-region (match-end 0)
				(progn (search-forward "\nENDCHAR\n")
				       (match-beginning 0)
				       ))
	      (goto-char (point-min))
	      (while (re-search-forward "^[0-9A-F]+$" nil t)
		(setq dest (vconcat dest
				    (vector
				     (buffer-substring
				      (match-beginning 0)(match-end 0))
				     )))
		)
	      )
	    (re-search-backward "^STARTCHAR \\([^\n]+\\)\n")
	    (setq char (buffer-substring (match-beginning 1) (match-end 1)))
	    )))
    (if dest
	(progn
	  (insert (format "%s (%d)\n" char encoding))
	  (insert (po-hex-list-to-block dest))
	  (insert "\n")
	  (mapcar (function
		   (lambda (str)
		     (insert str)
		     (insert "\n")
		     ))
		  (bdf-to-bitmap dest))
	  ))))

(defun po-bdf-get-hex-vector ()
  (save-excursion
    (goto-char (point-min))
    (let (dest)
      (while (= (forward-line) 0)
	(let (str)
	  (while (not (eolp))
	    (let ((rest po-block-alist)
		  pair)
	      (while (and (setq pair (car rest))
			  (not (looking-at (cdr pair)))
			  )
		(setq rest (cdr rest))
		)
	      (if pair
		  (progn
		    (setq str (concat str (char-to-string (car pair))))
		    (forward-char 4)
		    )
		(end-of-line)
		)
	      ))
	  (if str
	      (setq dest (vconcat dest (vector str)))
	    )
	  ))
      dest)))

(defun po-bdf-update-char ()
  (let ((str (mapconcat (function identity)
			(po-bdf-get-hex-vector) "\n"))
	(encoding po-bdf-current-encoding)
	)
    (save-excursion
      (set-buffer po-bdf-original-buffer)
      (goto-char (point-min))
      (re-search-forward (format "^ENCODING %d\n" encoding))
      (search-forward "\nBITMAP\n")
      (let* ((beg (match-end 0))
	     (end (progn
		    (search-forward "\nENDCHAR\n")
		    (match-beginning 0)
		    ))
	     (b0 (buffer-substring beg end))
	     )
	(or (string= str b0)
	    (progn
	      (delete-region beg end)
	      (goto-char beg)
	      (insert str)
	      ))))))

(defvar po-bdf-original-buffer nil)
(defvar po-bdf-current-encoding nil)

(defconst po-bdf-mode-map nil)
(if po-bdf-mode-map
    nil
  (setq po-bdf-mode-map (make-keymap))
  (suppress-keymap po-bdf-mode-map)
  (define-key po-bdf-mode-map " " 'po-bdf-toggle-dot)
  (define-key po-bdf-mode-map "p" 'po-bdf-previous-char)
  (define-key po-bdf-mode-map "n" 'po-bdf-next-char)
  (define-key po-bdf-mode-map "j" 'po-bdf-goto-char)
  )

(defun po-bdf-toggle-dot ()
  (interactive)
  (save-excursion
    (let ((chr (char-after (point))))
      (delete-char 1)
      (if (eq chr ?・)
	  (insert "■")
	(insert "・")
	)
      (goto-char (point-max))
      (re-search-backward "^[・■]+\n")
      (delete-region (match-end 0)(point-max))
      (goto-char (point-max))
      (mapcar (function
	       (lambda (str)
		 (insert str)
		 (insert "\n")
		 ))
	      (bdf-to-bitmap (po-bdf-get-hex-vector))
	      )
      )))

(defun po-bdf-edit ()
  (interactive)
  (let ((orig-buf (buffer-name)))
    (switch-to-buffer (get-buffer-create (concat "*edit " orig-buf "*")))
    (erase-buffer)
    (kill-all-local-variables)
    (make-local-variable 'po-bdf-original-buffer)
    (make-local-variable 'po-bdf-current-encoding)
    (setq po-bdf-original-buffer orig-buf)
    (setq po-bdf-current-encoding
	  (save-excursion
	    (set-buffer orig-buf)
	    ;;(goto-char (point-min))
	    (re-search-forward "^ENCODING \\([0-9]+\\)\n" nil t)
	    (string-to-number (buffer-substring (match-beginning 1)
						(match-end 1)))
	    ))
    )
  (po-bdf-display-block po-bdf-current-encoding po-bdf-original-buffer)
  (goto-char (point-min))
  (forward-line)
  (use-local-map po-bdf-mode-map)
  )

(defun po-bdf-previous-char ()
  (interactive)
  (let ((encoding po-bdf-current-encoding)
	(p (point))
	)
    (save-excursion
      (set-buffer po-bdf-original-buffer)
      (goto-char (point-min))
      (re-search-forward (format "^ENCODING %d\n" encoding))
      (goto-char (match-beginning 0))
      (if (re-search-backward "^ENCODING \\([0-9]+\\)\n" nil t)
	  (setq encoding (string-to-int
			  (buffer-substring (match-beginning 1)
					    (match-end 1))))
	))
    (if encoding
	(progn
	  (po-bdf-update-char)
	  (setq po-bdf-current-encoding encoding)
	  (erase-buffer)
	  (po-bdf-display-block encoding po-bdf-original-buffer)
	  (goto-char p)
	  )
      (ding)
      )))

(defun po-bdf-next-char ()
  (interactive)
  (let ((encoding po-bdf-current-encoding)
	(p (point))
	)
    (save-excursion
      (set-buffer po-bdf-original-buffer)
      (goto-char (point-min))
      (re-search-forward (format "^ENCODING %d\n" encoding))
      (if (re-search-forward "^ENCODING \\([0-9]+\\)\n" nil t)
	  (setq encoding (string-to-int
			  (buffer-substring (match-beginning 1)
					    (match-end 1))))
	))
    (if encoding
	(progn
	  (po-bdf-update-char)
	  (setq po-bdf-current-encoding encoding)
	  (erase-buffer)
	  (po-bdf-display-block encoding po-bdf-original-buffer)
	  (goto-char p)
	  )
      (ding)
      )))

(defun po-bdf-goto-char (char)
  (interactive "sCHAR: ")
  (let ((encoding po-bdf-current-encoding)
	(p (point))
	)
    (save-excursion
      (set-buffer po-bdf-original-buffer)
      (goto-char (point-min))
      (re-search-forward (format "^ENCODING %d\n" encoding))
      (if (and (re-search-forward
		(format "^STARTCHAR %s\n" (regexp-quote char))
		nil t)
	       (re-search-forward "^ENCODING \\([0-9]+\\)\n" nil t)
	       )
	  (setq encoding (string-to-int
			  (buffer-substring (match-beginning 1)
					    (match-end 1))))
	))
    (if encoding
	(progn
	  (po-bdf-update-char)
	  (setq po-bdf-current-encoding encoding)
	  (erase-buffer)
	  (po-bdf-display-block encoding po-bdf-original-buffer)
	  (goto-char p)
	  )
      (ding)
      )))


;;; @ end
;;;

(provide 'po)

;;; po.el ends here
