;; vm-bitmap.el -- bitmap utilities for VM.

;; Copyright (C) 1999 Free Software Foundation, Inc.

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Created: 1999/08/20
;; Revised: 1999/10/26
;; Keywords: bitmap, x-face, vm

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

(require 'alist)
(require 'x-face-mule)


;;; Functions.
;;

;; Avoid byte compile warnings.
(defvar vm-mail-buffer)
(defvar vm-presentation-buffer)
(eval-when-compile (autoload 'vm-preview-current-message "vm-page"))

;; Attempt to pickup the macro `vm-select-folder-buffer'.
(eval-when-compile
  (or (load "vm-macro" t)
      (load "vm/vm-macro" t)
      ;; For old VM.
      (load "vm-misc" t)
      (load "vm/vm-misc" t)))

(defun vm-bitmap-redefine ()
  "Redifine functions for the use of bitmap-mule."
  (require 'vm-page)
  ;;  (or (fboundp 'x-face-mule-original-vm-energize-headers-and-xfaces)
  ;;      (condition-case nil
  ;;	  (fset 'x-face-mule-original-vm-energize-headers-and-xfaces
  ;;		(symbol-function 'vm-energize-headers-and-xfaces))
  ;;	(error nil)))
  (fset 'vm-energize-headers-and-xfaces 'x-face-mule-highlight-header))

(eval-and-compile (autoload 'smiley-toggle-buffer "smiley-mule"))

(defun vm-smiley-display (&optional arg)
  "Display \"smileys\" as small graphical icons.
With arg, turn displaying on if and only if arg is positive."
  (interactive "P")
  (save-excursion
    (vm-select-folder-buffer)
    (if vm-presentation-buffer
	(progn
	  (set-buffer vm-presentation-buffer)
	  (save-restriction
	    (widen)
	    (goto-char (point-min))
	    (if (search-forward "\n\n" nil t)
		(progn
		  (narrow-to-region (point) (point-max))
		  (let ((modified (buffer-modified-p))
			(inhibit-read-only t)
			buffer-read-only)
		    (smiley-toggle-buffer arg)
		    (set-buffer-modified-p modified)))))))))


;;; Setup.
;;

(set-alist 'x-face-mule-highlight-x-face-refresh-method-alist
	   'vm-summary-mode
	   (function (lambda ()
		       (vm-select-folder-buffer)
		       (vm-preview-current-message))))

(add-hook 'vm-quit-hook 'x-face-mule-save-cache-file)


(provide 'vm-bitmap)

;; vm-bitmap.el ends here
