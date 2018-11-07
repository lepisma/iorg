;;; iorg.el --- Self contained org-mode files with images -*- lexical-binding: t; -*-

;; Copyright (c) 2018 Abhinav Tushar

;; Author: Abhinav Tushar <lepisma@fastmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; URL: https://github.com/lepisma/iorg

;;; Commentary:

;; Self contained org-mode files with images
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar-local iorg-overlays nil
  "List of invisibility overlays applied.")

(defun iorg-encode (data)
  (base64-encode-string data))

(defun iorg-decode (data-string)
  (base64-decode-string data-string))

(defun iorg-get-image-ranges ()
  "Return a list of (beg . end) ranges representing image data."
  (save-excursion
    (goto-char (point-min))
    (let ((ranges '())
          (beg nil)
          (end nil))
      (while (setq beg (re-search-forward "^#\\+BEGIN_IMAGE" nil t))
        (incf beg) ;; For that extra line end
        (setq end (- (re-search-forward "^#\\+END_IMAGE" nil t) 12))
        (setq ranges (cons (cons beg end) ranges)))
      ranges)))

(defun iorg-show-image (range)
  (let* ((image-data (buffer-substring-no-properties (car range) (cdr range)))
         (overlay (make-overlay (car range) (cdr range)))
         (img (create-image (iorg-decode image-data) nil t)))
    (overlay-put overlay 'display img)
    (push overlay iorg-overlays)))

(defun iorg-enable ()
  (dolist (range (iorg-get-image-ranges))
    (iorg-show-image range)))

(defun iorg-disable ()
  (dolist (overlay iorg-overlays)
    (delete-overlay overlay))
  (setq iorg-overlays nil))

(define-minor-mode iorg-mode
  "Mode for displaying in-buffer base64 images."
  :init-value nil
  :lighter nil
  (if iorg-mode (iorg-enable) (iorg-disable)))

(provide 'iorg)

;;; iorg.el ends here
