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

(require 'f)
(require 'org)

;; NOTE: The link doesn't do anything actionable right now
(org-add-link-type "iorg" #'iorg-link-action #'iorg-export)

(defvar-local iorg-overlays nil
  "List of overlays applied in buffer.")

(defun iorg-encode (image-bytes)
  (base64-encode-string image-bytes))

(defun iorg-decode (data-string)
  (base64-decode-string data-string))

(defun iorg-link-action (_path)
  ;; TODO: Maybe toggle?
  (message "No action available for iorg links."))

(defun iorg-export (_path _desc _backend)
  ;; TODO: At least need the html export support
  (message "No export function available for iorg links."))

;;;###autoload
(defun iorg-insert-image-at-point (image-file-path image-id)
  "Insert an iorg image block using the given information. `image-id' defines
the name of the block if specified."
  (interactive "f\nsImage ID (leave empty for no id): ")
  (let ((image-bytes (f-read-bytes image-file-path))
        (image-id (if (string-equal "" image-id) nil image-id)))
    (iorg-insert-block image-bytes image-id)))

(defun iorg-insert-block (image-bytes &optional image-id)
  "Insert image-bytes at point and optionally apply the image-id"
  (let ((encoded (iorg-encode image-bytes)))
    (insert (format "#+BEGIN_IMAGE\n%s\n#+END_IMAGE" encoded)))
  (when image-id
    (re-search-backward "^#\\+BEGIN_IMAGE" nil t)
    (insert (format "#+NAME: %s\n" image-id))))

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

(defun iorg-get-iorg-links ()
  "Return a list of iorg link items from the buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "iorg")
        link))))

(defun iorg-link-to-range (link)
  "Get the range from iorg link."
  (let ((id (org-element-property :path link)))
    (iorg-id-to-range id)))

(defun iorg-id-to-range (id)
  "Return image block range that matches the given id."
  (let ((ranges (iorg-get-image-ranges)))
    (find id ranges :key (lambda (range) (iorg-range-to-id range)) :test #'string-equal)))

(defun iorg-range-to-id (range)
  "Return name of the block signified by the range."
  (save-excursion
    (goto-char (car range))
    (previous-line 2)
    (let ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (if (string-prefix-p "#+name:" (downcase line-text))
          (string-trim (substring-no-properties line-text 7))))))

(defun iorg-show-links ()
  "Display all the link type images"
  (dolist (link (iorg-get-iorg-links))
    (let ((img (iorg-get-image (iorg-link-to-range link)))
          (overlay (make-overlay (org-element-property :begin link) (org-element-property :end link))))
      (overlay-put overlay 'display img)
      (push overlay iorg-overlays))))

(defun iorg-get-image (range)
  "Return image object. TODO: Cache this."
  (let ((image-data (buffer-substring-no-properties (car range) (cdr range))))
    (create-image (iorg-decode image-data) nil t)))

(defun iorg-show-range (range)
  (let ((overlay (make-overlay (car range) (cdr range)))
        (img (iorg-get-image range)))
    (overlay-put overlay 'display img)
    (push overlay iorg-overlays)))

(defun iorg-hide-range (range)
  "Hide data of a range to save space."
  (let ((overlay (make-overlay (car range) (cdr range))))
    (overlay-put overlay 'invisible t)
    (push overlay iorg-overlays)))

(defun iorg-enable ()
  (dolist (range (iorg-get-image-ranges))
    (if (iorg-range-to-id range)
        (iorg-hide-range range)
      (iorg-show-range range)))
  (iorg-show-links))

(defun iorg-disable ()
  (dolist (overlay iorg-overlays)
    (delete-overlay overlay))
  (setq iorg-overlays nil))

;;;###autoload
(define-minor-mode iorg-mode
  "Mode for displaying in-buffer base64 images."
  :init-value nil
  :lighter nil
  (if iorg-mode (iorg-enable) (iorg-disable)))

(provide 'iorg)

;;; iorg.el ends here
