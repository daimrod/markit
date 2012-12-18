;;; markit.el --- Provide some Vim facilities
;; Copyright (C) 2011 Grégoire Jadi

;; Author: Grégoire Jadi <gregoire.jadi@gmail.com>

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Markit provides some Vim facilities to Emacs such as vi( and va".
;;
;; The following bindings are used:
;;  - C-c v i to mark the region, including the delimiters
;;  - C-c v e to mark the region, excluding the delimiters
;;
;; If you wish to have something like ci", enable
;; `delete-selection-mode'.

;;; Code:

(eval-when-compile (require 'cl))

(defvar markit-translation-table
  '((?\( . ?\))
    (?\" . ?\")
    (?< . ?>)
    (?' . ?')
    (?\[ . ?\])
    (?{ . ?}))
  "Table used to find the equivalent characters.")

(defvar markit-mode-map (make-sparse-keymap)
  "Keymap for the Markit minor mode.")

(define-key markit-mode-map (kbd "C-c v i") 'markit-mark-region-include)
(define-key markit-mode-map (kbd "C-c v e") 'markit-mark-region-exclude)

(define-minor-mode markit-mode
  "Add Vim-like vi( functionality to Emacs.

Markit mode is a buffer-local minor mode."
  :lighter " Markit"
  :keymap markit-mode-map)

(define-globalized-minor-mode global-markit-mode
  markit-mode markit-mode-on)

(defun markit-mode-on ()
  (markit-mode t))

(defun markit-mark-region-exclude (whole-buffer? char)
  "Mark a region between two equivalent characters.

The delimiters are NOT marked, use `markit-mark-whole-region' to
do so.

The two characters are obtained with `markit-get-pairs' using the
`markit-translation-table'."
  (interactive "P\ncMark region: ")
  (markit-mark-region whole-buffer? nil char))

(defun markit-mark-region-include (whole-buffer? char)
  "Mark a region between two equivalent characters.

The delimiters ARE marked, use `markit-mark-inner-region' if you
don't want them.

The two characters are obtained with `markit-get-pairs' using the
`markit-translation-table'."
  (interactive "P\ncMark region: ")
  (markit-mark-region whole-buffer? t char))

(defun markit-get-pairs (char)
  "Returns a list of two characters according to the values found
in `markit-translation-table'."
  (let ((ret (assoc char markit-translation-table)))
    (setq ret
          (if ret
              ret
            (rassoc char markit-translation-table)))
    (list (car ret) (cdr ret))))

(defun markit-mark-region (whole-buffer? include? char)
  (let ((pos-origin (point))
        pos-tmp
        no-error?)
    (multiple-value-bind (char- char+)
        (markit-get-pairs char)
      ;; search backward the « opening » character and push the correct position
      (save-excursion
        (setq no-error? (markit-search whole-buffer? char- char+ 'backward)
              pos-tmp (point)))
      (when no-error?
        (push-mark (if include?
                       pos-tmp
                     (+ pos-tmp 1))
                   nil t)
        ;; search forward the « closing » character and go at the correct position
        (save-excursion
          (setq no-error? (markit-search whole-buffer? char+ char- 'forward)
                pos-tmp (point)))
        (if no-error?
            (goto-char (if include?
                           (+ pos-tmp 1)
                         pos-tmp))
          (pop-mark)))
      (unless no-error?
        (goto-char pos-origin)
        (message "[No Match]")))))

(defun markit-search (whole-buffer? char-to-match char-comp direction)
  (let (min max tmp fn-move fn-out? fn-char)
    (if whole-buffer?
        (setq min (point-min)
              max (point-max))
      (setq min (window-start)
            max (window-end)))
    ;; initialize the context depending on the direction
    (cond ((eq direction 'backward)
           (setq fn-move #'backward-char
                 fn-out? #'(lambda (pos)
                             (< pos min))))
          ((eq direction 'forward)
           (setq fn-move #'forward-char
                 fn-out? #'(lambda (pos)
                             (> pos max))))
          (t (error "Only 'backward and 'forward values are accepted as direction")))
    (markit-search-iter fn-move fn-out? char-to-match char-comp)))

(defun markit-search-iter (fn-move fn-out? char-to-match char-comp)
  ;; if the current character is the complementary we ignore it
  (if (and
       (char-after)
       (char-equal char-comp (char-after)))
      (funcall fn-move))
  (let ((acc 0)                         ; initialize the stack
        (continue? t)
        ret prev-pos)
    (while continue?
      (setq prev-pos (point))
      (cond
       ((not (char-after))
        (setq continue? nil
              ret nil))
       ;; stop when the correct character is found and the stack
       ;; is empty
       ((and
         (= acc 0)
         (char-equal char-to-match (char-after)))
        (setq continue? nil
              ret (point)))
       ;; pop an element from the stack if it's not empty
       ;; and the correct character is found
       ((and
         (> acc 0)
         (char-equal char-to-match (char-after)))
        (decf acc)
        (funcall fn-move))
       ;; push an element onto the stack if the complementary
       ;; character is found
       ((char-equal char-comp (char-after))
        (incf acc)
        (funcall fn-move))
       ;; otherwise continue the search
       (t
        (funcall fn-move)))
      (when continue?
        (if (or
             (= (- prev-pos (point)) 0)
             (funcall fn-out? (point)))
            (setq continue? nil
                  ret nil))))
    ret))

(provide 'markit)

;;; markit.el ends here
