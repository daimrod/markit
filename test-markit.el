;;; test-markit.el ---

;; Copyright (C) 2012 Grégoire Jadi

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

;;; Code:

(require 'ert)
(require 'markit)

(ert-deftest test-markit-get-pairs ()
  (should (equal (markit-get-pairs ?\)) '(?\( ?\))))
  (should (equal (markit-get-pairs ?{) '(?{ ?})))
  (should (equal (markit-get-pairs ?') '(?' ?')))

  (should-error (markit-get-pairs nil)
                :type 'wrong-type-argument)

  (should-error (markit-get-pairs ?a)
                :type 'scan-error))

(provide 'test-markit)

;;; test-markit.el ends here
