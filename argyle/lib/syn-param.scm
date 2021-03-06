;;; syn-param.sls --- Operators with Extended Parameter Syntax

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.

;;; Commentary:

;;; Code:
#!r6rs

(library (argyle lib syn-param)
  (export
    with-extended-parameter-operators
    with-extended-parameter-operators*)
  (import (rnrs)
          (argyle lib private include))

  (include-file ((argyle lib syn-param private) syn-param)))
