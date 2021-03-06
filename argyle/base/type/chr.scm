(ns (argyle base type chr))
(use (argyle base fn))

(defp chr? char?)
(defp chr=? char=?)
(defp chr<? char<?)
(defp chr->int char->integer)
(defp chr>? char>?)
(defp chrs char-set)
(defp chrs? char-set?)
(defp chrs=? char-set=)
(defp chrs->lst char-set->list)
(defp chrs->str char-set->string)
(defp chrs-join char-set-adjoin)
(defp chrs-has? char-set-contains?)
(defp chrs-cpy char-set-copy)
(defp chrs-cnt char-set-count)
(defp chrs-del char-set-delete)
(defp chrs~ char-set-complement)
(defp chrs\ char-set-difference)
(defp chrs^ char-set-intersection)
(defp chrsU char-set-union)
