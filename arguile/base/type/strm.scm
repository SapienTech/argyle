(module (arguile base type strm))
(use (arguile base fn)
     (arguile base mac)
     (srfi srfi-41)
     (srfi srfi-45))
(export scons sfn slet strm-match strm-of)

(defp strm->lst stream->list)
(defp scar stream-car)
(defp scdr stream-cdr)
(defp strm-join stream-concat)
(defp strm-const stream-constant)
(defp s stream-drop)
(defp strm-fltr stream-filter)
(defp strms-fld stream-fold)
(defp strm-each stream-for-each)
(defp strm-frm stream-from)
(defp strm-itr stream-iterate)
(defp strm-len stream-length)
(defp strm-map stream-map)
(defp snil stream-null)
(defp snil? stream-null?)
(defp strm-pair stream-pair?)
(defp strm-range stream-range)
(defp strm-ref stream-ref)
(defp strm-rev stream-reverse)
(defp strm-scan stream-scan)
(defp strm-tke stream-take)
(defp strm-unfld stream-unfold)
(defp strm-zip stream-zip)

(mac scons
  ((obj strm) #'(stream-cons obj strm)))
(mac sfn
  ((args e1 e2 ...) #'(stream-lambda args e1 e2 ...)))

(mac slet
  ((tag ((name val) ...) e1 e2 ...)
   #'(stream-let ((name val) ...) e1 e2 ...)))

(mac strm-match
  ((strm-exp (pat . exp) ...)
   #'(stream-match strm-map (pat . exp) ...)))

(mac strm-of
  ((exp rst ...) #'(stream-of exr rst ...)))
