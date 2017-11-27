(ns (argyle generic coll))

(use (argyle base)
     (argyle generic)
     (argyle data tbl)
     (argyle data vec)
     (argyle data q)
     (argyle data set))

(gen map (@ (srfi srfi-1) map))
(xtnd map (f <fn> v <vec>) (vec-map f v))
(xtnd map (f <fn> s <str> . rst) (apply str-map f s rst))
(xtnd map (f <fn> t <tbl>) (tbl-map->lst f t))

(gen filter)

(gen reduce)
(gen cpy)

