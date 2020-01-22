(ns tst.mbrainz
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.string :as str]
    [datomic.api :as d]
    [schema.core :as s]
    [tupelo.datomic :as td]
    [tupelo.datomic.schema :as tdsk]
    [tupelo.schema :as ts]
    ))

(def datomic-uri "datomic:dev://localhost:4334/mbrainz-1968-1973") ; the URI for our test db
(def conn (d/connect datomic-uri)) ; create & save a connection to the db

;---------------------------------------------------------------------------------------------------
; Convenience function to keep syntax a bit more concise
(defn live-db [] (d/db conn))


(defn query-sym->kw
  [qs]
  (it-> qs
    (sym->str it)
    (str/replace it #"^." \:)
    (str->sym it) ) )

; #todo need checks to stop collection result (:find [?e ...])
; #todo and scalar result (:find [?e .])
(defmacro ^:no-doc query-map-base    ; #todo remember 'with'
  ; returns a HashSet of datomic entity objects
  "Base macro for improved API syntax for datomic.api/q query function (Entity API)"
  [& args]
  ; (newline) (println "find-base =>" args)
  (when-not (= :where (nth args 4))
    (throw (IllegalArgumentException.
             (str "find-base: 5th arg must be :where, received=" args))))
  (let
    [let-find-map  (apply hash-map (take 4 args))                             >> (spyx let-find-map)
     where-entries (td/where-clause (drop 5 args))                               >> (spyx where-entries)
     args-map      (glue let-find-map {:where where-entries})                 >> (spyx args-map)
     let-vec       (grab :let args-map)                                       >> (spyx let-vec)
     let-map       (apply hash-map let-vec)                                   >> (spyx let-map)
     let-syms      (keys let-map)                                             >> (spyx let-syms)
     let-srcs      (vals let-map)                                             >> (spyx let-srcs)
     yield-vec     (grab :yield args-map)                                     >> (spyx yield-vec)
     yield-kws     (mapv  query-sym->kw yield-vec)                            >> (spyx yield-kws)
     where-vec     (grab :where args-map)                                     >> (spyx where-vec)
     ]
    (flush)
    (when-not (vector? let-vec)
      (throw (IllegalArgumentException. (str "find-base: value for :let must be a vector; received=" let-vec))))
    (when-not (vector? yield-vec)
      (throw (IllegalArgumentException. (str "find-base: value for :yield must be a vector; received=" yield-vec))))
    (when-not (vector? where-vec)
      (throw (IllegalArgumentException. (str "find-base: value for :where must be a vector; received=" where-vec))))
    `(d/q  '{:find   ~yield-vec
             :where  ~where-vec
             :in     [ ~@let-syms ] }
       ~@let-srcs)))

; #todo change :find -> :return  ?
(defmacro query-map
  "Returns search results as a set of tuples (i.e. a TupleSet, or #{ [s/Any] } in Prismatic Schema),
   where each tuple is unique. Usage:

    (td/query
       :let    [$        (d/db *conn*)     ; assign multiple variables just like
                ?name    \"Caribbean\"]    ;   in Clojure 'let' special form
       :yield  [?e ?name]
       :where  {:db/id ?eid  :person/name ?name  :location ?loc}
               {:db/id ?eid  :weapon/type :weapon/wit} )

  Unlike datomic.api/q, the query form does not need to be wrapped in a map literal nor is any
  quoting required. Most importantly, the :in keyword has been replaced with the :let keyword, and
  the syntax has been copied from the Clojure let special form so that both the query variables (the
  variables $ and ?name in this case) are more closely aligned with their actual values. Also, the
  implicit DB $ must be explicitly tied to its data source in all cases (as shown above).
  The `:let` and `:yield` clauses may be in any order, but the `:where` clause must come last.
  "
  [& args]
  `(set (for [tuple# (query-map-base ~@args) ]
          (vec tuple#))))


(dotest

  ;(spy :janis-joplin
  ;  (td/query
  ;    :let [$ (live-db)
  ;          ?str-name "Janis Joplin"]
  ;    :yield [?gid ?ident-type ?ident-gender]
  ;    :where  {:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
  ;    {:db/id ?eid-type :db/ident ?ident-type}
  ;    {:db/id ?eid-gender :db/ident ?ident-gender}
  ;    ))

  (spy :janis-joplin
    (query-map
      :let [$ (live-db)
            ?str-name "Janis Joplin"]
      :yield [?gid ?ident-type ?ident-gender]
      :where  {:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
              {:db/id ?eid-type :db/ident ?ident-type}
              {:db/id ?eid-gender :db/ident ?ident-gender}
      ))


  )







