(ns tst.mbrainz
  (:use tupelo.core tupelo.test)
  (:require
    [clojure.pprint :as pprint]
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
    (apply str (drop 1 it))
    (str->kw it)))

; #todo change :find -> :return  ?
(defn ^:no-doc query-map-impl
  [args]
  (when-not (= :where (nth args 4))
    (throw (IllegalArgumentException.
             (str "find-base: 5th arg must be :where, received=" args))))
  (let
    [let-find-map  (apply hash-map (take 4 args))                             >> (spyx let-find-map)
     where-vec     (td/where-clause (drop 5 args))                            >> (spyx-pretty where-vec)
     let-vec       (grab :let let-find-map)                                   >> (spyx let-vec)
     let-map       (apply hash-map let-vec)                                   >> (spyx let-map)
     let-syms      (keys let-map)                                             >> (spyx let-syms)
     let-srcs      (vals let-map)                                             >> (spyx let-srcs)
     yield-vec     (grab :yield let-find-map)                                 >> (spyx yield-vec)
     yield-kws     (mapv  query-sym->kw  yield-vec)                           >> (spyx yield-kws)
     ]
    `(let [
           query-tuples# (d/q '{:find  ~yield-vec
                                :in    [~@let-syms]
                                :where ~where-vec
                                }
                           ~@let-srcs)
           result-set#   (set (for [tuple# query-tuples#]
                                (zipmap ~yield-kws (vec tuple#))))]
       result-set#)))

(defmacro query-map
  "Returns search results as a set of maps (i.e. a TupleSet, or #{ [s/Any] } in Prismatic Schema),
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

  Each map in result set is keyword labeled such that:

    (query-map
      ...
      :yield [?gid ?ident-type ?ident-gender]  ; Datomic query symbols like `?some-symbol`
      ...)
  produces output like:

    #{...    ; maps keyed by keyword version `:some-symbol` with `?` stripped => `:`
        {:gid           #uuid '76c9a186-75bd-436a-85c0-823e3efddb7f'
         :ident-type    :artist.type/person
         :ident-gender  :artist.gender/female}
      ...)
     "
  [& args]
  (query-map-impl args))

; #todo change :find -> :return  ?
(defn ^:no-doc query-map-cond-impl
  [args]
  (when-not (= :where (nth args 6))
    (throw (IllegalArgumentException.
             (str "find-base: 7th arg must be :where, received=" args))))
  (let
    [let-find-map  (apply hash-map (take 6 args))                             >> (spyx let-find-map)
     where-vec     (td/where-clause (drop 7 args))                            >> (spyx-pretty where-vec)
     preds-vec     (grab :preds let-find-map)                                 >> (spyx-pretty preds-vec)
     preds2-vec    (mapv vector preds-vec)                                    >> (spyx-pretty preds2-vec)
     let-vec       (grab :let let-find-map)                                   >> (spyx let-vec)
     let-map       (apply hash-map let-vec)                                   >> (spyx let-map)
     let-syms      (keys let-map)                                             >> (spyx let-syms)
     let-srcs      (vals let-map)                                             >> (spyx let-srcs)
     yield-vec     (grab :yield let-find-map)                                 >> (spyx yield-vec)
     yield-kws     (mapv  query-sym->kw  yield-vec)                           >> (spyx yield-kws)
     where-vec-final (glue where-vec preds2-vec)                           >> (spyx where-vec-final)
     ]
    `(let [
           query-tuples# (d/q '{:find  ~yield-vec
                                :in    [~@let-syms]
                                :where ~where-vec-final
                                }
                           ~@let-srcs)
           result-set#   (set (for [tuple# query-tuples#]
                                (zipmap ~yield-kws (vec tuple#))))]
       result-set#)))

(defmacro query-map-cond
  "Returns search results as a set of maps (i.e. a TupleSet, or #{ [s/Any] } in Prismatic Schema),
   where each tuple is unique. Usage:

    (td/query
       :let    [$        (d/db *conn*)     ; assign multiple variables just like
                ?name    \"Caribbean\"]    ;   in Clojure 'let' special form
       :preds  [ (< 1960 ?year) (< ?year 1970) ]
       :yield  [?e ?name]
       :where  {:db/id ?eid  :person/name ?name  :location ?loc}
               {:db/id ?eid  :weapon/type :weapon/wit} )

  Unlike datomic.api/q, the query form does not need to be wrapped in a map literal nor is any
  quoting required. Most importantly, the :in keyword has been replaced with the :let keyword, and
  the syntax has been copied from the Clojure let special form so that both the query variables (the
  variables $ and ?name in this case) are more closely aligned with their actual values. Also, the
  implicit DB $ must be explicitly tied to its data source in all cases (as shown above).
  The `:let` and `:yield` clauses may be in any order, but the `:where` clause must come last.

  Each map in result set is keyword labeled such that:

    (query-map
      ...
      :yield [?gid ?ident-type ?ident-gender]  ; Datomic query symbols like `?some-symbol`
      ...)
  produces output like:

    #{...    ; maps keyed by keyword version `:some-symbol` with `?` stripped => `:`
        {:gid           #uuid '76c9a186-75bd-436a-85c0-823e3efddb7f'
         :ident-type    :artist.type/person
         :ident-gender  :artist.gender/female}
      ...)
     "
  [& args]
  (query-map-cond-impl args))

(dotest
  ; Testing the macro
  (when false
    (println "-----------------------------------------------------------------------------")
    (pprint/pprint
      (query-map-impl
        '[:let [$ (live-db)
                ?str-name "Janis Joplin"]
          :yield [?gid ?ident-type ?ident-gender]
          :where {:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
          {:db/id ?eid-type :db/ident ?ident-type}
          {:db/id ?eid-gender :db/ident ?ident-gender} ] )))

  (nl)
  (is= #{{:gid #uuid "76c9a186-75bd-436a-85c0-823e3efddb7f", :ident-type :artist.type/person, :ident-gender :artist.gender/female}}
    (query-map
      :let [$ (live-db)
            ?str-name "Janis Joplin"]
      :yield [?gid ?ident-type ?ident-gender]
      :where
      {:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
      {:db/id ?eid-type :db/ident ?ident-type}
      {:db/id ?eid-gender :db/ident ?ident-gender}))

  (nl)
  (spyx-pretty :lennon-tracks-titles-partial
    (take 10
      (query-map
        :let [$ (live-db)
              ?artist-name "John Lennon"]
        :yield [?track-name]
        :where
        {:db/id ?eid-artist :artist/name ?artist-name}
        {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name})))

  (nl)
  (spyx-pretty :lennon-title-album-year
    (take 10
      (query-map
        :let [$ (live-db)
              ?artist-name "John Lennon"]
        :yield [?track-name ?release-name ?release-year]
        :where
          {:db/id ?eid-artist :artist/name ?artist-name}
          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
          {:db/id ?eid-media :medium/tracks ?eid-track }
          {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year } )
      ))



  (nl)
  ; Testing the macro
  (when false
    (println "-----------------------------------------------------------------------------")
    (pprint/pprint
      (query-map-cond-impl
       '[
        :let [$ (live-db)
                 ?artist-name "John Lennon"]
        :yield [?track-name ?release-name ?release-year]
        :preds [(< ?year 1970)]
        :where
        {:db/id ?eid-artist :artist/name ?artist-name}
        {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
        {:db/id ?eid-media :medium/tracks ?eid-track}
        {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year} ]) ))
    )

(dotest-focus
  (spyx-pretty :lennon-title-album-year
    (query-map-cond
      :let [$ (live-db)
            ?artist-name "John Lennon"]
      :yield [?track-name ?release-name ?release-year]
      :preds [(<= 1969 ?release-year) (<= ?release-year 1969) ]
      :where
      {:db/id ?eid-artist :artist/name ?artist-name}
      {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
      {:db/id ?eid-media :medium/tracks ?eid-track}
      {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}
      ))
  )







