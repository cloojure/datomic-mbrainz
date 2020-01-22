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
    [tupelo.schema :as tsk]
    [clojure.tools.reader.edn :as edn]
    [clojure.java.io :as io]))

(def datomic-uri "datomic:dev://localhost:4334/mbrainz-1968-1973") ; the URI for our test db
(def conn (d/connect datomic-uri)) ; create & save a connection to the db

(def rules (edn/read-string
             (slurp (io/resource "rules.edn"))))
(comment  ; sample rules-collection with one rule
  (def rules
    '[; Given ?t bound to track entity-ids, binds ?r to the corresponding
      ; set of album release entity-ids
      [(track-release ?t ?r)
       [?m :medium/tracks ?t]
       [?r :release/media ?m]]]))

;---------------------------------------------------------------------------------------------------
; Convenience function to keep syntax a bit more concise
(defn live-db [] (d/db conn))


(defn query-sym->kw
  [qs]
  (it-> qs
    (sym->str it)
    (apply str (drop 1 it))
    (str->kw it)))

(defn partition-even-odd
  "Separates elements of a vector by index into even & odd values.  Not lazy.
  [:a :b :c :d] => [ [:a :c] [:b :d] ] "
  [seq-arg]
  (let [evens (mapv only (partition 1 2 seq-arg))
        odds  (mapv only (partition 1 2 (drop 1 seq-arg)))]
    [evens odds]))

(s/defn query-sym? :- s/Bool
  "Returns true iff a symbol is a legal Datomic query symbol (beginning with a `?` char)"
  [arg :- s/Any]
  (and (symbol? arg)
    (= \? (first (name arg)))))

(s/defn query-sym-wild? :- s/Bool
  "Returns true iff a symbol is a legal Datomic query symbol (beginning with a `?` char),
  and is a wildcard symbol (ending in a `*` char)"
  [arg :- s/Any]
  (and (query-sym? arg)
    (= \* (last (name arg)))))

(s/defn check-symbol-usage
  "Throws an exception if:
     - a wildcard query symbol is appears more than once
     - any other symbol appears only once
   Else returns `:ok` "
  [syms-vec :- tsk/Vec]
  (nl)
  (let [syms-query-wild    (keep-if query-sym-wild? syms-vec)
        syms-reg           (drop-if query-sym-wild? syms-vec)
        freq-map-reg       (frequencies syms-reg)
        freq-map-wild      (frequencies syms-query-wild)
        orphan-syms        (vec (keys (submap-by-vals freq-map-reg [1] :missing-ok)))
        val-over-1?        (fn [k v] (when (< 1 v)
                                       [k v]))
        overused-wildcards (vec (keys (keep-if val-over-1? freq-map-wild))) ]
    (when (not-empty? orphan-syms)
      (throw (ex-info "Orphan symbols found:" (vals->map orphan-syms))))
    (when (not-empty? overused-wildcards)
      (throw (ex-info "Overused wildcards found:" (vals->map overused-wildcards))))
    :ok))

(dotest
  (is (query-sym? (quote ?name)))
  (isnt (query-sym? (quote name)))
  (isnt (query-sym? :name))
  (isnt (query-sym? "name"))
  (isnt (query-sym? 5))

  (is (query-sym-wild? (quote ?name*)))
  (isnt (query-sym-wild? (quote ?name)))
  (isnt (query-sym-wild? :name*))
  (isnt (query-sym-wild? "name*"))
  (isnt (query-sym-wild? 5))

  (is= :ok (check-symbol-usage '[aaa bbb bbb aaa]))
  (throws? (check-symbol-usage '[aaa bbb aaa ]))
  (is= :ok (check-symbol-usage '[?aaa* ?bbb ?bbb])) ; orphans must by query symbol AND wildcard to pass OK
  (throws? (check-symbol-usage '[?aaa* ?aaa* bbb ]))
  )

; #todo change :find -> :return  ?
(s/defn ^:no-doc query-map-impl
  [ctx :- tsk/KeyMap]
  (let
    [where-vec            (td/where-clause (grab :where ctx))               >> (spyx-pretty where-vec)
     preds-vec            (get ctx :preds [])                               >> (spyx-pretty preds-vec)
     preds2-vec           (mapv vector preds-vec)                           >> (spyx-pretty preds2-vec)
     rules-vec            (get ctx :rules [])                               >> (spyx-pretty rules-vec)
     let-vec              (grab :let ctx)                                   >> (spyx let-vec)
     [let-syms let-srcs]  (partition-even-odd let-vec)                      >> (spyx [let-syms let-srcs] )
     yield-vec            (grab :yield ctx)                                 >> (spyx yield-vec)
     yield-kws            (mapv  query-sym->kw  yield-vec)                  >> (spyx yield-kws)
     where-vec-final      (glue where-vec preds2-vec rules-vec)             >> (spyx where-vec-final)
     query-syms           (keep-if query-sym? (flatten [where-vec let-syms yield-vec rules-vec]))
     ]
    (check-symbol-usage query-syms)
    ; Look for orphaned symbols
    `(let [query-tuples# (d/q '{:find  ~yield-vec
                                :in    [~@let-syms]
                                :where ~where-vec-final }
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
  [ctx]
  (query-map-impl ctx))

(dotest
  (let [ctr (atom 0)]
    (doseq [it (d/datoms (live-db) :eavt)]
      (swap! ctr inc))
    (nl) (spy :total-datoms @ctr))

  (nl)
  (is= #{{:gid #uuid "76c9a186-75bd-436a-85c0-823e3efddb7f", :ident-type :artist.type/person, :ident-gender :artist.gender/female}}
    (query-map {:let   [$ (live-db)
                        ?str-name "Janis Joplin"]
                :yield [?gid ?ident-type ?ident-gender]
                :where [{:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
                        {:db/id ?eid-type :db/ident ?ident-type}
                        {:db/id ?eid-gender :db/ident ?ident-gender}]}))
  (nl)
  (spyx-pretty :lennon-tracks-titles-partial
    (take 10
      (query-map {:let   [$ (live-db)
                          ?artist-name "John Lennon"]
                  :yield [?track-name]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}]})))
  (nl)
  (spyx-pretty :lennon-title-album-year
    (take 10
      (query-map {:let   [$ (live-db)
                          ?artist-name "John Lennon"]
                  :yield [?track-name ?release-name ?release-year]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                          {:db/id ?eid-media :medium/tracks ?eid-track}
                          {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]})))

;    )
;(dotest

  (nl)
  ; Testing the macro
  (when false
    (println "-----------------------------------------------------------------------------")
    (pprint/pprint
      (query-map-impl
        '{:let   [$ (live-db)
                  ?artist-name "John Lennon"]
          :yield [?track-name ?release-name ?release-year]
          :preds [(< ?year 1970)]
          :where [{:db/id ?eid-artist :artist/name ?artist-name}
                  {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                  {:db/id ?eid-media :medium/tracks ?eid-track}
                  {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]})))

  ;(spyx-pretty :lennon-title-album-year
  ;  (query-map {:let   [$ (live-db)
  ;                      ?artist-name "John Lennon"]
  ;              :yield [?track-name ?release-name ?release-year]
  ;              :where [{:db/id ?eid-artist :artist/name ?artist-name}
  ;                      {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
  ;                      {:db/id ?eid-media :medium/tracks ?eid-track}
  ;                      {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]
  ;              :preds [(<= 1969 ?release-year)
  ;                      (<= ?release-year 1969)]}))

  (when false
    (nl) (println "#2 -----------------------------------------------------------------------------")
    (let [result (d/q
                   '[:find ?title ?album ?year
                     :in $ ?artist-name
                     :where
                     [?a :artist/name ?artist-name]
                     [?t :track/artists ?a]
                     [?t :track/name ?title]
                     [?m :medium/tracks ?t]
                     [?r :release/media ?m]
                     [?r :release/name ?album]
                     [?r :release/year ?year]
                     [(< ?year 1970)]]
                   (live-db) "John Lennon")]
      (spyx-pretty result)))

  (when false
    (nl) (println "#3 -----------------------------------------------------------------------------")
    (let [result (d/q
                   '[:find ?title ?album ?year
                     :in $ % ?artist-name
                     :where
                     [?a :artist/name ?artist-name]
                     [?t :track/artists ?a]
                     [?t :track/name ?title]
                     (track-release ?t ?r)
                     [?r :release/name ?album]
                     [?r :release/year ?year]
                     [(< ?year 1970)]]
                   (live-db) rules "John Lennon" ) ]
      (spyx-pretty result)))

    )

(dotest

  (when false
    (nl) (println "#4-----------------------------------------------------------------------------")
    (pprint/pprint
      (query-map-impl '{:let   [$ (live-db)
                                % rules
                                ?artist-name "John Lennon"]
                        :yield [?track-name ?release-name ?release-year]
                        :where [{:db/id ?eid-artist :artist/name ?artist-name}
                                {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                                {:db/id ?eid-release :release/name ?release-name :release/year ?release-year}]
                        :preds [(<= 1969 ?release-year)
                                (<= ?release-year 1969)
                                ]
                        :rules [(track-release ?eid-track ?eid-release)]})))
  (when true
    (nl) (println "-----------------------------------------------------------------------------")
    (println :lennon-with-rules)
    (spyx-pretty
      (query-map {:let   [$ (live-db)
                          % rules
                          ?artist-name "John Lennon"]
                  :yield [?track-name ?release-name ?release-year]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                          {:db/id ?eid-release :release/name ?release-name :release/year ?release-year}]
                  :preds [(<= 1969 ?release-year)
                          (<= ?release-year 1969)]
                  :rules [(track-release ?eid-track ?eid-release)]})))



  )




