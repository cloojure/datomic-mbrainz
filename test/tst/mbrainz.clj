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

(comment
  (def rules  ; sample rules-collection with one rule
    '[    ; list of rules

      ; Rule #1: Given ?t bound to track entity-ids, binds ?r to the corresponding set of album release entity-ids
      [(track-release ?t ?r) ; interpret as:  (fn-name arg1 arg2)
       [?m :medium/tracks ?t] ; #1 requirement: all must be true...
       [?r :release/media ?m] ; #2 requirement: ... or the rule fails
       ]

      ]))

; Read in the actual rules from the EDN fil
(def rules (edn/read-string
             (slurp (io/resource "rules.edn"))))

;---------------------------------------------------------------------------------------------------
; Convenience function to keep syntax a bit more concise
(defn live-db [] (d/db conn))



(dotest
  (let [ctr (atom 0)]
    (doseq [it (d/datoms (live-db) :eavt)]
      (swap! ctr inc))
    (nl) (spy :total-datoms @ctr))

  (nl)
  (is= #{{:gid #uuid "76c9a186-75bd-436a-85c0-823e3efddb7f", :ident-type :artist.type/person, :ident-gender :artist.gender/female}}
    (td/query-map {:let   [$ (live-db)
                        ?str-name "Janis Joplin"]
                :yield [?gid ?ident-type ?ident-gender]
                :where [{:db/id ?e :artist/name ?str-name :artist/gid ?gid :artist/type ?eid-type :artist/gender ?eid-gender}
                        {:db/id ?eid-type :db/ident ?ident-type}
                        {:db/id ?eid-gender :db/ident ?ident-gender}]}))
  (nl)
  (spyx-pretty :lennon-tracks-titles-partial
    (take 10
      (td/query-map {:let   [$ (live-db)
                          ?artist-name "John Lennon"]
                  :yield [?track-name]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}]})))
  (nl)
  (spyx-pretty :lennon-title-album-year
    (take 10
      (td/query-map {:let   [$ (live-db)
                          ?artist-name "John Lennon"]
                  :yield [?track-name ?release-name ?release-year]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                          {:db/id ?eid-media :medium/tracks ?eid-track}
                          {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]})))

  (nl)
  ; Testing the macro
  (when false
    (println "-----------------------------------------------------------------------------")
    (pprint/pprint
      (td/query-map-impl
        '{:let   [$ (live-db)
                  ?artist-name "John Lennon"]
          :yield [?track-name ?release-name ?release-year]
          :preds [(< ?year 1970)]
          :where [{:db/id ?eid-artist :artist/name ?artist-name}
                  {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                  {:db/id ?eid-media :medium/tracks ?eid-track}
                  {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]})))

  (when true
    (spyx-pretty :lennon-title-album-year
      (td/query-map {:let   [$ (live-db)
                          ?artist-name "John Lennon"]
                  :yield [?track-name ?release-name ?release-year]
                  :where [{:db/id ?eid-artist :artist/name ?artist-name}
                          {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                          {:db/id ?eid-media :medium/tracks ?eid-track}
                          {:db/id ?eid-release :release/media ?eid-media :release/name ?release-name :release/year ?release-year}]
                  :preds [(<= 1969 ?release-year)
                          (<= ?release-year 1969)]})))

  (when false
    (nl) (println "#2 orig syntax -----------------------------------------------------------------------------")
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
    (nl) (println "#3 orig syntax with rules -----------------------------------------------------------------------------")
    (let [result (d/q
                   '[:find ?title ?album ?year
                     :in $ ; bound to `(live-db)`
                     % ; bound to `rules`
                     ?artist-name ; bound to "John Lennon"
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
    (pprint/pprint ; testing the macro
      (td/query-map-impl '{:let   [$ (live-db)
                                   % rules
                                   ?artist-name "John Lennon"]
                           :yield [?track-name ?release-name ?release-year]
                           :where [{:db/id ?eid-artist :artist/name ?artist-name}
                                   {:db/id ?eid-track :track/artists ?eid-artist :track/name ?track-name}
                                   {:db/id ?eid-release :release/name ?release-name :release/year ?release-year}]
                           :preds [(<= 1969 ?release-year)
                                   (<= ?release-year 1969)]
                           :rules [(track-release ?eid-track ?eid-release)]})))
  (when true
    (nl) (println "-----------------------------------------------------------------------------")
    (println :lennon-with-rules)
    (spyx-pretty
      (td/query-map {:let   [$ (live-db)
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




