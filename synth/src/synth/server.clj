(ns synth.server
  (:use compojure.core)
  (:require [compojure.route :as route]
            [ring.util.response :as resp]
            [ring.middleware.params :as params]
            [ring.middleware.json :as middleware-json]
            [clj-leveldb :as db]
            [ring.middleware.defaults :as defaults]
            [clojure.data.json :as json]))

(defonce db (db/create-db "db/leveldb"
                          {:key-decoder byte-streams/to-string
                           :val-decoder byte-streams/to-string
                           :create-if-missing? true}))

(defn save-synth
  [synths steps]
  (let [session-id (str (java.util.UUID/randomUUID))]
    (db/put db session-id (json/write-str {:synths synths
                                                 :steps steps}))
    session-id))

(defn get-synth
  [session-id]
  (db/get db session-id))

(defroutes app-routes
  (GET "/" []
       (resp/redirect "/index.html"))
  (POST "/db" [synths steps]
        (resp/response (save-synth synths steps)))
  (GET "/db/:session-id" [session-id]
       (get-synth session-id))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (->  app-routes
       (defaults/wrap-defaults (assoc defaults/site-defaults :security false))
       middleware-json/wrap-json-params
       middleware-json/wrap-json-response))
