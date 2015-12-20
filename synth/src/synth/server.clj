(ns synth.server
  (:use compojure.core)
  (:require [compojure.route :as route]
            [ring.util.response :as resp]
            [ring.middleware.params :as params]
            [clj-leveldb :as db]
            [ring.middleware.defaults :as defaults]))

(def db (db/create-db "db/leveldb"
                      {:key-decoder byte-streams/to-string
                       :val-decoder byte-streams/to-string
                       :create-if-missing? true}))

(defn save-synth
  [synth session-id]
  (db/put db session-id synth))

(defn get-synth
  [session-id]
  (db/get db session-id))

(defroutes app-routes
  (GET "/" [] (resp/resource-response "index.html" {:root "public"}))
  (POST "/db" [synth :as req]
        (let [session-id (str (java.util.UUID/randomUUID))]
          (save-synth synth session-id)
          {:session-id  session-id}))
  (GET "/db/:session-id" [session-id] (get-synth session-id))
  (route/resources "/")
  (route/not-found "Not Found"))

(def app
  (defaults/wrap-defaults app-routes (assoc defaults/site-defaults :security false)))
