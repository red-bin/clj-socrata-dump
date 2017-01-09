(ns socrata.core
  (:gen-class)
  (:use clojure.test )
  (:require [clj-http.client :as client]
            [cheshire.core :refer :all]
            [clojure.string :as str]
            [me.raynes.fs :as fs]
            [clojure.java.io :as io]
            [clojure.tools.cli :refer [parse-opts]]))

(defn download-json 
  [url]
  (let [http-resp (client/get url {:accept :json :insecure? true})]
       (decode (:body http-resp) true)))

(def data-dir "/opt/data/socrata")
(def base-url "https://data.cityofchicago.org" )
(def views-url (format "%s/api/search/views.json?limit=2000" base-url))
(def dataset-url (format "%s/data.json" base-url))

(def datasets (:dataset (download-json dataset-url)))
(def views (map :view (:results (download-json views-url))))

(defn dataset-links
  [dataset]
  (map :downloadURL (:distribution dataset)))

(defn dataset-id
  [dataset]
  (last (str/split (:identifier dataset) #"/")))

(defn view-metadata 
  [view-id]
  (first (filter #(= (:id %) view-id) views)))

(defn query-hashmap 
  [query-str] 
  (apply conj 
         (apply (fn [k v] (hash-map (keyword k) v))
                (str/split query-str #"[&=;]"))))

(defn response-filename
  [response]
  (let [dl-url (last (:trace-redirects response))
        parsed-url (client/parse-url dl-url)
        query-map (query-hashmap (:query-string parsed-url))]
    (if (contains? query-map :filename)
        (:filename query-map)
        (last (str/split (:uri parsed-url) #"/")))))

(defn prepare-dir 
  [view-id]
  (let [dir (format "%s/%s" data-dir view-id)]
       (if (not (fs/exists? dir))
           (let [metafile (format "%s/view.json" dir) 
                 metadata (view-metadata view-id)]
                (fs/mkdir dir)
                (spit metafile metadata)))))

(defn save-dataset
  [view-id http-response]
  (prepare-dir view-id)
  (let [filename (response-filename (dissoc http-response :body))
        filepath (format "%s/%s/%s" data-dir view-id filename)]
        (println (format "Saving to %s " filepath))
        (with-open [w (io/output-stream filepath)]
                   (.write w (:body http-response)))))
 
(defn request-dataset
  [url redirect?]
  (if redirect? 
      (println (format "Downloading %s" url)))
  (client/get 
    url {:insecure? true 
         :follow-redirects redirect?
         :as :byte-array
         :async? true
         :throw-exceptions false}))

(defn fetch-dataset [view-id url]
  ;socrata 400s with a HEAD request. Workaround using redirects.
  (let [redir-location 
            (->> (request-dataset url false) :headers :Location)
        response 
            (if (nil? redir-location)
                (request-dataset url true)
                (request-dataset redir-location true))]
        (save-dataset view-id response)))
        
(defn fetch-full-dataset
  [dataset] 
  (let [links (dataset-links dataset)
        view-id (dataset-id dataset)]
    (println (format "\nGetting %s" view-id))
    (->> (map #(fetch-dataset view-id %) links)
         (doall))))

(defn download-socrata []
  (doall (map fetch-full-dataset datasets)))

(defn -main
  [& args])
