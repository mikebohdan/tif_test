(ns tdf-test.core
  (:require [clojure.string :as str]))

(defn- host-pattern
  [pattern template]
  (->> (re-find #"host\((.+?)\)" template)
       (last)
       (assoc pattern :host)))

(defn- query-pattern
  [pattern template]
  (->> (re-seq #"queryparam\((.+?)\)" template)
       (map last)
       (map #(clojure.string/split % #"="))
       (map first)
       (map keyword)
       (assoc pattern :query)))

(defn- get-path-keywords
  [template]
  (->> (clojure.string/split template #"/")
       (filter #(clojure.string/starts-with? % "?"))
       (map #(subs % 1))
       (map keyword)))

(defn- get-path-re
  [template]
  (re-pattern (clojure.string/replace template #"\?[^/]+" "(.+)")))

(defn- path-pattern
  [pattern template]
  (let [path (last (re-find #"path\((.+?)\)" template))
        path-re (get-path-re path)
        path-keywords (get-path-keywords path)]
    (assoc pattern :path {:re path-re :keywords path-keywords})))

(defn new-pattern
  [config]
  (-> {}
      (path-pattern config)
      (host-pattern config)
      (query-pattern config)))

(defn- parse-url
  [url]
  (->> url
       (re-find #"http[s]?://([\w\.\d]+)([\/\w\d\-_\~]*)\/?(\?.*)?")
       (rest)
       (zipmap [:host :path :query])))

(defn- assoc-conj
  [map key val]
  (assoc map key
             (if-let [cur (get map key)]
               (if (vector? cur)
                 (conj cur val)
                 [cur val])
               val)))

(defn- parse-query-string
  [query]
  (reduce
    (fn [params pair]
      (if-let [[k v] (clojure.string/split pair #"=" 2)]
        (assoc-conj params (keyword k) v)
        params))
    {}
    (clojure.string/split query #"&")))

(defn- recognize-query
  [query pattern params]
  ;; delete 1 char which is '?'
  (if query
    (let [query-args (parse-query-string (subs query 1))]
      (->> (:query pattern)
           (map
             (fn [kw]
               (if (contains? query-args kw)
                 [kw (kw query-args)]
                 (throw
                   (Exception. (str (name kw) " queryparam missing"))))))
           (into params))
      )
    params))

(defn- recognize-host
  [host pattern params]
  (println pattern)
  (println host)
  (if (= host (:host pattern))
    params
    (throw (Exception. "host mistmatch"))))

(defn- recognize-path
  [path pattern params]
  (if-let [args (rest (re-find (get-in pattern [:path :re]) path))]
    (->> args
         (zipmap (get-in pattern [:path :keywords]))
         (into params))
    (throw (Exception. "path does not match"))))

(defn recognize [pattern url]
  (when-let [parsed-url (parse-url url)]
    (reduce (fn [args [key value]]
              (let [recognizer (case key
                                 :path recognize-path
                                 :query recognize-query
                                 :host recognize-host)]
                (recognizer value pattern args)))
            []
            parsed-url)))
