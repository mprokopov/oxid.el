#!/usr/bin/env bb
(require '[cheshire.core :as json])

(def project-root (first *command-line-args*))

;; (def project-root "/Users/mprokopov/oxid/fwtm-shop/oxideshop")
;; (def project-root "/Users/mprokopov/oxid/bergspezl-shop/oxideshop")

(def path (str project-root "/var/configuration/shops/1.yaml"))

(defn modules []
  (->>
   (yaml/parse-string
    (slurp path))
   :modules
   (map second)))


(defn title [module]
  (get-in module [:title :en]))

(defn name-title [{title :title name :name}]
  ;; [name title]
  (println name " - " title)
  )


;; (doseq [m modules]
;;   (println (json/generate-string [{m}])))

(print
 (json/generate-string (modules)))

(comment
  (keys
   (last (modules)))

  (get-in 
   (last (modules)) [:title :en])

  (dorun
   (map name-title (modules)))
  )
