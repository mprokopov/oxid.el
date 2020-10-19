#!/usr/bin/env bb


(def project-root (first *command-line-args*))
(def path (str project-root "/var/configuration/shops/1.yaml"))

(def modules
(->>
  (yaml/parse-string
    (slurp path))
    :modules
    (map first)
    (map #(subs (str %) 1))))

(doseq [m modules]
 (println m))
