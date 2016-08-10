(set-env!
  :asset-paths  #{"rsc"}
  :source-paths #{"src"}
  :dependencies '[[org.clojure/clojure       "1.8.0"          :scope "test"]
                  [org.clojure/clojurescript "1.9.89"         :scope "test"]
                  [adzerk/boot-cljs          "1.7.228-1"      :scope "test"]
                  [adzerk/boot-reload        "0.4.11"         :scope "test"]
                  [adzerk/bootlaces          "0.1.13"         :scope "test"]
                  [hoplon/boot-hoplon        "0.2.0"          :scope "test"]
                  [tailrecursion/boot-static "0.0.1-SNAPSHOT" :scope "test"]
                  [hoplon/ui                 "0.0.1-SNAPSHOT"]
                  [markdown-clj              "0.9.89"]])

(require
  '[adzerk.bootlaces          :refer :all]
  '[adzerk.boot-cljs          :refer [cljs]]
  '[adzerk.boot-reload        :refer [reload]]
  '[hoplon.boot-hoplon        :refer [hoplon]]
  '[tailrecursion.boot-static :refer [serve]])

(def +version+ "0.0.1-SNAPSHOT")

(bootlaces! +version+)

(deftask build []
  (comp (speak) (hoplon) (cljs :optimizations :advanced) (target)))

(deftask develop
  [o optimizations OPT  kw  "Optimizations to pass the cljs compiler."
   p port          PORT int "The port from which to serve the application."]
  (let [o (or optimizations :none)
        p (or port          4000)]
    (comp (watch) (speak) (hoplon) (reload) (cljs :optimizations o) (serve :port p))))

(task-options!
  develop {:port 4000}
  target  {:dir  #{"tgt"}})
