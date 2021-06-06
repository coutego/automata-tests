(defproject automata-tests "0.1.0-SNAPSHOT"
  :source-paths ["src" "test"]

  :dependencies [[org.clojure/clojure "1.10.3"]
                 [org.clojure/clojurescript "1.10.866"]
                 [thheller/shadow-cljs "2.14.2"]
                 [binaryage/devtools "1.0.3"]
                 [nrepl "0.8.3"]
                 [reagent "1.0.0"]
                 [org.clojure/core.match "1.0.0"]
                 [metosin/malli "0.5.1"]]
                                        ;[teknql/aave "f0f0d68"]]

  :plugins [[lein-shadow "0.2.0"]
            [lein-codox "0.10.7"]
            [reifyhealth/lein-git-down "0.4.0"]]

  :repositories [["public-github" {:url "git://github.com"}]
                 ["private-github" {:url "git://github.com" :protocol :ssh}]]

  :codox {:language :clojurescript
          ;; :html     {:namespace-list :flat}
          :metadata {:doc/format :markdown}}

  :shadow-cljs
  {:builds
   {:app
    {:target     :browser
     :output-dir "public/js"
     :asset-path "/js"
     :modules    {:app {:entries [cel-aut.app]}}
     :build-options {:cache-level :off}
     :devtools   {:after-load cel-aut.app/mount-root
                  :http-root "public"
                  :http-port 3000}}
    :browser-test
    {:target    :browser-test
     :ns-regexp "-test$"
     :runner-ns shadow.test.browser
     :build-options {:cache-level :off}
     :test-dir  "target/browser-test"
     :devtools  {:http-root "target/browser-test"
                 :http-port 3010}}}}

  :aliases {"dev"          ["with-profile" "dev" "do"
                            ["shadow" "watch" "app"]]
            "prod"         ["with-profile" "prod" "do"
                            ["shadow" "release" "app"]]
            "build-report" ["with-profile" "prod" "do"
                            ["shadow" "run" "shadow.cljs.build-report" "app" "target/build-report.html"]
                            ["shell" "open" "target/build-report.html"]]
            "karma"        ["with-profile" "prod" "do"
                            ["shadow" "compile" "karma-test"]
                            ["shell" "karma" "start" "--single-run" "--reporters" "junit,dots"]]}

  :profiles
  {:dev {:dependencies [[binaryage/devtools "1.0.0"]]
         :source-paths ["dev"]}
   :prod {}})
