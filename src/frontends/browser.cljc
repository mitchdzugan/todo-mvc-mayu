(ns frontends.browser
  (:require [allpa.core :as a]
            [accountant.core :as accountant]
            [mayu.attach :as attach]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [router :as r]
            [ui.entry]
            [cognitect.transit :as t]))

(defonce a-s-route (atom {}))
(defonce a-off (atom (fn [])))

(def writer (a/writer :json))
(def reader (a/reader :json))

(defn mount-root []
  (let [{:keys [off]}
        (attach/attach (js/document.getElementById "app")
                       {:get-stored-todos
                        (fn []
                          (->> (js/localStorage.getItem "todos-mayu")
                               (t/read reader)))
                        :set-stored-todos
                        (fn [todos]
                          (->> (t/write writer todos)
                               (js/localStorage.setItem "todos-mayu")))
                        ::r/s-route @a-s-route}
                       ui.entry/root)]
    (reset! a-off off)))

(defn main! []
  (println "Client init")
  (let [e-route (e/on! (e/Event))
        {s-route :signal} (s/build (s/from nil e-route))]
    (reset! a-s-route s-route)
    (accountant/configure-navigation!
     {:nav-handler #(let [path (if (< (count %1) 2)
                                 %1
                                 (reduce str "" (drop 2 %1)))]
                        (e/push! e-route (r/match-by-path path)))
      :path-exists? #(-> true)})
    (accountant/dispatch-current!)
    (mount-root)))

(defn reload! []
  (println "reloading")
  (@a-off)
  (mount-root))
