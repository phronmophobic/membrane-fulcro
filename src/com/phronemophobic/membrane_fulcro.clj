(ns com.phronemophobic.membrane-fulcro
  (:require  [com.fulcrologic.fulcro.application :as app]
             [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
             [com.fulcrologic.fulcro.mutations :refer [defmutation]]
             [com.fulcrologic.fulcro.algorithms.lookup :as ah]
             [com.fulcrologic.fulcro.algorithms.indexing :as indexing]
             [com.fulcrologic.fulcro.dom :as dom]
             [com.fulcrologic.fulcro.inspect.inspect-client :as inspect]
             [com.fulcrologic.fulcro.algorithms.merge :as merge]
             [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
             [membrane.ui :as ui]
             [membrane.skia :as skia])
  (:gen-class))


(defn my-optimized-render!
  "Render the UI. The keyframe render runs a full UI query and then asks React to render the root component.
  The optimizations for this kind of render are purely those provided by `defsc`'s default
  shouldComponentUpdate, which causes component to act like React PureComponent (though the props compare in cljs
  is often faster).

  If `:hydrate?` is true it will use the React hydrate functionality (on browsers) to render over
  server-rendered content in the DOM.

  If `:force-root? true` is included in the options map then not only will this do a keyframe update, it will also
  force all components to return `false` from `shouldComponentUpdate`."
  [app {:keys [force-root? hydrate?] :as options}]
  (binding [comp/*blindly-render* force-root?]
    (let [{:com.fulcrologic.fulcro.application/keys [runtime-atom state-atom]} app
          {:com.fulcrologic.fulcro.application/keys [root-factory root-class mount-node]} @runtime-atom
          r!               (if hydrate?
                             (or (ah/app-algorithm app :hydrate-root!) )
                             (or (ah/app-algorithm app :render-root!)))
          state-map        @state-atom
          query            (comp/get-query root-class state-map)
          data-tree        (if query
                             (fdn/db->tree query state-map state-map)
                             state-map)
          app-root (r! (root-factory data-tree) mount-node)]
      (swap! runtime-atom assoc :com.fulcrologic.fulcro.application/app-root app-root)
      app-root)))



(def app-view (atom nil))

(skia/run #(deref app-view))

(defn component->view [component]
  (let [opts (comp/component-options component)
        render (:render opts)]
    (render component)))

(defn render-root! [root mount-node]
  (reset! app-view (component->view root)))

(def app (app/fulcro-app
          {
           ;; :hydrate-root! (fn [& args]
           ;;                  (tap> (list :hydrate args)))
           :optimized-render! my-optimized-render!
           :render-root! render-root!}))






(defmutation bump-number [ignored]
  (action [{:keys [state]}]
          (swap! state update :ui/number inc)))

(defsc Root [this {:ui/keys [number]}]
  {:query         [:ui/number]
   :initial-state {:ui/number 0}}
  (ui/vertical-layout
   (ui/label "This is an example.")
   (ui/button  (str "You've clicked this button " number " times.")
               (fn []
                 (comp/transact! this [(bump-number {})])
                 nil))))

;; get app state
;; (app/current-state app)
;; add an instance
;; (merge/merge-component! app Root {:ui/number -1})



(defn my-mount!
  "Mount the app.  If called on an already-mounted app this will have the effect of re-installing the root node so that
  hot code reload will refresh the UI (useful for development).

  - `app`  The Fulcro app
  - `root`  The Root UI component
  - `node` The (string) ID or DOM node on which to mount.
  - `options` An optional map with additional mount options.


  `options` can include:

  - `:initialize-state?` (default true) - If NOT mounted already: Pulls the initial state tree from root component,
  normalizes it, and installs it as the application's state.  If there was data supplied as an initial-db, then this
  new initial state will be *merged* with that initial-db.
  - `:hydrate?` (default false) - Indicates that the DOM will already contain content from the
    server that should be attached instead of overwritten. See ReactDOM.hydrate.
  "
  ([app root node]
   (my-mount! app root node {:initialize-state? true}))
  ([app root node {:keys [initialize-state? hydrate?]}]
   (let [initialize-state? (if (boolean? initialize-state?) initialize-state? true)
         reset-mountpoint! (fn []
                             (let [root-factory (comp/factory root)]
                               (do
                                 (swap! (::app/runtime-atom app) assoc
                                        ;; ::mount-node dom-node
                                        ::app/root-factory root-factory
                                        ::app/root-class root)
                                 (app/update-shared! app)
                                 (indexing/index-root! app)
                                 (app/render! app {:force-root? true
                                                   :hydrate?    hydrate?}))))]
     (if (app/mounted? app)
       (reset-mountpoint!)
       (do
         (inspect/app-started! app)
         (when initialize-state?
           (app/initialize-state! app root))
         (reset-mountpoint!)
         (when-let [cdm (-> app ::app/config :client-did-mount)]
           (cdm app)))))))

(my-mount! app Root :my-root)

;; (merge/merge-component! app Person {:person/id 11 :person/name "Joe"})

#_(defsc Root [this props]
  
  {:use-hooks? true}
  (ui-person {:person/name "Joe" :person/age 22}))








(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
