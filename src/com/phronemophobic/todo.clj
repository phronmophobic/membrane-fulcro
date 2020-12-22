(ns com.phronemophobic.todo
  (:require  [com.fulcrologic.fulcro.application :as app]
             [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
             [com.fulcrologic.fulcro.mutations :refer [defmutation] :as mut]
             [com.fulcrologic.fulcro.algorithms.lookup :as ah]
             [com.fulcrologic.fulcro.algorithms.indexing :as indexing]
             [com.fulcrologic.fulcro.dom :as dom]
             [com.fulcrologic.fulcro.inspect.inspect-client :as inspect]
             [com.fulcrologic.fulcro.algorithms.merge :as merge]
             [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
             [membrane.ui :as ui]
             [membrane.basic-components :as basic]
             membrane.component
             [clojure.zip :as z]
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



;; (def app-view (atom nil))

;; (skia/run #(deref app-view))

(defn component->view [component]
  (let [opts (comp/component-options component)
        render (:render opts)]
    (render component)))




(defn ui-zip [ui]
  (z/zipper #(and (seq (membrane.ui/children %))
                  (satisfies? membrane.ui/IMakeNode %))
            membrane.ui/children
            membrane.ui/make-node
            ui))

(defn render-all [ui]
  (loop [loc (ui-zip ui)]
    (if (z/end? loc)
      (z/root loc)
      (recur (let [loc (z/edit loc
                               (fn [x]
                                 (loop [x x]
                                   (if (comp/component-instance? x)
                                     (recur (component->view x))
                                     x))))]
               (z/next loc))))))


(defn dispatch! [app tx]
  (when (seq tx)
    ;; (prn "dispatch! "tx)    
    (comp/transact! app tx))
  nil)

(defn fulcro-view [dispatch! root]
  (membrane.ui/on-scroll
   (fn [offset mpos]
     (let [steps (membrane.ui/scroll root offset mpos)]
       (dispatch! steps)))
   (membrane.ui/on-mouse-move-global
    (fn [pos]
      (let [steps (membrane.ui/mouse-move-global root pos)]
        (dispatch! steps)))
    (membrane.ui/on-mouse-move
     (fn [pos]
       (let [steps (membrane.ui/mouse-move root pos)]
         (dispatch! steps)))
     (membrane.ui/on-mouse-event
      (fn [pos button mouse-down? mods]
        (let [steps (membrane.ui/mouse-event root pos button mouse-down? mods)]
          (if (seq steps)
            (dispatch! steps)
            #_(when mouse-down?
                (handle-step [:set [:context :focus] nil] emit!)))))
      (membrane.ui/on-key-press
       (fn [s]
         (let [steps (membrane.ui/key-press root s)]
           (dispatch! steps)
           steps)
         )
       (membrane.ui/on-key-event
        (fn [key scancode action mods]
          (let [steps (membrane.ui/key-event root key scancode action mods)]
            (dispatch! steps)
            steps)
          )
        (membrane.ui/on-clipboard-cut
         (fn []
           (let [steps (membrane.ui/clipboard-cut root)]
             (dispatch! steps)
             steps))
         (membrane.ui/on-clipboard-copy
          (fn []
            (let [steps (membrane.ui/clipboard-copy root)]
              (dispatch! steps)
              steps))
          (membrane.ui/on-clipboard-paste
           (fn [s]
             (let [steps (membrane.ui/clipboard-paste root s)]
               (dispatch! steps)
               steps))
           root))))))))))

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

;; (my-mount! app Root :my-root)

;; let [[k v] (comp/ident comp initial-state)]

#_(defsc Root [this {::keys [child child-type]}]
  ;; {:query [::child ::child-type]}
  (child-type child))

#_(defsc Root [this {::keys [child]}]
  {:query [{::child (comp/get-query Child)}]
   :initial-state {}}
  (child-factory child))


#_(def root-factory (comp/factory Root))


(defn make-root [Child initial-state]
  (clojure.core/let
      [child-factory (comp/factory Child)
       child-ident (comp/ident Child initial-state)
       options__26421__auto__
       {
        :query
        (fn
          query*
          [this]
          [#:com.phronemophobic.todo{:child (comp/get-query Child)}]),
        ;; :initial-state
        ;; (fn
        ;;   build-initial-state*
        ;;   [params]
        ;;   (com.fulcrologic.fulcro.components/make-state-map
        ;;    {::child child-ident}
        ;;    #:com.phronemophobic.todo{:child Child}
        ;;    params))
        :render
        (fn
          render-Root
          [this]
          (com.fulcrologic.fulcro.components/wrapped-render
           this
           (clojure.core/fn
             []
             (clojure.core/let
                 [#:com.phronemophobic.todo{:keys [child]}
                  (com.fulcrologic.fulcro.components/props this)]
               (child-factory child) 
               #_(child-factory child)))))}]
    (com.fulcrologic.fulcro.components/configure-component!
     "Root2"
     :com.phronemophobic.todo/Root2
     options__26421__auto__)))

(defmutation set-child
  "docstring"
  [{:keys [child-ident]}]
  (action [{:keys [state] :as env}]
          (swap! state assoc ::child child-ident)
          nil
          #_(swap! state ...))
)

(defn quick-view!
  "Pop up a window of the component with the state"

  ([comp initial-state] ;;[app root node {:keys [initialize-state? hydrate?]}]
   (let [
         app-view (atom nil)
         render-root! (fn [root _]
                        (reset! app-view
                                (fulcro-view
                                 (partial dispatch! root)
                                 (render-all root))))
         app (app/fulcro-app
              {:optimized-render! my-optimized-render!
               :render-root! render-root!})
         root (make-root comp initial-state)
         root-factory (comp/factory root)
         ]
     (do
       (app/initialize-state! app root)
       #_(when initialize-state?
         )

       (swap! (::app/runtime-atom app) assoc
              ;; ::mount-node dom-node
              ::app/root-factory root-factory
              ::app/root-class root)
       (merge/merge-component! app comp initial-state)
       (let [child-ident (comp/ident comp initial-state)]
         (comp/transact! app [(list `set-child {:child-ident child-ident})]))

       (app/update-shared! app)
       (indexing/index-root! app)

       ;; (merge/merge! app initial-state []  )
       (app/render! app {:force-root? true})

       (skia/run #(deref app-view) {:draw nil})
       app))))


(defmutation merge-state
  "docstring"
  [{new-state :new-state}]
  (action [{:keys [state] :as env}]
          ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
          
          (swap! state merge new-state))
)

(defn quick-view-root!
  "Pop up a window of the component with the state"

  ([comp initial-state] ;;[app root node {:keys [initialize-state? hydrate?]}]
   (let [
         app-view (atom nil)
         render-root! (fn [root _]
                        (reset! app-view
                                (fulcro-view
                                 (partial dispatch! root)
                                 (render-all root))))
         app (app/fulcro-app
              {:optimized-render! my-optimized-render!
               :render-root! render-root!})
         root comp
         root-factory (comp/factory root)
         ]
     (do

       (app/initialize-state! app root)

       (swap! (::app/runtime-atom app) assoc
              ;; ::mount-node dom-node
              ::app/root-factory root-factory
              ::app/root-class root)
       (comp/transact! app [(list `merge-state {:new-state initial-state})])

       (app/update-shared! app)
       (indexing/index-root! app)

       (prn (app/current-state app))

       ;; (merge/merge! app initial-state []  )
       (app/render! app {:force-root? true})

       (skia/run #(deref app-view) {:draw nil})
       app))))




(defsc Todo [this {:todo/keys [id description complete?] :as props}]
  {:ident :todo/id
   :query         [:todo/id :todo/description :todo/complete?]
   ;;:initial-state {:ui/number 0}
   }
  (ui/horizontal-layout
   (ui/checkbox complete?)
   (ui/label description)))




(defmutation toggle-checkbox
  "docstring"
  [{:checkbox/keys [id]}]
  (action [{:keys [state] :as env}]
          ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
          (prn @state)
          (prn "toggling checkbox" id)
          (swap! state update-in [:checkbox/id id  :checkbox/checked?] not))
)
(def dropdown-table :bootstrap.dropdown/by-id)
(defn dropdown-ident [id-or-props]
  [dropdown-table (::id id-or-props)]
  #_(if (map? id-or-props)
      [(:type id-or-props) (::id id-or-props)]
      [dropdown-table id-or-props]))

(comment
  (defui ^:once Dropdown
    static prim/IQuery
    (query [this] [::id ::active-item ::label ::open? {::items (prim/get-query DropdownItem)} :type])
    static prim/Ident
    (ident [this props] (dropdown-ident props))
    Object
    (render [this]
            (let [{:keys [::id ::label ::active-item ::items ::open?]} (prim/props this)
                  {:keys [onSelect kind stateful? value]} (prim/get-computed this)
                  active-item-label (->> items
                                         (some #(and (= active-item (::id %)) %))
                                         ::label)
                  value-label       (->> items
                                         (some #(and (= value (::id %)) %))
                                         ::label)
                  label             (cond
                                      (and value value-label) value-label
                                      (and active-item-label stateful?) active-item-label
                                      :otherwise label)
                  onSelect          (fn [item-id]
                                      (prim/transact! this `[(close-all-dropdowns {}) (set-dropdown-item-active ~{:id id :item-id item-id})])
                                      (when onSelect (onSelect item-id)))
                  open-menu         (fn [evt]
                                      (.stopPropagation evt)
                                      (prim/transact! this `[(close-all-dropdowns {}) (set-dropdown-open ~{:id id :open? (not open?)})])
                                      false)]
              (button-group {:className (if open? "open" "")}
                            (button {:className (cond-> "dropdown-toggle"
                                                  kind (str " btn-" (name kind))) :aria-haspopup true :aria-expanded open? :onClick open-menu}
                                    (tr-unsafe label) " " (dom/span {:className "caret"}))
                            (dom/ul {:className "dropdown-menu"}
                                    (map #(ui-dropdown-item % :onSelect onSelect :active? (cond
                                                                                            stateful? (= (::id %) active-item)
                                                                                            value (= value (::id %)))) items)))))))




(defn my-events [body]
  (let [do-nothing (fn [handler & args]
                     (let [intents (seq (apply handler args))]
                       (when intents
                         (map
                          (fn [intent]
                            (if (vector? intent)
                              (list `membrane-dispatch! {:intent intent})
                              intent))
                          intents))
                       ))]
    (ui/wrap-on :mouse-down do-nothing
                :key-press do-nothing
                :mouse-up do-nothing
                :mouse-event do-nothing
                :mouse-move do-nothing
                body)))

(defmutation membrane-dispatch!
  "docstring"
  [{:keys [intent]}]
  (action [{:keys [state] :as env}]
          (let [dispatch! (membrane.component/default-handler state)]
            (apply dispatch! intent))
          nil
          #_(swap! state ...)))

(defmutation request-focus
  "docstring"
  [{:keys [focus-id]}]
  (action [{:keys [state] :as env}]
          (swap! state assoc :focus focus-id)))

(defsc TextArea [ this {:textarea/keys [cursor
                                        text
                                        down-pos
                                        mpos
                                        select-cursor
                                        last-click
                                        font
                                        id
                                        border?]
                        :keys [focus]}]
  {:initial-state (fn [props]
                    (merge {:textarea/cursor 0
                            :textarea/text "hello there"
                            :textarea/border? true}
                           props))
   :ident :textarea/id
   :query [:focus
           :textarea/cursor
           :textarea/text
           :textarea/down-pos
           :textarea/mpos
           :textarea/select-cursor
           :textarea/last-click
           :textarea/font
           :textarea/border?
           :textarea/id]}
  (my-events
   (ui/on
    ::basic/request-focus
    (fn []
      [`(request-focus {:focus-id ~[:textarea/id id]})])
    (basic/textarea-view
     :cursor cursor
     :$cursor [:textarea/id id :textarea/cursor]
     :focus? (= focus [:textarea/id id])
     :text text
     :$text [:textarea/id id :textarea/text]
     :down-pos down-pos
     :$down-pos [:textarea/id id :textarea/down-pos]
     :mpos mpos
     :$mpos [:textarea/id id :textarea/mpos]
     :select-cursor select-cursor
     :$select-cursor [:textarea/id id :textarea/select-cursor]
     :last-click last-click
     :$last-click [:textarea/id id :textarea/last-click]
     :font font
     :border? border?))))

(def ui-textarea* (comp/factory TextArea))
(defn ui-textarea [props]
  (component->view (ui-textarea* props)))

(comment
  (quick-view! TextArea {:textarea/cursor 0
                         :textarea/text "hello"
                         :textarea/border? true
                         :textarea/id 1}))

(defsc TextAreaRoot [this {:keys [textarea1 textarea2 focus]}]
  {:initial-state (fn [_]
                    {:textarea1 (comp/get-initial-state TextArea
                                                        {:textarea/cursor 0
                                                         :textarea/text "one"
                                                         :textarea/border? true
                                                         :textarea/id 1})
                     :textarea2 (comp/get-initial-state TextArea
                                                        {:textarea/cursor 0
                                                         :textarea/text "two"
                                                         :textarea/border? true
                                                         :textarea/id 2})
                     :focus [:textarea/id 1]})
   :query [{:textarea1 (comp/get-query TextArea)}
           {:textarea2 (comp/get-query TextArea)}
           :focus]}
  (ui/vertical-layout
   (ui/label "Text Area App")
   (ui-textarea (merge textarea1 {:focus focus}))
   (ui-textarea (merge textarea2 {:focus focus}))))

(comment
  (quick-view-root! TextAreaRoot {}))


(defsc Checkbox [this {:checkbox/keys [id checked?] :as props}]
  {:ident :checkbox/id
   :query [:checkbox/id :checkbox/checked?]
   :use-hooks? true}

  (ui/on
   :mouse-down
   (fn [_]
     [`(toggle-checkbox {:checkbox/id ~id})])
   (ui/checkbox checked?)))

(def checkbox-factory (comp/factory Checkbox))
(defn my-checkbox [props]
  (component->view (checkbox-factory props)))
#_(let [checkbox-factory (comp/factory Checkbox {:keyfn :checkbox/id})]
    (defn checkbox

      [props & {:keys [onSelect kind value stateful?] :as attrs}]
      (ui-dropdown-factory (comp/computed props attrs))))

(defn -main [ & args]
  (def test-app
    (quick-view! Checkbox {:checkbox/id 1
                           :checkbox/checked? true})))

#_(quick-view! Todo
             {:todo/id 1
              :todo/description 10
              :todo/complete? false}
             
             )


(defsc CheckboxTest [this {:keys [checkbox1
                                  checkbox2
                                  checkbox3
                                  foo] :as props}]
  {:initial-state {:foo :bar}
   :query [{:checkbox1 (comp/get-query Checkbox)}
           {:checkbox2 (comp/get-query Checkbox)}
           {:checkbox3 (comp/get-query Checkbox)}
           :foo]
   :use-hooks? true}
  (do
    (ui/vertical-layout
     (my-checkbox (or checkbox1 {:checkbox/id 1}))
     (my-checkbox (or checkbox2 {:checkbox/id 2}))
     (my-checkbox (or checkbox3 {:checkbox/id 3})))))

#_(quick-view-root! CheckboxTest {:checkbox1 [:checkbox/id 1]
                                :checkbox2 [:checkbox/id 2]
                                :checkbox3 [:checkbox/id 3]
                                :checkbox/id {1 {:checkbox/id 1}
                                              2 {:checkbox/id 2}
                                              3 {:checkbox/id 3}}})


#_(quick-view-root! CheckboxTest {:checkbox1 [:checkbox/id 1]
                                :checkbox2 [:checkbox/id 2]
                                :checkbox3 [:checkbox/id 3]})

;; cheat sheet
(comment

  ;; get app state
  (app/current-state app)

  ;; add an instance
  (merge/merge-component! app Root {:ui/number -1})

  ;; get query (comp/get-query root-class state-map)
  (comp/get-query Todo)

  ;; query the tree?
  (com.fulcrologic.fulcro.algorithms.denormalize/db->tree query state state)

  ,)







(defsc Thing [this {:keys [x]}]
  {:query [:x]}
  nil)

(def ui-thing-1 (comp/factory Thing {:qualifier :x}))
