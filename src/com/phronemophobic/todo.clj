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
             [com.fulcrologic.fulcro.algorithms.tx-processing.synchronous-tx-processing :as stx]
             [membrane.ui :as ui]
             [membrane.basic-components :as basic]
             membrane.component
             [com.phronemophobic.fulcro :as f
              :refer [uuid
                      component->view
                      show!
                      show-sync!]]
             [membrane.skia :as skia])
  (:gen-class))

;; (defn uuid [] (.toString (java.util.UUID/randomUUID)))


#_(defn my-optimized-render!
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


#_(defn component->view [component]
  (let [opts (comp/component-options component)
        render (:render opts)]
    (render component)))


#_(defmutation request-focus
  "docstring"
  [{:keys [focus-id]}]

  (action [{:keys [state] :as env}]
          (swap! state assoc :focus focus-id)))

#_(defn dispatch! [app tx]
  (when (seq tx)
    ;; (prn "dispatch! "tx)    
    (comp/transact! app tx))
  nil)

#_(defn fulcro-view [dispatch! root]
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
            (when mouse-down?
              (dispatch! [(request-focus nil)])))))
      (membrane.ui/on-key-press
       (fn [s]
         (let [steps (membrane.ui/key-press root s)]
           (dispatch! steps)
           steps))
       (membrane.ui/on-key-event
        (fn [key scancode action mods]
          (let [steps (membrane.ui/key-event root key scancode action mods)]
            (dispatch! steps)
            steps))
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

#_(defn my-mount!
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

#_(defsc Root [this props]
  {:query [{::child (comp/get-query Child)}]}
  (component->view (component->view (child-factory child))))

#_(defn make-root [Child initial-state]
  (clojure.core/let
      [child-factory (comp/factory Child)
       child-ident (comp/ident Child initial-state)
       options__26421__auto__
       {
        :query
        (fn
          query*
          [this]
          [{::child (comp/get-query Child)}]),
        :render
        (fn
          render-Root
          [this]
          (com.fulcrologic.fulcro.components/wrapped-render
           this
           (clojure.core/fn
             []
             (clojure.core/let
                 [{::keys [child]}
                  (com.fulcrologic.fulcro.components/props this)]
               (component->view
                (child-factory child))))))}]
    (com.fulcrologic.fulcro.components/configure-component!
     "Root2"
     ::Root2
     options__26421__auto__)))

#_(defmutation set-child
  "docstring"
  [{:keys [child-ident]}]
  (action [{:keys [state] :as env}]
          (swap! state assoc ::child child-ident)
          nil))

#_(defn show!
  "Pop up a window of the component with the state"

  ([comp initial-state] ;;[app root node {:keys [initialize-state? hydrate?]}]
   (let [
         app-view (atom nil)
         render-root! (fn [root _]
                        (reset! app-view
                                (fulcro-view
                                 (partial dispatch! root)
                                 (component->view root))))
         app (stx/with-synchronous-transactions
               (app/fulcro-app
                {:optimized-render! my-optimized-render!
                 :render-root! render-root!}))
         root (make-root comp initial-state)
         root-factory (comp/factory root)
         ]
     (do
       (app/initialize-state! app root)

       (swap! (::app/runtime-atom app) assoc
              ;; ::mount-node dom-node
              ::app/root-factory root-factory
              ::app/root-class root)
       (merge/merge-component! app comp initial-state)
       (let [child-ident (comp/ident comp initial-state)]
         (comp/transact! app [(list `set-child {:child-ident child-ident})]))

       (app/update-shared! app)
       ;; not doing anything right now
       ;; (indexing/index-root! app)

       (app/render! app {:force-root? true})

       (skia/run #(deref app-view) {:draw nil})
       app))))


#_(defmutation merge-state
  "docstring"
  [{new-state :new-state}]
  (action [{:keys [state] :as env}]
          ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
          
          (swap! state merge new-state))
)

#_(defn quick-view-root!
  "Pop up a window of the component with the state"

  ([comp initial-state] ;;[app root node {:keys [initialize-state? hydrate?]}]
   (let [
         app-view (atom nil)
         render-root! (fn [root _]
                        (reset! app-view
                                (fulcro-view
                                 (partial dispatch! root)
                                 (component->view root))))
         app (stx/with-synchronous-transactions
               (app/fulcro-app
                {:optimized-render! my-optimized-render!
                 :render-root! render-root!}))
         root comp
         root-factory (comp/factory root)
         ]
     (do

       (app/initialize-state! app root)

       (swap! (::app/runtime-atom app) assoc
              ;; ::mount-node dom-node
              ::app/root-factory root-factory
              ::app/root-class root)
       ;; (comp/transact! app [(list `merge-state {:new-state initial-state})])

       (app/update-shared! app)
       (indexing/index-root! app)

       (prn (app/current-state app))

       ;; (merge/merge! app initial-state []  )
       (app/render! app {:force-root? true})

       (skia/run #(deref app-view) {:draw nil})
       app))))


#_(defn wrap-mouse-move-global [handler body]
  (ui/on-mouse-move-global
   (fn [pos]
     (handler (fn [pos] (ui/mouse-move-global body pos))
              pos))
   body))

#_(defn wrap-membrane-intents [body]
  (let [wrapper (fn [handler & args]
                  (let [intents (seq (apply handler args))]
                    (when intents
                      (map
                       (fn [intent]
                         (if (vector? intent)
                           (list `membrane-dispatch! {:intent intent})
                           intent))
                       intents))))]
    (wrap-mouse-move-global wrapper
     (ui/wrap-on :mouse-down wrapper
                 :key-press wrapper
                 :mouse-up wrapper
                 :mouse-event wrapper
                 :mouse-move wrapper
                 body))))



#_(defmutation membrane-dispatch!
  "docstring"
  [{:keys [intent]}]
  (action [{:keys [state] :as env}]
          (let [dispatch! (membrane.component/default-handler state)]
            (try
              (apply dispatch! intent)
              (catch Exception e
                (prn "could not process " intent))))
          nil
          #_(swap! state ...)))






#_(defn fulcroize-membrane-component* [c prefix cname]
  (let [v (resolve c)
        ;; args (into {} (map vec (partition 2 (rest form))))
        fn-meta (meta v)

        arglists (:arglists fn-meta)
        first-arglist (first arglists)
        [ampersand arg-map] first-arglist

        args (disj (set (:keys arg-map))
                   'context)
        defaults (:or arg-map)
        defaults (into {} (map (fn [[k v]]
                                 [(keyword prefix (name k))
                                  v])
                               defaults))

        focusable? (contains? args 'focus?)
        args (disj args 'focus?)

        props {(keyword prefix "keys") (conj (vec args) 'id)}
        props (if focusable?
                (assoc props :keys '[focus])
                props)

        ident (keyword prefix "id")
        initial-state (assoc defaults
                             ident `(uuid))


        query (mapv (fn [arg]
                     (keyword prefix (name arg)))
                   args)
        query (if focusable?
                (conj query [:focus '(quote _)])
                query)
        query (conj query (keyword prefix "id"))

        call-args (for [arg args]
                    [(keyword arg) arg])
        $call-args (for [arg args]
                     [(keyword (str "$" (name arg)))
                      [ident `(list '~'keypath ~'id) (keyword prefix (name arg))]])

        all-call-args (sequence cat
                                (concat call-args
                                        $call-args) )
        all-call-args (if focusable?
                        (concat all-call-args
                                [:focus? (list '= 'focus [ident 'id])])
                        all-call-args)

        body `(~c ~@all-call-args)
        body (if focusable?
               `(ui/on ::basic/request-focus
                       (fn []
                         [(request-focus {:focus-id ~[ident 'id]})])
                       ~body)
               body)

        factory-name (symbol (str "ui-" (clojure.string/lower-case (name cname))
                                  "*"))

        ]
    `(do
       (defsc ~cname [ ~'this ~props]
         {:initial-state
          ;; defsc requires an initial state function
          ;; to be a list (and not a seq)
          ~(list 'fn '[params]
                 `(merge ~initial-state ~'params))
          ;; (~'fn [params#]
          ;;  (merge ~initial-state params#))
          :ident ~ident
          :query ~query
          }
         (wrap-membrane-intents ~body))
       (def ~factory-name
          (comp/factory ~cname))
       (defn ~(symbol (str "ui-" (clojure.string/lower-case (name cname))))
         ~'[props]
         (component->view (~factory-name ~'props))))))

#_(defmacro fulcroize-membrane-component [c prefix cname]
  (fulcroize-membrane-component* c prefix cname))

(f/fulcroize-membrane-component basic/button "button" Button)

(f/fulcroize-membrane-component basic/textarea-view "textarea" TextArea)


(f/fulcroize-membrane-component basic/checkbox "checkbox" Checkbox)


(defmutation toggle-todo
  "docstring"
  [{:todo/keys [id] :as arg}]
  (action [{:keys [state] :as env}]
          (swap! state update-in [:todo/id id :todo/checked?] not)

          nil))

(defmutation update-todo-description
  ""
  [{todo-id :todo/id
    textarea-id :textarea/id}]
  (action [{:keys [state] :as env}]
          (swap! state
                   (fn [state]
                   (let [new-description (get-in state [:textarea/id textarea-id :textarea/text])]
                     (assoc-in state [:todo/id todo-id :todo/description] new-description))))


          nil))

(defsc TodoItem [this {:todo/keys [id checked? description ]
                       :keys [todo-textarea todo-checkbox]
                       :as props}]
  {:initial-state (fn [params]
                    (let [tid (get params :todo/id (uuid))]
                     (merge
                      {:todo/checked? false
                       :todo/id tid
                       :todo/description ""
                       :todo-textarea (comp/get-initial-state TextArea {:textarea/id [::todo-item tid]
                                                             :textarea/text (get params :todo/description "")})
                       :todo-checkbox (comp/get-initial-state Checkbox {:checkbox/id [::todo-item tid]})}
                      params)))
   :ident :todo/id
   :query [:todo/id
           :todo/checked?
           :todo/description
           {:todo-checkbox (comp/get-query Checkbox)}
           {:todo-textarea (comp/get-query TextArea)}]}
  (ui/horizontal-layout
   (ui/on-bubble
    (fn [intents]
      (when (seq intents)
        (concat intents
                [(toggle-todo {:todo/id id})])))
    (ui-checkbox todo-checkbox))
   (ui/on-bubble
    (fn [intents]
      (when (seq intents)
        (concat intents
                [(update-todo-description {:todo/id id
                                           :textarea/id (:textarea/id todo-textarea)})])))
    (ui-textarea todo-textarea))))


(def todo-item-factory (comp/factory TodoItem))
(defn ui-todo-item [props]
  (component->view (todo-item-factory props)))


(comment
  (def app
    (show! TodoItem (comp/get-initial-state TodoItem)))
  ,)

(defmutation add-todo
  "docstring"
  [{id :todo-list/id
    todo-description :todo/description}]
  (action [{:keys [app state] :as env}]
          (let [tid (uuid)]
            ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
            (merge/merge-component! app TodoItem (comp/get-initial-state TodoItem {:todo/id tid
                                                                                   :todo/description todo-description}))
            (swap! state update-in [:todo-list/id id :todo-list/todo-list] conj [:todo/id tid]))
          nil))


(defmutation clear-textarea
  "docstring"
  [{id :textarea/id}]
  (action [{:keys [state] :as env}]
          (swap! state update-in [:textarea/id id]
                 assoc
                 :textarea/text ""
                 :textarea/cursor 0)
          nil))

(defsc TodoList [this {:todo-list/keys [id todo-list button new-todo-textarea]}]
  { ;;:initial-state
   :ident :todo-list/id
   :initial-state (fn [params]
                    (let [tlid (get params :todo-list/id (uuid))]
                      (merge
                       {:todo-list/id tlid
                        :todo-list/todo-list []
                        :todo-list/button (comp/get-initial-state Button
                                                                  {:button/text "Add Todo"})
                        :todo-list/new-todo-textarea (comp/get-initial-state TextArea
                                                                             {:textarea/id [:todo-list/id tlid]})}
                       params)))
   :query [{:todo-list/todo-list (comp/get-query TodoItem)}
           {:todo-list/button (comp/get-query Button)}
           {:todo-list/new-todo-textarea (comp/get-query TextArea)}
           :todo-list/id]}
  (do
    (apply
     ui/vertical-layout
     (ui/horizontal-layout
      (ui/on
       :mouse-down
       (fn [_]
         [(add-todo {:todo-list/id id
                     :todo/description (:textarea/text new-todo-textarea)})
          (clear-textarea {:textarea/id (:textarea/id new-todo-textarea)})
          ])
       (ui-button button))
      (ui-textarea new-todo-textarea))
     (map ui-todo-item todo-list))))

(comment
  (def app (show! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))
  ,)

(defn -main [ & args]
  (def app (show-sync! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))
  )






