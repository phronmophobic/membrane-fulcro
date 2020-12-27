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

(defn uuid [] (.toString (java.util.UUID/randomUUID)))


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
               (component->view
                (child-factory child))))))}]
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
                                 (component->view root))))
         app (app/fulcro-app
              {:optimized-render! my-optimized-render!
               :render-root! render-root!})
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
                                 (component->view root))))
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
       ;; (comp/transact! app [(list `merge-state {:new-state initial-state})])

       (app/update-shared! app)
       (indexing/index-root! app)

       (prn (app/current-state app))

       ;; (merge/merge! app initial-state []  )
       (app/render! app {:force-root? true})

       (skia/run #(deref app-view) {:draw nil})
       app))))


(defmutation toggle-checkbox
  "docstring"
  [{:checkbox/keys [id]}]
  (action [{:keys [state] :as env}]
          ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
          (swap! state update-in [:checkbox/id id  :checkbox/checked?] not)
          nil)
  )
(def dropdown-table :bootstrap.dropdown/by-id)
(defn dropdown-ident [id-or-props]
  [dropdown-table (::id id-or-props)]
  #_(if (map? id-or-props)
      [(:type id-or-props) (::id id-or-props)]
      [dropdown-table id-or-props]))




(defn wrap-mouse-move-global [handler body]
  (ui/on-mouse-move-global
   (fn [pos]
     (handler (fn [pos] (ui/mouse-move-global body pos))
              pos))
   body))

(defn wrap-membrane-intents [body]
  (let [wrapper (fn [handler & args]
                  (let [intents (seq (apply handler args))]
                    (when intents
                      (map
                       (fn [intent]
                         (if (vector? intent)
                           (list `membrane-dispatch! {:intent intent})
                           intent))
                       intents))
                    ))]
    (wrap-mouse-move-global wrapper
     (ui/wrap-on :mouse-down wrapper
                 :key-press wrapper
                 :mouse-up wrapper
                 :mouse-event wrapper
                 :mouse-move wrapper
                 body))))



(defmutation membrane-dispatch!
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

(defmutation request-focus
  "docstring"
  [{:keys [focus-id]}]

  (action [{:keys [state] :as env}]
          (swap! state assoc :focus focus-id)))




(defn fulcroize-membrane-component* [c prefix cname]
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

(defmacro fulcroize-membrane-component [c prefix cname]
  (fulcroize-membrane-component* c prefix cname))

(fulcroize-membrane-component basic/button "button" Button)

(fulcroize-membrane-component basic/textarea-view "textarea" TextArea)

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
                            :textarea/text ""
                            :textarea/border? true
                            :textarea/id (uuid)}
                           props))
   :ident :textarea/id
   :query [[:focus '_]
           :textarea/cursor
           :textarea/text
           :textarea/down-pos
           :textarea/mpos
           :textarea/select-cursor
           :textarea/last-click
           :textarea/font
           :textarea/border?
           :textarea/id]}
  (do
    ;; (prn (list "=" focus [:textarea/id id] (= focus [:textarea/id id])) )
    (my-events
     (ui/on
      ::basic/request-focus
      (fn []
        [`(request-focus {:focus-id ~[:textarea/id id]})])
      (basic/textarea-view
       :cursor cursor
       :$cursor [:textarea/id (list 'keypath id) :textarea/cursor]
       :focus? (= focus [:textarea/id id])
       :text text
       :$text [:textarea/id (list 'keypath id) :textarea/text]
       :down-pos down-pos
       :$down-pos [:textarea/id (list 'keypath id) :textarea/down-pos]
       :mpos mpos
       :$mpos [:textarea/id (list 'keypath id) :textarea/mpos]
       :select-cursor select-cursor
       :$select-cursor [:textarea/id (list 'keypath id) :textarea/select-cursor]
       :last-click last-click
       :$last-click [:textarea/id (list 'keypath id) :textarea/last-click]
       :font font
       :border? border?)))))

(def ui-textarea* (comp/factory TextArea))
(defn ui-textarea [props]
  (component->view (ui-textarea* props)))

(comment
  (quick-view! TextArea {:textarea/cursor 0
                         :textarea/text "hello"
                         :textarea/border? true
                         :textarea/id 1})
  ,)

(defsc TextAreaRoot [this {:keys [textarea1 textarea2 ]}]
  {:initial-state (fn [_]
                    {:textarea1 (comp/get-initial-state TextArea
                                                        {:textarea/text "one"
                                                         :textarea/id (uuid)})
                     :textarea2 (comp/get-initial-state TextArea
                                                        {:textarea/text "two"
                                                         :textarea/id (uuid)})})
   :query [{:textarea1 (comp/get-query TextArea)}
           {:textarea2 (comp/get-query TextArea)}]}
  (ui/vertical-layout
   (ui/label "Text Area App")
   (ui-textarea textarea1)
   (ui-textarea textarea2)))

(def ui-textarearoot (comp/factory TextAreaRoot))

(comment
  (quick-view-root! TextAreaRoot {}))

(defsc Checkbox [this {:checkbox/keys [id checked?] :as props}]
  {:ident :checkbox/id
   :query [:checkbox/id :checkbox/checked?]
   :initial-state (fn [params]
                    (merge
                     {:checked? false
                      :id (uuid)}
                     params))
   :use-hooks? true}

  (ui/on
   :mouse-down
   (fn [_]
     [(toggle-checkbox {:checkbox/id id})])
   (ui/checkbox checked?)))


(def checkbox-factory (comp/factory Checkbox))
(defn ui-checkbox [props]
  (component->view (checkbox-factory props)))


(defmutation toggle-todo
  "docstring"
  [{:todo/keys [id] :as arg}]
  (action [{:keys [state] :as env}]
          ;; (prn (get-in @state  [:todo/id]) )
          (swap! state update-in [:todo/id id :todo/checked?] not)

          nil)
  )

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
                       :keys [ta cb]
                       :as props}]
  {:initial-state (fn [params]
                    (let [tid (get params :todo/id (uuid))]
                     (merge
                      {:todo/checked? false
                       :todo/id tid
                       :todo/description ""
                       :ta (comp/get-initial-state TextArea {:textarea/id [::todo-item tid]})
                       :cb (comp/get-initial-state Checkbox {:checkbox/id [::todo-item tid]})}
                      params)))
   :use-hooks? true
   :ident :todo/id
   :query [:todo/id
           :todo/checked?
           :todo/description
           {:cb (comp/get-query Checkbox)}
           {:ta (comp/get-query TextArea)}]}
  (ui/horizontal-layout
   (ui/on
    `toggle-checkbox
    (fn [checkbox-id]
      [ ;; (toggle-todo nil)
       (toggle-checkbox checkbox-id)
       (toggle-todo {:todo/id id})])
    (ui-checkbox cb))
   (ui/on-bubble
    (fn [intents]

      (when (seq intents)
        (concat intents
                [(update-todo-description {:todo/id id
                                           :textarea/id (:textarea/id ta)})])))
    (ui-textarea ta))))





(def todo-item-factory (comp/factory TodoItem))
(defn ui-todo-item [props]
  (component->view (todo-item-factory props)))


(comment
  (def app
    (quick-view! TodoItem (comp/get-initial-state TodoItem)))
  ,)

(defmutation add-todo
  "docstring"
  [{id :todo-list/id}]
  (action [{:keys [app state] :as env}]
          (let [tid (uuid)]
            ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
            (merge/merge-component! app TodoItem (comp/get-initial-state TodoItem {:todo/id tid}))
            (swap! state update-in [:todo-list/id id :todo-list/todo-list] conj [:todo/id tid]))
          nil))

(defsc TodoList [this {:todo-list/keys [id todo-list button]}]
  { ;;:initial-state
   :ident :todo-list/id
   :initial-state (fn [params]
                    (merge
                     {:todo-list/id (uuid)
                      :todo-list/todo-list []
                      :todo-list/button (comp/get-initial-state Button
                                                                {:button/text "Add Todo"})}
                     params))
   :use-hooks? true
   :query [{:todo-list/todo-list (comp/get-query TodoItem)}
           {:todo-list/button (comp/get-query Button)}
           :todo-list/id]}
  (apply
   ui/vertical-layout
   (ui/on
    :request-focus
    (fn [])
    :mouse-down
          (fn [_]
            [(add-todo {:todo-list/id id})])
          (ui-button button))
   (map ui-todo-item todo-list)))

(comment
  (def app (quick-view! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))
  ,)

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




