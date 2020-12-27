(ns com.phronemophobic.todo-terminal
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
             [clojure.zip :as z]
             [com.phronemophobic.fulcro :as f
              :refer [uuid
                      component->view
                      show!
                      show-sync!]]
             [membrane.lanterna :as lanterna])
  (:gen-class))

(defn log [& msgs]
  (spit "lanterna.log" (str (clojure.string/join " " msgs) "\n") :append true))

(f/fulcroize-membrane-component lanterna/textarea-view "textarea" TextArea)
(f/fulcroize-membrane-component lanterna/checkbox "checkbox" Checkbox)


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

(defsc TodoList [this {:todo-list/keys [id todo-list new-todo-textarea]}]
  { ;;:initial-state
   :ident :todo-list/id
   :initial-state (fn [params]
                    (let [tlid (get params :todo-list/id (uuid))]
                      (merge
                       {:todo-list/id tlid
                        :todo-list/todo-list []
                        :todo-list/new-todo-textarea (comp/get-initial-state TextArea
                                                                             {:textarea/id [:todo-list/id tlid]})}
                       params)))
   :query [{:todo-list/todo-list (comp/get-query TodoItem)}
           {:todo-list/new-todo-textarea (comp/get-query TextArea)}
           :todo-list/id]}
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
     (lanterna/button "Add Todo"))
    (ui-textarea new-todo-textarea))
   (map ui-todo-item todo-list)))


(defn run-terminal!
  ([root initial-state]
   (let [{:keys [app view-atom]} (f/mount! root)]
     (merge/merge-component! app root initial-state)
     (let [child-ident (comp/ident root initial-state)]
       (comp/transact! app [(f/set-child {:child-ident child-ident})])
       (app/render! app {:force-root? true})

       (lanterna/run-sync #(deref view-atom))
       app)))
  )

(defn -main [ & args]
  (def test-app
    (def app (run-terminal! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))))






