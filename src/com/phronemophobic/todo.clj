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
             [membrane.fulcro :as mf
              :refer [uuid
                      component->view
                      show!
                      show-sync!]]
             [membrane.skia :as skia])
  (:gen-class))


(mf/fulcroize-membrane-component basic/button "button" Button)

(mf/fulcroize-membrane-component basic/textarea-view "textarea" TextArea)


(mf/fulcroize-membrane-component basic/checkbox "checkbox" Checkbox)


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

(defn delete-X []
  (ui/with-style :membrane.ui/style-stroke
    (ui/with-color
      [1 0 0]
      (ui/with-stroke-width
        3
        [(ui/path [0 0]
                  [10 10])
         (ui/path [10 0]
                  [0 10])]))))

(declare TodoItem)
(defmutation add-todo
  "docstring"
  [{id :todo-list/id
    todo-description :todo/description}]
  (action [{:keys [app state] :as env}]
          (let [tid (uuid)]
            (merge/merge-component! app TodoItem (comp/get-initial-state TodoItem {:todo/id tid
                                                                                   :todo/description todo-description}))
            (swap! state update-in [:todo-list/id id :todo-list/todo-list] conj [:todo/id tid]))
          nil))

(defmutation delete-todo
  "docstring"
  [m]
  (action [{:keys [app state] :as env}]
          (let [tid (uuid)]
            ;; (prn (get-in @state[:checkbox/id :checkbox/checked?]) )
            (swap! state merge/remove-ident*
                   [:todo/id (:todo/id m)]
                   [:todo-list/id (:todo-list/id m) :todo-list/todo-list]))
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
   (ui/on
    :mouse-down
    (fn [_]
      [(delete-todo {:todo/id id})])
    (delete-X))
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
   (map (fn [item]
          (ui/on
           `delete-todo
           (fn [m]
             [(delete-todo (assoc m :todo-list/id id))])
           (ui-todo-item item)))
        todo-list)))

(comment
  (def app (show! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))
  ,)

(defn -main [ & args]
  (def app (show-sync! TodoList (comp/get-initial-state TodoList {:todo-list/id (uuid)})))
  )






