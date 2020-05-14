(ns ui.entry
  (:require [allpa.core
             :refer [defprotomethod deftagged]]
            [mayu.macros :refer [defui ui]]
            [mayu.frp.event :as e]
            [mayu.frp.signal :as s]
            [mayu.dom :as dom]
            [router :as r]))

(deftagged Todos [todos editing-id])
(deftagged Todo [id text completed?])

(deftagged AddTodo [text])
(deftagged ToggleTodo [id])
(deftagged SetAll [])
(deftagged DeleteTodo [id])
(deftagged DeleteCompleted [])
(deftagged BeginEdit [id])
(deftagged CommitEdit [text])
(deftagged CancelEdit [])

(defn get-next-id [{:keys [todos]}]
  (->> todos (apply max-key :id) :id inc))

(defprotomethod reduce-action [state ^this {:keys [text id]}]
  AddTodo
  (update state :todos #(conj % (->Todo (get-next-id state) text false)))

  ToggleTodo
  (let [{:keys [todos]} state]
    (->> todos
         (map #(if (= id (:id %1))
                 (update %1 :completed? not)
                 %1))
         vec
         (assoc state :todos)))

  SetAll
  (let [{:keys [todos]} state
        all-completed? (every? :completed? todos)]
    (->> todos
         (map #(assoc % :completed? (not all-completed?)))
         vec
         (assoc state :todos)))

  DeleteTodo
  (let [{:keys [todos]} state]
    (->> todos
         (remove #(= id (:id %)))
         vec
         (assoc state :editing-id nil :todos)))

  DeleteCompleted
  (let [{:keys [todos]} state]
    (->> todos (remove :completed?) vec (assoc state :todos)))

  BeginEdit
  (assoc state :editing-id id)

  CommitEdit
  (let [{:keys [todos editing-id]} state]
    (->> todos
         (map #(if (= editing-id (:id %1))
                 (assoc %1 :text text)
                 %1))
         vec
         (assoc state :editing-id nil :todos)))

  CancelEdit
  (assoc state :editing-id nil))

(defui todo-view [{:keys [id text completed?]}]
  editing-id <- (dom/envs :editing-id)
  let [editing? (= id editing-id)]
  <[li {:class {:editing editing?
                :completed completed?}} $=
    <[if editing?
      <[then
        <[dom/collect-reduce-and-bind :input #(-> %2) text $[value]=
          <[input {:autofocus true
                   :class "edit"
                   :value value} $=
            e-mount <- dom/get-mount-event
            (dom/consume! e-mount
                          #(do (.focus %1)
                               (.setSelectionRange %1
                                                   (count value)
                                                   (count value))))
            ] d-input >
          let [e-enter (->> (dom/on-key-down d-input)
                            (e/filter #(= 13 (.-keyCode %))))
               e-blur (dom/on-blur d-input)]
          (->> (e/join e-blur e-enter)
               (e/map #(.trim value))
               (e/map #(if (empty? %1)
                         (->DeleteTodo id)
                         (->CommitEdit %1)))
               (dom/emit ::todos))
          (->> (dom/on-key-down d-input)
               (e/filter #(= 27 (.-keyCode %)))
               (e/map #(->CancelEdit))
               (dom/emit ::todos))
          (->> (dom/on-input d-input)
               (e/map #(.. % -target -value))
               (dom/emit :input))]]
      <[else
        <[div {:class "view"} $=
          <[input {:class "toggle"
                   :type "checkbox"
                   :checked completed?}
            ] d-toggle >
          (->> (dom/on-input d-toggle)
               (e/map #(->ToggleTodo id))
               (dom/emit ::todos))
          <[label text] d-label >
          (->> (dom/on-dblclick {:capture false} d-label)
               (e/map #(->BeginEdit id))
               (dom/emit ::todos))
          <[button {:class "destroy"}
            ] d-destroy >
          (->> (dom/on-click d-destroy)
               (e/map #(->DeleteTodo id))
               (dom/emit ::todos))]]]])

(defui todos-view []
  route <- (dom/envs :route)
  s-todos <- (dom/envs ::todos)
  <[dom/bind s-todos $[{:keys [todos editing-id]}]=
    <[dom/assoc-env :editing-id editing-id $=
      <[when-not (empty? todos)
        let [all-completed? (every? :completed? todos)]
        <[div $=
          <[section {:class "main"} $=
            <[input {:class "toggle-all"
                     :id "toggle-all"
                     :checked all-completed?
                     :type "checkbox"}
              ] d-toggle-all >
            (->> (dom/on-input d-toggle-all)
                 (e/map #(->SetAll))
                 (dom/emit ::todos))
            <[label {:for "toggle-all"} "Mark as complete"]
            <[ul {:class "todo-list"} $=
              let [viewed-todos (case route
                                  ::r/all todos
                                  ::r/active (remove :completed? todos)
                                  ::r/completed (filter :completed? todos))]
              <[for viewed-todos $[{:keys [id] :as todo}]=
                <[keyed id
                  <[todo-view todo]]]]]
          <[footer {:class "footer"} $=
            <[span {:class "todo-count"} $=
              let [active-count (->> todos
                                     (remove :completed?)
                                     count)]
              <[strong active-count]
              <[dom/text (str " item"
                              (if (= 1 active-count)
                                ""
                                "s")
                              " left")]]
            <[ul {:class "filters"} $=
              <[li $= <[a {:class {:selected (= route ::r/all)}
                           :href "/#/"} "All"]]
              <[li $= <[a {:class {:selected (= route ::r/active)}
                           :href "/#/active"} "Active"]]
              <[li $= <[a {:class {:selected (= route ::r/completed)}
                           :href "/#/completed"} "Completed"]]]
            <[when-not (->> (filter :completed? todos)
                            empty?)
              <[button {:class "clear-completed"}
                "Clear Completed"
                ] d-clear >
              (->> (dom/on-click d-clear)
                   (e/map #(->DeleteCompleted))
                   (dom/emit ::todos))]]]]]])

(defui root []
  s-route <- (dom/envs ::r/s-route)
  <[dom/bind s-route $[route]=

    <[dom/assoc-env :route (-> route :data :name) $=
      <[section {:class "todoapp"} $=
        get-stored-todos <- (dom/envs :get-stored-todos)
        set-stored-todos <- (dom/envs :set-stored-todos)
        let [init-todos (or (get-stored-todos) [])
             init-state (->Todos init-todos nil)]
        <[dom/collect-and-reduce ::todos reduce-action init-state $=
          s-todos <- (dom/envs ::todos)
          <[dom/bind s-todos $[{:keys [todos]}]=
            [(set-stored-todos todos)]]
          <[header {:class "header"} $=
            <[h1 "todos"]
            <[dom/collect-reduce-and-bind ::input #(-> %2) "" $[value]=
              <[input {:class "new-todo"
                       :placeholder "What needs to be done?"
                       :autofocus true
                       :value value}
                ] d-input >
              (->> (dom/on-input d-input)
                   (e/map #(.. % -target -value))
                   (dom/emit ::input))
              let [e-add (->> (dom/on-key-down d-input)
                              (e/filter #(= 13 (.-keyCode %)))
                              (e/map #(.trim value))
                              (e/remove empty?)
                              (e/map #(->AddTodo %)))]
              (dom/emit ::input (e/map #(-> nil) e-add))
              (dom/emit ::todos e-add)]]
          <[todos-view]]]]]
  <[footer {:class "info"} $=
    <[p "Double-click to edit a todo"]
    <[p "Written by Mitch Dzugan"]
    <[p $=
      <[dom/text "Based on template at "]
      <[a {:href "https://github.com/tastejs/todomvc-app-template"}
        "tastejs/todomvc-app-template"]]])
