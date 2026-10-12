(ns bareblog.app.state
  "The state of a page, what a message does to it, and what the page shows for it.

   The state holds what the page was told, the site and its own data or a problem, and what the
   reader chose: the text to search for, a tag, a theme preset, and whether the menu is open."
  (:require [bareblog.address :as address]
            [bareblog.app.view :as view]
            [bareblog.site :as site]))

(defn initial
  "The state of a page that was told nothing yet, with what the reader chose before."
  [{:keys [query tag preset]}]
  {:query (or query "") :tag tag :preset preset :menu? false})

(defn chooses-in-place?
  "True when the page of `state` can show what the reader chose of the list: it is a page of
   the home page."
  [state]
  (= :home (get-in state [:page :page :kind])))

(defn- told [state {:keys [site page problem]}]
  (if problem
    (assoc state :problem problem)
    (-> state (dissoc :problem) (assoc :site site :page page))))

(defn- search [state text]
  (assoc state :query (or text "")))

(defn- pick-tag
  "The state after the tag `tag-name` is picked: it is chosen, or let go when it was chosen."
  [state tag-name]
  (cond
    (nil? tag-name)             state
    (= tag-name (:tag state))   (assoc state :tag nil)
    :else                       (assoc state :tag tag-name)))

(defn- clear [state _]
  (assoc state :query "" :tag nil))

(defn- pick-preset
  "The state after `preset` is picked. A pick of no preset changes nothing."
  [state preset]
  (if (seq preset)
    (assoc state :preset preset)
    state))

(defn- menu
  "The state after the menu is opened or closed."
  [state open?]
  (assoc state :menu? (boolean open?)))

(defn- open-menu [state _]
  (menu state true))

(defn- close-menu [state _]
  (menu state false))

(def ^:private steps
  "What each meaning of a message does to the state."
  {:told       told
   :search     search
   :tag        pick-tag
   :clear      clear
   :preset     pick-preset
   :menu       menu
   :open-menu  open-menu
   :close-menu close-menu})

(defn step
  "The state after `message`, which is a meaning and an optional argument. A message that
   changes no state leaves it as it is."
  [state [meaning argument]]
  (if-some [change (steps meaning)]
    (change state argument)
    state))

(defn picks-in-place?
  "True when a click with the facts in `click` picks a tag in place on the page of `state`, and
   does not follow its link: the page can show the choice, and the click is a plain one, with
   the main button and no key held that opens a link elsewhere."
  [state {:keys [button modified?]}]
  (and (chooses-in-place? state) (zero? button) (not modified?)))

(defn- to-search
  "The address of the home page with what `state` searches for."
  [state]
  (str (address/home 1) (address/query {:q (:query state)})))

(defn- to-page [_state number]
  (address/home number))

(defn- to-submitted-search
  "Where a search that is sent leads: to the home page, from a page that cannot show it."
  [state _]
  (when-not (chooses-in-place? state)
    (to-search state)))

(def ^:private destinations
  "Where each meaning of a message leads, from the state and the argument of the message."
  {:to-page       to-page
   :submit-search to-submitted-search})

(defn destination
  "The address that `message` takes the reader to, or nil when it takes them nowhere."
  [state [meaning argument]]
  (when-some [leads-to (destinations meaning)]
    (leads-to state argument)))

(defn- asked
  "What the address asks for of the list, when the page is one that can show it."
  [{:keys [query tag] :as state}]
  (when (chooses-in-place? state)
    {:q query :tag (or tag "")}))

(defn shown
  "What the page shows for `state`, as a value: a notice or nil, the view of the document or
   nil when there is nothing to show yet, what the address asks for by name or nil when the
   address is left alone, and the preset to keep."
  [{:keys [site page problem query tag preset menu?] :as state}]
  {:notice (or problem (site/notice site))
   :view   (when (and site page (not problem))
             (view/page site page {:query query :tag tag :preset preset :menu? menu?}))
   :asked  (asked state)
   :preset preset})
