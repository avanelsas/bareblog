(ns bareblog.app.state
  "The state of a page, what a message does to it, and what the page shows for it.

   The state holds where the top of the blog is, as `:root`, what the page was told, the
   `:site` and its own `:page` or a `:problem`, and
   under `:local` what is the page's own: what the reader chose, which is the text to search
   for, a tag and a theme preset, whether the menu and the palette are open, and what the page
   knows of its keyboard.

   A message goes through `step`, and `shown` says what the page then shows: a notice, the view
   of the document, what the address asks for, and the preset to keep. An event, and what the
   socket says, are both messages."
  (:require [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.site :as site]))

(defn initial
  "The state of a page that was told nothing yet, with where the top of its blog is, what the
   reader chose before, and what the page knows of its keyboard."
  [{:keys [root query tag preset command-key?]}]
  {:root  (or root address/top)
   :local {:query        (or query "")
           :tag          tag
           :preset       preset
           :menu?        false
           :palette?     false
           :command-key? (boolean command-key?)}})

(defn template
  "The blog template of the blog that the page of `state` was told of."
  [state]
  (templates/of-blog (get-in state [:site :blog])))

(defn- kind
  "The kind of the page of `state`, or nil when it was told nothing yet."
  [state]
  (get-in state [:page :page :kind]))

(defn shows-choice?
  "True when the page of `state` shows what the reader chose of the posts, where the blog
   template says that a choice takes effect."
  [state]
  (templates/chooses-on? (template state) (kind state)))

(defn- told [state {:keys [site page problem]}]
  (if problem
    (assoc state :problem problem)
    (-> state (dissoc :problem) (assoc :site site :page page))))

;; Every other step changes what is local to the page, and is given that.

(defn- search [local text]
  (assoc local :query (or text "")))

(defn- pick-tag
  "What is local after the tag `tag-name` is picked: it is chosen, or let go when it was."
  [local tag-name]
  (cond
    (nil? tag-name)           local
    (= tag-name (:tag local)) (assoc local :tag nil)
    :else                     (assoc local :tag tag-name)))

(defn- clear [local _]
  (assoc local :query "" :tag nil))

(defn- pick-preset
  "What is local after `preset` is picked. A pick of no preset changes nothing."
  [local preset]
  (if (seq preset)
    (assoc local :preset preset)
    local))

(defn- menu
  "What is local after the menu is opened or closed."
  [local open?]
  (assoc local :menu? (boolean open?)))

(defn- open-menu [local _]
  (menu local true))

(defn- close-menu [local _]
  (menu local false))

(defn- open-palette [local _]
  (assoc local :palette? true))

(defn- close-palette [local _]
  (assoc local :palette? false))

(def ^:private local-steps
  "What each meaning of a message does to what is local to the page."
  {:search        search
   :tag           pick-tag
   :tag-link      pick-tag
   :clear         clear
   :preset        pick-preset
   :menu          menu
   :open-menu     open-menu
   :close-menu    close-menu
   :open-palette  open-palette
   :close-palette close-palette})

(defn step
  "The state after `message`, which is a meaning and an optional argument. A message that
   changes no state leaves it as it is."
  [state [meaning argument]]
  (cond
    (= :told meaning)     (told state argument)
    (local-steps meaning) (update state :local (local-steps meaning) argument)
    :else                 state))

(defn- lists-entries?
  "True when the page of `state` is a home page that lists entries, with the tags to choose
   from above them."
  [state]
  (and (= :home (kind state)) (= :entries (:home (template state)))))

;; A link to a tag leads to the page of the tag. Where the entries of the home page stand
;; under the tags to choose from, a click on the tag of an entry does what a click on one of
;; those does. Anywhere else the link is followed, so that the page of a tag can be reached.
(defn link-picks?
  "True when a click with the facts in `click` on a link to a tag picks the tag on the page of
   `state`, and does not follow the link: the page lists entries to choose from, and the click
   is a plain one, with the main button and no key held that opens a link elsewhere."
  [state {:keys [button modified?]}]
  (and (lists-entries? state) (zero? button) (not modified?)))

(defn- to-page [state number]
  (address/home (:root state) number))

(defn- to-submitted-search
  "Where a search that is sent leads: to the home page with the search, from a page that
   cannot show it."
  [state _]
  (when-not (shows-choice? state)
    (str (address/home (:root state) 1) (address/query {:q (get-in state [:local :query])}))))

(defn- to-post
  "Where picking a post leads: to the address that the pick names."
  [_state address]
  address)

(def ^:private destinations
  "Where each meaning of a message leads, from the state and the argument of the message."
  {:to-page       to-page
   :submit-search to-submitted-search
   :open-post     to-post})

(defn destination
  "The address that `message` takes the reader to, or nil when it takes them nowhere."
  [state [meaning argument]]
  (when-some [leads-to (destinations meaning)]
    (leads-to state argument)))

(defn- asked
  "What the address asks for of the list, when the page is one that can show it."
  [{{:keys [query tag]} :local :as state}]
  (when (shows-choice? state)
    {:q query :tag (or tag "")}))

(defn shown
  "What the page shows for `state`, as a value: a notice or nil, the view of the document or
   nil when there is nothing to show yet, what the address asks for by name or nil when the
   address is left alone, and the preset to keep."
  [{:keys [root site page problem local] :as state}]
  {:notice (or problem (site/notice site))
   :view   (when (and site page (not problem))
             (view/page root site page local))
   :asked  (asked state)
   :preset (:preset local)})
