(ns bareblog.app.state
  "The state of a page, what a message does to it, and what the page shows for it.

   The state holds where the top of the blog is, as `:root`, what the page was told, the
   `:site` and its own `:page` or a `:problem`, and
   under `:local` what is the page's own: what the reader chose, which is the text to search
   for, a tag, a date and a theme preset, whether the menu and the palette are open, how far
   the reader has scrolled through the page, what the page knows of its keyboard, and for
   comments, the comment the form answers, the reader's name, and what a send said.

   A message goes through `step`, and `shown` says what the page then shows: a notice, the view
   of the document, what the address asks for, and the preset to keep. An event, and what the
   socket says, are both messages."
  (:require [bareblog.app.view :as view]
            [bareblog.blog.address :as address]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [clojure.string :as str]))

(defn initial
  "The state of a page that was told nothing yet, with where the top of its blog is, what the
   reader chose before, the name they last commented with, and what the page knows of its
   keyboard."
  [{:keys [root query tag date preset command-key? commenter]}]
  {:root  (or root address/top)
   :local {:query        (or query "")
           :tag          tag
           :date         (not-empty date)
           :preset       preset
           :menu?        false
           :palette?     false
           :command-key? (boolean command-key?)
           :commenter    (or commenter "")}})

(defn blog-shape
  "The shape of the blog that the page of `state` was told of."
  [state]
  (shape/of-blog (get-in state [:site :blog])))

(defn- kind
  "The kind of the page of `state`, or nil when it was told nothing yet."
  [state]
  (get-in state [:page :page :kind]))

(defn shows-choice?
  "True when the page of `state` shows what the reader chose of the posts, where the blog
   template says that a choice takes effect."
  [state]
  (shape/chooses-on? (blog-shape state) (kind state)))

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

;; A date is a day, as `2026-10-12`, or a month, as `2026-10`. The posts of a date are those
;; whose date starts with it.
(defn- pick-date [local date]
  (assoc local :date (not-empty date)))

(defn- clear [local _]
  (assoc local :query "" :tag nil :date nil))

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

;; In thousandths, the line moves within a few pixels of scrolling on a long post, and the
;; page is shown again at most a thousand times on the way down.
(defn read-share
  "How far a window that has scrolled `scrolled` pixels has come through a page that it can
   scroll `room` pixels, from 0 to 1, or nil when there is no room to scroll."
  [scrolled room]
  (when (pos? room)
    (-> (/ (double scrolled) room) (max 0.0) (min 1.0) (* 1000) Math/round (/ 1000.0))))

(defn- read-to [local share]
  (assoc local :read share))

;; The form of comments answers one comment, or none. A send says that it is on its way, and
;; then once what came of it: a reply to a comment takes the place of what it said. The form
;; moves back under the thread after a send, and that leaves what it said.
(defn- reply-to
  "What is local after the form is asked to answer the comment `id`, or none when it is empty."
  [local id]
  (cond-> (assoc local :reply-to (not-empty id))
    (seq id) (assoc :sent nil)))

(defn- comment-sending
  "What is local while a comment under the name `commenter` is on its way to the script."
  [local commenter]
  (assoc local :sent {:sending? true} :commenter commenter))

(defn- comment-sent
  "What is local after a comment was sent, with its `outcome`."
  [local outcome]
  (assoc local :sent outcome))

(def ^:private refusals
  "Each refusal of the script by the start of its error, as what a reader is told of it."
  {"comments are closed" :closed
   "a name of"           :name
   "a comment of"        :text})

(defn- refused-as
  "The kind of the refusal that starts with `start`, when the error `error` is that refusal."
  [error [start kind]]
  (when (str/starts-with? (str error) start) kind))

(defn- refusal-of
  "What a reader is told of the error `error`: one of the `refusals` of the script, or else
   `:failed`, for anything that went wrong on the way or at Google."
  [error]
  (or (some (partial refused-as error) refusals) :failed))

(defn outcome
  "What came of sending the comment `text`, from `answer`, what the script answered: that it
   was received, or why it was not, with the text, to send again."
  [text {:keys [ok error]}]
  (if ok
    {:received? true}
    {:error (refusal-of error) :text text}))

(def ^:private local-steps
  "What each meaning of a message does to what is local to the page."
  {:search          search
   :tag             pick-tag
   :tag-link        pick-tag
   :day             pick-date
   :month           pick-date
   :clear           clear
   :preset          pick-preset
   :menu            menu
   :open-menu       open-menu
   :close-menu      close-menu
   :open-palette    open-palette
   :close-palette   close-palette
   :read            read-to
   :reply-to        reply-to
   :comment-sending comment-sending
   :comment-sent    comment-sent})

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
  (and (= :home (kind state)) (= :entries (:home (blog-shape state)))))

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
  [{{:keys [query tag date]} :local :as state}]
  (when (shows-choice? state)
    {:q query :tag (or tag "") :date (or date "")}))

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
