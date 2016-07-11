(ns+ human-dynamics.computeable-law
  (:page
    "index.html")
  (:refer-clojure
    :exclude [- name next])
  (:require
    [clojure.string  :refer [capitalize]]
    [javelin.core    :refer [cell= defc defc= with-let]]
    [hoplon.core     :refer [for-tpl when-tpl case-tpl defelem]]
    [hoplon.ui       :refer [elem button image toggle form window *scroll*]]
    [hoplon.ui.elems :refer [in]]
    [hoplon.ui.attrs :refer [- c r s b d]]
    [markdown.core   :refer [md->html]]))

;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def data-url "http://data.computablelaw.org")
(def icon-url "https://fonts.googleapis.com/icon?family=Material+Icons")
(def font-url "https://fonts.googleapis.com/css?family=RobotoDraft:400,500,700,400italic")

;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn prev [coll i] (mod (inc i) (count coll)))
(defn next [coll i] (mod (inc i) (count coll)))

(defn get-state [states path]
  (get-in states (vec (interpose :states path))))

(defn ->time [time]
  (.slice (str "0" (.toLocaleTimeString (js/Date. time) (.-language js/navigator) #js{:hour "2-digit" :minute "2-digit"})) -8 -3))

;;; state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc db {})

;;; service ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(.get js/jQuery (str data-url "/content.edn") #(swap! db merge (cljs.reader/read-string %)))

;;; query ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc= app-name   (-> db :name))
(defc= app-title  (-> db :title))

(defc= route      (-> db :route))
(defc= path       (-> route first))
(defc= base       (-> path first))
(defc= qargs      (-> route second))
(defc= id         (-> qargs :id))

(defc= states     (-> db :states))
(defc= base-state (-> states (get base)))
(defc= state      (-> states (get-state path)))
(defc= view       (-> state :view))
(defc= data       (-> base-state :data))
(defc= datum      (get data id))

(defc= state-name (-> state :name))

(cell= (prn :view view))

;;; command ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn change-route [db route]
  (assoc db :route route))

(defn change-state [db path & qargs]
  (change-route db [path (not-empty (apply hash-map qargs))]))

(defn initiate [db route status _]
  (change-route db (if (empty? route) [[:organizers]] route)))

;;; view styles ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; breakpoints
(def sm 760)
(def md 1240)
(def lg 1480)

; sizes
(def space 42)

(def pad-sm 8)
(def pad-md 16)
(def pad-lg 24)

;; colors
(def black      (c 0x000000))
(def grey       (c 0x9E9E9E))
(def white      (c 0xFFFFFF))
(def grey-300   (c 0xE0E0E0))
(def font-black (c 0x333333))

; fonts
(def helvetica ["Helvetica Neue" "Lucida Grande" :sans-serif])

(defelem markdown [attrs mdstr]
  (with-let [e (elem attrs)]
    (cell= (set! (.-innerHTML (in e)) (md->html (first mdstr))))))

(defelem view-title [attrs elems]
  (elem :sh (r 1 1) :f (b 64 sm 86) :ff helvetica :ft :900
    attrs elems))

(defelem view-subtitle [attrs elems]
  (elem :sh (r 1 1) :pl 4
    attrs elems))

(defelem view-body [attrs mdstr]
  (elem :sh (r 1 1)
     attrs mdstr))

(defelem section-title [attrs elems]
  (elem :sh (r 1 1) :f 38 :ft :500
    attrs elems))

(defelem section-subtitle [attrs elems]
  (elem :sh (r 1 1) :pt 2 :f 18 :fc (c 0 0 0 0.5)
    attrs elems))

(defelem section-body [attrs mdstr]
  (elem :sh (r 1 1) :pt 10 :f 22
     attrs mdstr))

(defelem icon [attrs elems]
  (elem :f 24 :fh 24 :ff "Material Icons" :m :pointer :fr :optimizeLegibility
    attrs elems))

(defelem panel [attrs elems]
  (button :sh (b (r 1 1) sm (- (r 1 1) 156)) :pv 12 :c (s :over (c 0xF5f7f9 0.8)) :m :pointer
    attrs elems))

;;; view components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn basic-view [state]
  (elem :sh (r 1 1) :p space :g space
    (view-title (cell= (:title   state)))
    (view-body  (cell= (:content state)))))

(defn event-view [state]
  (elem :sh (r 1 1) :p space :g space))
    ; (view-title    (cell= (:title state)))))
    ; (view-subtitle (cell= (apply str (interpose " " (:speakers item)))))
    ; (view-body     (cell= (:description item)))))

(defn events-view [state]
  (elem :sh 1200 :p space :g space
    (view-title (cell= (:title state)))
    (for-tpl [[i {:keys [title speakers description start-time stop-time]}] (cell= (map-indexed vector (:data state)))]
      (elem :gh (+ space 24)
        (elem :sh (b (r 1 1) sm 80) :bc (c 0 0 0 0.25) :pt 20 :pb 12
          (elem :sh (b nil sm (r 1 1)) :f 28 :ff helvetica :ft :500 :fc (c 0xFFAF1E)
            (cell= (->time start-time)))
          (elem :sh (b nil sm (r 1 1)) :f 28 :ff helvetica :ft :500 :fc (c 0xFFAF1E)
            (b "  -  " sm ""))
          (elem :sh (b (r 1 2) sm (r 1 1)) :f 28 :ff helvetica :ft :500 :fc (c 0xFFAF1E)
            (cell= (->time stop-time))))
        (panel :click #(swap! db change-state [:events :event] :id @i)
          (section-title
             title)
          (section-subtitle
             (cell= (apply str (interpose " • " speakers))))
          (section-body
             (cell= (when description (subs description 0 180)))))))))

(defn project-view [state]
  (prn :state state)
  (elem :sh (r 1 1) :p space :g space
    (elem :sh (r 1 1)
      (view-title     (cell= (:title datum)))
      (view-subtitle  (cell= (str (:focus datum) " • " (:status datum)))))
    (view-body (cell= (apply str (interpose "\n" (:partners  datum)))))
    (view-body (cell= (:summary  datum)))
    (view-body (cell= (:goals    datum)))
    (view-body (cell= (:outcomes datum)))
    (view-body (cell= (apply str (interpose "\n" (:resources datum)))))))

(defn projects-view [state]
  (elem :sh 1200 :p space :g space
    (view-title (cell= (:title state)))
    (for-tpl [[i {:keys [title focus status]}] (cell= (map-indexed vector (:data state)))]
      (panel :sh (r 1 1) :m :pointer :click #(swap! db change-state [:projects :project] :id @i)
        (section-title    title)
        (section-subtitle focus)))))

(defn photo-view [state]
  (elem :sh (r 1 1) :sv (r 1 1) :p space :g space :ah :center :av :middle
    (icon :f 64 :click #(swap! db change-state [:photos :photo] :id (prev (:data @base-state) @id))
      "keyboard_arrow_left")
    (image :sh (r 4 5) :d (d 0 1 (c 0 0 0 0.37) 4) :click #(swap! db change-state [:photos])
      :url (cell= (when-let [url (:image datum)] (str data-url url))))
    (icon :f 64 :click #(swap! db change-state [:photos :photo] :id (next (:data @base-state) @id))
      "keyboard_arrow_right")))

(defn photos-view [state]
  (elem :sh (r 1 1) :p space :g space :scroll true
    (view-title (cell= (:title state)))
    (for-tpl [[i {:keys [thumb]}] (cell= (map-indexed vector (:data state)))]
      (image :sh (b (r 1 1) sm (r 1 2) md (r 1 3) lg (r 1 4)) :d (d 0 12 (c 0x010811 0.08) 40) :m :pointer :click #(swap! db change-state [:photos :photo] :id @i)
        :url (cell= (when thumb (str data-url thumb)))))))

;;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defc menu-open false)

(defelem sidebar [attrs elems]
  (elem :pv 40 :gv 30 :ah :center :c black :click #(reset! menu-open false) :o 0.3 attrs
    (image :sv 130 :m :pointer :url "mit-ml-logo.jpg" :click #(do (reset! menu-open false) (swap! db change-state [:organizers])))
    (elem :sh (r 1 1) :sv (- (r 1 1) (+ 130 54 30 30)) :gv 46 :bc grey-300
      (for-tpl [[ident {:keys [title]}] states]
        (elem :sh (r 1 1) :ph 40 :pv 4 :av :middle :bl 3 :bcl (cell= (if (= ident base) white black)) :f 18 :ff helvetica :ft :800 :fc white :m :pointer :click #(do (reset! menu-open false) (swap! db change-state [@ident]))
          (elem :pt 2 :fx :lowercase
            title))))
    (elem :sh (r 1 1) :ph 36 :ff helvetica :fc white
      (image :sh 130 :url "creative-commons-logo.png" :m :pointer :click #(.open js/window "https://creativecommons.org/")))))

(defelem main [attrs elems]
  (elem attrs
    (elem :sh (r 1 1) :sv 64 :ph 42 :pv pad-sm :gh pad-lg :av :middle :bb 1 :bc (c 0xEEEEEE) ;:d (d 0 1 (c 0 0 0 0.37) 4)
      (when-tpl (b true lg false)
        (icon :click #(swap! menu-open not) :fc black "menu"))
      (elem :sh (b (- (r 1 1) (+ 24 130 pad-lg pad-lg)) lg (- (r 1 1) (+ 130 24))) :f 22 :ff helvetica :fx :uppercase :ft :500
        (b app-name sm app-title))
      (elem :g pad-sm :ah :right :av :middle
        (image :sh 60 :url "kauffman-logo.png" :m :pointer :click #(.open js/window "http://www.kauffman.org/"))))
    (elem :sh (r 1 1) :sv (- (r 1 1) 64) :scroll true
      elems)))

(defelem overlay [attrs elems]
  (elem :c (c 0 0 0 0.5) :click #(reset! menu-open false)
    attrs elems))

(window
  :title        (cell= (str app-name " - " state-name))
  :route        route
  :styles       [icon-url font-url]
  :initiated    (partial swap! db initiate)
  :routechanged (partial swap! db change-route)
  :ff helvetica :fr :optimizeLegibility :fm :antialiased
  (when-tpl (b menu-open lg true)
    (sidebar :sh 240 :sv (r 1 1) :xl (b menu-open lg false)))
  (main :sh (b (r 1 1) lg (- (r 1 1) 240)) :sv (r 1 1)
    (case-tpl view
      :basic    (basic-view    state)
      :event    (event-view    state)
      :events   (events-view   state)
      :project  (project-view  state)
      :projects (projects-view state)
      :photo    (photo-view    state)
      :photos   (photos-view   state)))
  (overlay :xl 240 :xr 0 :sh (r 1 1) :sv (r 1 1) :v (b menu-open lg false)))
