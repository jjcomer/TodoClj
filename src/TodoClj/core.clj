;; TodoClj is my attempt to learn Clojure
;; The idea behind the project is to imitate the functionality of
;; [Todo.txt](http://todotxt.com)

(ns TodoClj.core
  (:use
    [cljcolour
     :only
     [add-foreground-colour
      boldblue
      boldcyan
      boldgreen
      boldpurple
      boldred
      boldyellow]]
    [clojure.contrib.io :only [read-lines write-lines]]
    [clojure.string :only [trim upper-case split]]
    [fs :only [exists?]])
    (:gen-class))

(def file-location "Todo.txt")
(def context-regex #"@\S+")
(def project-regex #"\+\S+")
(def priority-regex #"\([A-Za-z]\)")
(def date-regex #"\d{4}-\d{2}-\d{2}")
(def isNotWindows (not (re-find #"[Ww]indows" (System/getProperty "os.name"))))
(def counter (let [count (ref 0)] #(dosync (alter count inc))))

(defn find-prefixed-words
    "Given the description of a task a set will be built with all prefixed words"
    [regex description]
    (let [matches (set (filter #(not (nil? %)) (map #(re-find regex %) description)))]
        (set (map #(.substring % 1) matches))))

(defn find-priority
    [text]
    (let [match (re-find priority-regex text)]
        (if (nil? match)
            nil
            (upper-case (.substring match 1 2)))))

(defn find-date
    [text]
    (re-find date-regex text))

(defn parse-line
    "This function is to be used in the mapping of the file"
    [raw]
    (let [state (if (= "X" (first raw)) :done :todo)
          priority (find-priority (if (= state :done)
                                    (second raw)
                                    (first raw)))
          description (if (and (= state :done) priority)
                                (rest (rest raw))
                                (if (or (= state :done) priority)
                                    (rest raw)
                                    raw))]
        { :position
            (counter)
          :state
            state
          :priority
            priority
          :contexts
            (find-prefixed-words context-regex description)
          :projects
            (find-prefixed-words project-regex description)
          :description
            (apply str (interpose " " description))}))

(def compare-todos
    (comparator (fn
    [[ num1 { s1 :state p1 :priority d1 :description}]
     [ num2 { s2 :state p2 :priority d2 :description}]]
     (let [tp1 (if p1 p1 "[")
           tp2 (if p2 p2 "[")]
     (if (not (= s1 s2))
        (= s1 :todo)
        (if (not (= tp1 tp2))
            (> 0 (compare tp1 tp2))
            (> 0 (compare d1 d2))))))))

(defn priority-colour
    [priority]
    (let [colours [boldred boldgreen boldyellow boldblue boldpurple boldcyan]]
        (nth colours (mod (int (.charAt priority 0)) (count colours)))))

(defn pretty-print
    "Convert a todo map into a string"
    [{ state :state priority :priority description :description} use-colour]
    (let [state-text (if(= :done state) " X" "")
          priority-text (if priority (str "(" priority ")") "")
          final-text (trim (apply str (interpose " " [state-text priority-text description])))]
        (if (and use-colour priority)
            (add-foreground-colour (priority-colour priority) final-text)
            final-text)))

(defn read-in-file
    "Read in and convert the Todo file into an internal representation"
    [file-location]
    (if (exists? file-location)
        (into [] (map #(parse-line (split % #" ")) (read-lines file-location)))
        []))

(defn write-out-file
    "Write the pretty print version of the Todo Items to a file"
    [todo-items file-location]
    (write-lines file-location (map #(pretty-print % false) todo-items)))

(defn add-action
    "Adds a new Todo task"
    [todos file-location raw]
    (let [new-todo (parse-line (split raw #" "))]
        (println (str "TODO: '" (pretty-print new-todo (and isNotWindows true)) "' added on line " (inc (count todos))))
        (write-out-file (conj todos new-todo) file-location)))

(defn find-matches
    [todo matches todo-key]
        (every? #(not (nil? %)) (map #((todo-key todo) %) matches)))

(defn list-action
    "Lists out todos"
    [todos filters]
    (let [contexts (find-prefixed-words context-regex filters)
          projects (find-prefixed-words project-regex filters)]
          (->> todos
              (filter #(or (= contexts #{}) (find-matches % contexts :contexts)))
              (filter #(or (= projects #{}) (find-matches % projects :projects)))
              (map #(vector (:position %) %))
              (sort compare-todos)
              (map (fn [[tnum todo]] [tnum (pretty-print todo (and isNotWindows true))]))
              (map #(format "%03d:\t%s%n" (first %) (second %)))
              (apply println))))

(defn alter-todo
    [todos todo-num todo-key todo-value]
    (let [altered-todo (assoc (nth todos (dec todo-num)) todo-key todo-value)]
        (->> todos
            (#(assoc % (dec todo-num) altered-todo))
            (#(write-out-file % file-location)))))

(defn do-action
    [todos [todo-num & args]]
    (alter-todo todos (Integer/parseInt todo-num) :state :done))

(defn pri-action
    [todos [todo-num pri & args]]
    (alter-todo todos (Integer/parseInt todo-num) :priority pri))

(defn depri-action
    [todos [todo-num & args]]
    (alter-todo todos (Integer/parseInt todo-num) :priority nil))

(defn rm-action
  "This function deletes a todo from the file"
  [todos [to-remove & args]]
  (let [remove-todo (nth todos (dec (Integer/parseInt to-remove)))]
    (->> todos
      (remove #(= % remove-todo))
      (#(write-out-file % file-location)))))

(defn lsp-action
    "List filtering on priority"
    [todos [pFilter & filters]]
    (->> todos
        (filter #(= (:priority %) pFilter))
        (#(list-action % filters))))

(defn clean-action
    "Remove all completed tasks"
    [todos]
    (->> todos
        (filter #(not (= (:state %) :done)))
        (#(write-out-file % file-location))))

(defn -main [command & args]
    (let [todos (read-in-file file-location)]
        (case (upper-case command)
            "LIST" (list-action todos args)
            "LS" (list-action todos args)
            "ADD" (add-action todos file-location (first args))
            "LSP" (lsp-action todos args)
            "PRI" (pri-action todos args)
            "DEPRI" (depri-action todos args)
            "DO" (do-action todos args)
            "CLEAN" (clean-action todos)
            "RM" (rm-action todos args)
            :else (println "Invalic Command"))))
