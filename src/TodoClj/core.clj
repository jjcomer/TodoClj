;; Josh Comer - 2011

(ns TodoClj.core
	(:require [clojure.contrib [duck-streams :as io] [seq-utils :as seq]] [clojure.string :as string] [fs] [cljcolour :as colour])
	(:gen-class))

(def file-location "Todo.txt")
(def context-regex #"@\S+")
(def project-regex #"\+\S+")
(def priority-regex #"\([A-Za-z]\)")
(def date-regex #"\d{4}-\d{2}-\d{2}")

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
			(string/upper-case (.substring match 1 2)))))

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
		{ :state
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
	(let [colours [colour/boldred colour/boldgreen colour/boldyellow colour/boldblue colour/boldpurple colour/boldcyan]]
		(nth colours (mod (int (.charAt priority 0)) (count colours)))))

(defn pretty-print
	"Convert a todo map into a string"
	[{ state :state priority :priority description :description} use-colour]
	(let [state-text (if(= :done state) " X" "")
		  priority-text (if priority (str "(" priority ")") "")
		  final-text (string/trim (apply str (interpose " " [state-text priority-text description])))]
		(if (and use-colour priority)
			(colour/add-foreground-colour (priority-colour priority) final-text)
			final-text)))

(defn read-in-file
	"Read in and convert the Todo file into an internal representation"
	[file-location]
	(if (fs/exists? file-location)
		;index will be added only when necessary
		(into [] (map #(parse-line (string/split % #" ")) (io/read-lines file-location)))
		[]))

(defn write-out-file
	"Write the pretty print version of the Todo Items to a file"
	[todo-items file-location]
	(io/write-lines file-location (map #(pretty-print % false) todo-items)))

(defn add-action
	"Adds a new Todo task"
	[todos file-location raw]
	(let [new-todo (parse-line (string/split raw #" "))]
		(println (str "TODO: '" (pretty-print new-todo true) "' added on line " (inc (count todos))))
		(write-out-file (conj todos new-todo) file-location)))

(defn list-action
	"Lists out todos"
	[todos filters]
	(let [contexts (find-prefixed-words context-regex filters)
		  projects (find-prefixed-words project-regex filters)]
		  (->> todos
			  (filter #(or (= contexts #{}) (seq/find-first (:contexts %) contexts)))
			  (filter #(or (= projects #{}) (seq/find-first (:projects %) projects)))
			  (seq/indexed)
			  (sort compare-todos)
			  (map (fn [[tnum todo]] [tnum (pretty-print todo true)]))
			  (map #(format "%03d:\t%s%n" (inc (first %)) (second %)))
			  (apply print))))

(defn -main [command & args]
	(let [todos (read-in-file file-location)]
		(case (string/lower-case command)
			"list" (print args)
			"ls" (list-action todos args)
			"add" (add-action todos file-location (first args))
			"lsp" []
			"pri" []
			"depri" []
			"do" []
			"clean" [])))
