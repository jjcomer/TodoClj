;; Josh Comer - 2011

(ns TodoClj.core
	(:require [clojure.contrib [duck-streams :as io]] [clojure.string :as string] [fs] [TodoClj.colour :as colour])
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
		(map #(.substring % 1) matches)))

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
	[[type & raw-desc]]
	(let [description (apply str (interpose " " raw-desc))]
		{ :state
			(if (= "X" type)
				:done
				:todo)
		  :priority
		  	(find-priority type)
		  :date
		  	(find-date (first raw-desc))
		  :contexts
		  	(find-prefixed-words context-regex raw-desc)
		  :projects
		  	(find-prefixed-words project-regex raw-desc)
		  :description
		    description}))

(defn pretty-print
	"Convert a todo map into a string"
	[{ state :state priority :priority date :date description :description}]
	(let [state-text (if(= :done state) "X" "")
		  priority-text (if (not (nil? priority)) (str "(" priority ")") "")]
		(apply str (interpose " " [state-text priority-text date description]))))

(defn read-in-file
	"Read in and convert the Todo file into an internal representation"
	[file-location]
	(if (fs/exists? file-location)
		(map #(parse-line (string/split % #" ")) (io/read-lines file-location))
		[]))

(defn write-out-file
	"Write the pretty print version of the Todo Items to a file"
	[todo-items file-location]
	(io/write-lines file-location (map #(string/trim (pretty-print %)) todo-items)))

(defn -main [command & args]
	(case (string/lower-case command)
		"list" []
		"ls" (println (colour/add-colour colour/boldred colour/blue "Hello World"))
		"create" []
		"update" []))

