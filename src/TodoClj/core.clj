;; Josh Comer - 2011

(ns TodoClj.core
	(:require [clojure.contrib [duck-streams :as io]]))

(def file-location "Todo.txt")

(defn parse-line
	"This function is to be used in the mapping of the file"
	[[type & raw-desc]]
	(let [description (apply str (interpose " " raw-desc))]
		(if (= "T" type)
			[:Todo description]
			[:Done description])))

(defn read-in-file
	"Read in and convert the Todo file into an internal representation"
	[file-location]
	(map #(parse-line (read-line (str "[" % "]"))) (io/read-lines file-location)))