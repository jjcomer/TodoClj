(ns TodoClj.colour)

(def black {:fore "0;30" :back "0;40"})
(def red {:fore "0;31" :back "0;41"})
(def green {:fore "0;32" :back "0;42"})
(def yellow {:fore "0;33" :back "0;43"})
(def blue {:fore "0;34" :back "0;44"})
(def purple {:fore "0;35" :back "0;45"})
(def cyan {:fore "0;36" :back "0;46"})
(def lightgrey {:fore "0;37" :back "0;47"})
(def darkgrey {:fore "1;30" :back "1;40"})
(def boldred {:fore "1;31" :back "1;41"})
(def boldgreen {:fore "1;32" :back "1;42"})
(def boldyellow {:fore "1;33" :back "1;43"})
(def boldblue {:fore "1;34" :back "1;44"})
(def boldpurple {:fore "1;35" :back "1;45"})
(def boldcyan {:fore "1;36" :back "1;46"})
(def white {:fore "1;37" :back "1;47"})

(defn add-foreground-colour
	"Add foreground colour to the string"
	[{foreground :fore} text]
	(str "\u001b[" foreground "m" text "\u001b[m"))

(defn add-background-colour
	"Add background colour to the string"
	[{background :back} text]
	(str "\u001b[" background "m" text "\u001b[m"))

(defn add-colour
	"Add foreground and background colours to a string"
	[{foreground :fore} {background :back} text]
	(str "\u001b[" background "m\u001b[" foreground "m" text "\u001b[m\u001b[m"))