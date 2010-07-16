(ns poclj.core
  (:require [clojure.contrib.str-utils2 :as str-utils2]
            [clojure.contrib.str-utils :as str-utils]
            [name.choi.joshua.fnparse :as fnparse]
            [clojure.contrib.error-kit :as error-kit]))

(defstruct node-s :kind :content)
(defstruct state-s :remainder :column :line)

(def remainder-a
     (accessor state-s :remainder))

(def make-node
     (partial struct node-s))

(def make-scalar-node
     (partial make-node :scalar))

(def make-msgid-node
     (partial make-node :msgid))

(def make-msgstr-node
     (partial make-node :msgstr))

(def make-comment-node
     (partial make-node :comment))

(def make-reference-node
     (partial make-node :reference))

(defn- nb-char [subrule]
  (fnparse/invisi-conc subrule (fnparse/update-info :column inc)))

(def nb-char-lit
     (comp nb-char fnparse/lit))

(defn- b-char [subrule]
  (fnparse/invisi-conc subrule (fnparse/update-info :line inc)))

(def apply-str
     (partial apply str))

; Error handling
(error-kit/deferror parse-error [] [state message message-args]
  {:msg (str (format "POFile error at line %s, column %s: "
                     (:line state)
                     (:column state))
             (apply format message message-args))
   :unhandled (error-kit/throw-msg Exception)})

(defn- expectation-error-fn [expectation]
  (fn [remainder state]
    (error-kit/raise parse-error state "%s expected where \"%s\" is"
                     [expectation (or (first remainder) "the end of the file")])))

; Rules
(def string-delimiter
     (nb-char-lit \"))

(def escape-indicator
     (nb-char-lit \\))

(def space (nb-char-lit \space))

(def newline-lit (fnparse/lit \newline))
(def return-lit (fnparse/lit \return))

(def line-break (b-char (fnparse/rep+ (fnparse/alt newline-lit return-lit))))

(def whitespace
     (fnparse/rep+ (fnparse/alt space newline-lit return-lit)))

(def pofile-char
     fnparse/anything)

(def unescaped-char
     (fnparse/except pofile-char (fnparse/alt escape-indicator string-delimiter)))

(def escaped-characters
     {\\ \\, \/ \/, \b \backspace, \f \formfeed, \n \newline, \r \return, \t \tab})

(def normal-escape-sequence
     (fnparse/semantics (fnparse/lit-alt-seq (keys escaped-characters) nb-char-lit)
                        escaped-characters))

(def char-sequence
     (fnparse/except string-delimiter (fnparse/rep+ pofile-char)))

(def escape-sequence
     (fnparse/complex [_ escape-indicator
                       fnparse/character (fnparse/alt char-sequence
                                                      normal-escape-sequence)]
                      fnparse/character))

(def string-char
     (fnparse/alt escape-sequence unescaped-char))

(def limited-string-char
     (fnparse/lit-alt-seq [\a \t]))

(def string-lit
     (fnparse/complex [_ string-delimiter
                       contents (fnparse/rep* string-char)
                       _ string-delimiter]
                      (-> contents apply-str)))

(def msgid-lit
     (fnparse/constant-semantics (fnparse/lit-conc-seq "msgid" nb-char-lit)
                                 (make-scalar-node "msgid")))

(def msgid-entry
     (fnparse/complex [_ msgid-lit
                       _ (fnparse/rep+ space)
                       contents string-lit
                       _ line-break]
                      (-> contents apply-str make-msgid-node)))

(def msgstr-lit
     (fnparse/constant-semantics (fnparse/lit-conc-seq "msgstr" nb-char-lit)
                                 (make-scalar-node "msgstr")))

(def msgstr-entry
     (fnparse/complex [_ msgstr-lit
                       _ (fnparse/rep+ space)
                       contents string-lit
                       _ (fnparse/rep* line-break)]
                      (-> contents apply-str make-msgstr-node)))

(def comment-lit
     (fnparse/constant-semantics (fnparse/lit-conc-seq "#." nb-char-lit)
                                 (make-scalar-node "#.")))

(def comment-content
     (fnparse/rep+ (fnparse/except pofile-char line-break)))

(def comment-entry
     (fnparse/complex [_ comment-lit
                       _ space
                       contents comment-content
                       _ line-break]
                      (-> contents apply-str make-comment-node)))

(def reference-lit
     (fnparse/constant-semantics (fnparse/lit-conc-seq "#:" nb-char-lit)
                                 (make-scalar-node "#:")))

(def reference-entry
     (fnparse/complex [_ reference-lit
                       _ space
                       contents comment-content
                       _ line-break]
                      (-> contents apply-str make-reference-node)))

(def poentry
     (fnparse/conc comment-entry
                   reference-entry
                   msgid-entry
                   msgstr-entry))

(def pofile
     poentry)

(defn parse [tokens]
  (binding [fnparse/*remainder-accessor* remainder-a]
    (fnparse/rule-match poentry
                        #(error-kit/raise parse-error "invalid document \"%s\""
                                (apply-str (remainder-a %)))
                        #(error-kit/raise parse-error "leftover data after a valid node \"%s\""
                                (apply-str (remainder-a %2)))
                        (struct state-s tokens 0 0))))
