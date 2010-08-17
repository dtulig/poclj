(ns poclj.core
  (:require [clojure.contrib.str-utils2 :as str-utils2]
            [clojure.contrib.str-utils :as str-utils]
            [name.choi.joshua.fnparse :as fnparse]
            [clojure.contrib.error-kit :as error-kit]
            [clojure.contrib.seq-utils :as seq-utils]))

(defstruct state-s :remainder :column :line)

(def remainder-a
     (accessor state-s :remainder))

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
     (fnparse/rep+ (fnparse/except pofile-char
                                   string-delimiter)))

(def escape-sequence
     (fnparse/complex [_ escape-indicator
                       character (fnparse/alt pofile-char
                                              normal-escape-sequence)]
                      character))

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
     (fnparse/lit-conc-seq "msgid" nb-char-lit))

(def msg-entry
     (fnparse/rep+ (fnparse/alt string-lit line-break)))

(def msgid-entry
     (fnparse/complex [_ msgid-lit
                       _ space
                       contents msg-entry]
                      (-> (filter string? contents) apply-str)))

(def msgstr-lit
     (fnparse/lit-conc-seq "msgstr" nb-char-lit))

(def msgstr-entry
     (fnparse/complex [_ msgstr-lit
                       _ space
                       contents msg-entry]
                      (-> (filter string? contents) apply-str)))

(def meta-content
     (fnparse/rep+ (fnparse/except pofile-char line-break)))

(def comment-lit
     (fnparse/lit-conc-seq "#." nb-char-lit))

(def comment-entry
     (fnparse/rep* (fnparse/complex [_ comment-lit
                                     _ space
                                     contents meta-content
                                     _ line-break]
                                    (apply-str contents))))

(def reference-lit
     (fnparse/lit-conc-seq "#:" nb-char-lit))

(def reference-entry
     (fnparse/rep* (fnparse/complex [_ reference-lit
                                     _ space
                                     contents meta-content
                                     _ line-break]
                                    (apply-str contents))))

(def tcomment-lit
     (fnparse/lit-conc-seq "#" nb-char-lit))

(def tcomment-entry
     (fnparse/rep* (fnparse/complex [_ tcomment-lit
                                     _ space
                                     contents meta-content
                                     _ line-break]
                                    (apply-str contents))))

(def flag-lit
     (fnparse/lit-conc-seq "#," nb-char-lit))

(def flag-entry
     (fnparse/rep*
      (fnparse/complex [_ flag-lit
                        _ space
                        contents meta-content
                        _ line-break]
                       (apply-str contents))))

(def header-msgid-entry
     (fnparse/conc msgid-lit
                   space
                   (fnparse/rep= 2 string-delimiter)
                   line-break))

(def header-msgstr-entry
     (fnparse/conc msgstr-lit
                   space
                   (fnparse/rep= 2 string-delimiter)
                   line-break))

(def header-item
     (fnparse/complex [_ string-delimiter
                       key (fnparse/rep+ (fnparse/except fnparse/anything
                                                         (nb-char-lit \:)))
                       _ (nb-char-lit \:)
                       value (fnparse/rep+ (fnparse/except fnparse/anything
                                                           string-delimiter))
                       _ string-delimiter
                       _ (fnparse/rep* line-break)]
                      [(apply-str key) (str-utils2/trim (apply-str value))]))

(def header-entry
     (fnparse/conc header-msgid-entry
                   header-msgstr-entry
                   (fnparse/rep* header-item)))

(def poentry
     (fnparse/conc tcomment-entry
                   comment-entry
                   reference-entry
                   flag-entry
                   msgid-entry
                   msgstr-entry
                   (fnparse/rep* line-break)))

(def pofile
     (fnparse/conc header-entry
                   (fnparse/rep* line-break)
                   (fnparse/rep* poentry)))

(defn parse-pofile [tokens]
  (binding [fnparse/*remainder-accessor* remainder-a]
    (fnparse/rule-match pofile
                        #(error-kit/raise parse-error
                                          %
                                          "%s"
                                          (remainder-a %))
                        #(error-kit/raise parse-error "leftover data after a valid node \"%s\""
                                (apply-str (remainder-a %2)))
                        (struct state-s tokens 0 0))))

(defn join-lines [line]
  (apply str (interpose " " line)))

; gettext entry item
(defstruct node-s
  :tcomment
  :comment
  :occurences
  :flags
  :msgid
  :msgstr)

(defn parse [tokens]
  (map #(struct node-s
                (join-lines (first %))
                (join-lines (nth % 1))
                (nth % 2)
                (nth % 3)
                (nth % 4)
                (nth % 5)) (nth (parse-pofile tokens) 2)))
