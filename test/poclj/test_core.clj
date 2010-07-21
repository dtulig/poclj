(ns poclj.test-core
  (:use [poclj.core] :reload-all)
  (:use [clojure.test]))

(defmacro basic-msg [s]
  `(str ~s "
msgid \"aA\"
msgstr \"bB\""))

(deftest parse-entry
  (let [entry (first (parse "msgid \"aA\"
msgstr \"bB\""))]
    (and (is (.equals "aA" (:msgid entry)))
         (is (.equals "bB" (:msgstr entry))))))

(deftest parse-occurences
  (let [entry (first (parse (basic-msg "#: /cC.txt")))]
    (is (.equals "/cC.txt" (first (:occurences entry))))))

(deftest parse-multi-occurences
  (let [entry (first (parse (basic-msg "#: /cC.txt
#: /dD.txt")))]
    (is (.equals "/cC.txt" (first (:occurences entry))))
    (is (.equals "/dD.txt" (second (:occurences entry))))))

(deftest parse-comment
  (let [entry (first (parse (basic-msg "#. lorem ipsum sim dolet")))]
    (is (.equals "lorem ipsum sim dolet" (:comment entry)))))

(deftest parse-multi-comment
  (let [entry (first (parse (basic-msg "#. lorem ipsum sim dolet
#. dolet sim lorem ipsum")))]
    (is (.equals "lorem ipsum sim dolet dolet sim lorem ipsum" (:comment entry)))))

(deftest parse-tcomment
  (let [entry (first (parse (basic-msg "# lorem ipsum")))]
    (is (.equals "lorem ipsum" (:tcomment entry)))))

(deftest parse-multi-tcomment
  (let [entry (first (parse (basic-msg "# lorem ipsum
# sim dolet")))]
    (is (.equals "lorem ipsum sim dolet" (:tcomment entry)))))
