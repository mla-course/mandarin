(ns mla.mandarin
  "Chinese text segmentation into sentences, phrases, words and characters,
   with context-aware Hanyu Pinyin annotations."
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [com.hankcs.hanlp HanLP]
           [com.hankcs.hanlp.dictionary CoreDictionary]
           [com.hankcs.hanlp.seg.common Term]))

(s/def ::text string?)
(s/def ::pinyin string?)
(s/def ::start nat-int?)
(s/def ::end nat-int?)
(s/def ::character (s/keys :req-un [::text ::pinyin]))
(s/def ::characters (s/coll-of ::character :kind vector?))
(s/def ::word (s/keys :req-un [::text ::pinyin ::start ::end] :opt-un [::characters]))
(s/def ::words (s/coll-of ::word :kind vector?))
(s/def ::parts (s/coll-of map? :kind vector?))
(s/def ::sentence (s/keys :req-un [::text ::pinyin ::words ::characters ::parts]))
(s/def ::sentences (s/coll-of ::sentence :kind vector?))
(s/def ::analysis (s/keys :req-un [::text ::pinyin ::sentences]))

(s/def ::known? ifn?)
(s/def ::syllables (s/coll-of string? :kind vector?))
(s/def ::overrides (s/nilable (s/map-of (s/and string? seq) ::syllables)))
(s/def ::opts (s/keys :opt-un [::known? ::overrides]))

(def ^:private fullwidth->western-punct
  "Western equivalent of each full-width mark. Values carry no spacing:
   where spaces go is decided in one place, `render-pinyin`."
  {"。" "."
   "！" "!"
   "？" "?"
   "：" ":"
   "；" ";"
   "，" ","
   "、" ","
   "“" "\""
   "”" "\""
   "‘" "'"
   "’" "'"
   "「" "\""
   "」" "\""
   "『" "'"
   "』" "'"
   "（" "("
   "）" ")"
   "【" "["
   "】" "]"
   "《" "<"
   "》" ">"
   "〈" "<"
   "〉" ">"
   "～" "~"
   "—" "-"
   "…" "..."})

(defn- normalize-pinyin-punct
  "Convert full-width Chinese punctuation marks to Western equivalents."
  [s]
  (apply str (map #(get fullwidth->western-punct (str %) (str %)) s)))

(defn- punctuation?
  "Whether a term's word and NLP nature tag denote punctuation."
  [word nature]
  (or (str/starts-with? nature "w")
      (boolean (re-matches #"[\p{P}\p{S}]+" word))))

(defn- whitespace?
  "Whether `word` is only whitespace, including U+3000 and no-break spaces."
  [word]
  (boolean (re-matches #"[\s\p{Z}]+" word)))

(def ^:private opening-mark?
  "Characters that open a bracket or quotation; nothing is spaced after them."
  (set "([{<“‘（【《「『〈〔［｛"))

(defn- opening? [{:keys [punct? text]}]
  (boolean (and punct? (opening-mark? (first text)))))

(defn- gap
  "The separator between consecutive tokens `prev` and `tok`: punctuation
   attaches to what precedes it, except an opening mark, which attaches to
   what follows it."
  [prev tok]
  (if (or (nil? prev)
          (opening? prev)
          (and (:punct? tok) (not (opening? tok))))
    ""
    " "))

(defn- render-pinyin
  "One pinyin string for the annotated `tokens` (maps with `:pinyin`,
   `:text` and `:punct?`). The only place spacing is decided."
  [tokens]
  (str/join (map (fn [prev tok] (str (gap prev tok) (:pinyin tok)))
                 (cons nil tokens)
                 tokens)))

(defn- terminal?
  "Whether a segment ends a sentence."
  [{:keys [text]}]
  (boolean (re-matches #"[。！？!?…]+" text)))

(defn han?
  "Whether `s` contains a Han character."
  [s]
  (boolean (re-find #"\p{IsHan}" s)))

(defn- extract-character-pinyins
  "Tone-marked pinyin for each character of `word`, taken from the word's
   own pinyin list so polyphonic characters keep their context."
  [word]
  (mapv (fn [ch pinyin]
          (let [mark (when pinyin (.getPinyinWithToneMark pinyin))]
            {:text   (str ch)
             :pinyin (if (and mark (not= mark "none"))
                       mark
                       (normalize-pinyin-punct (str ch)))}))
        word
        (HanLP/convertToPinyinList word)))

(defn- with-characters
  "`segment` with `:characters` set to `chars`; its `:pinyin` follows them."
  [segment chars]
  (assoc segment
         :characters chars
         :pinyin     (str/join (map :pinyin chars))))

(defn- word-segment-from-text [word]
  (with-characters {:text word} (extract-character-pinyins word)))

(defn- term->segment [^Term term]
  (let [word   (.word term)
        nature (str (.nature term))]
    (assoc (word-segment-from-text word)
           :nature nature
           :punct? (punctuation? word nature))))

(def ^:private non-words
  "Han strings HanLP is known to merge into one term although they are two
   words. Extend as such cases are found."
  #{})
;; #{"你喝"}

(def ^:private trusted-natures
  "Multi-character terms with these natures are legitimate
  even when absent from the core dictionary."
  #{"nr" "nrf" "nrj" "ns" "nt" "nz" "nx" "m" "mq" "t"})

(defn- han-compound? [text]
  (and (< 1 (count text)) (boolean (re-matches #"\p{IsHan}+" text))))

(defn- unknown-compound?
  "Whether a multi-character Han `segment` should be split: it is neither
   vouched for by `known?` nor a trusted nature or core-dictionary word,
   or it is a listed `non-words` entry."
  [known? {:keys [text nature]}]
  (and (han-compound? text)
       (not (known? text))
       (or (contains? non-words text)
           (not (or (trusted-natures nature)
                    (CoreDictionary/contains text))))))

(defn- split-segment
  "One single-character word per character, keeping the compound's
   context-aware per-character pinyin."
  [{:keys [characters]}]
  (map (fn [{:keys [text pinyin] :as c}]
         {:text text :pinyin pinyin :characters [c] :punct? false})
       characters))

(defn- refine
  "`segment` as a seq of one or more segments: unknown Han compounds are
   split into their characters, everything else passes through."
  [known? segment]
  (if (unknown-compound? known? segment)
    (split-segment segment)
    [segment]))

(defn- override-spans
  "Non-overlapping `[from syllables]` of `overrides` keys found in `text`,
   scanning left to right, longest key first."
  [overrides text]
  (let [lens (->> (keys overrides) (map count) distinct (sort >))
        n    (count text)]
    (loop [i 0, out []]
      (if (>= i n)
        out
        (if-let [[len syllables]
                 (some (fn [len]
                         (when-let [s (and (<= (+ i len) n)
                                           (overrides (subs text i (+ i len))))]
                           [len s]))
                       lens)]
          (recur (+ i len) (conj out [i syllables]))
          (recur (inc i) out))))))

(defn- apply-overrides
  "`segments` with the pinyin of characters covered by `overrides` replaced
   by the author's syllables, wherever they fall among the segments."
  [overrides segments]
  (if (empty? overrides)
    segments
    (let [chars   (into [] (mapcat :characters) segments)
          pinyins (reduce (fn [v [from syllables]]
                            (reduce-kv #(assoc %1 (+ from %2) %3) v syllables))
                          (mapv :pinyin chars)
                          (override-spans overrides (apply str (map :text chars))))]
      (first
       (reduce (fn [[out pos] {:keys [characters] :as segment}]
                 (let [end       (+ pos (count characters))
                       annotated (mapv #(assoc %1 :pinyin %2) characters (subvec pinyins pos end))]
                   [(conj out (with-characters segment annotated)) end]))
               [[] 0]
               segments)))))

(defn- hanlp-segments
  "Refined HanLP segments of `text` (no offsets; whitespace dropped), with
   `overrides` applied. As whitespace is dropped first, an override key can
   match across it."
  [text {:keys [known? overrides]}]
  (->> (HanLP/segment text)
       (remove (fn [^Term term] (whitespace? (.word term))))
       (map term->segment)
       (mapcat (partial refine known?))
       (map #(dissoc % :nature))
       (apply-overrides overrides)))


(defn- with-offsets
  "Adds `:start`/`:end` (indices into `src`) to each of `segments`, which
   must appear in `src` in order."
  [src segments]
  (:segments
   (reduce (fn [{:keys [pos] :as acc} {word :text :as segment}]
             (let [start (or (str/index-of src word pos)
                             (throw (ex-info "Segment does not match the source text"
                                             {:segment word :from pos})))
                   end   (+ start (count word))]
               (-> acc
                   (assoc :pos end)
                   (update :segments conj (assoc segment :start start :end end)))))
           {:pos 0 :segments []}
           segments)))

(defn- segments
  "`text` as refined, context-aware segments with offsets."
  [text opts]
  (with-offsets text (hanlp-segments text opts)))

(defn- span
  "`{:text :start :end}` covering the segments `items` in `src`."
  [src items]
  (let [start (:start (first items))
        end   (:end (last items))]
    {:text (subs src start end) :start start :end end}))

(defn- phrase
  "Consecutive non-punctuation `words`."
  [src words]
  (assoc (span src words)
         :pinyin (render-pinyin words)
         :words  words))

(defn- group-parts
  "`words` as a vector of phrases and (single) punctuation segments."
  [src words]
  (into [] (mapcat #(if (:punct? (first %)) % [(phrase src (vec %))]))
        (partition-by :punct? words)))

(defn- split-sentences
  "Splits `words` after each run of terminal punctuation; a trailing
   unterminated remainder is its own sentence. A leading run of terminal
   punctuation is a sentence of its own."
  [words]
  (loop [words (seq words), out []]
    (if-not words
      out
      (let [[body words] (split-with (complement terminal?) words)
            [stop words] (split-with terminal? words)]
        (recur (seq words) (conj out (into [] cat [body stop])))))))

(defn- capitalize-first [s]
  (if (seq s) (str (str/upper-case (subs s 0 1)) (subs s 1)) s))

(defn- sentence [src words]
  (let [parts (group-parts src words)]
    (assoc (span src words)
           :pinyin      (capitalize-first (render-pinyin words))
           :terminated? (terminal? (last words))
           :parts       parts
           :words       words
           :characters  (into [] (mapcat :characters) words))))


(def ^:private default-opts {:known? (constantly false)})

(defn- resolve-opts
  "`opts` merged over the defaults. Throws `ex-info` when an option is
   malformed or an override has a different number of syllables than
   characters, which would otherwise silently shift the pinyin of the text
   that follows."
  [opts]
  (let [opts (merge default-opts opts)]
    (when-not (s/valid? ::opts opts)
      (throw (ex-info "Invalid options" (s/explain-data ::opts opts))))
    (doseq [[from syllables] (:overrides opts)
            :when (not= (count from) (count syllables))]
      (throw (ex-info "Invalid options: an override needs one syllable per character"
                      {:override from :syllables syllables})))
    opts))

(defn pinyin-of
  "Space-separated pinyin of `text`; Chinese punctuation becomes Western.
   Takes the same `opts` as `analyze`."
  ([text] (pinyin-of text nil))
  ([text opts]
   {:pre [(string? text)]}
   (render-pinyin (hanlp-segments text (resolve-opts opts)))))

(defn analyze
  "Analyze Chinese text into sentences, phrases, words and characters."
  ([text] (analyze text nil))
  ([text opts]
   {:pre [(string? text)]}
   (let [opts      (resolve-opts opts)
         sentences (mapv (partial sentence text) (split-sentences (segments text opts)))]
     {:text      text
      :pinyin    (str/join " " (map :pinyin sentences))
      :sentences sentences})))

(defn- ->analysis
  "`input` if it already is an analysis map, else the analysis of the text."
  [input]
  (if (map? input) input (analyze input)))

(defn sentences
  "Return sentence segments from text or an existing analysis map."
  [input]
  (:sentences (->analysis input)))

(defn words
  "Return word segments from text or an existing analysis map."
  [input]
  (into [] (mapcat :words) (:sentences (->analysis input))))

(defn characters
  "Return character segments from text or an existing analysis map."
  [input]
  (into [] (mapcat :characters) (:sentences (->analysis input))))
