(ns mla.mandarin-test
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test :refer [deftest is are testing run-tests]]
            [mla.mandarin :as mandarin]))

(deftest analyzes-empty-input
  (testing "Empty input produces an empty segmentation"
    (is (= {:text "" :pinyin "" :sentences []}
           (mandarin/analyze "")))))

(deftest analyzes-multiple-sentences
  (let [result (mandarin/analyze "你好！你今天好吗？")]
    (is (= "你好！你今天好吗？" (:text result)))
    (is (= 2 (count (:sentences result))))
    (is (= ["你好！" "你今天好吗？"]
           (mapv :text (:sentences result))))))

(deftest segments-words-and-characters
  (let [result (mandarin/analyze "我喜欢中文。")
        sentence (first (:sentences result))
        words (:words sentence)]
    (is (seq words))
    (is (every? #(contains? % :text) words))
    (is (every? #(contains? % :pinyin) words))
    (is (every? #(contains? % :characters) words))
    (is (every? #(contains? % :text) (:characters sentence)))
    (is (every? #(contains? % :pinyin) (:characters sentence)))))

(deftest convenience-functions
  (let [text "你好！再见。"]
    (is (= 2 (count (mandarin/sentences text))))
    (is (seq (mandarin/words text)))
    (is (= ["你" "好" "！" "再" "见" "。"]
           (mapv :text (mandarin/characters text))))))

(defn- py [s] (mandarin/pinyin-of s))

(deftest pinyin-of
  (testing "each of the four tones plus neutral tone"
    (are [expected input] (= expected (py input))
      "mā" "妈"   ; 1st tone
      "má" "麻"   ; 2nd tone
      "mǎ" "马"   ; 3rd tone
      "mà" "骂"   ; 4th tone
      "ma" "吗")) ; neutral tone

  (testing "common two-character words and reduplication (HanLP preserves citation tones)"
    (are [expected input] (= expected (py input))
      "nǐhǎo"   "你好"
      "zhōngguó" "中国"
      "xièxiè"   "谢谢"   ; HanLP preserves underlying 4th tone
      "péngyǒu"  "朋友"   ; HanLP preserves underlying 3rd tone on 友
      "xuéxí"    "学习"
      "māmā"     "妈妈"
      "bàbà"     "爸爸"))  ; HanLP preserves underlying 4th tone on 爸

  (testing "3-3 tone sandhi: HanLP preserves underlying tone marks"
    (is (= "nǐhǎo" (py "你好"))))

  (testing "polyphonic characters by context"
    (are [expected input] (= expected (py input))
      ;; 重 (zhòng / chóng)
      "zhòngyào" "重要"
      "chóngfù"  "重复"
      "chóngxīn" "重新"
      "tǐzhòng"  "体重"
      ;; 行 (xíng / háng)
      "yínháng"  "银行"
      "xíngzǒu"  "行走"
      "lǚxíng"   "旅行"
      "hángyè"   "行业"
      ;; 长 (cháng / zhǎng)
      "chángduǎn" "长短"
      "zhǎngdà"   "长大"
      "shìzhǎng"  "市长"
      ;; 乐 (lè / yuè)
      "kuàilè"   "快乐"
      "yīnyuè"   "音乐"
      ;; 了 (le / liǎo)
      "wǒ chī le"  "我吃了"
      "liǎojiě"  "了解"
      ;; 的 (de / dí)
      "wǒ de"     "我的"
      "mùdì"     "目的"
      ;; 中 (zhōng / zhòng)
      "zhōngguó" "中国"
      "zhōngxīn" "中心"
      "zhòngjiǎng" "中奖"))

  (testing "edge cases and pure non-Chinese pass-throughs"
    (are [expected input] (= expected (py input))
      "hello" "hello"
      "123"   "123"
      "!?"    "!?"
      "nihao" "nihao"))

  (testing "mixed content: Chinese with Latin, digits, and punctuation"
    (are [expected input] (= expected (py input))
      "wǒ ài Clojure"   "我爱 Clojure"
      "2026 nián"       "2026 年"
      "nǐhǎo, shìjiè!" "你好，世界！"
      "nǐhǎo."         "你好。"
      "nǐhǎo!"         "你好！"
      "nǐhǎo?"         "你好？"))

  (testing "full sentences with mixed tones, punctuation and polyphones"
    (are [expected input] (= expected (py input))
      "wǒ xǐhuān xuéxí zhōngwén." "我喜欢学习中文。"
      "tā hěn zhòngyào."           "他很重要。"
      "qǐng chóngfù yī biàn."       "请重复一遍。")))

(deftest han-segmentation-contract
  (testing "control: HanLP already splits 你的"
    (is (= ["你" "的"] (mapv :text (mandarin/words "你的"))))
    (is (= "Nǐ de" (:pinyin (mandarin/analyze "你的")))))
  (testing "regression: non-dictionary compound is split"
    (is (= ["你" "喝"] (mapv :text (mandarin/words "你喝"))))
    (is (= "Nǐ hē" (:pinyin (mandarin/analyze "你喝"))))))

(defn- word-texts [text] (mapv :text (mandarin/words text)))

(deftest splits-non-dictionary-compounds
  (testing "pronoun + verb: not words, must split"
    (are [text expected] (= expected (word-texts text))
      "你喝" ["你" "喝"]
      "我喝" ["我" "喝"]
      "我吃" ["我" "吃"]
      "你吃" ["你" "吃"]
      "他去" ["他" "去"]
      "她来" ["她" "来"]
      "你来" ["你" "来"]
      "我走" ["我" "走"]
      "你说" ["你" "说"]
      "他说" ["他" "说"]
      "我想" ["我" "想"]
      "我要" ["我" "要"]
      "你有" ["你" "有"]
      "我是" ["我" "是"]
      "他在" ["他" "在"]
      "你看" ["你" "看"]
      "我听" ["我" "听"]
      "我读" ["我" "读"]
      "你写" ["你" "写"]
      "我买" ["我" "买"]
      "你找" ["你" "找"]
      "我爱" ["我" "爱"]))

  (testing "pronoun + verb + object, more than two characters"
    (are [text expected] (= expected (word-texts text))
      "我爱你"   ["我" "爱" "你"]
      "我喜欢你" ["我" "喜欢" "你"]
      "你想我"   ["你" "想" "我"]
      "他爱她"   ["他" "爱" "她"]
      "我找你"   ["我" "找" "你"]))

  (testing "punctuation stays its own word and does not affect the split"
    (are [text expected] (= expected (word-texts text))
      "你喝，我吃。"  ["你" "喝" "，" "我" "吃" "。"]
      "你去，他来！"  ["你" "去" "，" "他" "来" "！"]
      "你喝？"        ["你" "喝" "？"])))

(deftest keeps-real-compounds
  (testing "control: genuine words (HSK 1-5) are not split"
    (are [text] (= [text] (word-texts text))
      "你好" "谢谢" "朋友" "学习" "喜欢" "中国" "老师" "学生"
      "非常" "已经" "因为" "所以" "但是" "如果" "重要" "努力" "图书馆"))

  (testing "control: HanLP already splits these"
    (are [text expected] (= expected (word-texts text))
      "你的"   ["你" "的"]
      "我们"   ["我们"]
      "你们好" ["你们" "好"])))

(deftest pinyin-follows-word-splits
  (are [text expected] (= expected (:pinyin (mandarin/analyze text)))
    "你喝"     "Nǐ hē"
    "我吃"     "Wǒ chī"
    "他去"     "Tā qù"
    "我爱你"   "Wǒ ài nǐ"
    "你喝，我吃。" "Nǐ hē, wǒ chī."
    "你好"     "Nǐhǎo")
  (are [text expected] (= expected (mandarin/pinyin-of text))
    "你喝" "nǐ hē"
    "我吃" "wǒ chī"))

(defn- word-token [text pinyin] {:text text :pinyin pinyin :punct? false})
(defn- punct-token [text pinyin] {:text text :pinyin pinyin :punct? true})

(deftest render-pinyin-spacing
  (let [render #'mandarin/render-pinyin]
    (testing "nothing to render"
      (is (= "" (render []))))

    (testing "words are separated by one space"
      (is (= "nǐ hǎo" (render [(word-token "你" "nǐ") (word-token "好" "hǎo")]))))

    (testing "closing punctuation attaches left and is followed by a space"
      (is (= "nǐ, wǒ."
             (render [(word-token "你" "nǐ") (punct-token "，" ",")
                      (word-token "我" "wǒ") (punct-token "。" ".")]))))

    (testing "brackets hug their content"
      (is (= "wǒ (nǐ) hǎo"
             (render [(word-token "我" "wǒ") (punct-token "（" "(")
                      (word-token "你" "nǐ") (punct-token "）" ")")
                      (word-token "好" "hǎo")]))))

    (testing "quotes: the opening mark is told from the closing one by its source text"
      (is (= "tā shuō: \"nǐ\""
             (render [(word-token "他" "tā") (word-token "说" "shuō")
                      (punct-token "：" ":") (punct-token "“" "\"")
                      (word-token "你" "nǐ") (punct-token "”" "\"")]))))

    (testing "a leading opening mark or consecutive opening marks add no space"
      (is (= "(\"nǐ\")"
             (render [(punct-token "（" "(") (punct-token "“" "\"")
                      (word-token "你" "nǐ") (punct-token "”" "\"")
                      (punct-token "）" ")")]))))

    (testing "punctuation at the very start gets no leading space"
      (is (= "! nǐ"
             (render [(punct-token "！" "!") (word-token "你" "nǐ")]))))))

(deftest punctuation-spacing-end-to-end
  (testing "brackets (full-width and ASCII) are not followed by a space"
    (are [expected input] (= expected (py input))
      "wǒ (nǐhǎo) shìjiè" "我（你好）世界"
      "wǒ (nǐhǎo) shìjiè" "我(你好)世界"))
  (testing "colon and semicolon are followed by a space like the comma"
    (are [expected input] (= expected (py input))
      "nǐhǎo: shìjiè" "你好：世界"
      "nǐhǎo; shìjiè" "你好；世界"
      "nǐhǎo, shìjiè" "你好，世界")))

(deftest character-pinyin-carries-no-spacing
  (is (= ["nǐ" "hǎo" "!"]
         (mapv :pinyin (mandarin/characters "你好！")))))

(deftest sentence-pinyin-agrees-with-pinyin-of
  (testing "for a single sentence the two renderings differ only by the capital"
    (are [text] (let [p (py text)]
                  (= (str (str/upper-case (subs p 0 1)) (subs p 1))
                     (:pinyin (mandarin/analyze text))))
      "你好，世界！"
      "我（你好）世界。"
      "你喝，我吃。"
      "我爱 Clojure")))

(deftest unicode-whitespace-is-not-a-word
  (testing "ideographic space and no-break space are dropped like ASCII space"
    (are [text expected] (= expected (py text))
      "你好　世界" "nǐhǎo shìjiè"
      "你好 世界" "nǐhǎo shìjiè"
      "你好 世界"      "nǐhǎo shìjiè"))
  (testing "they never surface as words or characters"
    (let [analysis (mandarin/analyze "你好　世界")]
      (is (not-any? #(re-find #"[\s\p{Z}]" (:text %)) (mandarin/words analysis)))
      (is (not-any? #(re-find #"[\s\p{Z}]" (:text %)) (mandarin/characters analysis))))))

;; --- Input validation ------------------------------------------------------

(deftest rejects-non-string-input
  (testing "a precondition guards both entry points"
    (is (thrown? AssertionError (mandarin/analyze nil)))
    (is (thrown? AssertionError #_:clj-kondo/ignore (mandarin/analyze 42)))
    (is (thrown? AssertionError (mandarin/pinyin-of nil)))))

(deftest rejects-malformed-options
  (are [opts] (thrown? clojure.lang.ExceptionInfo (mandarin/analyze "重要" opts))
    {:known? "not a function"}
    {:known? nil}
    {:overrides {"重要" "chóng yào"}}            ; syllables must be a vector
    {:overrides {"重要" ["chóng"]}}              ; too few syllables
    {:overrides {"重要" ["chóng" "yào" "x"]}}    ; too many syllables
    {:overrides {"" []}}                         ; empty key
    {:overrides {:key ["a"]}})                   ; key must be a string
  (testing "pinyin-of validates the same way"
    (is (thrown? clojure.lang.ExceptionInfo
                 (mandarin/pinyin-of "重要" {:overrides {"重要" ["chóng"]}}))))
  (testing "nil and empty options are fine"
    (are [opts] (= "zhòngyào" (mandarin/pinyin-of "重要" opts))
      nil
      {}
      {:overrides nil}
      {:overrides {}})))

;; --- Options ---------------------------------------------------------------

(deftest known-words-are-not-split
  (let [known-words (fn [text opts]
                      (mapv :text (mandarin/words (mandarin/analyze text opts))))]
    (testing "by default 你喝 is split"
      (is (= ["你" "喝"] (known-words "你喝" nil))))
    (testing "a set of known words"
      (is (= ["你喝"] (known-words "你喝" {:known? #{"你喝"}}))))
    (testing "any predicate"
      (is (= ["你喝"] (known-words "你喝" {:known? #(= % "你喝")}))))
    (testing "other compounds are still split"
      (is (= ["我" "吃"] (known-words "我吃" {:known? #{"你喝"}}))))
    (testing "pinyin follows"
      (is (= "Nǐhē" (:pinyin (mandarin/analyze "你喝" {:known? #{"你喝"}}))))
      (is (= "nǐhē" (mandarin/pinyin-of "你喝" {:known? #{"你喝"}}))))))

(deftest pinyin-overrides
  (let [char-pinyins (fn [text opts]
                       (mapv :pinyin (mandarin/characters (mandarin/analyze text opts))))]
    (testing "an override replaces the pinyin of its string"
      (is (= "chóngyào"
             (mandarin/pinyin-of "重要" {:overrides {"重要" ["chóng" "yào"]}}))))
    (testing "word and sentence pinyin follow the characters"
      (let [analysis (mandarin/analyze "重要" {:overrides {"重要" ["chóng" "yào"]}})]
        (is (= "Chóngyào" (:pinyin analysis)))
        (is (= ["chóngyào"] (mapv :pinyin (mandarin/words analysis))))))
    (testing "a single-character key changes only that character"
      (is (= ["chóng" "yào"] (char-pinyins "重要" {:overrides {"重" ["chóng"]}}))))
    (testing "an override applies across word boundaries"
      (is (= ["wǒ" "chī" "liǎo"]
             (char-pinyins "我吃了" {:overrides {"我吃了" ["wǒ" "chī" "liǎo"]}}))))
    (testing "every occurrence is replaced"
      (is (= ["chóng" "yào" "," "chóng" "yào"]
             (char-pinyins "重要，重要" {:overrides {"重要" ["chóng" "yào"]}}))))
    (testing "the longest key wins"
      (is (= ["chóng" "yào"]
             (char-pinyins "重要" {:overrides {"重"   ["zhòng"]
                                               "重要" ["chóng" "yào"]}}))))
    (testing "a key that does not occur changes nothing"
      (is (= "zhòngyào"
             (mandarin/pinyin-of "重要" {:overrides {"银行" ["yín" "xíng"]}}))))))

;; --- Structural invariants -------------------------------------------------

(def ^:private corpus
  ["" "你好" "你好！你今天好吗？" "我喜欢学习中文。你呢？" "你喝，我吃。"
   "我爱 Clojure" "2026 年" "你好　世界" "！你好" "你好！！再见" "（你好）"
   "hello" "123" "他说：“你好。”" "我吃了"])

(defn- strip-whitespace [text] (str/replace text #"[\s\p{Z}]+" ""))

(defn- ordered? [spans]
  (every? (fn [[a b]] (<= (:end a) (:start b))) (partition 2 1 spans)))

(deftest analysis-invariants
  (doseq [text corpus
          :let [result    (mandarin/analyze text)
                sentences (:sentences result)
                words     (mandarin/words result)]]
    (testing (pr-str text)
      (is (s/valid? ::mandarin/analysis result)
          (s/explain-str ::mandarin/analysis result))
      (is (every? #(= (:text %) (subs text (:start %) (:end %)))
                  (concat sentences words))
          "offsets index the source text")
      (is (ordered? sentences) "sentences are ordered and disjoint")
      (is (ordered? words) "words are ordered and disjoint")
      (is (= (strip-whitespace text)
             (apply str (map :text (mandarin/characters result))))
          "characters reproduce the text minus whitespace")
      (is (every? #(= (:pinyin %) (apply str (map :pinyin (:characters %)))) words)
          "a word's pinyin is its characters' pinyin"))))

(deftest split-sentences-edge-cases
  (let [split (fn [texts] (#'mandarin/split-sentences (map (fn [t] {:text t}) texts)))
        texts (fn [sentences] (mapv #(mapv :text %) sentences))]
    (are [ws expected] (= expected (texts (split ws)))
      []                             []
      ["你好"]                       [["你好"]]
      ["你好" "！"]                  [["你好" "！"]]
      ["你好" "！" "你" "呢" "？"]   [["你好" "！"] ["你" "呢" "？"]]
      ["你好" "！！" "再见"]         [["你好" "！！"] ["再见"]]
      ["你好" "。" "！" "再见"]      [["你好" "。" "！"] ["再见"]]
      ["！" "你好"]                  [["！"] ["你好"]]
      ["你好" "，" "世界" "。"]      [["你好" "，" "世界" "。"]]
      ["你好" "。" "再见"]           [["你好" "。"] ["再见"]])
    (testing "sentences are vectors, as the specs require"
      (is (every? vector? (split ["你好" "！" "再见"]))))))

(deftest sentence-parts
  (let [sentence (first (mandarin/sentences "你好，世界。"))]
    (testing "phrases and punctuation alternate"
      (is (= ["你好" "，" "世界" "。"] (mapv :text (:parts sentence))))
      (is (= ["nǐhǎo" "," "shìjiè" "."] (mapv :pinyin (:parts sentence)))))
    (testing "a phrase holds the words between punctuation"
      (is (= ["你好"] (mapv :text (:words (first (:parts sentence)))))))
    (testing "the sentence is terminated, an unfinished remainder is not"
      (is (true? (:terminated? sentence)))
      (is (= [true false] (mapv :terminated? (mandarin/sentences "你好！再见")))))))

(deftest accessors-accept-text-or-analysis
  (let [text     "你好！我喜欢学习中文。"
        analysis (mandarin/analyze text)]
    (is (= (mandarin/sentences text)  (mandarin/sentences analysis)))
    (is (= (mandarin/words text)      (mandarin/words analysis)))
    (is (= (mandarin/characters text) (mandarin/characters analysis)))
    (is (= (mapv :text (mandarin/sentences text)) ["你好！" "我喜欢学习中文。"]))))

(defn -main [& _]
  (let [result (run-tests 'mla.mandarin-test)]
    (when (pos? (+ (:fail result) (:error result)))
      (System/exit 1))))
