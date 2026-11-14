# DATA_DICTIONARY.md

出典：環境省「犬・猫の引取り及び負傷動物等の収容並びに処分の状況」（動物愛護管理行政事務提要 令和7年度版）。原データは `data/raw/`、作成スクリプトは `R/01_extract_pdf.R`。

---

## 1. `data/processed/national_timeseries.csv` — 全国の推移

1行 = 年度 × 動物種。平成16年度（2004）〜令和6年度（2024）、63行。

| 列名 | 日本語 | 型 | 定義・注意 |
|---|---|---|---|
| `era_label` | 年度（和暦） | string | 原典の表記（例：令和6年度） |
| `fiscal_year` | 年度 | integer | 開始年の西暦。令和6年度 = 2024（2024年4月〜2025年3月） |
| `species` | 動物種 | string | `dog` / `cat` / `total`（犬猫合計） |
| `intake` | 引取り数 | integer | 負傷動物の収容は含まない |
| `returned_adopted` | 返還・譲渡数 | integer | **返還と譲渡の合算。この表では分けられない** |
| `euthanized` | 殺処分数 | integer | 保管中の病気等による自然死を含む |
| `note` | 注記 | string | 平成16・17年度の犬の引取り数は、狂犬病予防法に基づく抑留を勘案した推計値 |
| `source_inconsistency` | 原典の不一致 | boolean | `TRUE` = 原典で犬＋猫≠合計（平成24年度の殺処分数、差180）。値は原典のまま |

原典には「負傷動物込み」の系列もあるが、引取りと定義が異なるため、このCSVには入れていない。

---

## 2. `data/processed/municipal_r06.csv` — 自治体別（令和6年度）

1行 = 自治体 × 動物種。129自治体（都道府県47、指定都市20、中核市62）× 犬・猫 = 258行。

**重要：** 都道府県の数値には、その県内の指定都市・中核市の数値を含まない（原典の注記）。県の数字は「県が直接担当する地域」の値。

| 列名 | 日本語 | 型 | 定義・注意 |
|---|---|---|---|
| `jurisdiction_type` | 自治体の種類 | string | `prefecture` / `designated_city` / `core_city` |
| `fiscal_year` | 年度 | integer | 2024 |
| `jurisdiction` | 自治体名 | string | 原典の表記 |
| `species` | 動物種 | string | `dog` / `cat` |
| `intake_owner_adult` / `_young` | 飼い主からの引取り（成熟／幼齢） | integer | |
| `intake_unknown_adult` / `_young` | 所有者不明の引取り（成熟／幼齢） | integer | 成熟には狂犬病予防法に基づく抑留を含む。一部、条例に基づく収容を含む |
| `intake_total_adult` / `_young` | 引取り合計（成熟／幼齢） | integer | |
| `intake_total` | 引取り数 | integer | 成熟＋幼齢（幼齢が空欄なら成熟のみ） |
| `returned` | 返還数 | integer | |
| `adopted` | 譲渡数 | integer | 分類PDFの定義では譲渡先を「希望者又は愛護団体等」と書いており、**保護団体への譲渡を含むと考えられる**（自治体への確認は未） |
| `euthanized_1` | 殺処分① | integer | 譲渡することが適切ではない（治癒の見込みがない病気、攻撃性など） |
| `euthanized_2` | 殺処分② | integer | ①以外の殺処分（譲渡先の確保や適切な飼養管理が困難。収容能力の限界を含む） |
| `euthanized_3` | 殺処分③ | integer | 引取り後の死亡（病気・老衰・事故・幼齢による死亡など） |
| `euthanized_total` | 殺処分数 計 | integer | ①＋②＋③ |
| `*_young` | 上の各処分数のうち幼齢個体 | integer | 内数 |

- 空欄（NA）は原典の「-」。該当なし、または区分していないことを示す。0とは区別する。
- 幼齢個体とは、主に離乳していない個体。成熟と幼齢を区分していない自治体は、すべて成熟個体として計上している。

---

## 3. 派生変数（分析で作る）

| 変数 | 定義 | 注意 |
|---|---|---|
| `adoption_rate` | `adopted / intake_total` | 年度をまたぐ収容があるため、返還＋譲渡＋殺処分の合計は引取り数と一致しない。「引取り数あたり」と明記する |
| `euthanasia_rate` | `euthanized_total / intake_total` | 同上 |
| `preventable_euthanasia` | `euthanized_2` | 「譲渡先や収容能力があれば避けられた可能性がある」殺処分。事業の課題と最も直接つながる |

## 検証（`R/01_extract_pdf.R` が毎回実行）

1. 自治体別の合計 = 原典の「合計」行（全列）
2. 犬の合計行 = 全国表の令和6年度の値（引取り・返還＋譲渡・殺処分）
3. 全国表：犬＋猫＝合計（平成24年度の殺処分数のみ原典で不一致。上記のとおり）

## 3. `data/processed/municipal_panel.csv` — 自治体別（令和元年度〜令和6年度）

- 列は `municipal_r06.csv` と同じ。`fiscal_year` が 2019〜2024 の6年度分を縦に積んだもの（1,536行）
- 自治体数は年度で違う（2019：125、2020：127、2021年度以降：129）。期間中に中核市が増えたため
- 各年度で次の検証を通過：自治体の合計＝PDFの合計行、犬の合計＝全国の推移表
- 原典の空欄・ダッシュ（「-」「‐」）は NA。令和4年度の明石市は犬・所有者不明・幼齢のセルが空欄で、NA として位置をそろえた（`R/01_extract_pdf.R` の `blank_cells`）
