# R/01_extract_pdf.R
# 環境省のPDF（data/raw/）から表を抜き出し、整形済みCSV（data/processed/）を作る。
# 実行：プロジェクトのルートで source("R/01_extract_pdf.R")
#
# 出力
#   data/processed/national_timeseries.csv  全国の推移（平成16年度〜令和6年度）
#   data/processed/municipal_r06.csv        自治体別（令和6年度、犬・猫）
#
# 方針：PDFのテキストを「行」単位で読み、数字の並び（トークン）の個数で
#       どの表のどの行かを判定する。数字の個数が想定と違う行があれば止める
#       （黙って間違ったデータを作らないため）。

suppressPackageStartupMessages({
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(readr)
  library(purrr)
})

# 日本語の文字列比較（"合計" など）にはUTF-8のロケールが必要。
# Mac/Positronでは通常UTF-8。ターミナルで止まったら LC_ALL=en_US.UTF-8 を設定する。
if (!isTRUE(l10n_info()$`UTF-8`)) stop("UTF-8ロケールで実行してください（例：Sys.setlocale('LC_ALL', 'en_US.UTF-8')）")

RAW <- "data/raw"
OUT <- "data/processed"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

# ---- helpers -------------------------------------------------------------

# PDFを行のベクトルにする（ページをまたいで連結）
pdf_lines <- function(path) {
  pdf_text(path) |> str_split("\n") |> unlist()
}

# "12,300" -> 12300, "-" -> NA（"-" は該当なし・未計上）
to_num <- function(x) {
  x <- str_remove_all(x, ",")
  suppressWarnings(as.numeric(ifelse(x == "-", NA, x)))
}

# 数字（または "-"）だけでできたトークンか
is_num_token <- function(x) str_detect(x, "^(-|[0-9][0-9,]*)$")

tokens <- function(line) str_split(str_trim(line), "\\s+")[[1]]

# 和暦の年度 -> 西暦の開始年（令和6年度 -> 2024）
era_to_year <- function(label) {
  label <- str_replace(label, "元", "1")
  n <- as.integer(str_extract(label, "[0-9]+"))
  case_when(
    str_starts(label, "平成") ~ 1988L + n,
    str_starts(label, "令和") ~ 2018L + n,
    TRUE ~ NA_integer_
  )
}

# ---- 1. 全国の推移（r07_national.pdf の「参考」表） ------------------------
# 1行 = 年度ラベル + 13個の値：
#   犬(引取り, 返還・譲渡, 殺処分) 猫(同) 合計(同) 殺処分率  負傷動物込み(収容, 返還・譲渡, 殺処分)

nat_lines <- pdf_lines(file.path(RAW, "r07_national.pdf"))
ts_lines  <- nat_lines[str_detect(nat_lines, "^\\s*(平成|令和)\\S*年度\\s+[0-9]")]
ts_lines  <- ts_lines[map_int(ts_lines, \(l) length(tokens(l))) == 14]  # 先頭の詳細表（令和6年度の1行）は列数が違うので除外

national_wide <- map_dfr(ts_lines, function(l) {
  tk <- tokens(l)
  v  <- to_num(tk[2:14][-10])            # 10番目は殺処分率（%）なので除く
  tibble(
    era_label   = tk[1],
    fiscal_year = era_to_year(tk[1]),
    dog_intake = v[1], dog_returned_adopted = v[2], dog_euthanized = v[3],
    cat_intake = v[4], cat_returned_adopted = v[5], cat_euthanized = v[6],
    total_intake = v[7], total_returned_adopted = v[8], total_euthanized = v[9]
  )
})

stopifnot(
  nrow(national_wide) == 21,                         # 平成16〜令和6年度
  identical(national_wide$fiscal_year, 2004:2024),
  !anyNA(national_wide)
)

national_ts <- national_wide |>
  tidyr::pivot_longer(
    -c(era_label, fiscal_year),
    names_to = c("species", ".value"),
    names_pattern = "^(dog|cat|total)_(.*)$"
  ) |>
  mutate(note = if_else(species == "dog" & fiscal_year %in% 2004:2005,
                        "犬の引取り数は狂犬病予防法に基づく抑留を勘案した推計値", NA_character_))

# 検算：犬 + 猫 = 合計
# 既知の不一致：平成24年度（2012）の殺処分数は、原典PDFで 犬38,447 + 猫123,220 = 161,667 だが
# 合計欄は161,847（差180）。原典の値を変えずに残し、source_inconsistency 列で印を付ける。
mismatch <- national_wide |>
  filter(dog_intake + cat_intake != total_intake |
         dog_returned_adopted + cat_returned_adopted != total_returned_adopted |
         dog_euthanized + cat_euthanized != total_euthanized) |>
  pull(fiscal_year)
if (!identical(mismatch, 2012L)) stop("想定外の検算エラー：", paste(mismatch, collapse = ", "))
warning("原典PDFの不一致（犬+猫≠合計）：", paste(mismatch, collapse = ", "), "年度。値は原典のまま保持", call. = FALSE)

national_ts <- national_ts |>
  mutate(source_inconsistency = species == "total" & fiscal_year %in% mismatch)

# ---- 2. 自治体別（r07_municipal.pdf） ---------------------------------------
# 1自治体 = 3行
#   上段（12値）：犬[返還, 譲渡, 殺処分①, ②, ③, 計] 猫[同]
#   中段（自治体名 + 16値）：犬[飼い主から 成熟, 幼齢, 所有者不明 成熟, 幼齢, 合計 成熟, 幼齢]
#                            猫[同] 犬猫合計[引取り, 返還, 譲渡, 殺処分]
#   下段（12値）：上段のうち幼齢個体

mun_lines <- pdf_lines(file.path(RAW, "r07_municipal.pdf"))
nonblank  <- mun_lines[str_trim(mun_lines) != ""]

is_mid <- map_lgl(nonblank, function(l) {
  tk <- tokens(l)
  length(tk) == 17 && !is_num_token(tk[1]) && all(is_num_token(tk[-1]))
})
mid_idx <- which(is_mid)

parse_row <- function(i) {
  top <- tokens(nonblank[i - 1]); mid <- tokens(nonblank[i]); bot <- tokens(nonblank[i + 1])
  if (length(top) != 12 || length(bot) != 12 ||
      !all(is_num_token(top)) || !all(is_num_token(bot))) {
    stop("想定外の行構成：", mid[1])
  }
  top <- to_num(top); bot <- to_num(bot); m <- to_num(mid[-1])
  one <- function(sp, o_in, o_out) tibble(
    jurisdiction = mid[1], species = sp,
    intake_owner_adult   = m[o_in + 1], intake_owner_young   = m[o_in + 2],
    intake_unknown_adult = m[o_in + 3], intake_unknown_young = m[o_in + 4],
    intake_total_adult   = m[o_in + 5], intake_total_young   = m[o_in + 6],
    returned = top[o_out + 1], adopted = top[o_out + 2],
    euthanized_1 = top[o_out + 3], euthanized_2 = top[o_out + 4],
    euthanized_3 = top[o_out + 5], euthanized_total = top[o_out + 6],
    returned_young = bot[o_out + 1], adopted_young = bot[o_out + 2],
    euthanized_1_young = bot[o_out + 3], euthanized_2_young = bot[o_out + 4],
    euthanized_3_young = bot[o_out + 5], euthanized_total_young = bot[o_out + 6]
  )
  bind_rows(one("dog", 0, 0), one("cat", 6, 6))
}

municipal_all <- map_dfr(mid_idx, parse_row) |>
  mutate(intake_total = intake_total_adult + coalesce(intake_total_young, 0))

# 政令指定都市（20市）。それ以外の「市」は中核市として扱う
designated <- c("札幌市", "仙台市", "さいたま市", "千葉市", "横浜市", "川崎市", "相模原市",
                "新潟市", "静岡市", "浜松市", "名古屋市", "京都市", "大阪市", "堺市", "神戸市",
                "岡山市", "広島市", "北九州市", "福岡市", "熊本市")

municipal_all <- municipal_all |>
  mutate(jurisdiction_type = case_when(
    jurisdiction == "合計"           ~ "national_total",
    jurisdiction %in% designated     ~ "designated_city",
    str_ends(jurisdiction, "市")     ~ "core_city",
    str_ends(jurisdiction, "[都道府県]") ~ "prefecture",
    TRUE ~ NA_character_
  ), fiscal_year = 2024L, .before = 1)

stopifnot(!anyNA(municipal_all$jurisdiction_type),
          sum(municipal_all$jurisdiction_type == "prefecture") / 2 == 47,
          sum(municipal_all$jurisdiction_type == "designated_city") / 2 == 20)

# ---- 3. 検証 ---------------------------------------------------------------
# (a) 自治体の合計 = PDFの「合計」行
total_row <- municipal_all |> filter(jurisdiction == "合計")
municipal <- municipal_all |> filter(jurisdiction != "合計")

num_cols <- names(municipal)[sapply(municipal, is.numeric) & names(municipal) != "fiscal_year"]
sums <- municipal |> group_by(species) |> summarise(across(all_of(num_cols), \(x) sum(x, na.rm = TRUE)))
cmp  <- total_row |> select(species, all_of(num_cols)) |> arrange(species)
diff <- as.matrix(sums |> arrange(species) |> select(-species)) - as.matrix(cmp |> select(-species))
if (any(diff != 0, na.rm = TRUE)) {
  print(which(diff != 0, arr.ind = TRUE)); stop("自治体の合計がPDFの合計行と一致しない")
}

# (b) 犬の合計行 = 全国PDFの令和6年度の値（引取り17,399 / 返還+譲渡15,427 / 殺処分1,964）
dog_tot <- total_row |> filter(species == "dog")
nat_r6  <- national_wide |> filter(fiscal_year == 2024)
stopifnot(
  dog_tot$intake_total == nat_r6$dog_intake,
  dog_tot$returned + dog_tot$adopted == nat_r6$dog_returned_adopted,
  dog_tot$euthanized_total == nat_r6$dog_euthanized
)

# ---- 4. 保存 ---------------------------------------------------------------
write_csv(national_ts, file.path(OUT, "national_timeseries.csv"), na = "")
write_csv(municipal |> relocate(intake_total, .after = intake_total_young),
          file.path(OUT, "municipal_r06.csv"), na = "")

message(sprintf("OK: national_timeseries.csv (%d rows), municipal_r06.csv (%d rows, %d jurisdictions)",
                nrow(national_ts), nrow(municipal), n_distinct(municipal$jurisdiction)))
message("検証：自治体合計 = PDF合計行 = 全国値（犬の引取り・返還＋譲渡・殺処分）")
