# R/00_setup.R
# 初回だけ実行する。プロジェクト専用のパッケージ環境（renv）を作り、
# 必要なパッケージを入れて、そのバージョンを renv.lock に記録する。
# 再現性（reproducibility）のため：他の人は renv::restore() で同じ環境を再現できる。
#
# 注意：スクリプト全体を一度に実行しないこと。
# STEP 1 のあとにRセッションが再起動するので、STEPごとに選択して実行する（Cmd + Enter）。

# ---- STEP 1：renvを入れて、プロジェクト専用のライブラリを作る -------------
install.packages("renv")
renv::init(bare = TRUE)
# → 「Do you want to proceed?」には y
# → 「please restart the R session」と出たら Cmd + Shift + 0 で再起動

# ---- STEP 2：再起動後、パッケージを入れる（数分かかる） ---------------------
install.packages(c(
  "tidyverse",  # データ整形・可視化（dplyr, tidyr, ggplot2, readr, stringr）
  "pdftools",   # PDFからテキストを抽出
  "janitor",    # 列名の整理、合計行のチェック
  "duckdb",     # RからSQLで集計（サーバー不要）
  "MASS",       # glm.nb（Negative Binomial回帰）
  "quarto",     # レポートのレンダリング
  "shiny",      # ダッシュボード（任意）
  "here"        # プロジェクトのルートからファイルの場所を指定する
))

# ---- STEP 3：使ったパッケージとバージョンを renv.lock に記録する ------------
renv::snapshot()
