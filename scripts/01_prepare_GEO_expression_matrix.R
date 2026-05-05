############################################################
# BioResearch-Agent
# Script: 01_prepare_GEO_expression_matrix.R
#
# 功能：
#   这是一个 GEO 表达矩阵整理模板脚本。
#   用于演示 BioResearch-Agent 如何辅助医学科研初学者
#   完成表达矩阵读取、基因名整理、重复基因合并和样本分组文件创建。
#
# 注意：
#   这个脚本是模板，不是针对某一个固定数据集的最终版本。
#   实际使用时，需要根据具体 GEO 数据集的文件名、分组信息和基因注释方式进行修改。
############################################################


## =========================================================
## 1. 设置项目目录
## =========================================================

# 使用前请把工作目录设置为你的项目根目录。
# 例如：
# setwd("D:/example/claude/GSE126848")

cat("Step 1: 请先确认当前工作目录是否为项目根目录。\n")
cat("当前工作目录是：", getwd(), "\n\n")


## =========================================================
## 2. 加载需要的 R 包
## =========================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("当前缺少 R 包：", pkg)
    message("请先运行 install.packages('", pkg, "') 进行安装。")
  }
}

library(readr)
library(dplyr)
library(tibble)

cat("Step 2: R 包检查完成。\n\n")


## =========================================================
## 3. 设置输入和输出文件夹
## =========================================================

# 约定：
# 00_raw    用于存放原始数据
# 02_output 用于存放整理后的结果

raw_dir <- "00_raw"
output_dir <- "02_output"

if (!dir.exists(raw_dir)) {
  dir.create(raw_dir, recursive = TRUE)
  cat("已创建原始数据文件夹：", raw_dir, "\n")
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  cat("已创建输出结果文件夹：", output_dir, "\n")
}

cat("Step 3: 文件夹检查完成。\n")
cat("原始数据文件夹：", raw_dir, "\n")
cat("输出结果文件夹：", output_dir, "\n\n")


## =========================================================
## 4. 读取表达矩阵
## =========================================================

# 这里假设原始表达矩阵文件名为 expression_matrix_raw.csv
# 实际使用时，可以让 AI Agent 根据用户的真实文件名自动修改这里。

expression_file <- file.path(raw_dir, "expression_matrix_raw.csv")

if (!file.exists(expression_file)) {
  stop(
    "没有找到表达矩阵文件：", expression_file, "\n",
    "请把原始表达矩阵放入 00_raw 文件夹，并命名为 expression_matrix_raw.csv。\n",
    "如果你的文件名不同，请修改 expression_file 这一行。"
  )
}

expr_raw <- read_csv(expression_file)

cat("Step 4: 表达矩阵读取成功。\n")
cat("原始矩阵维度：", nrow(expr_raw), "行，", ncol(expr_raw), "列。\n\n")


## =========================================================
## 5. 初步检查表达矩阵结构
## =========================================================

cat("表达矩阵前几列列名如下：\n")
print(head(colnames(expr_raw)))

cat("\n表达矩阵前几行如下：\n")
print(head(expr_raw))

# 默认认为第一列是基因名或基因 ID。
# 实际项目中，可能是 gene_symbol、GeneID、ENSEMBL、probe_id 等。
gene_col <- colnames(expr_raw)[1]

cat("\n默认识别第一列为基因列：", gene_col, "\n\n")


## =========================================================
## 6. 清理基因名
## =========================================================

# 去除基因名为空的行。
expr_clean <- expr_raw %>%
  filter(!is.na(.data[[gene_col]])) %>%
  filter(.data[[gene_col]] != "")

cat("Step 6: 已去除基因名为空的行。\n")
cat("去除空基因名后剩余：", nrow(expr_clean), "行。\n\n")


## =========================================================
## 7. 合并重复基因
## =========================================================

# 很多 GEO 数据中，一个 gene symbol 可能对应多行。
# 常见处理方式是对重复基因取平均表达值。
# 注意：这只是常用模板，实际研究中也可以选择最大表达量或其他方法。

expr_clean <- expr_clean %>%
  group_by(.data[[gene_col]]) %>%
  summarise(
    across(
      where(is.numeric),
      ~ mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

expr_clean <- as.data.frame(expr_clean)
rownames(expr_clean) <- expr_clean[[gene_col]]
expr_clean[[gene_col]] <- NULL

cat("Step 7: 重复基因已合并。\n")
cat("整理后矩阵维度：", nrow(expr_clean), "个基因，", ncol(expr_clean), "个样本。\n\n")


## =========================================================
## 8. 创建样本分组文件模板
## =========================================================

# 这里先自动创建一个样本分组模板。
# group 默认为 unknown，后续需要用户根据 GEO 样本信息手动或自动修改为 control / disease 等。

sample_names <- colnames(expr_clean)

sample_group <- data.frame(
  sample_id = sample_names,
  group = "unknown"
)

cat("Step 8: 样本分组模板已创建。\n")
cat("请根据真实样本信息，把 group 列修改为 control、disease 或其他分组名称。\n\n")


## =========================================================
## 9. 输出整理结果
## =========================================================

expression_output <- file.path(output_dir, "expression_matrix_clean.csv")
group_output <- file.path(output_dir, "sample_group.csv")

write.csv(
  expr_clean,
  file = expression_output,
  quote = FALSE
)

write.csv(
  sample_group,
  file = group_output,
  row.names = FALSE,
  quote = FALSE
)

cat("Step 9: 输出文件生成成功。\n")
cat("1. 整理后的表达矩阵：", expression_output, "\n")
cat("2. 样本分组模板：", group_output, "\n\n")


## =========================================================
## 10. 输出分析日志
## =========================================================

log_file <- file.path(output_dir, "analysis_log.txt")

log_text <- c(
  "BioResearch-Agent GEO expression matrix preparation log",
  paste0("Run time: ", Sys.time()),
  paste0("Input file: ", expression_file),
  paste0("Gene column: ", gene_col),
  paste0("Clean matrix genes: ", nrow(expr_clean)),
  paste0("Clean matrix samples: ", ncol(expr_clean)),
  paste0("Expression output: ", expression_output),
  paste0("Group output: ", group_output),
  "Note: sample_group.csv is only a template. Please modify group labels according to real metadata."
)

writeLines(log_text, con = log_file)

cat("Step 10: 分析日志已生成：", log_file, "\n\n")

cat("GEO 表达矩阵整理流程完成。\n")
