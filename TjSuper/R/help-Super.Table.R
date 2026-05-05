#' Stat = "Super.Table" 参数说明
#'
#' @name TjssnStat-Super.Table
#' @aliases Super.Table TjssnStat_Super_Table
#'
#' @title TjssnStat 中 Super.Table 的参数说明
#'
#' @description
#' 当 `TjssnStat()` 的 `Stat = "Super.Table"` 时，客户端会将用户输入的
#' `data_param`、`var_param`、`model_param` 和 `output_param` 打包上传，
#' 服务器端根据 `Stat = "Super.Table"` 调用对应的 Table 1 分析脚本，
#' 并返回标准化结果对象。
#'
#' `Super.Table` 主要用于医学研究、临床基线表、心理学量表、公卫调查、
#' 护理研究、检验指标比较、IPW/加权平衡性表等场景。它可以自动处理
#' 连续变量和分类变量，支持描述性统计、组间差异检验、参数法/非参数法
#' 自动判断、两两比较、SMD、SMD 置信区间、加权分析、wide/long 表格输出
#' 和 Quick.Check 自动质控日志。
#'
#' @section 一句话理解:
#'
#' `TjssnStat()` 是统一入口；`Stat = "Super.Table"` 告诉服务器：
#' 本次任务是生成 Table 1 或分组比较表。
#'
#' 客户最常写的是：
#'
#' \preformatted{
#' RESSUB <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("age", "bmi", "sex"),
#'     col_vars = "group"
#'   )
#' )
#' }
#'
#' @section 推荐学习顺序:
#'
#' \enumerate{
#'   \item 先跑通最小调用：`data_param$data` + `row_vars` + `col_vars`。
#'   \item 再学习 `row_vars` 和 `col_vars` 的组合策略。
#'   \item 然后学习 `wide` / `long` 输出。
#'   \item 再学习检验方法、缺失值、总体列、百分比方向。
#'   \item 最后学习两两比较、SMD、加权分析和 Word 输出。
#' }
#'
#' @section 参数体系:
#'
#' `Super.Table` 推荐按四个参数块理解：
#'
#' \describe{
#'   \item{`data_param`}{数据输入。主要放数据框。}
#'   \item{`var_param`}{变量设定。主要放 `row_vars`、`col_vars`、`formula_var`、变量标签和权重变量。}
#'   \item{`model_param`}{统计逻辑。主要控制检验方法、两两比较、缺失值、总体列和加权逻辑。}
#'   \item{`output_param`}{输出格式。主要控制小数位、SMD、表格样式、wide/long 布局和语言。}
#' }
#'
#' @section data_param 数据输入:
#'
#' \describe{
#'   \item{`data`}{必填。待分析数据，必须是 `data.frame` 或可以转换为 `data.frame` 的对象。}
#' }
#'
#' 数据要求：
#'
#' \itemize{
#'   \item 变量名必须和 `row_vars`、`col_vars`、`formula_var`、`weight_var` 中写的一致。
#'   \item 分组变量建议提前转成 `factor`，这样水平顺序和参考组更稳定。
#'   \item 连续变量应保持 numeric；分类变量建议使用 character 或 factor。
#'   \item 正式分析前可先用 `SuperView()` 查看变量类型、缺失和水平数。
#' }
#'
#' 示例：
#'
#' \preformatted{
#' data_param = list(
#'   data = df
#' )
#' }
#'
#' @section var_param 变量设定:
#'
#' \describe{
#'   \item{`row_vars`}{字符向量。放在表格行里的变量，也就是被描述或被比较的变量。可以包含连续变量和分类变量。}
#'   \item{`col_vars`}{字符向量。分组变量，例如治疗组、性别、疾病分型、暴露组、时间点等。}
#'   \item{`formula_var`}{字符向量或列表。用于精确指定分析组合，例如 `"age + bmi ~ group"` 或 `"sex ~ group"`。}
#'   \item{`row_var_labels`}{命名向量或命名列表。控制表格中变量显示名。}
#'   \item{`col_level_order`}{命名列表。控制分组变量水平显示顺序。}
#'   \item{`weight_var`}{字符型。权重变量名，用于频数权重、IPW、抽样权重等加权分析。}
#' }
#'
#' `Super.Table` 至少需要满足以下二者之一：
#'
#' \itemize{
#'   \item 提供 `row_vars` 和 `col_vars`；
#'   \item 提供 `formula_var`。
#' }
#'
#' 批量组合示例：
#'
#' \preformatted{
#' var_param = list(
#'   row_vars = c("age", "bmi", "sex", "diabetes"),
#'   col_vars = "group"
#' )
#' }
#'
#' 精确组合示例：
#'
#' \preformatted{
#' var_param = list(
#'   formula_var = c(
#'     "age + bmi ~ group",
#'     "sex ~ group"
#'   )
#' )
#' }
#'
#' 多个分组变量示例：
#'
#' \preformatted{
#' var_param = list(
#'   row_vars = c("age", "bmi", "sex"),
#'   col_vars = c("group", "sex")
#' )
#' }
#'
#' @section model_param 统计逻辑:
#'
#' `model_param` 会影响统计检验、两两比较、缺失值处理和加权分析结果。
#'
#' \describe{
#'   \item{`compare`}{逻辑值。是否启用两两比较。默认 `FALSE`。}
#'   \item{`num_compare`}{连续变量是否启用两两比较。若为 `NULL`，通常继承 `compare`。}
#'   \item{`fct_compare`}{分类变量是否启用两两比较。若为 `NULL`，通常继承 `compare`。}
#'
#'   \item{`ref_group`}{参考组。可用于指定各组与某一参考组比较。可以是单个值，也可以是命名向量。}
#'   \item{`num_ref_group`}{连续变量参考组。}
#'   \item{`fct_ref_group`}{分类变量参考组。}
#'
#'   \item{`adjust_method`}{多重比较校正方法，默认 `"BH"`。常见取值包括 `"none"`、`"bonferroni"`、`"holm"`、`"BH"`、`"BY"`。}
#'   \item{`num_adjust_method`}{连续变量多重比较校正方法。}
#'   \item{`fct_adjust_method`}{分类变量多重比较校正方法。}
#'
#'   \item{`alpha`}{显著性阈值，默认 `0.05`。}
#'   \item{`num_alpha`}{连续变量显著性阈值。}
#'   \item{`fct_alpha`}{分类变量显著性阈值。}
#'
#'   \item{`pairwise_policy`}{两两比较触发策略。可选 `"if_overall_sig"`、`"always"`、`"never"`。默认 `"if_overall_sig"`。}
#'   \item{`pairwise_method`}{两两比较方法。可选 `"auto"`、`"legacy"` 或服务器支持的具体方法。}
#'   \item{`force_pairwise`}{强制指定某些变量进行两两比较。}
#'   \item{`pairwise_label_no_sig`}{无显著性时是否显示两两比较标记，默认 `FALSE`。}
#'
#'   \item{`col_missing`}{分组变量缺失值处理方式，默认 `"drop"`。}
#'   \item{`col_missing_label`}{分组变量缺失值显示标签。}
#'
#'   \item{`show_overall`}{是否显示总体列，默认 `TRUE`。}
#'   \item{`num_show_overall`}{连续变量是否显示总体列。}
#'   \item{`fct_show_overall`}{分类变量是否显示总体列。}
#'
#'   \item{`fct_include_missing`}{分类变量缺失值是否作为一个水平入表，默认 `TRUE`。}
#'   \item{`fct_missing_label`}{分类变量缺失值标签，默认 `"Auto-Missing"`。}
#'
#'   \item{`num_test_method`}{连续变量检验方法。可选 `"auto"`、`"parametric"`、`"nonparametric"`。默认 `"auto"`。}
#'   \item{`num_test_var_method`}{按变量指定连续变量检验方法，例如 `c(age = "parametric", bmi = "nonparametric")`。}
#'   \item{`num_norm_p`}{正态性判断阈值，默认 `0.01`。}
#'   \item{`num_var_p`}{方差齐性判断阈值，默认 `0.01`。}
#'
#'   \item{`weight_type`}{权重类型，默认 `"frequency"`。常见取值包括 `"frequency"`、`"ipw"`、`"survey"`。}
#'   \item{`weight_scale`}{权重缩放方式，默认 `"none"`。常见取值包括 `"none"`、`"normalize"`、`"group_normalize"`。}
#'   \item{`weighted_desc`}{是否进行加权描述统计，默认 `TRUE`。}
#'   \item{`weighted_test`}{是否进行加权统计检验，默认 `TRUE`。}
#'   \item{`weighted_smd`}{是否计算加权 SMD，默认 `TRUE`。}
#'   \item{`weighted_var_type`}{加权方差类型。可选 `"unbiased"` 或 `"population"`。}
#' }
#'
#' @section output_param 输出格式:
#'
#' `output_param` 主要影响展示格式，不建议把真正会改变统计检验逻辑的参数放在这里。
#'
#' \describe{
#'   \item{`show_overall_n`}{是否显示总体样本量，默认 `TRUE`。}
#'
#'   \item{`show_smd`}{是否显示 SMD，默认 `FALSE`。}
#'   \item{`num_show_smd`}{连续变量是否显示 SMD。}
#'   \item{`fct_show_smd`}{分类变量是否显示 SMD。}
#'
#'   \item{`show_smd_ci`}{是否显示 SMD 置信区间，默认 `FALSE`。}
#'   \item{`smd_ci_level`}{SMD 置信区间水平，默认 `0.95`。}
#'   \item{`smd_ci_method`}{SMD 置信区间方法。可选 `"auto"`、`"analytic"`、`"bootstrap"`。}
#'   \item{`smd_ci_bootstrap`}{bootstrap 次数，默认 `500`。}
#'   \item{`smd_ci_seed`}{bootstrap 随机种子，默认 `20240503`。}
#'
#'   \item{`digits`}{通用小数位数，默认 `3`。}
#'   \item{`num_digits`}{连续变量小数位数。}
#'   \item{`fct_digits`}{分类变量小数位数。}
#'   \item{`stat_digits`}{检验统计量小数位数，默认 `3`。}
#'   \item{`p_digits`}{P 值小数位数，默认 `3`。}
#'   \item{`smd_digits`}{SMD 小数位数，默认 `3`。}
#'   \item{`smd_ci_digits`}{SMD 置信区间小数位数。}
#'
#'   \item{`label_stat`}{是否显示统计量标签，默认 `TRUE`。}
#'   \item{`label_unit`}{是否显示单位标签，默认 `FALSE`。}
#'   \item{`num_unit`}{连续变量单位，通常使用命名向量。}
#'   \item{`fct_unit`}{分类变量单位。}
#'
#'   \item{`fct_percent`}{分类变量百分比方向。可选 `"row"` 或 `"col"`。医学基线表常用 `"col"`。}
#'   \item{`fct_style`}{分类变量显示样式，默认 `"default"`。}
#'   \item{`fct_style_define`}{自定义分类变量显示模板。}
#'   \item{`fct_summary`}{指定分类变量输出项。}
#'
#'   \item{`num_style`}{连续变量显示样式，常见取值包括 `"compact"`、`"journal"`、`"strict_iqr"`、`"legacy"`。}
#'   \item{`num_style_define`}{自定义连续变量显示模板。}
#'   \item{`num_summary`}{指定连续变量输出项。}
#'
#'   \item{`show_weighted_n`}{是否显示加权样本量。}
#'   \item{`show_unweighted_n`}{是否显示未加权样本量。}
#'   \item{`header_pct_digits`}{表头百分比小数位数。}
#'
#'   \item{`cross_table_layout`}{交叉表布局。可选 `"wide"` 或 `"long"`。默认 `"wide"`。}
#'   \item{`cross_table_stack`}{long 表拼接方式。可选 `"auto"`、`"by_col"`、`"by_pair"`、`"all"`。}
#'   \item{`long_show_group_var`}{long 布局是否显示分组变量。}
#'   \item{`long_stat_col`}{long 布局统计量列名，默认 `"集中趋势"`。}
#'   \item{`long_n_col`}{long 布局样本量或百分比列名。}
#'   \item{`long_repeat_variable`}{long 布局是否重复显示变量名。}
#'   \item{`long_repeat_group_var`}{long 布局是否重复显示分组变量。}
#'   \item{`long_keep_var_label`}{long 布局是否保留变量标签。}
#'   \item{`long_add_blank_row`}{long 布局是否增加空白行。}
#'   \item{`long_stat_from_var_label`}{是否从变量标签生成 long 表统计量列。可选 `"auto"`、`"always"`、`"never"`。}
#'   \item{`long_drop_empty_stat_cols`}{long 布局是否删除空统计列。}
#'
#'   \item{`lang`}{输出语言，默认 `"en"`。}
#'   \item{`verbose`}{是否输出详细信息。调试时可设为 `TRUE`。}
#'   \item{`max_show_vars`}{最多展示变量数，默认 `100`。}
#' }
#'
#' @section 结果对象结构:
#'
#' `TjssnStat(Stat = "Super.Table")` 通常返回一个列表对象。常用路径包括：
#'
#' \describe{
#'   \item{`RESSUB$result$table$model.res`}{核心统计结果对象。}
#'   \item{`RESSUB$result$table$model.res$Result`}{按分组变量组织的结果列表。}
#'   \item{`RESSUB$result$table$model.res$Result[[col_var]]$cross_table`}{某个分组变量对应的主表。}
#'   \item{`RESSUB$result$table$model.res$CrossTable`}{long 布局或拼接总表。}
#'   \item{`RESSUB$result$table$model.res$Summary`}{分析状态汇总。}
#'   \item{`RESSUB$result$table$model.res$Pairs`}{实际分析的 row ~ col 组合。}
#'   \item{`RESSUB$result$output.infor$word.text.res`}{用于 Word 报告的自动说明文字。}
#'   \item{`RESSUB$result$log.print$Quick.Check.Text`}{Quick.Check 自动质控日志文本。}
#' }
#'
#' 推荐提取主结果：
#'
#' \preformatted{
#' RES <- RESSUB$result$table$model.res
#' TAB <- RES$Result[[col_var]]$cross_table
#' cat(RESSUB$result$log.print$Quick.Check.Text)
#' }
#'
#' @section 案例 1：最短代码，先跑通:
#'
#' \preformatted{
#' data("TjSuper_df")
#'
#' RESSUB_01 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM1", "NUM2", "NUM3", "CHR2", "CHR3"),
#'     col_vars = "CHR1"
#'   )
#' )
#'
#' RES_01 <- RESSUB_01$result$table$model.res
#' TAB_01 <- RES_01$Result$CHR1$cross_table
#' cat(RESSUB_01$result$log.print$Quick.Check.Text)
#' }
#'
#' @section 案例 2：多个分组变量，输出 long 总表:
#'
#' 多个 `col_vars` 时，`long` 更适合汇总、导出 Excel 或批量阅读。
#'
#' \preformatted{
#' RESSUB_02 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM1", "NUM2", "NUM3", "CHR2", "CHR3"),
#'     col_vars = c("CHR1", "CHR11")
#'   ),
#'
#'   output_param = list(
#'     cross_table_layout = "long",
#'     cross_table_stack = "all",
#'     long_show_group_var = TRUE,
#'     long_stat_col = "集中趋势",
#'     long_n_col = "N(%)"
#'   )
#' )
#'
#' RES_02 <- RESSUB_02$result$table$model.res
#' TAB_02_LONG <- RES_02$CrossTable
#' }
#'
#' @section 案例 3：formula_var 精确指定组合:
#'
#' `formula_var` 适合只分析指定的 row ~ col 组合，而不是展开所有组合。
#'
#' \preformatted{
#' RESSUB_03 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     formula_var = c(
#'       "NUM1 + NUM2 + NUM3 ~ CHR1",
#'       "CHR3 ~ CHR2"
#'     )
#'   ),
#'
#'   output_param = list(
#'     cross_table_layout = "wide"
#'   )
#' )
#'
#' RES_03 <- RESSUB_03$result$table$model.res
#' RES_03$Pairs
#' }
#'
#' @section 案例 4：连续变量检验方法控制:
#'
#' \preformatted{
#' RESSUB_04 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM1", "NUM2", "NUM3", "NUM4", "NUM5"),
#'     col_vars = "CHR1"
#'   ),
#'
#'   model_param = list(
#'     num_test_method = "auto",
#'     num_test_var_method = c(
#'       NUM1 = "parametric",
#'       NUM2 = "nonparametric",
#'       NUM3 = "auto"
#'     ),
#'     num_norm_p = 0.01,
#'     num_var_p = 0.01
#'   )
#' )
#'
#' RES_04 <- RESSUB_04$result$table$model.res
#' TAB_04 <- RES_04$Result$CHR1$cross_table
#' TEST_04 <- RES_04$Result$CHR1$test_info
#' }
#'
#' @section 案例 5：分类变量百分比和缺失值:
#'
#' \preformatted{
#' RESSUB_05 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("CHR2", "CHR3"),
#'     col_vars = "CHR1"
#'   ),
#'
#'   model_param = list(
#'     fct_include_missing = TRUE,
#'     fct_missing_label = "Missing",
#'     show_overall = TRUE
#'   ),
#'
#'   output_param = list(
#'     fct_percent = "col",
#'     show_overall_n = TRUE
#'   )
#' )
#'
#' TAB_05 <- RESSUB_05$result$table$model.res$Result$CHR1$cross_table
#' }
#' }
#'
#' @section 案例 6：变量标签、分组顺序、小数位和单位:
#'
#' \preformatted{
#' RESSUB_06 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM1", "NUM2", "NUM3", "CHR2", "CHR3"),
#'     col_vars = "CHR1",
#'
#'     row_var_labels = c(
#'       NUM1 = "年龄",
#'       NUM2 = "身高",
#'       NUM3 = "体重",
#'       CHR2 = "性别",
#'       CHR3 = "疾病分型"
#'     ),
#'
#'     col_level_order = list(
#'       CHR1 = c("Class5", "Class4", "Class3", "Class2", "Class1")
#'     )
#'   ),
#'
#'   output_param = list(
#'     digits = 2,
#'     stat_digits = 2,
#'     p_digits = 3,
#'     show_smd = TRUE,
#'     show_smd_ci = TRUE,
#'     smd_digits = 3,
#'     smd_ci_digits = 3,
#'     label_stat = TRUE,
#'     label_unit = TRUE,
#'     num_unit = c(
#'       NUM1 = "years",
#'       NUM2 = "cm",
#'       NUM3 = "kg"
#'     )
#'   )
#' )
#' }
#'
#' @section 案例 7：SMD 与 SMD 95%CI:
#'
#' \preformatted{
#' RESSUB_07 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM12", "NUM13", "NUM15", "NUM16", "CHR2", "CHR3"),
#'     col_vars = "CHR11"
#'   ),
#'
#'   output_param = list(
#'     show_smd = TRUE,
#'     show_smd_ci = TRUE,
#'     smd_ci_method = "analytic",
#'     smd_ci_level = 0.95,
#'     smd_digits = 3,
#'     smd_ci_digits = 3
#'   )
#' )
#' }
#'
#' Bootstrap 版本：
#'
#' \preformatted{
#' output_param = list(
#'   show_smd = TRUE,
#'   show_smd_ci = TRUE,
#'   smd_ci_method = "bootstrap",
#'   smd_ci_bootstrap = 100,
#'   smd_ci_seed = 20240503
#' )
#' }
#'
#' @section 案例 8：多组两两比较:
#'
#' \preformatted{
#' RESSUB_08 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM12", "NUM13", "NUM15", "NUM16", "CHR2", "CHR3"),
#'     col_vars = "CHR1"
#'   ),
#'
#'   model_param = list(
#'     compare = TRUE,
#'     pairwise_policy = "if_overall_sig",
#'     pairwise_method = "auto",
#'     adjust_method = "holm",
#'     alpha = 0.05,
#'     pairwise_label_no_sig = FALSE
#'   ),
#'
#'   output_param = list(
#'     digits = 2,
#'     p_digits = 3
#'   )
#' )
#'
#' RES_08 <- RESSUB_08$result$table$model.res
#' TAB_08 <- RES_08$Result$CHR1$cross_table
#' PAIR_08 <- RES_08$Result$CHR1$sig_letter_log
#' }
#'
#' 指定参考组：
#'
#' \preformatted{
#' model_param = list(
#'   compare = TRUE,
#'   ref_group = "Class1",
#'   pairwise_policy = "if_overall_sig",
#'   pairwise_method = "auto",
#'   adjust_method = "BH"
#' )
#' }
#'
#' @section 案例 9：加权分析:
#'
#' \preformatted{
#' RESSUB_09 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM12", "NUM13", "NUM15", "NUM16", "CHR2", "CHR3"),
#'     col_vars = "CHR11",
#'     weight_var = "wt_sampling"
#'   ),
#'
#'   model_param = list(
#'     weight_type = "frequency",
#'     weight_scale = "none",
#'     weighted_desc = TRUE,
#'     weighted_test = TRUE,
#'     weighted_smd = TRUE,
#'     weighted_var_type = "unbiased"
#'   ),
#'
#'   output_param = list(
#'     show_smd = TRUE,
#'     show_smd_ci = TRUE,
#'     smd_ci_method = "auto",
#'     show_weighted_n = TRUE,
#'     show_unweighted_n = TRUE
#'   )
#' )
#'
#' RES_09 <- RESSUB_09$result$table$model.res
#' TAB_09 <- RES_09$Result$CHR11$cross_table
#' WEIGHT_09 <- RES_09$Result$CHR11$weight_info
#' }
#'
#' `weight_scale = "normalize"` 适合把总权重标准化到总样本量。
#' `weight_scale = "group_normalize"` 适合把每组权重标准化到该组原始样本量。
#'
#' @section 案例 10：连续变量自定义制表:
#'
#' \preformatted{
#' RESSUB_10 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("NUM12", "NUM13"),
#'     col_vars = "CHR1"
#'   ),
#'
#'   output_param = list(
#'     num_style_define = list(
#'       MyNumStyle = list(
#'         central = "median(q1, q3)",
#'         mean_sd = "mean±sd",
#'         miss_n = "missing.n",
#'         miss_pct = "missing.pct",
#'         min_max = "min-max"
#'       )
#'     ),
#'     num_style = "MyNumStyle",
#'     num_summary = c("central", "mean_sd", "miss_n", "miss_pct", "min_max"),
#'     digits = 2
#'   )
#' )
#' }
#'
#' @section 案例 11：分类变量自定义制表:
#'
#' \preformatted{
#' RESSUB_11 <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = TjSuper_df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("CHR2", "CHR3"),
#'     col_vars = "CHR1"
#'   ),
#'
#'   model_param = list(
#'     fct_include_missing = TRUE,
#'     fct_missing_label = "Missing"
#'   ),
#'
#'   output_param = list(
#'     fct_percent = "row",
#'     fct_style_define = list(
#'       MyFctStyle = list(
#'         n_pct_missing = "n(pct%)/missing.n",
#'         missing_info = "missing.n(missing.pct%)"
#'       )
#'     ),
#'     fct_style = "MyFctStyle",
#'     fct_summary = c("n_pct_missing")
#'   )
#' )
#' }
#'
#' @section 案例 12：Word 报告输出:
#'
#' 服务器返回结果中通常包含用于 Word 报告的表格对象和文字对象。
#'
#' \preformatted{
#' RES <- RESSUB$result$table$model.res
#' TEXT <- RESSUB$result$output.infor$word.text.res
#'
#' SuperWord.Super.Table(
#'   model_list = RES,
#'   report_text = TEXT,
#'   output_path = "Descriptive_Report.docx",
#'   page_orientation = "landscape"
#' )
#' }
#'
#' @section 推荐完整模板:
#'
#' \preformatted{
#' RESSUB <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'
#'   data_param = list(
#'     data = df
#'   ),
#'
#'   var_param = list(
#'     row_vars = c("age", "bmi", "sex", "diabetes"),
#'     col_vars = "group",
#'     row_var_labels = c(
#'       age = "Age",
#'       bmi = "BMI",
#'       sex = "Sex",
#'       diabetes = "Diabetes"
#'     )
#'   ),
#'
#'   model_param = list(
#'     num_test_method = "auto",
#'     compare = TRUE,
#'     pairwise_policy = "if_overall_sig",
#'     pairwise_method = "auto",
#'     adjust_method = "BH",
#'     alpha = 0.05,
#'     fct_include_missing = TRUE,
#'     fct_missing_label = "Missing"
#'   ),
#'
#'   output_param = list(
#'     cross_table_layout = "wide",
#'     fct_percent = "col",
#'     show_overall_n = TRUE,
#'     show_smd = TRUE,
#'     show_smd_ci = TRUE,
#'     digits = 2,
#'     stat_digits = 2,
#'     p_digits = 3,
#'     smd_digits = 3,
#'     lang = "en"
#'   )
#' )
#'
#' RES <- RESSUB$result$table$model.res
#' cat(RESSUB$result$log.print$Quick.Check.Text)
#' }
#'
#' @section 常见问题:
#'
#' \describe{
#'   \item{变量名写错}{`row_vars`、`col_vars`、`weight_var` 中的变量名必须和数据框列名完全一致。}
#'   \item{分类变量没有转成 factor}{虽然服务器可能会自动处理部分情况，但正式分析建议显式转成 factor。}
#'   \item{参考组不存在}{`ref_group` 必须是分组变量真实存在的水平。}
#'   \item{把连续变量放进 col_vars}{`col_vars` 应该是分组变量，不建议放真正的连续指标。}
#'   \item{多个 col_vars 却想看一个总表}{建议使用 `cross_table_layout = "long"` 和 `cross_table_stack = "all"`。}
#'   \item{不知道结果在哪里}{优先查看 `RESSUB$result$table$model.res` 和 `RESSUB$result$log.print$Quick.Check.Text`。}
#' }
#'
#' @section 相关帮助:
#'
#' \itemize{
#'   \item 主函数：\code{\link{TjssnStat}}
#'   \item 所有模块：\code{\link{TjssnStat_topics}}
#'   \item 打开模块帮助：\code{\link{TjssnStat_help}}
#' }
#'
#' @examples
#' \dontrun{
#' # 查看所有 TjssnStat 模块
#' TjssnStat_help()
#'
#' # 打开 Super.Table 参数帮助
#' TjssnStat_help("Super.Table")
#'
#' # 最小调用
#' RESSUB <- TjSuper::TjssnStat(
#'   Stat = "Super.Table",
#'   data_param = list(data = df),
#'   var_param = list(
#'     row_vars = c("age", "bmi", "sex"),
#'     col_vars = "group"
#'   )
#' )
#' }
NULL
