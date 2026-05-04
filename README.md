# TjSuper Super.Table1 客户教程

`TjSuper::TjssnStat()` 是面向医学统计场景的统一分析接口。  
`Super.Table1` 主要用于生成医学论文、临床研究、心理学、公卫、护理、检验等场景中的 Table 1。

适用功能包括：

- 描述性统计
- 组间差异分析
- 连续变量 / 分类变量自动识别
- 参数法 / 非参数法自动判断
- SMD 与 SMD 95%CI
- 两两比较
- 加权分析
- Quick.Check 自动质控日志
- wide / long 两种结果布局

---

## 1. 快速开始

最短代码如下：

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("NUM1", "NUM2", "NUM3", "CHR2", "CHR3"),
    col_vars = "CHR1"
  )
)

RES <- RESSUB$result$table$model.res
RES$Result$CHR1$cross_table
cat(RESSUB$result$log.print$Quick.Check.Text)
```

---

## 2. 结果结构

常用结果读取方式：

| 对象 | 含义 |
|---|---|
| `RESSUB$result$table$model.res` | 统计主结果 |
| `RESSUB$result$log.print$Quick.Check.Text` | Quick.Check 文本日志 |
| `RES$Result[[col_var]]$cross_table` | wide 宽表结果 |
| `RES$CrossTable` | long 拼接总表 |
| `RES$Pairs` | 实际分析组合 |

---

## 3. 四大参数块

`Super.Table1` 推荐按四个参数块理解：

| 参数块 | 作用 | 常用参数 |
|---|---|---|
| `data_param` | 数据输入 | `data` |
| `var_param` | 变量设定 | `row_vars`, `col_vars`, `formula_var`, `weight_var` |
| `model_param` | 统计逻辑 | `num_test_method`, `compare`, `self_pair`, `weight_type` |
| `output_param` | 输出制表 | `cross_table_layout`, `show_smd_ci`, `digits`, `p_digits` |

---

## 4. data_param：数据输入

| 参数 | 类型 | 说明 |
|---|---|---|
| `data` | `data.frame` | 输入待分析数据集，必填 |

示例：

```r
data_param = list(
  data = df
)
```

---

## 5. var_param：变量设定

| 参数 | 作用 | 示例 |
|---|---|---|
| `row_vars` | 被描述变量 | 连续变量、分类变量均可 |
| `col_vars` | 分组变量 | 治疗组、性别、分型、时间点 |
| `formula_var` | 精确指定 row ~ col 组合 | 只分析指定组合 |
| `weight_var` | 权重变量 | IPW、抽样权重、频数权重 |

示例：

```r
var_param = list(
  row_vars = c("NUM1", "NUM2", "CHR2", "CHR3"),
  col_vars = "CHR1"
)
```

---

## 6. row_vars 与 col_vars 的组合策略

### 6.1 标准 Table 1

多个 `row_vars` 加一个 `col_vars`，生成最常见的医学基线表。

```r
var_param = list(
  row_vars = c("age", "bmi", "sex", "diabetes"),
  col_vars = "group"
)
```

### 6.2 多个分组变量

多个 `col_vars` 时，可分别生成多个分组表。

```r
var_param = list(
  row_vars = c("NUM1", "NUM2", "CHR2", "CHR3"),
  col_vars = c("CHR1", "CHR2")
)
```

### 6.3 精确组合

如果只想分析指定的 row ~ col 组合，可以使用 `formula_var`。

```r
var_param = list(
  formula_var = c(
    "NUM1 ~ CHR1",
    "NUM2 ~ CHR2",
    "CHR3 ~ CHR1"
  )
)
```

---

## 7. self_pair：自配对处理

当 `row_var == col_var` 时，默认会自动跳过，避免无意义分析。

| 参数 | 选项 | 含义 |
|---|---|---|
| `self_pair` | `skip` | 默认跳过 row_var == col_var |
| `self_pair` | `keep` | 保留自配对组合 |
| `self_pair` | `error` | 遇到自配对直接报错 |

示例：

```r
model_param = list(
  self_pair = "skip"
)
```

---

## 8. wide / long 输出布局

### 8.1 wide：多个宽表

`wide` 适合 Word 论文表格展示。  
多个 `col_vars` 会分别返回多个宽表，不强行拼接。

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("NUM1", "NUM2", "CHR2", "CHR3"),
    col_vars = c("CHR1", "CHR2")
  ),

  output_param = list(
    cross_table_layout = "wide"
  )
)

RES <- RESSUB$result$table$model.res
RES$Result$CHR1$cross_table
RES$Result$CHR2$cross_table
```

### 8.2 long：拼接总表

`long` 适合 Excel 导出、批量汇总和纵向阅读。

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("NUM1", "NUM2", "CHR2", "CHR3"),
    col_vars = c("CHR1", "CHR2")
  ),

  output_param = list(
    cross_table_layout = "long",
    cross_table_stack = "all",
    long_show_group_var = TRUE
  )
)

RES <- RESSUB$result$table$model.res
RES$CrossTable
```

---

## 9. 核心选项速查

| 参数 | 选项 | 含义 / 使用场景 |
|---|---|---|
| `cross_table_layout` | `wide` | 多个 `col_vars` 分别返回多个宽表 |
| `cross_table_layout` | `long` | 多个 `col_vars` 可拼接为总表 |
| `cross_table_stack` | `all` | 所有 row ~ col 组合合并成总长表 |
| `num_test_method` | `auto` | 自动根据正态性和方差齐性选择方法 |
| `num_test_method` | `parametric` | 强制 t 检验 / ANOVA 等参数法 |
| `num_test_method` | `nonparametric` | 强制 Wilcoxon / Kruskal-Wallis 等非参数法 |
| `fct_percent` | `row` / `col` | 分类变量百分比方向 |
| `fct_include_missing` | `TRUE` / `FALSE` | 分类缺失是否入表 |
| `alpha` | `0.05` | 显著性阈值 |

---

## 10. 统计检验方法

连续变量检验方法可自动判断，也可手动指定。

| 参数 | 说明 |
|---|---|
| `num_test_method` | 连续变量检验方法 |
| `num_norm_p` | 正态性判断阈值 |
| `num_var_p` | 方差齐性判断阈值 |
| `alpha` | 显著性阈值 |

示例：

```r
model_param = list(
  num_test_method = "auto",
  num_norm_p = 0.05,
  num_var_p = 0.05,
  alpha = 0.05
)
```

---

## 11. 分类变量设置

| 参数 | 说明 |
|---|---|
| `fct_percent` | 百分比方向，可选 `row` 或 `col` |
| `fct_include_missing` | 缺失值是否作为一类进入表格 |
| `fct_test_method` | 分类变量检验方法 |

示例：

```r
model_param = list(
  fct_percent = "col",
  fct_include_missing = TRUE
)
```

---

## 12. 制表格式参数

常用输出格式参数：

| 参数 | 作用 |
|---|---|
| `digits` | 数值小数位 |
| `stat_digits` | 统计量小数位 |
| `p_digits` | P 值小数位 |
| `smd_digits` | SMD 小数位 |
| `header_pct_digits` | 表头百分比小数位 |
| `label_stat` | 统计量标签 |
| `label_unit` | 单位标签 |
| `row_var_labels` | 变量显示名 |

示例：

```r
output_param = list(
  digits = 2,
  p_digits = 3,
  smd_digits = 3,
  show_smd = TRUE,
  show_smd_ci = TRUE
)
```

---

## 13. 自定义模板

高级制表参数：

| 参数 | 作用 |
|---|---|
| `num_style_define` | 自定义连续变量模板 |
| `num_summary` | 指定连续变量输出项 |
| `fct_style_define` | 自定义分类变量模板 |
| `fct_summary` | 指定分类变量输出项 |
| `long_stat_from_var_label` | long 表中多统计量转多列 |

---

## 14. 两两比较

适合多组比较后进行组间两两比较。

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("NUM12", "NUM13", "NUM15", "CHR2", "CHR3"),
    col_vars = "CHR1"
  ),

  model_param = list(
    compare = TRUE,
    pairwise_policy = "if_overall_sig",
    pairwise_method = "auto",
    adjust_method = "holm"
  )
)
```

常用参数：

| 参数 | 含义 |
|---|---|
| `compare` | 是否启用两两比较 |
| `pairwise_policy` | 何时进行两两比较 |
| `pairwise_method` | 两两比较方法 |
| `adjust_method` | 多重比较校正方法 |

---

## 15. 加权分析

适合 IPW、抽样权重、频数权重等场景。

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("NUM12", "NUM13", "CHR2", "CHR3"),
    col_vars = "CHR11",
    weight_var = "wt_sampling"
  ),

  model_param = list(
    weight_type = "frequency",
    weight_scale = "normalize",
    weighted_desc = TRUE,
    weighted_test = TRUE,
    weighted_smd = TRUE
  ),

  output_param = list(
    show_smd = TRUE,
    show_smd_ci = TRUE
  )
)
```

常用参数：

| 参数 | 含义 |
|---|---|
| `weight_var` | 权重变量 |
| `weight_type` | 权重类型，如 `frequency` / `ipw` / `survey` |
| `weight_scale` | 权重缩放方式 |
| `weighted_desc` | 是否进行加权描述 |
| `weighted_test` | 是否进行加权检验 |
| `weighted_smd` | 是否计算加权 SMD |

---

## 16. SMD 与 SMD 95%CI

SMD 常用于基线均衡性评价，特别适合倾向评分匹配、IPW 和观察性研究。

```r
output_param = list(
  show_smd = TRUE,
  show_smd_ci = TRUE
)
```

---

## 17. 医学统计常见场景

| 场景 | 推荐参数 |
|---|---|
| 临床基线表 | `row_vars = 基线指标`, `col_vars = 治疗组`, `show_smd = TRUE` |
| 心理学量表 | 量表分数常用 `auto` 或 `nonparametric` |
| 公卫调查 | `weight_var` + `weight_type = survey` / `frequency` |
| 护理干预 | `compare = TRUE`, `show_overall = TRUE` |
| 检验指标 | 多组比较 + `pairwise_policy = if_overall_sig` |
| IPW 平衡性表 | `weighted_smd = TRUE`, `show_smd_ci = TRUE` |

---

## 18. Quick.Check 自动质控日志

服务器端会返回 `Quick.Check` 和 `Quick.Check.Text`。  
推荐客户直接使用 `cat()` 输出文本日志。

```r
cat(RESSUB$result$log.print$Quick.Check.Text)

RESSUB$result$log.print$Quick.Check$Significant
RESSUB$result$log.print$Quick.Check$Non.Significant
RESSUB$result$log.print$Quick.Check$No.P.Value
```

日志通常包含：

| 模块 | 内容 |
|---|---|
| `Overview` | 分析组合数、成功数、失败数 |
| `Significant` | 有统计学意义变量 |
| `Non.Significant` | 无统计学意义变量 |
| `No.P.Value` | 无可评价 P 值变量 |
| `Issues` | 错误与异常信息 |

---

## 19. 推荐完整模板

```r
RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",

  data_param = list(
    data = df
  ),

  var_param = list(
    row_vars = c("age", "bmi", "sex", "diabetes"),
    col_vars = "group"
  ),

  model_param = list(
    num_test_method = "auto",
    compare = TRUE,
    pairwise_policy = "if_overall_sig",
    adjust_method = "holm"
  ),

  output_param = list(
    cross_table_layout = "wide",
    show_smd = TRUE,
    show_smd_ci = TRUE,
    digits = 2,
    p_digits = 3
  )
)

RES <- RESSUB$result$table$model.res

RES$Result$group$cross_table

cat(RESSUB$result$log.print$Quick.Check.Text)
```

---

## 20. 常见问题

### 为什么 GitHub README 不能显示 React 页面？

因为 GitHub README 只支持 Markdown 和部分 HTML，不会执行 React 代码。

以下内容不能直接放进 README：

```js
import React from "react";
import { motion } from "framer-motion";
import { Activity } from "lucide-react";

export default function App() {
  return <div>Hello</div>;
}
```

如果要做 React 网页，请使用 Vite / React 项目，并部署到 GitHub Pages。

### README 里应该放什么？

README 应该放：

- 项目介绍
- 安装方法
- 快速开始代码
- 参数说明
- 示例
- 常见问题

不应该直接放完整 React 组件代码。

---

## 21. 本地开发建议

如果后续要做成漂亮网页，推荐使用以下结构：

```text
lib/
  README.md
  package.json
  index.html
  src/
    main.jsx
    App.jsx
    index.css
```

React 页面代码应放在：

```text
src/App.jsx
```

而不是：

```text
README.md
```

---

## 22. 安装与运行示例

如果做 React 文档站，可使用：

```bash
npm install
npm run dev
```

如果只是 GitHub README 文档，则无需安装，直接维护本文件即可。

---

## License

Internal documentation for TjSuper Super.Table1.
