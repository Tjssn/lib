import React, { useMemo, useState } from "react";
import { motion } from "framer-motion";
import {
  Activity,
  BarChart3,
  BookOpen,
  Boxes,
  CheckCircle2,
  ClipboardList,
  Code2,
  Database,
  FileText,
  GitBranch,
  HeartPulse,
  Layers3,
  LineChart,
  ListChecks,
  Menu,
  Network,
  PanelTop,
  Scale,
  Search,
  Settings2,
  ShieldCheck,
  Sparkles,
  Table2,
  Tags,
  Weight,
  X,
} from "lucide-react";

const codeQuickStart = `RESSUB <- TjSuper::TjssnStat(
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
cat(RESSUB$result$log.print$Quick.Check.Text)`;

const codeWideMulti = `RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",
  data_param = list(data = df),
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
RES$Result$CHR2$cross_table`;

const codeLongAll = `RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",
  data_param = list(data = df),
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
RES$CrossTable`;

const codePairwise = `RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",
  data_param = list(data = df),
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
)`;

const codeWeighted = `RESSUB <- TjSuper::TjssnStat(
  Stat = "Super.Table1",
  data_param = list(data = df),
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
)`;

const sections = [
  { id: "home", label: "概览", icon: Sparkles },
  { id: "quick", label: "快速开始", icon: Code2 },
  { id: "params", label: "四大参数块", icon: Boxes },
  { id: "rowcol", label: "row / col 策略", icon: GitBranch },
  { id: "layout", label: "wide / long", icon: Layers3 },
  { id: "tests", label: "统计检验", icon: LineChart },
  { id: "table", label: "制表细节", icon: Table2 },
  { id: "advanced", label: "高级功能", icon: ShieldCheck },
  { id: "scenes", label: "医学场景", icon: HeartPulse },
  { id: "quickcheck", label: "Quick.Check", icon: ClipboardList },
];

const paramBlocks = [
  {
    name: "data_param",
    title: "数据输入",
    icon: Database,
    tone: "from-blue-500 to-cyan-500",
    items: [
      ["data", "data.frame，必填", "输入待分析数据集"],
    ],
  },
  {
    name: "var_param",
    title: "变量设定",
    icon: Network,
    tone: "from-emerald-500 to-teal-500",
    items: [
      ["row_vars", "被描述变量", "连续变量、分类变量均可"],
      ["col_vars", "分组变量", "治疗组、性别、分型、时间点"],
      ["formula_var", "精确组合", "只分析指定 row ~ col"],
      ["weight_var", "权重变量", "IPW、抽样权重、频数权重"],
    ],
  },
  {
    name: "model_param",
    title: "统计逻辑",
    icon: Settings2,
    tone: "from-orange-500 to-amber-500",
    items: [
      ["num_test_method", "auto / parametric / nonparametric", "连续变量检验方法"],
      ["compare", "TRUE / FALSE", "是否启用两两比较"],
      ["self_pair", "skip / keep / error", "处理 row_var == col_var"],
      ["weight_type", "frequency / ipw / survey", "加权分析类型"],
    ],
  },
  {
    name: "output_param",
    title: "输出制表",
    icon: PanelTop,
    tone: "from-violet-500 to-fuchsia-500",
    items: [
      ["cross_table_layout", "wide / long", "宽表或长表"],
      ["show_smd_ci", "TRUE / FALSE", "SMD 95%CI"],
      ["digits / p_digits", "数值与 P 值小数位", "论文表格格式控制"],
      ["num_style_define", "自定义模板", "高级制表"],
    ],
  },
];

const scenarios = [
  { title: "临床基线表", text: "row_vars = 基线指标，col_vars = 治疗组，show_smd = TRUE", icon: HeartPulse },
  { title: "心理学量表", text: "量表分数常用 auto 或 nonparametric 检验", icon: Activity },
  { title: "公卫调查", text: "weight_var + weight_type = survey / frequency", icon: Database },
  { title: "护理干预", text: "compare = TRUE，show_overall = TRUE", icon: ShieldCheck },
  { title: "检验指标", text: "多组比较 + pairwise_policy = if_overall_sig", icon: BarChart3 },
  { title: "IPW 平衡性表", text: "weighted_smd = TRUE，show_smd_ci = TRUE", icon: Scale },
];

const options = [
  ["self_pair", "skip", "默认跳过 row_var == col_var，避免无意义自配对"],
  ["self_pair", "keep", "保留自配对组合，兼容特殊分析需求"],
  ["self_pair", "error", "遇到自配对直接报错，用于严格质控"],
  ["cross_table_layout", "wide", "多个 col_vars 分别返回多个宽表，不强行拼接"],
  ["cross_table_layout", "long", "多个 col_vars 可拼接为 RES$CrossTable"],
  ["cross_table_stack", "all", "所有 row~col 组合合并成总长表"],
  ["num_test_method", "auto", "自动根据正态性和方差齐性选择方法"],
  ["num_test_method", "parametric", "强制 t 检验 / ANOVA 等参数法"],
  ["num_test_method", "nonparametric", "强制 Wilcoxon / Kruskal-Wallis 等非参数法"],
];

function cn(...classes) {
  return classes.filter(Boolean).join(" ");
}

function Badge({ children, className = "" }) {
  return (
    <span className={cn("inline-flex items-center rounded-full border border-slate-200 bg-white/80 px-3 py-1 text-xs font-medium text-slate-700 shadow-sm", className)}>
      {children}
    </span>
  );
}

function SectionTitle({ id, eyebrow, title, desc, icon: Icon }) {
  return (
    <div id={id} className="scroll-mt-28">
      <div className="mb-4 flex items-center gap-3">
        <div className="flex h-11 w-11 items-center justify-center rounded-2xl bg-blue-600 text-white shadow-lg shadow-blue-600/20">
          <Icon className="h-5 w-5" />
        </div>
        <div>
          <div className="text-sm font-semibold uppercase tracking-wider text-blue-600">{eyebrow}</div>
          <h2 className="text-2xl font-bold tracking-tight text-slate-950 md:text-3xl">{title}</h2>
        </div>
      </div>
      {desc && <p className="max-w-3xl text-base leading-7 text-slate-600">{desc}</p>}
    </div>
  );
}

function CodeBlock({ code, title = "R" }) {
  const [copied, setCopied] = useState(false);

  const copy = async () => {
    try {
      await navigator.clipboard.writeText(code);
      setCopied(true);
      setTimeout(() => setCopied(false), 1200);
    } catch (_) {
      setCopied(false);
    }
  };

  return (
    <div className="overflow-hidden rounded-2xl border border-slate-800 bg-slate-950 shadow-xl shadow-slate-950/10">
      <div className="flex items-center justify-between border-b border-slate-800 bg-slate-900 px-4 py-3">
        <div className="flex items-center gap-2 text-sm font-semibold text-slate-200">
          <Code2 className="h-4 w-4 text-cyan-300" />
          {title}
        </div>
        <button
          onClick={copy}
          className="rounded-lg border border-slate-700 px-3 py-1 text-xs text-slate-300 transition hover:border-cyan-400 hover:text-white"
        >
          {copied ? "已复制" : "复制"}
        </button>
      </div>
      <pre className="max-h-[520px] overflow-auto p-4 text-sm leading-6 text-slate-100">
        <code>{code}</code>
      </pre>
    </div>
  );
}

function FloatingNav() {
  const [open, setOpen] = useState(false);

  return (
    <>
      <button
        onClick={() => setOpen(true)}
        className="fixed right-4 top-4 z-50 flex h-12 w-12 items-center justify-center rounded-2xl bg-slate-950 text-white shadow-xl lg:hidden"
      >
        <Menu className="h-5 w-5" />
      </button>
      <aside className="sticky top-6 hidden h-[calc(100vh-3rem)] shrink-0 overflow-auto rounded-3xl border border-slate-200 bg-white/80 p-3 shadow-xl shadow-slate-200/50 backdrop-blur lg:block lg:w-72">
        <NavContent />
      </aside>
      {open && (
        <div className="fixed inset-0 z-50 bg-slate-950/50 p-4 backdrop-blur-sm lg:hidden">
          <div className="ml-auto h-full max-w-sm rounded-3xl bg-white p-3 shadow-2xl">
            <div className="mb-2 flex items-center justify-between px-2 py-2">
              <div className="font-bold text-slate-950">目录</div>
              <button onClick={() => setOpen(false)} className="rounded-xl p-2 hover:bg-slate-100">
                <X className="h-5 w-5" />
              </button>
            </div>
            <NavContent onClick={() => setOpen(false)} />
          </div>
        </div>
      )}
    </>
  );
}

function NavContent({ onClick }) {
  return (
    <nav className="space-y-1">
      <div className="mb-3 rounded-2xl bg-gradient-to-br from-blue-600 to-cyan-500 p-4 text-white">
        <div className="flex items-center gap-2 text-sm font-semibold opacity-90">
          <BookOpen className="h-4 w-4" />
          TjSuper Docs
        </div>
        <div className="mt-2 text-xl font-bold leading-tight">Super.Table1 客户教程</div>
      </div>
      {sections.map((item) => {
        const Icon = item.icon;
        return (
          <a
            key={item.id}
            href={`#${item.id}`}
            onClick={onClick}
            className="flex items-center gap-3 rounded-2xl px-3 py-2.5 text-sm font-medium text-slate-700 transition hover:bg-blue-50 hover:text-blue-700"
          >
            <Icon className="h-4 w-4" />
            {item.label}
          </a>
        );
      })}
    </nav>
  );
}

function Hero() {
  return (
    <section id="home" className="relative overflow-hidden rounded-[2rem] border border-blue-100 bg-white p-8 shadow-2xl shadow-blue-100/70 md:p-12">
      <div className="absolute -right-24 -top-24 h-72 w-72 rounded-full bg-cyan-200/50 blur-3xl" />
      <div className="absolute -bottom-24 -left-24 h-72 w-72 rounded-full bg-blue-300/50 blur-3xl" />
      <div className="relative grid gap-8 lg:grid-cols-[1.15fr_0.85fr] lg:items-center">
        <div>
          <div className="mb-5 flex flex-wrap gap-2">
            <Badge className="border-blue-200 bg-blue-50 text-blue-700">医学统计 Table 1</Badge>
            <Badge className="border-emerald-200 bg-emerald-50 text-emerald-700">TjssnStat 客户版</Badge>
            <Badge className="border-violet-200 bg-violet-50 text-violet-700">Quick.Check 自动日志</Badge>
          </div>
          <motion.h1
            initial={{ opacity: 0, y: 14 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5 }}
            className="text-4xl font-black tracking-tight text-slate-950 md:text-6xl"
          >
            TjSuper
            <span className="block bg-gradient-to-r from-blue-700 to-cyan-500 bg-clip-text text-transparent">Super.Table1</span>
          </motion.h1>
          <p className="mt-6 max-w-2xl text-lg leading-8 text-slate-600">
            面向临床、心理、公卫、检验、护理等医学统计场景，一套参数完成描述性统计、差异分析、SMD、两两比较、加权分析和报告输出。
          </p>
          <div className="mt-8 flex flex-wrap gap-3">
            <a href="#quick" className="rounded-2xl bg-blue-600 px-5 py-3 text-sm font-bold text-white shadow-lg shadow-blue-600/25 transition hover:bg-blue-700">
              查看快速开始
            </a>
            <a href="#params" className="rounded-2xl border border-slate-200 bg-white px-5 py-3 text-sm font-bold text-slate-800 shadow-sm transition hover:border-blue-300 hover:bg-blue-50">
              浏览参数地图
            </a>
          </div>
        </div>
        <div className="rounded-[1.7rem] border border-slate-200 bg-slate-950 p-5 text-white shadow-2xl">
          <div className="mb-4 flex items-center justify-between">
            <div className="flex items-center gap-2 text-sm font-semibold text-cyan-200">
              <Search className="h-4 w-4" />
              Result Structure
            </div>
            <div className="flex gap-1.5">
              <span className="h-3 w-3 rounded-full bg-red-400" />
              <span className="h-3 w-3 rounded-full bg-yellow-400" />
              <span className="h-3 w-3 rounded-full bg-green-400" />
            </div>
          </div>
          <div className="space-y-3 text-sm">
            {[
              ["model.res", "统计主结果"],
              ["Quick.Check.Text", "一键质控日志"],
              ["Result[[col_var]]$cross_table", "宽表结果"],
              ["CrossTable", "long 拼接总表"],
              ["Pairs", "实际分析组合"],
            ].map(([a, b]) => (
              <div key={a} className="flex items-center justify-between rounded-2xl border border-white/10 bg-white/5 px-4 py-3">
                <code className="text-cyan-100">{a}</code>
                <span className="text-slate-300">{b}</span>
              </div>
            ))}
          </div>
        </div>
      </div>
    </section>
  );
}

function ParameterCard({ block }) {
  const Icon = block.icon;
  return (
    <div className="group overflow-hidden rounded-3xl border border-slate-200 bg-white shadow-lg shadow-slate-200/60 transition hover:-translate-y-1 hover:shadow-2xl hover:shadow-slate-200">
      <div className={cn("bg-gradient-to-r p-5 text-white", block.tone)}>
        <div className="flex items-center gap-3">
          <div className="flex h-11 w-11 items-center justify-center rounded-2xl bg-white/20">
            <Icon className="h-5 w-5" />
          </div>
          <div>
            <div className="font-mono text-sm opacity-90">{block.name}</div>
            <h3 className="text-xl font-bold">{block.title}</h3>
          </div>
        </div>
      </div>
      <div className="divide-y divide-slate-100 p-5">
        {block.items.map(([name, option, meaning]) => (
          <div key={name} className="grid gap-1 py-3 first:pt-0 last:pb-0">
            <div className="font-mono text-sm font-bold text-slate-950">{name}</div>
            <div className="text-sm text-blue-700">{option}</div>
            <div className="text-sm text-slate-500">{meaning}</div>
          </div>
        ))}
      </div>
    </div>
  );
}

function OptionTable() {
  return (
    <div className="overflow-hidden rounded-3xl border border-slate-200 bg-white shadow-lg shadow-slate-200/60">
      <div className="border-b border-slate-200 bg-slate-50 px-5 py-4">
        <div className="flex items-center gap-2 font-bold text-slate-950">
          <ListChecks className="h-5 w-5 text-blue-600" />
          核心选项速查
        </div>
      </div>
      <div className="overflow-x-auto">
        <table className="w-full min-w-[760px] text-left text-sm">
          <thead className="bg-white text-slate-500">
            <tr className="border-b border-slate-100">
              <th className="px-5 py-3 font-semibold">参数</th>
              <th className="px-5 py-3 font-semibold">选项</th>
              <th className="px-5 py-3 font-semibold">含义 / 使用场景</th>
            </tr>
          </thead>
          <tbody className="divide-y divide-slate-100">
            {options.map(([p, o, m], idx) => (
              <tr key={`${p}-${o}`} className={idx % 2 ? "bg-slate-50/50" : "bg-white"}>
                <td className="px-5 py-3 font-mono font-semibold text-slate-900">{p}</td>
                <td className="px-5 py-3"><Badge>{o}</Badge></td>
                <td className="px-5 py-3 text-slate-600">{m}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}

function FlowCard({ number, title, children, icon: Icon, color = "blue" }) {
  const palette = {
    blue: "border-blue-200 bg-blue-50 text-blue-700",
    green: "border-emerald-200 bg-emerald-50 text-emerald-700",
    orange: "border-orange-200 bg-orange-50 text-orange-700",
    purple: "border-violet-200 bg-violet-50 text-violet-700",
    red: "border-rose-200 bg-rose-50 text-rose-700",
  };
  return (
    <div className="rounded-3xl border border-slate-200 bg-white p-5 shadow-lg shadow-slate-200/50">
      <div className="mb-4 flex items-center gap-3">
        <div className={cn("flex h-10 w-10 items-center justify-center rounded-2xl border", palette[color])}>
          <Icon className="h-5 w-5" />
        </div>
        <div>
          <div className="text-xs font-bold uppercase tracking-wider text-slate-400">Step {number}</div>
          <div className="font-bold text-slate-950">{title}</div>
        </div>
      </div>
      <div className="text-sm leading-6 text-slate-600">{children}</div>
    </div>
  );
}

export default function TjSuperDocsLanding() {
  const year = useMemo(() => new Date().getFullYear(), []);

  return (
    <div className="min-h-screen bg-[radial-gradient(circle_at_top_left,#dbeafe,transparent_32%),radial-gradient(circle_at_70%_10%,#cffafe,transparent_24%),#f8fafc] text-slate-900">
      <div className="mx-auto flex max-w-[1500px] gap-6 px-4 py-6 lg:px-6">
        <FloatingNav />
        <main className="min-w-0 flex-1 space-y-14 pb-20">
          <Hero />

          <section id="quick" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="quick-title"
              eyebrow="01 / Quick Start"
              title="最短代码完成描述性统计与差异分析"
              desc="客户版统一通过 TjSuper::TjssnStat() 调用，统计结果、主表和日志都会按服务器结构返回。"
              icon={Code2}
            />
            <CodeBlock code={codeQuickStart} title="最简 Super.Table1" />
          </section>

          <section id="params" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="params-title"
              eyebrow="02 / Parameter Map"
              title="四大参数块：从数据到输出"
              desc="教程主线建议按 data_param → var_param → model_param → output_param 理解。客户只需要知道参数放在哪个块里。"
              icon={Boxes}
            />
            <div className="grid gap-5 md:grid-cols-2 xl:grid-cols-4">
              {paramBlocks.map((block) => (
                <ParameterCard key={block.name} block={block} />
              ))}
            </div>
          </section>

          <section id="rowcol" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="rowcol-title"
              eyebrow="03 / Row & Col"
              title="row_vars 与 col_vars 的组合策略"
              desc="row_vars 是被描述变量，col_vars 是分组变量。多个 row 与多个 col 会生成多组分析组合；self_pair 默认 skip，用于自动跳过 row_var == col_var。"
              icon={GitBranch}
            />
            <div className="grid gap-5 lg:grid-cols-3">
              <FlowCard number="1" title="标准 Table 1" icon={Table2} color="green">
                多个 <code className="font-mono text-slate-950">row_vars</code> + 单个 <code className="font-mono text-slate-950">col_vars</code>，生成最常见医学基线表。
              </FlowCard>
              <FlowCard number="2" title="多分组变量" icon={Network} color="blue">
                多个 <code className="font-mono text-slate-950">col_vars</code> 时，wide 返回多个独立宽表；long 可拼接为总表。
              </FlowCard>
              <FlowCard number="3" title="精确组合" icon={Tags} color="purple">
                使用 <code className="font-mono text-slate-950">formula_var</code> 指定部分 row~col 组合，适合分析师精细控制。
              </FlowCard>
            </div>
            <OptionTable />
          </section>

          <section id="layout" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="layout-title"
              eyebrow="04 / Layout"
              title="wide 与 long：分别返回还是拼接总表"
              desc="新逻辑中，wide + 多个 col_vars 不再报错，而是分别返回多个宽表；只有 long 才进行拼接。"
              icon={Layers3}
            />
            <div className="grid gap-6 xl:grid-cols-2">
              <div className="space-y-4">
                <div className="rounded-3xl border border-blue-200 bg-blue-50 p-5">
                  <div className="mb-2 flex items-center gap-2 font-bold text-blue-900"><Table2 className="h-5 w-5" />wide：多个宽表</div>
                  <p className="text-sm leading-6 text-blue-800">适合 Word 中分别展示不同分组变量的 Table 1，不强行合并列结构不同的表。</p>
                </div>
                <CodeBlock code={codeWideMulti} title="wide + 多个 col_vars" />
              </div>
              <div className="space-y-4">
                <div className="rounded-3xl border border-rose-200 bg-rose-50 p-5">
                  <div className="mb-2 flex items-center gap-2 font-bold text-rose-900"><Layers3 className="h-5 w-5" />long：拼接总表</div>
                  <p className="text-sm leading-6 text-rose-800">适合 Excel 导出、批量汇总和纵向阅读，通过 CrossTable 获取拼接后的长表。</p>
                </div>
                <CodeBlock code={codeLongAll} title="long + cross_table_stack = all" />
              </div>
            </div>
          </section>

          <section id="tests" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="tests-title"
              eyebrow="05 / Statistical Tests"
              title="检验方法、缺失与显著性控制"
              desc="连续变量可自动判断参数/非参数方法，也可全局强制或按变量单独指定；分类变量可控制百分比方向和缺失入表。"
              icon={LineChart}
            />
            <div className="grid gap-4 md:grid-cols-3">
              {[
                ["num_test_method", "auto / parametric / nonparametric", "连续变量检验方法"],
                ["num_norm_p", "0.01 / 0.05", "正态性判断阈值"],
                ["num_var_p", "0.01 / 0.05", "方差齐性判断阈值"],
                ["fct_percent", "row / col", "分类变量百分比方向"],
                ["fct_include_missing", "TRUE / FALSE", "分类缺失是否入表"],
                ["alpha", "0.05", "显著性阈值"],
              ].map(([a, b, c]) => (
                <div key={a} className="rounded-3xl border border-slate-200 bg-white p-5 shadow-lg shadow-slate-200/50">
                  <div className="font-mono text-sm font-bold text-slate-950">{a}</div>
                  <div className="mt-2 text-sm text-blue-700">{b}</div>
                  <div className="mt-2 text-sm text-slate-500">{c}</div>
                </div>
              ))}
            </div>
          </section>

          <section id="table" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="table-title"
              eyebrow="06 / Table Style"
              title="制表细节与自定义模板"
              desc="覆盖小数位、标签、单位、连续变量模板、分类变量模板，以及 long 表中多统计量转多列。"
              icon={Table2}
            />
            <div className="grid gap-5 lg:grid-cols-2">
              <div className="rounded-3xl border border-slate-200 bg-white p-6 shadow-lg shadow-slate-200/60">
                <h3 className="flex items-center gap-2 text-lg font-bold text-slate-950"><Settings2 className="h-5 w-5 text-violet-600" />常用格式参数</h3>
                <div className="mt-5 grid gap-3 sm:grid-cols-2">
                  {["digits", "stat_digits", "p_digits", "smd_digits", "header_pct_digits", "label_stat", "label_unit", "row_var_labels"].map((x) => (
                    <div key={x} className="rounded-2xl bg-slate-50 px-4 py-3 font-mono text-sm font-semibold text-slate-800">{x}</div>
                  ))}
                </div>
              </div>
              <div className="rounded-3xl border border-slate-200 bg-white p-6 shadow-lg shadow-slate-200/60">
                <h3 className="flex items-center gap-2 text-lg font-bold text-slate-950"><FileText className="h-5 w-5 text-blue-600" />模板扩展参数</h3>
                <div className="mt-5 space-y-3">
                  {["num_style_define：自定义连续变量模板", "num_summary：指定连续变量输出项", "fct_style_define：自定义分类变量模板", "fct_summary：指定分类变量输出项", "long_stat_from_var_label：多统计量转多列"].map((x) => (
                    <div key={x} className="rounded-2xl bg-blue-50 px-4 py-3 text-sm font-medium text-blue-900">{x}</div>
                  ))}
                </div>
              </div>
            </div>
          </section>

          <section id="advanced" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="advanced-title"
              eyebrow="07 / Advanced"
              title="SMD、两两比较与加权分析"
              desc="面向倾向评分、IPW、基线均衡性、多组事后比较等高级医学统计需求。"
              icon={ShieldCheck}
            />
            <div className="grid gap-6 xl:grid-cols-2">
              <CodeBlock code={codePairwise} title="两两比较" />
              <CodeBlock code={codeWeighted} title="加权分析 + SMD 95%CI" />
            </div>
          </section>

          <section id="scenes" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="scenes-title"
              eyebrow="08 / Medical Scenarios"
              title="常见医学统计场景模板"
              desc="把参数组合映射到客户最常见的真实需求，便于快速套用。"
              icon={HeartPulse}
            />
            <div className="grid gap-5 md:grid-cols-2 xl:grid-cols-3">
              {scenarios.map((scene) => {
                const Icon = scene.icon;
                return (
                  <div key={scene.title} className="rounded-3xl border border-slate-200 bg-white p-6 shadow-lg shadow-slate-200/60 transition hover:-translate-y-1 hover:shadow-2xl">
                    <div className="mb-4 flex h-12 w-12 items-center justify-center rounded-2xl bg-cyan-50 text-cyan-700">
                      <Icon className="h-6 w-6" />
                    </div>
                    <h3 className="text-lg font-bold text-slate-950">{scene.title}</h3>
                    <p className="mt-2 text-sm leading-6 text-slate-600">{scene.text}</p>
                  </div>
                );
              })}
            </div>
          </section>

          <section id="quickcheck" className="scroll-mt-28 space-y-6">
            <SectionTitle
              id="quickcheck-title"
              eyebrow="09 / Quality Log"
              title="Quick.Check：客户最容易读懂的结果日志"
              desc="服务器端返回 Quick.Check 和 Quick.Check.Text。推荐客户直接 cat() 输出文本日志，也可以读取结构化显著/不显著变量表。"
              icon={ClipboardList}
            />
            <div className="grid gap-6 lg:grid-cols-[0.9fr_1.1fr]">
              <div className="rounded-3xl border border-emerald-200 bg-emerald-50 p-6">
                <h3 className="flex items-center gap-2 text-lg font-bold text-emerald-950"><CheckCircle2 className="h-5 w-5" />推荐展示方式</h3>
                <pre className="mt-4 overflow-auto rounded-2xl bg-emerald-950 p-4 text-sm leading-6 text-emerald-50"><code>{`cat(RESSUB$result$log.print$Quick.Check.Text)

RESSUB$result$log.print$Quick.Check$Significant
RESSUB$result$log.print$Quick.Check$Non.Significant
RESSUB$result$log.print$Quick.Check$No.P.Value`}</code></pre>
              </div>
              <div className="rounded-3xl border border-slate-200 bg-white p-6 shadow-lg shadow-slate-200/60">
                <h3 className="mb-4 text-lg font-bold text-slate-950">日志展示内容</h3>
                <div className="space-y-3">
                  {["Overview：分析组合数、成功数、失败数", "Significant：有统计学意义变量", "Non.Significant：无统计学意义变量", "No.P.Value：无可评价 P 值变量", "Issues：错误与异常信息"].map((x) => (
                    <div key={x} className="flex items-center gap-3 rounded-2xl bg-slate-50 px-4 py-3 text-sm text-slate-700">
                      <CheckCircle2 className="h-4 w-4 text-emerald-600" />
                      {x}
                    </div>
                  ))}
                </div>
              </div>
            </div>
          </section>

          <footer className="rounded-3xl border border-slate-200 bg-white p-6 text-center text-sm text-slate-500 shadow-lg shadow-slate-200/60">
            TjSuper Super.Table1 客户教程界面 · {year} · 适合部署为 GitHub Pages / 包内 vignette / 内部文档站
          </footer>
        </main>
      </div>
    </div>
  );
}
