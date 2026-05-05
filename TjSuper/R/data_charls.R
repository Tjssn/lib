#' TjSuper_Charls 示例数据
#'
#' 一个整理后的 TjSuper_Charls 示例数据集
#'
#' 该数据包含个体识别信息、家庭识别信息、调查波次、人口学特征、血液检测指标、
#' 认知功能、家庭资产、医疗服务利用、健康行为、日常生活能力、慢性病、疾病治疗、
#' 体力活动、收入消费、体格测量和心理健康等变量。
#'
#' @format 一个数据框。主数据共 102,344 行，包含多个调查波次和多个主题模块变量：
#' \describe{
#'   \item{ID}{个体 ID。字符型变量，用于识别受访者个体。}
#'   \item{householdID}{家庭 ID。字符型变量，用于识别受访者所属家庭。}
#'   \item{hhid}{家庭编号。数值型变量。}
#'   \item{communityID}{社区 ID。字符型变量，用于识别受访者所在社区。}
#'   \item{Wave}{调查波次。包括 `Wave1`、`Wave2`、`Wave3` 和 `Wave4`。}
#'
#'   \item{rural2}{居住地，通常包括 `Rural` 和 `Urban`。}
#'   \item{hukou}{户口类型。}
#'   \item{rabplace_c}{出生地。}
#'   \item{mstat}{婚姻状况，多分类变量。}
#'   \item{raeduc_c}{教育水平。}
#'   \item{ragender}{性别，包括 `female` 和 `male`。}
#'   \item{mnev}{婚姻状态二分类变量。}
#'   \item{agey}{年龄。}
#'
#'   \item{Blood_weight}{血液样本权重，包含家庭和个体应答调整。}
#'   \item{bl_fasting}{是否空腹采血。}
#'   \item{bl_wbc}{白细胞计数。}
#'   \item{bl_mcv}{平均红细胞体积，MCV。}
#'   \item{bl_plt}{血小板计数。}
#'   \item{bl_bun}{血尿素氮，BUN，单位 mg/dl。}
#'   \item{bl_glu}{血糖，单位 mg/dl。}
#'   \item{bl_crea}{肌酐，单位 mg/dl。}
#'   \item{bl_cho}{总胆固醇，单位 mg/dl。}
#'   \item{bl_tg}{甘油三酯，单位 mg/dl。}
#'   \item{bl_hdl}{高密度脂蛋白胆固醇，单位 mg/dl。}
#'   \item{bl_ldl}{低密度脂蛋白胆固醇，单位 mg/dl。}
#'   \item{bl_crp}{C 反应蛋白，CRP，单位 mg/l。}
#'   \item{bl_hbalc}{糖化血红蛋白，百分比。}
#'   \item{bl_ua}{尿酸，单位 mg/dl。}
#'   \item{bl_hct}{红细胞压积。}
#'   \item{bl_hgb}{血红蛋白，单位 g/dl。}
#'   \item{bl_cysc}{胱抑素 C，单位 mg/l。}
#'
#'   \item{slfmem}{自我报告记忆状况。}
#'   \item{tr20}{认知功能总分。}
#'   \item{draw}{认知功能绘图测试。}
#'   \item{orient}{日期记忆或定向力得分。}
#'   \item{imrc}{即时单词回忆得分。}
#'   \item{dlrc}{延迟单词回忆得分。}
#'
#'   \item{achck}{金融机构现金及存款总价值。}
#'   \item{astck}{股票和基金总价值。}
#'   \item{abond}{政府债券价值。}
#'   \item{ahousa}{主要住宅的家庭价值。}
#'   \item{amort}{主要住宅抵押贷款总额。}
#'   \item{aothr}{其他储蓄或金融资产价值。}
#'   \item{adebt}{债务总额。}
#'   \item{atotfa}{非住房金融资产净值。}
#'   \item{atoth}{自住住房净值。}
#'   \item{atran}{车辆价值。}
#'   \item{atotb}{总资产。}
#'
#'   \item{doctor1m}{过去一个月是否门诊就诊。}
#'   \item{hosp1y}{过去一年是否住院。}
#'   \item{dentst1y}{过去一年是否接受牙科治疗。}
#'   \item{trdmed1m}{过去一个月是否接受中医相关服务。}
#'   \item{higov}{是否有公共医疗保险。}
#'   \item{hipriv}{是否有私人医疗保险。}
#'   \item{hiothp}{是否有其他医疗保险。}
#'   \item{totden1y}{过去一年牙科总支出。}
#'   \item{oopden1y}{过去一年牙科自付支出或相关费用。}
#'   \item{oopdoc1m}{过去一个月门诊自付费用。}
#'   \item{totdoc1m}{过去一个月门诊总支出。}
#'   \item{oophos1y}{过去一年住院自付费用。}
#'   \item{doctim1m}{过去一个月医疗机构就诊次数。}
#'   \item{hsptim1y}{过去一年住院次数。}
#'   \item{hspnite}{过去一年住院天数。}
#'
#'   \item{smokev}{是否曾经吸烟。}
#'   \item{smoken}{现在是否吸烟。}
#'   \item{drinkev}{是否曾经饮酒。}
#'   \item{drinkl}{过去一年是否饮酒。}
#'   \item{drinkr_c}{每日饮酒次数范围。}
#'   \item{drinkn_c}{过去一年饮酒频率。}
#'   \item{smokef}{日均吸烟量。}
#'
#'   \item{joga}{是否跑步或慢跑 1 公里存在困难。}
#'   \item{walk1kma}{是否步行 1 公里存在困难。}
#'   \item{walk100a}{是否步行 100 米存在困难。}
#'   \item{chaira}{是否久坐后起身存在困难。}
#'   \item{climsa}{是否连续爬多层楼梯存在困难。}
#'   \item{stoopa}{是否弯腰、跪下或蹲伏存在困难。}
#'   \item{lifta}{是否举起超过 10 斤重物存在困难。}
#'   \item{dimea}{是否捡起桌上的硬币存在困难。}
#'   \item{armsa}{是否手臂伸展超过肩部高度存在困难。}
#'
#'   \item{phonea}{IADL：打电话是否存在困难。}
#'   \item{housewka}{IADL：打扫卫生是否存在困难。}
#'   \item{mealsa}{IADL：做饭是否存在困难。}
#'   \item{shopa}{IADL：采购日用品是否存在困难。}
#'   \item{medsa}{IADL：服药是否存在困难。}
#'   \item{moneya}{IADL：管理财务是否存在困难。}
#'
#'   \item{urina}{ADL：排尿和排便是否存在困难。}
#'   \item{batha}{ADL：洗澡是否存在困难。}
#'   \item{toilta}{ADL：如厕是否存在困难。}
#'   \item{beda}{ADL：上下床是否存在困难。}
#'   \item{eata}{ADL：进食是否存在困难。}
#'   \item{dressa}{ADL：穿衣是否存在困难。}
#'
#'   \item{hlthlm_c}{身体健康是否影响工作。}
#'   \item{hibpe}{是否患有高血压。}
#'   \item{diabe}{是否患有糖尿病或血糖较高。该变量在当前示例数据中可能全部缺失。}
#'   \item{cancre}{是否患有癌症或恶性肿瘤。}
#'   \item{lunge}{是否患有慢性肺部疾病。}
#'   \item{hearte}{是否患有心脏病相关疾病。}
#'   \item{stroke}{是否患有卒中。}
#'   \item{psyche}{是否存在情绪、神经或精神问题。}
#'   \item{arthre}{是否患有关节炎或风湿病。}
#'   \item{dyslipe}{是否存在血脂异常、总胆固醇升高或 HDL 偏低。}
#'   \item{livere}{是否患有肝脏疾病。}
#'   \item{kidneye}{是否患有肾脏疾病。}
#'   \item{digeste}{是否患有胃部或其他消化系统疾病。}
#'   \item{asthmae}{是否患有哮喘。}
#'   \item{memrye}{是否患有记忆相关疾病。}
#'
#'   \item{rxhibp_c}{是否接受高血压治疗。}
#'   \item{rxdiab_c}{是否接受糖尿病或高血糖治疗。}
#'   \item{cncrmeds_c}{是否接受癌症药物治疗。}
#'   \item{cncrsurg}{是否接受癌症手术治疗。}
#'   \item{cncrradn}{是否接受癌症放疗或化疗。}
#'   \item{rxlung_c}{是否接受慢性肺病治疗。}
#'   \item{rxheart_c}{是否接受心脏病相关治疗。}
#'   \item{rxstrok_c}{是否接受卒中治疗。}
#'   \item{rxpsych}{是否接受精神或情绪问题治疗。}
#'   \item{rxarthr_c}{是否接受关节炎或风湿病治疗。}
#'   \item{rxdyslip_c}{是否接受血脂异常治疗。}
#'   \item{rxliver_c}{是否接受肝脏疾病治疗。}
#'   \item{rxkidney_c}{是否接受肾脏疾病治疗。}
#'   \item{rxdigest_c}{是否接受消化系统疾病治疗。}
#'   \item{rxmemry_c}{是否接受记忆相关疾病治疗。}
#'
#'   \item{vgact_c}{是否每周进行至少 10 分钟剧烈体力活动。}
#'   \item{mdact_c}{是否每周进行至少 10 分钟中等强度体力活动。}
#'   \item{ltact_c}{是否每周进行至少 10 分钟轻度体力活动。}
#'   \item{vgactx_c}{每周剧烈体力活动天数。}
#'   \item{mdactx_c}{每周中等强度体力活动天数。}
#'   \item{ltactx_c}{每周轻度体力活动天数。}
#'   \item{mobilsev}{7 项 mobility 综合评分。}
#'   \item{lowermob}{4 项下肢 mobility 综合评分。}
#'   \item{uppermob}{3 项上肢 mobility 综合评分。}
#'
#'   \item{ctot}{家庭总消费。}
#'   \item{cfood}{家庭食品消费。}
#'   \item{cnf1m}{上月家庭非食品消费。}
#'   \item{cnf1y}{上年家庭非食品消费。}
#'   \item{itearn}{税后个人收入。}
#'   \item{irent}{税前租金收入。}
#'   \item{ipena}{工作前私人养老金。}
#'   \item{ipubpen}{离职工作养老金。}
#'   \item{ipen}{总养老金。}
#'   \item{iothr}{其他收入。}
#'   \item{itot}{家庭总收入。}
#'
#'   \item{bpact30}{测量血压前是否有持续 30 分钟影响血压的活动。}
#'   \item{mbmicata}{适合亚裔人群的 BMI 分类。}
#'   \item{puff1, puff2, puff3}{肺功能呼吸测试 1、2、3。}
#'   \item{puff}{最大肺功能呼吸测试值。}
#'   \item{mheight}{身高。}
#'   \item{mweight}{体重。}
#'   \item{mwaist}{腰围。}
#'   \item{mbmi}{身体质量指数，BMI。}
#'   \item{lgrip1, lgrip2}{左手握力测量值 1 和 2。}
#'   \item{lgrip}{最大左手握力测量值。}
#'   \item{rgrip1, rgrip2}{右手握力测量值 1 和 2。}
#'   \item{rgrip}{最大右手握力测量值。}
#'   \item{wspeed1, wspeed2}{步速测试 1 和 2。}
#'   \item{systo1, systo2, systo3}{收缩压测量值 1、2、3。}
#'   \item{diasto1, diasto2, diasto3}{舒张压测量值 1、2、3。}
#'   \item{pulse1, pulse2, pulse3}{脉搏测量值 1、2、3。}
#'
#'   \item{shlta}{自我报告健康状况。}
#'   \item{cesd10}{CESD-10 抑郁症状量表得分。}
#'   \item{satlife}{生活满意度单题变量。}
#'   \item{satlifez}{生活满意度 Z 值。}
#' }
#'
#' @details
#' 该数据集是一个用于教学和函数演示的 TjSuper_Charls 风格示例数据。
#' 数据中既包含横跨多个波次的基础人口学变量，也包含部分波次或子样本中
#' 才有的血液检测、体格测量和医疗服务变量。因此，不同变量的缺失比例
#' 可能差异较大。
#'
#' 变量大致可分为以下模块：
#'
#' \itemize{
#'   \item 识别变量：`ID`、`householdID`、`hhid`、`communityID`、`Wave`。
#'   \item 人口学变量：居住地、户口、出生地、婚姻、教育、性别和年龄。
#'   \item 血液检测变量：血糖、血脂、肾功能、炎症指标、血红蛋白等。
#'   \item 认知功能变量：自评记忆、总分、绘图、定向力、即时和延迟回忆。
#'   \item 资产与收入变量：金融资产、住房资产、债务、消费和收入。
#'   \item 医疗服务变量：门诊、住院、牙科、中医、医保和医疗支出。
#'   \item 行为和功能变量：吸烟、饮酒、体力活动、ADL、IADL 和 mobility。
#'   \item 疾病和治疗变量：慢性病诊断及相应治疗情况。
#'   \item 体检变量：BMI、握力、步速、血压、脉搏和肺功能。
#'   \item 心理健康变量：CESD-10 和生活满意度。
#' }
#'
#' 该数据适合用于演示描述性统计、分组比较、缺失值处理、纵向数据整理、
#' survey/加权分析、表格自动输出和 Word 报告生成等流程。
#'
#' @source TjSuper_Charls 示例数据整理结果。变量含义根据用户提供的变量说明表整理。
#'
#' @examples
#' data("TjSuper_Charls")
#'
#' dim(TjSuper_Charls)
#' names(TjSuper_Charls)
#'
#' table(TjSuper_Charls$Wave, useNA = "ifany")
#' table(TjSuper_Charls$ragender, useNA = "ifany")
#'
#' summary(TjSuper_Charls$agey)
#' summary(TjSuper_Charls$cesd10)
#'

"TjSuper_Charls"
