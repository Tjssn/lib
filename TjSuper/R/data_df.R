#' Example weighted clinical-style dataset
#'
#' A built-in example dataset for demonstrating functions in the
#' TjSuper package. The dataset contains numeric variables, categorical
#' variables, sampling or analysis weights, propensity-score-related
#' variables, and survey-design variables.
#'
#' The variable names are anonymized. Variables beginning with `NUM`
#' are numeric variables, variables beginning with `CHR` are mostly
#' categorical variables, variables beginning with `wt_` are example
#' weight variables, and variables beginning with `survey_` are survey
#' design variables.
#'
#' @format A data frame with 12,740 rows and 95 variables:
#' \describe{
#'   \item{ID}{Numeric record identifier.}
#'
#'   \item{NUM1, NUM2, NUM3, NUM4, NUM5}{Continuous numeric variables
#'   with missing values. Their observed ranges are approximately from
#'   negative standardized values to positive standardized values.}
#'
#'   \item{NUM6}{Numeric variable with all observed values equal to 0.}
#'
#'   \item{NUM7, NUM8, NUM9, NUM10, NUM11, NUM12}{Numeric variables
#'   with positive continuous values and varying amounts of missingness.}
#'
#'   \item{NUM13}{Numeric variable with values approximately ranging
#'   from 140 to 183.}
#'
#'   \item{NUM14}{Numeric variable with values approximately ranging
#'   from 36 to 120.}
#'
#'   \item{NUM15}{Numeric variable with values approximately ranging
#'   from 10.1 to 49.95.}
#'
#'   \item{NUM16}{Numeric variable with values approximately ranging
#'   from -4 to 37.}
#'
#'   \item{NUM17}{Numeric variable with values approximately ranging
#'   from -7.75 to 4.32.}
#'
#'   \item{NUM18, NUM19}{Numeric variables with positive continuous
#'   values.}
#'
#'   \item{NUM20}{Numeric variable with values approximately ranging
#'   from 840 to 5,580.}
#'
#'   \item{NUM21, NUM22, NUM23, NUM24, NUM25}{Numeric variables with
#'   different observed ranges and small to moderate amounts of missingness.}
#'
#'   \item{NUM26, NUM27, NUM28, NUM29, NUM30}{Numeric variables observed
#'   for a subset of records. These variables contain missing values for
#'   approximately 2,270 records.}
#'
#'   \item{NUM31, NUM32, NUM33, NUM34, NUM35}{Additional continuous
#'   numeric variables with values on standardized or transformed scales.}
#'
#'   \item{CHR1, CHR2, CHR3, CHR4}{Categorical class variables with
#'   five observed levels, such as `Class1`, `Class2`, and other class
#'   categories.}
#'
#'   \item{CHR5}{Categorical variable indicating chronic hypertension
#'   status, with levels including `Chronic hypertension` and
#'   `No hypertension`.}
#'
#'   \item{CHR6}{Categorical variable related to gestational diabetes
#'   status.}
#'
#'   \item{CHR7}{Categorical variable with levels `Normal` and
#'   `Abnormal`.}
#'
#'   \item{CHR8}{Binary categorical variable with levels `No` and `Yes`.}
#'
#'   \item{CHR9}{Categorical anemia-related variable with levels such as
#'   `Mild anemia`, `Moderate anemia`, and other anemia categories.}
#'
#'   \item{CHR10}{Binary categorical variable with levels `Negative`
#'   and `Positive`.}
#'
#'   \item{CHR11, CHR12, CHR13, CHR14, CHR15, CHR16, CHR17, CHR18,
#'   CHR19, CHR20, CHR21}{Binary categorical variables with levels
#'   `No` and `Yes`.}
#'
#'   \item{CHR22}{Categorical variable related to reproductive technology,
#'   with levels including assisted reproductive technology and other
#'   categories.}
#'
#'   \item{CHR23}{Categorical body-mass-index group variable with levels
#'   such as `<18.5`, `18.5-24.9`, `25-29.9`, and `>=30`.}
#'
#'   \item{CHR24}{Categorical variable with levels including `Excessive`,
#'   `Inadequate`, and other categories.}
#'
#'   \item{CHR25}{Categorical variable with levels `Normal` and
#'   `Abnormal`.}
#'
#'   \item{CHR26}{Categorical ethnicity or nationality variable with
#'   levels including `Han` and `Other Nation`.}
#'
#'   \item{CHR27}{Numeric variable with all values missing in the
#'   provided dataset.}
#'
#'   \item{CHR28}{Binary categorical variable with levels `No` and `Yes`.}
#'
#'   \item{CHR29}{Categorical frequency variable with levels such as
#'   `1 time`, `2 times`, and `>=3 times`.}
#'
#'   \item{CHR30}{Categorical delivery-mode variable with levels including
#'   `Cesarean section` and `Vaginal delivery`.}
#'
#'   \item{CHR31}{Categorical education-related variable observed for a
#'   subset of records.}
#'
#'   \item{CHR32}{Categorical occupation-related variable observed for a
#'   subset of records.}
#'
#'   \item{CHR33}{Categorical body-mass-index group variable observed for
#'   a subset of records.}
#'
#'   \item{CHR34}{Categorical education-related variable with five
#'   observed levels.}
#'
#'   \item{CHR35}{Binary categorical variable with levels `No` and `Yes`.}
#'
#'   \item{wt_equal}{Equal weight variable. All observed values are 1.}
#'
#'   \item{wt_freq_int}{Integer frequency weight variable with values
#'   ranging from 1 to 5.}
#'
#'   \item{wt_mild}{Example continuous weight variable with mild variation.}
#'
#'   \item{wt_chr1_balance}{Example balancing weight variable related to
#'   `CHR1`.}
#'
#'   \item{wt_chr5_balance}{Example balancing weight variable related to
#'   `CHR5`.}
#'
#'   \item{wt_sampling}{Example sampling weight variable.}
#'
#'   \item{wt_missing_sensitive}{Example weight variable designed to be
#'   sensitive to missingness patterns.}
#'
#'   \item{wt_extreme}{Example weight variable containing relatively
#'   extreme values.}
#'
#'   \item{wt_extreme_trim}{Trimmed version of an extreme weight variable.}
#'
#'   \item{wt_ipw_ate}{Inverse-probability weight for an average
#'   treatment effect setting.}
#'
#'   \item{wt_ipw_ate_stab}{Stabilized inverse-probability weight for an
#'   average treatment effect setting.}
#'
#'   \item{wt_ipw_att}{Inverse-probability weight for an average treatment
#'   effect among the treated setting.}
#'
#'   \item{wt_overlap}{Example overlap weight variable.}
#'
#'   \item{ps_chr5}{Example propensity score variable related to `CHR5`.}
#'
#'   \item{wt_rare_event}{Example weight variable for a rare-event setting.}
#'
#'   \item{wt_sampling_groupnorm_chr1}{Group-normalized sampling weight
#'   related to `CHR1`.}
#'
#'   \item{wt_extreme_groupnorm_chr1}{Group-normalized extreme weight
#'   related to `CHR1`.}
#'
#'   \item{wt_bad_zero}{Example problematic weight variable containing
#'   zero values.}
#'
#'   \item{wt_bad_na}{Example problematic weight variable containing
#'   missing values.}
#'
#'   \item{wt_bad_negative}{Example problematic weight variable containing
#'   negative values.}
#'
#'   \item{survey_strata}{Survey stratum variable, stored as a factor.}
#'
#'   \item{survey_cluster}{Survey cluster or primary sampling unit variable,
#'   stored as a factor.}
#'
#'   \item{survey_fpc}{Finite population correction variable for survey
#'   design examples.}
#'
#'   \item{NUM_TEST1}{Additional numeric test variable. Its variable label
#'   is `NUM-TEST1`.}
#' }
#'
#' @details
#' This dataset is intended for examples, testing, and demonstrations of
#' functions that handle numeric variables, categorical variables, missing
#' data, weighting variables, propensity-score variables, and survey-design
#' inputs.
#'
#' Some variables intentionally contain special cases, such as all-zero
#' values, all-missing values, zero weights, missing weights, negative
#' weights, extreme weights, and group-normalized weights. These variables
#' are useful for testing data-checking and diagnostic functions.
#'
#' @source User-provided example dataset metadata.
#'
#' @examples
#' data("TjSuper_df")
#' dim(TjSuper_df)
#' names(TjSuper_df)
#' head(TjSuper_df)
"TjSuper_df"
