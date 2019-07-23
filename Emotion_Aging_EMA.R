setwd("/Users/daisyburr/Dropbox/Duke/Samanez-Larkin/subval/EMA")


#SETUP####
#*read in####
d <- read.csv("./Emotion_Aging_EMA.csv", header = T)


#*rename####
#did you attempt to resist?
levels(d$attempt_resist)[levels(d$attempt_resist)==1] <- "yes"
levels(d$attempt_resist)[levels(d$attempt_resist)==0] <- "no"

#did you enact the desire?
levels(d$desire_enacted)[levels(d$desire_enacted)==1] <- "yes"
levels(d$desire_enacted)[levels(d$desire_enacted)==0] <- "no"

#ware other ppl around you enacting the desire?
levels(d$others_present)[levels(d$others_present)==1] <- "yes"
levels(d$others_present)[levels(d$others_present)==0] <- "no"

#desire type?
levels(d$desire_type)[levels(d$desire_type)==1] <- "eating_drinking"
levels(d$desire_type)[levels(d$desire_type)==2] <- "alc_tab_drugs"
levels(d$desire_type)[levels(d$desire_type)==3] <- "media"
levels(d$desire_type)[levels(d$desire_type)==4] <- "social_media"
levels(d$desire_type)[levels(d$desire_type)==5] <- "spending"
levels(d$desire_type)[levels(d$desire_type)==6] <- "sex"
levels(d$desire_type)[levels(d$desire_type)==7] <- "sleep"
levels(d$desire_type)[levels(d$desire_type)==8] <- "social_contact"
levels(d$desire_type)[levels(d$desire_type)==9] <- "leisure"
levels(d$desire_type)[levels(d$desire_type)==10] <- "exercise"
levels(d$desire_type)[levels(d$desire_type)==11] <- "work"
levels(d$desire_type)[levels(d$desire_type)==12] <- "other"
levels(d$desire_type)[levels(d$desire_type)==13] <- "none"


#*new variables####
#study
d$study <- ifelse(grepl("DND",d$subject),'DND','SUBVAL')
d$study <- as.factor(d$study)

#succ_reg
library(sjmisc)
library(sjlabelled)
d$succ_reg <- ifelse(d$desire_type != 13 & d$attempt_resist == 1 & d$desire_enacted==1, 0, 1)
set_label(d$succ_reg) <- "Successful Regulation"

#well being mean (average life satisfaction, doesnt make sense to z score if using this)
d$swlsmean <- (d$swls1 + d$swls2 + d$swls3 + d$swls4 + d$swls5)/5

#avg p and n
d$p_avg <- (d$lap + d$hap + d$p)/3
d$n_avg <- (d$lan + d$han + d$n)/3

#date
d$date_ts_format <- as.POSIXct(d$date, format = "%m/%d/%y %H:%M")
#convert to time series
d$date_ts <- ts(d$date_ts_format, frequency = 30)

#remove na post date transformation
d <- na.omit(d)


#*calculate instability####
library(varian)
d$p_rmssd_avg <- rmssd_id(d$p_avg, d$subject, long = TRUE)
d$n_rmssd_avg <- rmssd_id(d$n_avg, d$subject, long = TRUE)

#supplementary just p and n
d$p_rmssd <- rmssd_id(d$p, d$subject, long = TRUE)
d$n_rmssd <- rmssd_id(d$n, d$subject, long = TRUE)


#*structure####
d$subject <- as.factor(d$subject)
d$gender <- as.factor(d$gender)
d$desire_type <- as.factor(d$desire_type)
d$date <- as.factor(d$date)
d$steps <- as.numeric(d$steps)
d$desire_conflict_personal_goals <- as.numeric(d$desire_conflict_personal_goals)
d$desire_strength <- as.numeric(d$desire_strength)
d$others_present <- as.factor(d$others_present)
d$attempt_resist <- as.factor(d$attempt_resist)
d$desire_enacted <- as.factor(d$desire_enacted)
d$succ_reg <- as.factor(d$succ_reg)
d$swlsmean <- as.numeric(d$swlsmean)
d$race <- as.factor(d$race)

#drop unused
d$gender <- droplevels(d$gender)

#confirm str
str(d)

#*descriptives####
library(Hmisc)
describe(d$gender)
#mean age = 41
#age range = 20-80
#female = .546 male - 0.454

library(psych)
describe(d$age)
#n         mean    sd    median   trimmed   mad     min    max    range    skew     kurtosis   se
#2441      41.1   15.49     40    40.49 2    0.76  20       80    60        0.24    -1.31     0.31


#*scale####
#age
d$age <- scale(d$age)

#Race brekadown
prop.table(table(d$Race))
#missing             #Asian       Black    Hispanic       White 
#0.328751097 0.009066979 0.054109389 0.008482012 0.599590524 

describe(d$succ_reg)
#       n  missing distinct 
#2434        0        2 

#Value          0     1
#Frequency    204  2230
#Proportion 0.084 0.916



#*MODELS####
library("optimx") #lmer optimizer
library(lme4) #for mixed models
library(lmerTest) #for mixed models; calculate p vals with Satterthwaite’s method
library(jtools) #for plots; p values calculated using Satterthwaite d.f.
library(interactions) #new jtools interactions plots
library(sjPlot) #for tables
library(car) #for anova type 3
library(sjlabelled) #for plots
library(ggplot2)
library(gridExtra)
library(grid)

#*mean affect####
#*pos####
#just age
P_mean_age <- lmer(p_avg ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_mean_age, center = TRUE, confint = TRUE)
#                     Est.   2.5%   97.5%   t val.     d.f.      p
#(Intercept)         2.87   2.78    2.97    60.84   114.62   0.00
#age                 0.12   0.03    0.22     2.61   113.83   0.01

P_age <- effect_plot(P_mean_age, pred = "age",  x.label = "Age", y.label = "Positive Affect", interval=TRUE, plot.points = TRUE)

export_summs(P_mean_age, error_format = "({std.error})",
             error_pos = c("below", "right", "same"), ci_level = 0.95,
             statistics = c(N = "nobs", R2 = "r.squared"), model.names = "Effect of Age on Positive Affect",
             to.file = "docx", file.name = "Pos_Affect_Age.docx", scale = TRUE, robust = TRUE)

#age and wb
P_mean <- lmer(p_avg ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_mean, center = TRUE, confint = TRUE)
#                     Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          2.88    2.79    2.96    65.63   112.01   0.00
#age                  0.14    0.06    0.23     3.24   111.12   0.00
#swlsmean             0.18    0.10    0.25     4.62   109.68   0.00
#age:swlsmean         0.02   -0.05    0.10     0.57   107.95   0.57

export_summs(P_mean, error_format = "({std.error})",
             error_pos = c("below", "right", "same"), ci_level = 0.95,
             statistics = c(N = "nobs", R2 = "r.squared"),
             to.file = "docx", file.name = "test.docx", scale = TRUE, robust = TRUE)


P_age_controlling_wb <- effect_plot(P_mean, pred = "age",  x.label = "Age \n (controlling for well-being)", y.label = "Positive Affect", interval=TRUE, plot.points = TRUE)
P_age_wb <- interact_plot(P_mean, pred = age, modx = swlsmean, x.label = "Age \n (non-significant interaction)", y.label = "Positive Affect", interval = TRUE, plot.points = TRUE, legend.main = "Well-being") 


#tables
export_summs(P_mean_age, P_mean, error_format = "({std.error})",
             error_pos = c("below", "right", "same"), ci_level = 0.90,
             statistics = c(N = "nobs", R2 = "r.squared"),
             to.file = "docx", file.name = "test.docx", scale = TRUE, robust = TRUE)

#*supplemental pos (just pos)
P_supp_age <- lmer(p ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_supp_age, center = TRUE, confint = TRUE)
#                     Est.   2.5%   97.5%   t val.     d.f.      p
#(Intercept)         3.19   3.07    3.30    53.84   114.32   0.00
#age                 0.16   0.04    0.27     2.63   113.52   0.01

#age and wb
P_supp_age_wb <- lmer(p ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_supp_age_wb, center = TRUE, confint = TRUE)
#                       Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          3.19    3.08    3.29    59.49   109.03   0.00
#age                  0.18    0.08    0.29     3.38   108.08   0.00
#swlsmean             0.24    0.15    0.33     5.03   106.69   0.00
#age:swlsmean         0.04   -0.05    0.13     0.86   104.90   0.39

#*neg####
#just age
N_mean_age <- lmer(n_avg ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_mean_age, center = TRUE, confint = TRUE)

#                     Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          1.51    1.46    1.57    51.62   114.23   0.00
#age                 -0.10   -0.16   -0.04    -3.47   113.03   0.00

N_age <- effect_plot(N_mean_age, pred = "age", x.label = "Age", y.label = "Negative Affect", interval=TRUE, plot.points = TRUE)

#age and wb
N_mean <- lmer(n_avg ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_mean, center = TRUE,  confint = TRUE)
#                       Est.      2.5%   97.5%   t val. d.f.     p
#(Intercept)           1.51    1.45    1.56    53.40   111.05   0.00
#age                  -0.11   -0.17   -0.06    -3.90   109.85   0.00
#swlsmean             -0.07   -0.11   -0.02    -2.62   108.47   0.01
#age:swlsmean         -0.05   -0.10   -0.01    -2.23   106.38   0.03

N_age_controlling_wb <- effect_plot(N_mean_age, pred = "age", x.label = "Age \n (controlling for well-being)", y.label = "Negative Affect", interval=TRUE, plot.points = TRUE)
N_wb <- effect_plot(N_mean, pred = "swlsmean", x.label = "Well-being", y.label = "Negative Affect", legend.main = "Well-being", interval = TRUE, plot.points = TRUE)
N_age_wb <- interact_plot(N_mean, pred = "age", modx = "swlsmean", x.label = "Age", y.label = "Negative Affect", legend.main = "Well-being", interval = TRUE, plot.points = TRUE)

grid.arrange(P_age,N_age,P_age_controlling_wb, N_age_controlling_wb, P_age_wb, N_age_wb, nrow = 2)

#*supplemental neg (just neg)
#just age
N_supp_age <- lmer(n ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_supp_age, center = TRUE, confint = TRUE)
#                     Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          1.33    1.26    1.41    34.32   115.40   0.00
#age                 -0.04   -0.11    0.04    -0.93   114.29   0.35


#age and wb
N_supp_age_wb <- lmer(n ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_supp_age_wb, center = TRUE, confint = TRUE)
#                      Est.     2.5%   97.5%   t val.     d.f.      p
#(Intercept)           1.33    1.25    1.40    36.36   110.57   0.00
#age                  -0.05   -0.12    0.02    -1.39   109.40   0.17
#swlsmean             -0.11   -0.17   -0.05    -3.47   108.03   0.00
#age:swlsmean         -0.07   -0.14   -0.01    -2.36   105.97   0.02


#*instability####
#optimizer
nlopt <- function(par, fn, lower, upper, control) {
  .nloptr <<- res <- nloptr(par, fn, lb = lower, ub = upper, 
                            opts = list(algorithm = "NLOPT_LN_BOBYQA", print_level = 1,
                                        maxeval = 1000, xtol_abs = 1e-6, ftol_abs = 1e-6))
  list(par = res$solution,
       fval = res$objective,
       conv = if (res$status > 0) 0 else res$status,
       message = res$message
  )
}

#*pos####
P <- lmer(p_rmssd_avg ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

summ(P, center = TRUE, confint = TRUE)
#                      Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           0.80    0.79    0.81   163.78   0.78   0.01
#age                  -0.08   -0.09   -0.07   -15.07   1.80   0.01
#swlsmean              0.00   -0.01    0.01     0.34   1.13   0.79
#age:swlsmean         -0.02   -0.03   -0.01    -4.87   2.32   0.03

#age
P_age <- effect_plot(P, pred = "age", x.label = "Age", y.label = "Positive Affect Instability", interval = TRUE, plot.points=TRUE)
P_age <- P_age + ylim(0,2)

#wb
P_wb <- effect_plot(P, pred ="swlsmean", x.label = "Well-being \n (non-significant)", y.label = "Positive Affect Instability", interval = TRUE, plot.points = TRUE, scale = TRUE)
P_wb <- P_wb + ylim(0,2)

#int
P_age_wb <- interact_plot(P, pred = "age", modx = "swlsmean", x.label = "Age", y.label = "Positive Affect Instability", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)
P_age_wb <- P_age_wb + ylim(0,2)

#supplmental (just pos not pooled variance)
P_supp <- lmer(p_rmssd ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(P_supp, center = TRUE, confint = TRUE)
#Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           1.02    1.01    1.04   141.50   6.07   0.00
#age                  -0.09   -0.10   -0.07   -11.88   5.32   0.00
#swlsmean              0.03    0.02    0.04     5.03   5.96   0.00
#age:swlsmean         -0.03   -0.04   -0.01    -4.13   4.45   0.01
effect_plot(P_supp, pred = "age", x.label = "Age", interval = TRUE, plot.points=TRUE)
effect_plot(P_supp, pred = "swlsmean", x.label = "Well-being", interval = TRUE, plot.points=TRUE)

interact_plot(P_supp, pred = "age", modx = "swlsmean", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)

#*neg####
N <- lmer(n_rmssd_avg ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(N, center = TRUE, confint = TRUE)
#                     Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           0.57    0.56    0.58   115.00   0.16   0.37
#age                  -0.05   -0.06   -0.04   -10.02   0.48   0.21
#swlsmean             -0.03   -0.04   -0.03    -7.82   1.81   0.02
#age:swlsmean         -0.06   -0.07   -0.05   -13.44   0.49   0.18

#age
N_age <- effect_plot(N, pred ="age", x.label = "Age \n (non-significant)", y.label = "Negative Affect Instability", interval = TRUE, plot.points = TRUE, scale = TRUE)
N_age <- N_age + ylim(0,2)

#wb
N_wb <- effect_plot(N, pred ="swlsmean", x.label = "Well-being", y.label = "Negative Affect Instability", interval = TRUE, plot.points = TRUE, scale = TRUE)
N_wb <- N_wb + ylim(0,2)

#int
N_age_wb <- interact_plot(N, pred = "age", modx = "swlsmean", x.label = "Age \n (non-significant interaction)", y.label = "Negative Affect Instability", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)
N_age_wb <- N_age_wb + ylim(0,2)

grid.arrange(P_age, N_age, P_wb, N_wb, P_age_wb, N_age_wb, as.table = FALSE, nrow = 2)


#supplmental (just neg not pooled variance)
N_supp <- lmer(n_rmssd ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(N_supp, center = TRUE, confint = TRUE)
#Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           0.61    0.59    0.63    64.22   8.08   0.00
#age                  -0.07   -0.09   -0.05    -7.12   8.21   0.00
#swlsmean             -0.07   -0.09   -0.06    -8.74   8.11   0.00
#age:swlsmean         -0.06   -0.08   -0.04    -7.02   6.53   0.00


effect_plot(N_supp, pred = "swlsmean", x.label = "Well-being", interval = TRUE, plot.points=TRUE)
interact_plot(N_supp, pred = "age", modx = "swlsmean", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)


#*were you successful####
#models cant include desire resist or desire enacted
suc_reg <- glmer(succ_reg ~ others_present + desire_conflict_personal_goals + desire_strength + age + p_rmssd_avg + p_avg + n_rmssd_avg + n_avg + swlsmean + others_present:age + desire_conflict_personal_goals:age + desire_strength:age + p_rmssd_avg:age + p_avg:age + n_rmssd_avg:age + n_avg:age + swlsmean:age + (1|subject), data = d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summ(suc_reg, center=TRUE, confint = TRUE)
#others_present                             -0.14   -0.48    0.21    -0.77   0.44
#desire_conflict_personal_goals             -0.36   -0.49   -0.23    -5.37   0.00
#desire_strength                            -0.14   -0.28   -0.00    -1.99   0.0466
#age                                         0.49    0.18    0.81     3.07   0.00
#p_rmssd_avg                                -0.14   -1.40    1.11    -0.22   0.82
#p_avg                                      -0.21   -0.46    0.05    -1.59   0.11
#n_rmssd_avg                                 0.71   -0.69    2.10     0.99   0.32
#n_avg                                      -0.45   -0.81   -0.09    -2.47   0.01
#swlsmean                                   -0.03   -0.25    0.20    -0.23   0.82
#others_present:age                         -0.02   -0.38    0.34    -0.11   0.91
#desire_conflict_personal_goals:age         -0.08   -0.22    0.05    -1.19   0.24
#desire_strength:age                        -0.07   -0.22    0.07    -0.98   0.32
#age:p_rmssd_avg                             0.90   -0.32    2.12     1.45   0.15
#age:p_avg                                  -0.00   -0.27    0.27    -0.02   0.99
#age:n_rmssd_avg                            -1.31   -2.73    0.12    -1.80   0.07
#age:n_avg                                  -0.23   -0.61    0.16    -1.16   0.25
#age:swlsmean                               -0.31   -0.54   -0.08    -2.66   0.01

#y axis is predicted probability of successful regulation
effect_plot(suc_reg, pred = "n_avg", x.label = "Negative affect", interval = TRUE, plot.points=TRUE)
succ_reg_age_wb <- interact_plot(suc_reg, pred = age, modx = swlsmean, interval = TRUE, outcome.scale = "response", y.label = "Predicted probabiltiy of successful regulation", x.label = "Age", legend.main = "Well-being")
succ_reg_age_nrmssd <- interact_plot(suc_reg, pred = age, modx = n_rmssd_avg, interval = TRUE, outcome.scale = "response", y.label = "Predicted probabiltiy of successful regulation", x.label = "Age (marginally \n significant interaction", legend.main = "Negative affect instability")

grid.arrange(succ_reg_age_wb, succ_reg_age_nrmssd)

coef_names = c("Age:Negative affect" = "age:n_avg", "Age:Positive affect" = "age:p_avg","Age:Well-being" = "age:swlsmean", "Age:Negative affect instability" = "age:n_rmssd_avg", "Age:Positive affect instability" = "age:p_rmssd_avg", "Age:Desire strength" = "desire_strength:age", "Age:Desire conflicts goals" = "desire_conflict_personal_goals:age", "Age:Others present" = "others_present:age", "Well-being" = "swlsmean", "Negative affect instability" = "n_rmssd_avg", "Positive affect instability" = "p_rmssd_avg", "Age" = "age", "Desire strength" = "desire_strength", "Desire conflicts goals" = "desire_conflict_personal_goals", "Others present" = "others_present", "Positive affect" = "p_avg", "Negative affect" = "n_avg")
plot_summs(suc_reg_2, center = TRUE, plot.distributions = TRUE, robust = TRUE, coefs = coef_names)


#supplementary (only pos and neg, not averaged)
suc_reg_supp <- glmer(succ_reg ~ others_present + desire_conflict_personal_goals + desire_strength + age + p_rmssd + p + n_rmssd + n + swlsmean + others_present:age + desire_conflict_personal_goals:age + desire_strength:age + p_rmssd:age + p:age + n_rmssd:age + n:age + swlsmean:age + (1|subject), data = d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summ(suc_reg_supp, center=TRUE, confint = TRUE)
#                                             Est.    2.5%   97.5%   z val.      p
#others_present                             -0.14   -0.48    0.21    -0.78   0.43
#desire_conflict_personal_goals             -0.37   -0.50   -0.24    -5.59   0.00
#desire_strength                            -0.15   -0.29   -0.01    -2.14   0.03
#age                                         0.52    0.20    0.84     3.23   0.00
#p_rmssd                                    -0.03   -0.87    0.81    -0.08   0.94
#p                                          -0.07   -0.28    0.13    -0.72   0.47
#n_rmssd                                     0.39   -0.21    0.99     1.29   0.20
#n                                          -0.18   -0.44    0.09    -1.30   0.19
#swlsmean                                    0.04   -0.18    0.26     0.33   0.74
#others_present:age                         -0.01   -0.36    0.35    -0.04   0.97
#desire_conflict_personal_goals:age         -0.12   -0.25    0.02    -1.65   0.10
#desire_strength:age                        -0.08   -0.22    0.06    -1.13   0.26
#age:p_rmssd                                 0.63   -0.24    1.50     1.41   0.16
#age:p                                      -0.01   -0.22    0.21    -0.05   0.96
#age:n_rmssd                                -0.25   -0.86    0.35    -0.82   0.41
#age:n                                      -0.16   -0.45    0.13    -1.07   0.28
#age:swlsmean                               -0.31   -0.54   -0.09    -2.69   0.01



