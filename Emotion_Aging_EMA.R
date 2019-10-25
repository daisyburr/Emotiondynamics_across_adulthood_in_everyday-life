#updated 10/21/19 by DB based on peer review
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
d$swlsmean <- scale(d$swlsmean)

#Race brekadown
prop.table(table(d$race))
#Asian         Black        Hispanic       White 
#0.036050799 0.091356002 0.009832036 0.862761163

succ_reg_table <- table(d$succ_reg)
prop.table(succ_reg_table)
#0 unsuccesful          1 successful
#0.08394758              0.91605242 


#average responses
average_responses <- aggregate(date_ts ~ vsubject, d, length)
describe(average_responses)
#         vars   n  mean    sd median trimmed   mad min max range  skew kurtosis   se
#subject    1 117 63.29 35.71     64   63.54 45.96   1 123   122 -0.05    -1.23 3.30
#date_ts     2 117 20.87 10.04     24   21.53  7.41   1  41    40 -0.67    -0.76 0.93


#*internal consistency of affect composites####
library(psych)
pos_com <- data.frame(d$lap, d$hap, d$p)
neg_com <- data.frame(d$lan, d$han, d$n)

alpha(pos_com)
#raw_alpha std.alpha G6(smc) average_r S/N   ase mean   sd median_r
#0.63      0.64     0.6      0.37 1.7 0.013  2.9 0.82     0.47

alpha(neg_com)
#raw_alpha std.alpha G6(smc) average_r  S/N   ase mean   sd median_r
#0.38      0.44    0.41      0.21 0.79 0.022  1.5 0.57     0.11

#*MODELS####
library(optimx) #lmer optimizer
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
library(cowplot)


#*mean affect####
#*pos####
#just age
P_mean_age <- lmer(p_avg ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_mean_age, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.02
#Pseudo-R² (total) = 0.36 
#                     Est.   2.5%   97.5%   t val.     d.f.      p
#(Intercept)         2.87   2.78    2.97    60.76   114.66   0.00
#age                 0.13   0.03    0.22     2.64   113.86   0.01


#age and wb
P_mean <- lmer(p_avg ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_mean, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.08
#Pseudo-R² (total) = 0.37 
#                     Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          2.88    2.79    2.96    65.63   112.01   0.00
#age                  0.14    0.06    0.23     3.24   111.12   0.00
#swlsmean             0.18    0.10    0.25     4.62   109.68   0.00
#age:swlsmean         0.02   -0.05    0.10     0.57   107.95   0.57

P_age_controlling_wb <- effect_plot(P_mean, pred = "age",  x.label = "Age \n (controlling for well-being)", y.label = "Positive affect", interval=TRUE, plot.points = TRUE)
P_age_controlling_wb <- P_age_controlling_wb + theme_classic()

P_age_wb <- interact_plot(P_mean, pred = age, modx = swlsmean, x.label = "Age \n (non-significant interaction)", y.label = "Positive affect", interval = TRUE, plot.points = TRUE, legend.main = "Well-being") 
P_age_wb <- P_age_wb + theme_classic() + theme(legend.position = "bottom")

#*supplemental pos (just pos not pooled variance)
P_supp_age <- lmer(p ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_supp_age, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.02
#Pseudo-R² (total) = 0.36 
#                     Est.   2.5%   97.5%   t val.     d.f.      p
#(Intercept)         3.19   3.07    3.30    53.75   114.36   0.00
#age                 0.16   0.04    0.28     2.65   113.54   0.01

#age and wb
P_supp_age_wb <- lmer(p ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(P_supp_age_wb, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.09
#Pseudo-R² (total) = 0.36 

# Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          3.19    3.08    3.29    59.23   109.74   0.00
#age                  0.19    0.08    0.29     3.43   109.46   0.00
#swlsmean             0.27    0.17    0.38     5.02   108.69   0.00
#age:swlsmean         0.05   -0.06    0.15     0.84   107.31   0.41

#*neg####
#just age
N_mean_age <- lmer(n_avg ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_mean_age, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.03
#Pseudo-R² (total) = 0.30 
#                     Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          1.51    1.46    1.57    51.65   114.14   0.00
#age                 -0.10   -0.16   -0.04    -3.47   112.92   0.00

#age and wb
N_mean <- lmer(n_avg ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_mean, center = TRUE,  confint = TRUE)
#Pseudo-R² (fixed effects) = 0.06
#Pseudo-R² (total) = 0.30 
#                       Est.      2.5%   97.5%   t val. d.f.     p
#(Intercept)           1.51    1.45    1.56    53.43   110.99   0.00
#age                  -0.11   -0.17   -0.06    -3.90   109.77   0.00
#swlsmean             -0.08   -0.13   -0.02    -2.63   108.47   0.01
#age:swlsmean         -0.06   -0.12   -0.01    -2.22   106.35   0.03


N_age_controlling_wb <- effect_plot(N_mean_age, pred = "age", x.label = "Age \n (controlling for well-being)", y.label = "Negative affect", interval=TRUE, plot.points = TRUE)
N_age_controlling_wb <- N_age_controlling_wb + theme_apa()

N_age_wb <- interact_plot(N_mean, pred = "age", modx = "swlsmean", x.label = "Age", y.label = "Negative affect", legend.main = "Well-being", interval = TRUE, plot.points = TRUE)
N_age_wb <- N_age_wb + theme_classic() + theme(legend.position = "bottom")


#shared legend
legend <- get_legend(
  P_age_wb + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plots <- plot_grid(
  P_age_controlling_wb + theme(legend.position="none"),
  N_age_controlling_wb+ theme(legend.position="none"),
  P_age_wb + theme(legend.position="none"),
  N_age_wb + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 2, 
  ncol = 2
)

plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))


ggsave("mean_affect_effects.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 7, dpi = 300)

#*supplemental neg (just neg not pooled variance)
#just age
N_supp_age <- lmer(n ~ age + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_supp_age, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.00
#Pseudo-R² (total) = 0.28 
#Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)          1.33    1.26    1.41    34.30   115.34   0.00
#age                 -0.04   -0.11    0.04    -0.93   114.22   0.35

#age and wb
N_supp_age_wb <- lmer(n ~ age * swlsmean + (1|subject), data = d, control = lmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "bobyqa", starttests = FALSE, kkt = FALSE)))
summ(N_supp_age_wb, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.05
#Pseudo-R² (total) = 0.28 
#                      Est.     2.5%   97.5%   t val.     d.f.      p
#(Intercept)           1.33    1.25    1.40    36.34   110.52   0.00
#age                  -0.05   -0.12    0.02    -1.39   109.34   0.17
#swlsmean             -0.13   -0.20   -0.06    -3.48   108.03   0.00
#age:swlsmean         -0.09   -0.16   -0.01    -2.35   105.96   0.02


#*instability####
#*pos####
P <- lmer(p_rmssd_avg ~ age * swlsmean + (1|subject), data = d,
             control = lmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B'), (maxfun=2e9)))
summ(P, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.81
# Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)           0.81    0.80    0.81   229.98   195.89   0.00
#age                  -0.07   -0.08   -0.07   -21.00   202.45   0.00
#swlsmean              0.00   -0.01    0.01     0.28   191.89   0.78
#age:swlsmean         -0.02   -0.03   -0.02    -6.42   192.54   0.00


#age
P_age <- effect_plot(P, pred = "age", x.label = "Age \n (controlling for well-being)", y.label = "Positive affect \n instability", interval = TRUE, plot.points=TRUE)
P_age <- P_age + ylim(0,2) + theme_classic() 

#wb
P_wb <- effect_plot(P, pred ="swlsmean", x.label = "Well-being \n (non-significant)", y.label = "Positive affect \n instability", interval = TRUE, plot.points = TRUE, scale = TRUE)
P_wb <- P_wb + ylim(0,2) + theme_classic()

#int
P_age_wb <- interact_plot(P, pred = "age", modx = "swlsmean", x.label = "Age", y.label = "Positive affect \n instability", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)
P_age_wb <- P_age_wb + ylim(0,2) + theme_classic() + theme(legend.position = "bottom")

#supplmental (just pos not pooled variance)
P_supp <- lmer(p_rmssd ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                     optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(P_supp, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.65
#Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           1.01    0.99    1.02   139.50   2.37   0.00
#age                  -0.08   -0.10   -0.07   -11.63   0.50   0.19
#swlsmean              0.04    0.02    0.05     5.18   0.56   0.25
#age:swlsmean         -0.04   -0.05   -0.02    -5.03   0.66   0.21


#*neg####
N <- lmer(n_rmssd_avg ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                 optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(N, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.76
#(Intercept)           0.57    0.56    0.58   114.01   1.29   0.00
#age                  -0.06   -0.07   -0.05   -11.25   0.77   0.10
#swlsmean             -0.04   -0.05   -0.03    -7.95   1.10   0.07
#age:swlsmean         -0.07   -0.08   -0.06   -13.45   0.68   0.11

#age
N_age <- effect_plot(N, pred ="age", x.label = "Age \n (controlling for well-being) \n (non-significant)", y.label = "Negative affect \n instability", interval = TRUE, plot.points = TRUE)
N_age <- N_age + ylim(0,2) + theme_classic()

#test
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
ggsave("N_age.eps", width = 20, height = 20, device = "eps", family = "Times")

#wb
N_wb <- effect_plot(N, pred ="swlsmean", x.label = "Well-being  \n (non-significant)", y.label = "Negative affect \n instability", interval = TRUE, plot.points = TRUE)
N_wb <- N_wb + ylim(0,2) + theme_classic()

#int
N_age_wb <- interact_plot(N, pred = "age", modx = "swlsmean", x.label = "Age \n (non-significant interaction)", y.label = "Negative affect \n instability", legend.main = "Well-being", interval = TRUE, plot.points=TRUE)
N_age_wb <- N_age_wb + ylim(0,2) + theme_classic() + theme(legend.position = "bottom")


#shared legend
legend <- get_legend(
  P_age_wb + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

plots <- plot_grid(
  P_age + theme(legend.position="none"),
  N_age + theme(legend.position="none"),
  P_wb + theme(legend.position="none"),
  N_wb + theme(legend.position="none"),
  P_age_wb + theme(legend.position="none"),
  N_age_wb + theme(legend.position="none"),
  align = 'vh',
  hjust = -1,
  nrow = 3, 
  ncol = 2
)

plot_grid(plots, legend, ncol = 1, rel_heights = c(1, .1))


ggsave("instability_effects.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 7, dpi = 300)

#supplmental (just neg not pooled variance)
N_supp <- lmer(n_rmssd ~ age * swlsmean + (1|subject), data = d,control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                                      optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
summ(N_supp, center = TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.60
#Est.    2.5%   97.5%   t val.   d.f.      p
#(Intercept)           0.61    0.59    0.63    64.21   6.17   0.00
#age                  -0.07   -0.09   -0.05    -7.42   8.49   0.00
#swlsmean             -0.09   -0.10   -0.07    -8.78   4.43   0.00
#age:swlsmean         -0.07   -0.09   -0.05    -6.92   9.00   0.00

#*age x well being predicting desire strength####
des_stren <- lmer(desire_strength ~ age * swlsmean + (1|subject), data = d)
summ(des_stren, center=TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.03
#Pseudo-R² (total) = 0.30 
#                    Est.    2.5%   97.5%   t val.     d.f.      p
#(Intercept)           4.39    4.25    4.54    59.04   103.95   0.00
#age                   0.21    0.06    0.36     2.80   103.01   0.01
#swlsmean              0.09   -0.05    0.24     1.24   101.74   0.22
#age:swlsmean         -0.11   -0.26    0.04    -1.49    99.99   0.14


#*age x well being predicting proportion desires were present####
library(dplyr)
d$desire_present <- ifelse(d$desire_type != 13, 1, 0)
proportion_desire_present <- d %>% 
  count(subject, desire_present) %>%          
  mutate(prop = prop.table(n))

proportion_desire_present$prop_desire_present <- proportion_desire_present$prop

d <- merge(d, proportion_desire_present, by = "subject")

d$prop_desire_present <- (1-d$prop_desire_present)

des_proportion <- lmer(prop_desire_present ~ age * swlsmean + (1|subject), data = d)
summ(des_proportion, center=TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.00 # no variablity
#Est.    2.5%   97.5%    t val.     d.f.      p
#(Intercept)           0.99    0.99    0.99   2562.34   113.00   0.00
#age                  -0.00   -0.00    0.00     -0.15   113.00   0.88
#swlsmean              0.00   -0.00    0.00      0.02   113.00   0.98
#age:swlsmean          0.00   -0.00    0.00      0.62   113.00   0.54


#*age x well being predicting proportion attempt resist####
library(dplyr)
proportion_attempt_resist <- d %>% 
  count(subject, attempt_resist) %>%          
  mutate(prop = prop.table(n))

proportion_attempt_resist$prop_attempt_resist <- proportion_attempt_resist$prop

d <- merge(d, proportion_attempt_resist, by = "subject")

d$prop_attempt_resist <- (1-d$prop_attempt_resist)

des_proportion_attempt_resist <- lmer(prop_attempt_resist ~ age * swlsmean + (1|subject), data = d)
summ(des_proportion_attempt_resist, center=TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.00 #no variability
#Pseudo-R² (total) = 0.30 
#                      Est.    2.5%   97.5%    t val.    d.f.      p
#(Intercept)           1.00    0.99    1.00   5284.83   96.53   0.00
#age                  -0.00   -0.00    0.00     -0.86   96.31   0.39
#swlsmean              0.00   -0.00    0.00      0.04   95.62   0.97
#age:swlsmean         -0.00   -0.00    0.00     -0.41   94.44   0.68


#*were you successful####
#models cant include desire resist or desire enacted
suc_reg <- glmer(succ_reg ~ others_present + desire_conflict_personal_goals + desire_strength + age + p_rmssd_avg + p_avg + n_rmssd_avg + n_avg + swlsmean + others_present:age + desire_conflict_personal_goals:age + desire_strength:age + p_rmssd_avg:age + p_avg:age + n_rmssd_avg:age + n_avg:age + swlsmean:age + (1|subject), data = d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summ(suc_reg, center=TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.16
#Pseudo-R² (total) = 0.37 
#                                            Est.    2.5%   97.5%   z val.      p
#(Intercept)                                 2.99    2.68    3.31    18.71   0.00
#others_present                             -0.08   -0.33    0.17    -0.61   0.54
#desire_conflict_personal_goals             -0.33   -0.43   -0.24    -6.75   0.00
#desire_strength                            -0.14   -0.25   -0.04    -2.63   0.01
#age                                         0.54    0.22    0.85     3.37   0.00
#p_rmssd_avg                                -0.29   -1.63    1.05    -0.42   0.67
#p_avg                                      -0.22   -0.42   -0.03    -2.26   0.02
#n_rmssd_avg                                 0.77   -0.67    2.21     1.05   0.29
#n_avg                                      -0.50   -0.77   -0.24    -3.68   0.00
#swlsmean                                   -0.06   -0.34    0.23    -0.39   0.70
#others_present:age                         -0.05   -0.32    0.21    -0.40   0.69
#desire_conflict_personal_goals:age         -0.11   -0.22   -0.01    -2.08   0.04
#desire_strength:age                        -0.09   -0.20    0.02    -1.63   0.10
#age:p_rmssd_avg                             1.02   -0.29    2.32     1.52   0.13
#age:p_avg                                   0.00   -0.21    0.21     0.01   0.99
#age:n_rmssd_avg                            -1.39   -2.88    0.11    -1.81   0.07
#age:n_avg                                  -0.33   -0.62   -0.04    -2.23   0.03
#age:swlsmean                               -0.48   -0.77   -0.18    -3.19   0.00

#y axis is predicted probability of successful regulation
succ_reg_age_wb <- interact_plot(suc_reg, pred = age, modx = swlsmean, interval = TRUE, outcome.scale = "response", y.label = "Predicted probabiltiy of successful regulation", x.label = "Age", legend.main = "Well-being")
succ_reg_age_wb <- succ_reg_age_wb + theme_classic() + theme(legend.position = "bottom")

ggsave("regulation_age_wb_effects.eps", plot = last_plot(), device = "eps",
       scale = 1, width = 7, height = 6, dpi = 300)

interact_plot(suc_reg, pred = age, modx = desire_conflict_personal_goals, interval = TRUE, outcome.scale = "response", y.label = "Predicted probabiltiy of successful regulation", x.label = "Age", legend.main = "Well-being")


#test
library(extrafont)
#font_import() only do this one time - it takes a while
loadfonts(device="win")
ggsave("regulation_effects.pdf", width = 20, height = 20, device = "pdf", family = "Times")

coef_names = c("Age:Negative affect" = "age:n_avg", "Age:Positive affect" = "age:p_avg","Age:Well-being" = "age:swlsmean", "Age:Negative affect instability" = "age:n_rmssd_avg", "Age:Positive affect instability" = "age:p_rmssd_avg", "Age:Desire strength" = "desire_strength:age", "Age:Desire conflicts goals" = "desire_conflict_personal_goals:age", "Age:Others present" = "others_present:age", "Well-being" = "swlsmean", "Negative affect instability" = "n_rmssd_avg", "Positive affect instability" = "p_rmssd_avg", "Age" = "age", "Desire strength" = "desire_strength", "Desire conflicts goals" = "desire_conflict_personal_goals", "Others present" = "others_present", "Positive affect" = "p_avg", "Negative affect" = "n_avg")
plot_summs(suc_reg, center = TRUE, plot.distributions = TRUE, robust = TRUE, coefs = coef_names)

ggsave("regulation_effects.pdf", plot = last_plot(), device = "pdf",
       scale = 1, width = 7, height = 6, dpi = 300)


#supplementary (only pos and neg, not averaged)
suc_reg_supp <- glmer(succ_reg ~ others_present + desire_conflict_personal_goals + desire_strength + age + p_rmssd + p + n_rmssd + n + swlsmean + others_present:age + desire_conflict_personal_goals:age + desire_strength:age + p_rmssd:age + p:age + n_rmssd:age + n:age + swlsmean:age + (1|subject), data = d, family = binomial, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=2e5)))
summ(suc_reg_supp, center=TRUE, confint = TRUE)
#Pseudo-R² (fixed effects) = 0.15
#Pseudo-R² (total) = 0.34 
#                                             Est.    2.5%   97.5%   z val.      p
#(Intercept)                                 3.02    2.71    3.33    19.25   0.00
#others_present                             -0.06   -0.31    0.19    -0.44   0.66
#desire_conflict_personal_goals             -0.35   -0.44   -0.25    -7.06   0.00
#desire_strength                            -0.17   -0.27   -0.06    -3.13   0.00
#age                                         0.52    0.22    0.82     3.38   0.00
#p_rmssd                                    -0.19   -1.08    0.69    -0.43   0.67
#p                                          -0.02   -0.16    0.12    -0.28   0.78
#n_rmssd                                     0.28   -0.31    0.88     0.93   0.35
#n                                           0.00   -0.01    0.02     0.29   0.77
#swlsmean                                    0.04   -0.24    0.31     0.25   0.80
#others_present:age                         -0.03   -0.29    0.23    -0.23   0.82
#desire_conflict_personal_goals:age         -0.13   -0.23   -0.03    -2.52   0.01
#desire_strength:age                        -0.11   -0.22   -0.00    -2.02   0.04
#age:p_rmssd                                 0.79   -0.13    1.71     1.68   0.09
#age:p                                       0.06   -0.09    0.21     0.80   0.42
#age:n_rmssd                                -0.48   -1.09    0.14    -1.52   0.13
#age:n                                       0.00   -0.01    0.02     0.29   0.77
#age:swlsmean                               -0.48   -0.77   -0.20    -3.31   0.00

#y axis is predicted probability of successful regulation


