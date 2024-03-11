library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(quickpsy)
library(formattable)
library(readr)
library(patchwork)
library(tidyverse)
library(Rmisc)
library(emmeans)
library(lmerTest)
library(ggpubr)

# read in and logit transform data ####
logit_pct_trans <- function(x) {
  x[x==0] <- 0.5*min(x[x!=0])
  x[x==100] <- 100 - 0.5*min(x[x!=0])
  qlogis(x/100)
}

# read in and inverse logit transform data ####
inv_logit_pct_trans <- function(x) {
  plogis(x)*100
}

data_folder <- "~/R_scripts/"

ccaf_data_long <- read_excel(
  paste0(data_folder, "data.xlsx"),
  range = "A1:Q61"
  ) %>% 
  select(
    matches("Code") | ends_with("group") | matches("sex") | starts_with("%") | matches("AUDIT")  | matches("drinksweek") | matches("AUD")
    ) %>% 
  mutate(group = if_else( # 1 = light where 2 = heavy
    group == "1",
    "light",
    "heavy"
  )) %>% 
  pivot_longer(
    cols = starts_with("%"),
    names_to = "alcohol_value_cha",
    names_prefix = "%alcohol ",
    values_to = "pct_chose_alcohol"
  ) %>% 
  mutate(
    alcohol_value_num = as.numeric(alcohol_value_cha),
    prop_chose_alcohol = pct_chose_alcohol/100,
    N = 32,
    n_chose_alcohol = prop_chose_alcohol*N,
    pct_chose_alcohol_logit = logit_pct_trans(pct_chose_alcohol),
    inv_pct_chose_alcohol_logit = inv_logit_pct_trans(pct_chose_alcohol_logit)
    )

# plot to check our inverse function works
ccaf_data_long %>% 
  ggplot(aes(x = prop_chose_alcohol, y = inv_pct_chose_alcohol_logit)) +
  geom_point()

mean(ccaf_data_long$pct_chose_alcohol)
inv_logit_pct_trans(mean(ccaf_data_long$pct_chose_alcohol_logit))
mean(ccaf_data_long$inv_pct_chose_alcohol_logit)

# save transformed data #
ccaf_data_long %>% 
  write_tsv("./ccaf_data_long.txt")

# descriptives by group
by_group_logit <- summarySE(ccaf_data_long, measurevar="pct_chose_alcohol_logit", groupvars=c("group"), conf.interval = 0.95)
by_group_logit %>% 
  mutate(
    ci_lower = pct_chose_alcohol_logit - ci,
    ci_upper = pct_chose_alcohol_logit + ci
  ) %>% 
  select(-c("sd","se","ci")) %>% 
  as_tibble() %>% 
  mutate(
    pct_chose_alcohol_logit_inv = inv_logit_pct_trans(pct_chose_alcohol_logit),
    ci_lower_inv = inv_logit_pct_trans(ci_lower),
    ci_upper_inv = inv_logit_pct_trans(ci_upper)
  )

# sneak peak figure for light drinkers
ccaf_data_long %>% 
  filter(group == "light") %>% 
  ggplot(aes(x = group, y = pct_chose_alcohol)) +
  geom_violin() +
  geom_hline(yintercept = 16.8) +
  geom_hline(yintercept = 41.0)

# descriptives by relative point value
by_alc_logit <- summarySE(ccaf_data_long, measurevar="pct_chose_alcohol_logit", groupvars=c("alcohol_value_cha"), conf.interval = 0.95)
by_alc_logit %>% 
  mutate(
    ci_lower = pct_chose_alcohol_logit - ci,
    ci_upper = pct_chose_alcohol_logit + ci
    ) %>% 
  select(-c("sd","se","ci")) %>% 
  as_tibble() %>% 
  mutate(
    pct_chose_alcohol_logit_inv = inv_logit_pct_trans(pct_chose_alcohol_logit),
    ci_lower_inv = inv_logit_pct_trans(ci_lower),
    ci_upper_inv = inv_logit_pct_trans(ci_upper)
  )

# sneak peak figures 
ccaf_data_long %>% 
  filter(alcohol_value_cha == "-2") %>%  
  ggplot(aes(x = "-2", y = pct_chose_alcohol_logit)) +
  geom_violin() 

ccaf_data_long %>% 
  filter(alcohol_value_cha == "+2") %>%  
  ggplot(aes(x = "+2", y = pct_chose_alcohol)) +
  geom_violin() 

# descriptives by sex
by_group_logit <- summarySE(ccaf_data_long, measurevar="pct_chose_alcohol_logit", groupvars=c("sex"), conf.interval = 0.95)
by_group_logit %>% 
  mutate(
    ci_lower = pct_chose_alcohol_logit - ci,
    ci_upper = pct_chose_alcohol_logit + ci
  ) %>% 
  select(-c("sd","se","ci")) %>% 
  as_tibble() %>% 
  mutate(
    pct_chose_alcohol_logit_inv = inv_logit_pct_trans(pct_chose_alcohol_logit),
    ci_lower_inv = inv_logit_pct_trans(ci_lower),
    ci_upper_inv = inv_logit_pct_trans(ci_upper)
  )

# sneak peak figures female
ccaf_data_long %>% 
  filter(sex == "1") %>%  
  ggplot(aes(x = "1", y = pct_chose_alcohol_logit)) +
  geom_violin() 

# sneak peak figures male
ccaf_data_long %>% 
  filter(sex == "2") %>%  
  ggplot(aes(x = "2", y = pct_chose_alcohol_logit)) +
  geom_violin() 

# plots' appearance ####
theme_set(theme_classic(base_size = 16) + theme(legend.position="none")) ## 14 for individual PSE

theme_nofacetbox <- theme(strip.background = element_blank(), 
                          strip.text = element_text(colour = 'black'),
                          show.legend=FALSE)

group_colours <- c(
  rgb(255,0,0, maxColorValue = 255), # heavy
  rgb(0,0,255, maxColorValue = 255) # light
  )

alc_order <- c("+2","0","-2")

alc_colours <- c(
  rgb(0, 204, 0, maxColorValue = 255), # +2
  rgb(0, 102, 204, maxColorValue = 255), # 0
  rgb(204, 0, 0, maxColorValue = 255) # -2
)

alc_linetypes <- c(
  "dashed", # +2
  "dashed", # 0
  "dashed" # -2
)

# Lme model on logit transformed data - group as factor
#library(lmerTest) # https://stats.stackexchange.com/a/138720
#library(emmeans) #https://stats.stackexchange.com/questions/237512/how-to-perform-post-hoc-test-on-lmer-model
ccaf_data_long$alcohol_value_cha <- factor(ccaf_data_long$alcohol_value_cha, levels = c("-2", "0", "+2"))
ccaf_lme_group <- lmer(pct_chose_alcohol_logit ~ group * alcohol_value_cha * sex + (1 | Code), data = ccaf_data_long)
summary(ccaf_lme_group)
anova(ccaf_lme_group)
effectsize::eta_squared(ccaf_lme_group)
emeans <- lsmeans(ccaf_lme_group, ~ sex*alcohol_value_cha)
print(emeans)
pairwise_diff <- pairs(emeans, adjust = "tukey")
print(pairwise_diff)

# Lme model on logit transformed data - AUDIT as factor
ccaf_lme_AUDIT <- lmer(pct_chose_alcohol_logit ~ AUDIT * alcohol_value_num * sex + (1|Code), data =ccaf_data_long)
summary(ccaf_lme_AUDIT)
anova(ccaf_lme_AUDIT)
effectsize::eta_squared(ccaf_lme_AUDIT)

# Scatterplot AUDIT by drinksweek with correlation coefficient and p-value (for reviewer)
scatter_AUDIT_drinksweek <- ggscatter(ccaf_data_long, x = "AUDIT", y = "drinksweek", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "AUDIT scores", ylab = "Drinks/week")

ggsave("./Fig_scatterplot_AUDIT_drinksweek.pdf")
(scatter_AUDIT_drinksweek)

# Chi square comparison of frequencies of "AUD" based on AUDIT ≥ 7 (Källmen et al. 2019) 
contingency_table_AUD <- table(filter(ccaf_data_long, alcohol_value_cha == 0)$group, 
                           filter(ccaf_data_long, alcohol_value_cha == 0)$AUD)

print(contingency_table_AUD)
chi_sq_AUD <- chisq.test(contingency_table_AUD)
print(chi_sq_AUD)


# logistic regression model of choice by alcohol value, group level ####
qp_group <- quickpsy(
  d = ccaf_data_long,
  x = alcohol_value_num,
  k = n_chose_alcohol,
  n = N,
  grouping = c("group"),
  log = FALSE,
  fun = logistic_fun,
  B = 1000
)

save(qp_group,file = "./qp_group.RData")
load("./qp_group.RData")

## Point of subjective equivalence (value of alcohol to be chosen as often as the snacks) ####
PSE_group <- qp_group$par %>% 
  filter(parn == "p1") %>%
  select(-parn) %>% 
  dplyr::rename(
    PSE = par,
    `CI lower` = parinf,
    `CI upper` = parsup
  )

PSE_group %>% formattable(digits = 2)

## slope (cost sensitivity)
slope_group <- qp_group$par %>% 
  filter(parn == "p2") %>%
  select(-parn) %>% 
  dplyr::rename(
    slope = par,
    `CI lower` = parinf,
    `CI upper` = parsup
  ) 

slope_group %>% formattable(digits = 2)

# comparisons
qp_group$parcomparisons

## sigmoid curves fit ####
quartz(width = 7.6, height = 5.6)
dev.size() # to check the size of manually resized quartz window

sigmoid_group <- ggplot() +
  geom_segment(data = PSE_group, aes(y = 50, yend = -Inf, x = PSE, xend = PSE, colour = group), lty =2, size = 0.4) + #vertical 50%
  geom_segment(data = PSE_group, aes(y = 50, yend = 50, x = -Inf, xend = PSE, colour = group), lty = 2, size = 0.4) + #horizontal 50%
  geom_line(data = qp_group$curves,
            mapping = aes(x=x, y=y*100, colour = group),
            size = 1.2) +
  geom_point(
    data = ccaf_data_long,
    mapping = aes(x = alcohol_value_num, y = pct_chose_alcohol, colour = group),
    position = position_jitter(width = 0.2, height = 0),
    shape = 16, alpha = 0.17, size = 4
  ) +
  scale_colour_manual(values = group_colours) +
  labs(y = "Percentage alcohol choice", x = "Relative point level" 
  )

ggsave("./Fig_sigmoid_by_group.pdf")
(sigmoid_group)

# model by individuals (otherwise as above) ####
# takes days to run, you can instead run below load("./qp_ind.RData")

#qp_ind <- quickpsy(
#  d = ccaf_data_long,
#  x = alcohol_value_num,
#  k = n_chose_alcohol,
#  n = N,
#  grouping = c("group","Code"),
#  log = FALSE,
#  fun = logistic_fun,
#  B = 1000
#)

#save(qp_ind,file = "./qp_ind.RData")
load("./qp_ind.RData")

## PSEs for each individual ####

PSE_ind <- qp_ind$par %>% 
  filter(parn == "p1") %>%
  select(-parn) %>% 
  dplyr::rename(
    PSE = par,
    `CI lower` = parinf,
    `CI upper` = parsup
  ) %>% 
  mutate(
    `PSE out of range` = PSE < -2 | PSE > 2,
    preference = case_when(
      Code == "AC_081" ~ "alcohol", # PSE/curve fit didn't work for this one, corrected manually
      `CI upper` < 0 ~ "alcohol",
      `CI lower` > 0 ~ "snacks",
      TRUE ~ "neither" # CI covers 0
    )
  ) 

PSE_ind %>% 
  group_by(preference) %>% 
  tally()

PSE_ind %>% 
  group_by(group, preference) %>% 
  tally()

PSE_ind %>% 
  group_by(group, `PSE out of range`) %>% 
  tally()

PSE_ind %>% 
  group_by(group, `PSE out of range`, preference) %>% 
  tally() %>% 
  filter(`PSE out of range`)

PSE_ind %>% 
  filter(`PSE out of range`)

PSE_ind %>% 
  write_csv("./PSE_individual.csv")

## curves for individuals ####

ind_figure_data <- list()
for (pref in c("alcohol", "neither", "snacks")) {
  
  ordered_codes <- PSE_ind %>% filter(preference == pref) %>% 
    arrange(group)%>% pull(Code)
  
  ind_figure_data[[pref]] <- list(
    "PSEs" = filter(PSE_ind, Code %in% ordered_codes),
    "curves" = filter(qp_ind$curves, Code %in% ordered_codes),
    "choices" = filter(ccaf_data_long, Code %in% ordered_codes),
    "ordered_codes" = ordered_codes
    
  )
}

rm(pref,ordered_codes)

plot_ind_curves <- function(fig_data, pref) {
  
  ggplot( data = fig_data[["PSEs"]] ) +
    facet_wrap(
      . ~ factor(Code, levels = fig_data[["ordered_codes"]]), 
      ncol = 8
      ) +
    geom_segment( #vertical 50%
      mapping = aes(y = 50, yend = -Inf, x = PSE, xend = PSE, colour = group),
      lty =2, size = 0.2
    ) + 
    geom_segment( #horizontal 50%
      mapping = aes(y = 50, yend = 50, x = -Inf, xend = Inf, colour = group), 
      lty = 2, size = 0.2
    ) + 
    geom_line(
      data = fig_data[["curves"]],
      mapping = aes(x=x, y=y*100, colour = group)
    ) +
    geom_point(
      data = fig_data[["choices"]],
      mapping = aes(x = alcohol_value_num, y = pct_chose_alcohol, colour = group),
      shape = 16, alpha = 0.5, size = 2
    ) +
    scale_x_continuous(breaks = c(-2, 0, 2)) +
    scale_y_continuous(breaks = c(0,50,100)) +
    scale_colour_manual(values = group_colours) +
    coord_cartesian(xlim = c(-2.2,2.2)) +
    labs(
      y = "% alcohol choice", 
      x = "Relative point level", 
      title = paste("Preferred", pref)
    ) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      text = element_text (size = 12),
      axis.line=element_line(size=0.2),
      axis.ticks=element_line(size=0.5),
      axis.text=element_text(size=8)
     )
}

ind_figures <- Map(plot_ind_curves, ind_figure_data, names(ind_figure_data))

quartz(width = 8.6, height = 11.3)

combined_layout <- "
A
A
B
B
B
C
C
C
"

sigmoid_ind <- ind_figures$alcohol / ind_figures$neither / ind_figures$snacks + 
  plot_annotation(tag_level = "A") +
  plot_layout(design = combined_layout, guides = "collect")

ggsave("./Fig_sigmoid_individual.pdf")
(sigmoid_ind)

## combine plots Figure 3 ####
fig3 <- ggarrange (sigmoid_group, sigmoid_ind, widths=c(1,1), ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(fig3, top = text_grob("Choice preference for alcohol compared to snack", size = 20))
ggsave("./Figure3.pdf")

# logistic regression model of choice by AUDIT score ####
#qp_audit <- quickpsy(
#  d = ccaf_data_long,
#  x = AUDIT,
#  k = n_chose_alcohol,
#  n = N,
#  grouping = c("alcohol_value_cha"),
#  log = FALSE,
#  fun = logistic_fun,
#  B = 1000
#)

#save(qp_audit,file = "./qp_audit.RData")
load("./qp_audit.RData")

## sigmoid curves fit to untransformed data ####
sigmoid_plot <- ggplot() +
  geom_line(data = qp_audit$curves,
            mapping = aes(x=x, y=y*100, colour = alcohol_value_cha, linetype = alcohol_value_cha),
            size = 1.2) +
  geom_point(
    data = ccaf_data_long,
    mapping = aes(x = AUDIT, y = pct_chose_alcohol, colour = alcohol_value_cha),
    position = position_jitter(width = 0.3, height = 0),
    shape = 16, alpha = 0.17, size = 3
  ) +
  scale_colour_manual(values = alc_colours, breaks = alc_order) +
  scale_linetype_manual(values = alc_linetypes, breaks = alc_order) +
  labs(y = "Percentage alcohol choice", x = "AUDIT", colour = "Relative point level", linetype = "Relative point level")+
  theme(legend.title = element_text(size=12))

ggsave("./Fig_AUDIT_relative_point_level_untransformed.pdf")
(sigmoid_plot)

## logit transform percentage scores ####
logit_plot <- ggplot() +
  geom_line(data = qp_audit$curves,
            mapping = aes(x=x, y=logit_pct_trans(y*100), colour = alcohol_value_cha, linetype = alcohol_value_cha),
            size = 1.2
            ) +
  geom_point(
    data = ccaf_data_long,
    mapping = aes(x = AUDIT, y = pct_chose_alcohol_logit, colour = alcohol_value_cha),
    position = position_jitter(width = 0.3, height = 0),
    shape = 16, alpha = 0.17, size = 4
    ) +
  scale_colour_manual(values = alc_colours, breaks = alc_order) +
  scale_linetype_manual(values = alc_linetypes, breaks = alc_order) +
  labs(y = "Percentage alcohol choice (logit)", x = "AUDIT", colour = "Relative point level", linetype = "Relative point level") +
  theme(legend.title = element_text(size=12))
  
ggsave("./Fig_AUDIT_relative_point_level_transformed.pdf")
(logit_plot)

## combine plots ####
quartz(width = 12.0, height = 5.8)
sigmoid_plot + logit_plot + plot_layout(guides = "collect") + plot_annotation(tag_level = "A") 
ggsave("./Fig_AUDIT_relative_point_level_combined.pdf")

## combine plots Figure 4 ####
quartz(width = 9.27, height = 5.71)
fig4 <- ggarrange (sigmoid_plot, logit_plot, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")
annotate_figure(fig4, top = text_grob("Choice preference for alcohol by alcohol use severity", size = 20))
ggsave("./Figure4.pdf")

## CIs of slope ####
qp_audit$par %>% 
  filter(parn == "p2") %>%
  select(-parn) %>% 
  dplyr::rename(
    slope = par,
    `CI lower` = parinf,
    `CI upper` = parsup
    ) %>% formattable(digits = 2)

# linear regression by rel point value minus 2
`regression_-2` <- ccaf_data_long %>% 
  filter(alcohol_value_cha == "-2") %>%  
  lm(pct_chose_alcohol_logit ~ AUDIT, data = .)
summary(`regression_-2`)

# linear regression by rel point value 0
regression_0 <- ccaf_data_long %>% 
  filter(alcohol_value_cha == "0") %>%  
  lm(pct_chose_alcohol_logit ~ AUDIT, data = .)
summary(regression_0)

# linear regression by rel point value plus 2
`regression_+2` <- ccaf_data_long %>% 
  filter(alcohol_value_cha == "+2") %>%  
  lm(pct_chose_alcohol_logit ~ AUDIT, data = .)
summary(`regression_+2`)

## linear choice variable (reproduce Irene's plot) ####
ggplot(
  data = ccaf_data_long,
  mapping = aes(x = AUDIT, y = pct_chose_alcohol, colour = alcohol_value_cha)
) +
  geom_point(
    position = position_jitter(width = 0.1, height = 0),
    shape = 16, alpha = 0.5, size = 3
  ) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
  scale_colour_manual(values = alc_colours, breaks = alc_order) +
  scale_linetype_manual(values = alc_linetypes, breaks = alc_order) +
  labs(y = "% chose alcohol", colour = "alcohol value", linetype = "alcohol value")
