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
library(corrplot)

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
  sheet = "stats", #stats_no_missing_data
  range = "A1:AC61"
  ) %>% 
  select(
    matches("Code") | ends_with("group") | starts_with("%") | matches("AUDIT") | matches("drinksweek") | matches("sex") | matches("^BIS$") | matches("NEO")
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

load("./qp_ind.RData")

ind_slopes <- qp_ind$par %>% 
  filter(parn == "p2") %>% 
  mutate(Slope = par) %>% 
  select(Code, Slope)

ccaf_data_long <- full_join(ccaf_data_long, ind_slopes)

# save transformed data #
ccaf_data_long %>% 
  write_tsv("./ccaf_data_long.txt")

ccaf_data_wide <- pivot_wider(ccaf_data_long,
  id_cols = c(Code, AUDIT, drinksweek, matches("BIS"), matches("NEO"), matches("Slope")),
  names_from = alcohol_value_cha,
  values_from = c(pct_chose_alcohol,pct_chose_alcohol_logit)
  ) 

ccaf_data_wide %>% 
  write_tsv("./ccaf_data_wide.txt")

# Calculate the suppression score and get descriptives
ccaf_data_wide$suppression <- ccaf_data_wide$`pct_chose_alcohol_logit_-2` - ccaf_data_wide$`pct_chose_alcohol_logit_0`
print(ccaf_data_wide$suppression)
summary(ccaf_data_wide$suppression)
shapiro_suppression <- shapiro.test(ccaf_data_wide$suppression)
print(shapiro_suppression)

### Correlation matrix ###

# Structure data frame for correlation matrix 
  # Check that all strings are numeric
  print(ccaf_data_wide)
  str(ccaf_data_wide)
  # Select only relevant columns
  # matches("AUDIT") | matches("drinksweek") | matches("suppression") | contains("pct_chose_alcohol_logit") | matches("^BIS$") | matches("NEO") | matches("Slope")
  ccaf_data_wide <- select(ccaf_data_wide,
      matches("AUDIT") | matches("drinksweek") | matches("suppression") | contains("pct_chose_alcohol_logit") | matches("BIS") | matches("NEO")
    ) %>%
    rename_with(~ "alch_choice_-2", .cols = starts_with("pct_chose_alcohol_logit_-2")) %>%
    rename_with(~ "alch_choice_0", .cols = starts_with("pct_chose_alcohol_logit_0")) %>%
    rename_with(~ "alch_choice_+2", .cols = starts_with("pct_chose_alcohol_logit_+2")
    ) 
  print(ccaf_data_wide)
    
  # Remove rows with NA values
    if (anyNA(ccaf_data_wide)) {
      ccaf_data_wide <- na.omit(ccaf_data_wide)
    }

# Calculate the correlation matrix and associated p-values
correlation_matrix <- cor(ccaf_data_wide)
p_val_matrix <- cor.mtest(ccaf_data_wide, conf.level = 0.95)

# Correlation plot with blank for nonsig correlations
corrplot(correlation_matrix, p.mat = p_val_matrix$p, method = 'circle', type = 'lower', insig='blank',
         addCoef.col = 'black', number.cex = 0.5, tl.cex = 0.6,  diag=FALSE)

# Correlation plot with blank for nonsig corr plus corr coeff 
pdf("Fig_correlation_matrix_plot.pdf", width = 9, height = 8)
  corrplot(correlation_matrix, p.mat= p_val_matrix$p, method = 'circle', type = 'lower', insig='blank', diag=FALSE)$corrPos -> corr_pos
  text(corr_pos$x, corr_pos$y, round(corr_pos$corr, 2), cex = 0.7)
dev.off()

# Correlation plot Bonf-Corrected with blank for nonsig corr
corrplot(correlation_matrix, p.mat= p_val_matrix$p, sig.level = 0.0045, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', number.cex = 0.5, tl.cex = 0.6,  diag=FALSE)

# Correlation plot Bonf-Corrected with blank for nonsig corr plus corr coeff 
pdf("Fig_correlation_matrix_plot_bonf.pdf", width = 9, height = 8)
  corrplot(correlation_matrix, p.mat= p_val_matrix$p, sig.level = 0.0045, method = 'circle', type = 'lower', insig='blank', diag=FALSE)$corrPos -> p1
  text(p1$x, p1$y, round(p1$corr, 2), cex = 0.7)
  dev.off()
