rm(list = ls())
dir <- "D:/Staff/Haopeng/OneDrive - Lancaster University/Projects/Student Projects/Sasha/R test"
setwd(dir)

library(tidyverse)
library(readxl)
library(ggplot2)
library(rstatix)


library(dplyr)
library(DescTools)
library(ez)
library(effectsize)

# import data
dat <- read.csv("ErrorRateLongFormat.art.csv")%>%df_select(vars = c("id", "Cursor", "Amp", "Width", "ErrorRate"))

# set factor columns as factor
dat$Cursor <- as.factor(dat$Cursor)
dat$Amp <- as.factor(dat$Amp)
dat$Width <- as.factor(dat$Width)
dat$id <- as.factor(dat$id)

# change the names of factors if needed
dat$Cursor <- fct_recode(dat$Cursor, "NoGain" = "1", "Low" = "2", "Medium" = "3", "High" = "4")
dat$Amp <- fct_recode(dat$Amp, "7" = "1", "25" = "2", "40" = "3")
dat$Width <- fct_recode(dat$Width, "4" = "3")

library(ggpubr)
bxp <- ggboxplot(
dat, x = "Amp", y = "ErrorRate",
  color = "Cursor", palette = "jco",
  facet.by = "Width", short.panel.labs = FALSE
  )

# outliers
## check number of extreme outliers
extreme_outliers <- dat %>%
  group_by(Amp, Width, Cursor) %>%
  identify_outliers(ErrorRate) %>%
  filter(is.extreme == TRUE) # Filter for extreme outliers
nrow(extreme_outliers) # number of extreme outliers

## apply winzorization
winsorize <- function(x) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = TRUE)
  x[x < qnt[1]] <- qnt[1]
  x[x > qnt[2]] <- qnt[2]
  return(x)
}
dat <- dat %>%
  group_by(Amp, Width, Cursor) %>%
  mutate(ErrorRate = winsorize(ErrorRate))

## double check outliers are removed
extreme_outliers <- dat %>%
  group_by(Amp, Width, Cursor) %>%
  identify_outliers(ErrorRate) %>%
  filter(is.extreme == TRUE) # Filter for extreme outliers
nrow(extreme_outliers) # number of extreme outliers


# check for normality of raw data for each subgroups
nomality_check.plot_qq(dat, "ErrorRate", "Width", "Cursor", "Amp")

shapiro_result <- dat %>%
  group_by(Cursor, Amp, Width) %>%
  shapiro_test(ErrorRate)
shapiro_result$Normality <- shapiro_result$p >= 0.05 ## manually check the data table for test result

# check histogram
histogram_plot <- ggplot(dat, aes(x = ErrorRate)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 30) +  # Specifying the number of bins
  theme_bw() +
  facet_grid(Cursor + Amp ~ Width, labeller = label_both) +  # Corrected labeller
  labs(title = "Histogram", x = "", y = "")
print(histogram_plot)

# check qq plot
qq_plot <- ggqqplot(dat, "ErrorRate", ggtheme = theme_bw()) +
    facet_grid(Cursor + Amp ~ Width, labeller = "label_both") 
print(qq_plot)



# if normally distributed
m <- anova_test(dv = ErrorRate, within = c(Amp, Width, Cursor), wid = id, data = dat)
get_anova_table(m) ## auto apply Mauchly’s Test of Sphericity and Greenhouse-Geisser correction
#Report format: F(DFn, DFd) = ?, p = ?, eta2 = ges. keep 2 digits; ges = generalized eta square

# post-hoc pairwise comparisons
pwc <- dat %>%
  group_by(Amp, Width) %>%
  pairwise_t_test(ErrorRate ~ Cursor, paired = TRUE, p.adjust.method = "bonferroni") %>% 
  select(-df, -statistic, -p, -n1, -n2)  %>% # Remove details 
  filter(Amp == "7", Width == "2")
pwc



## plot stars on box plot
# pwc <- pwc %>% add_xy_position(x = "Width")
# pwc.filtered <- pwc %>% 
#   filter(Amp == "7", Width == "2")
# bxp + 
#   stat_pvalue_manual(pwc.filtered, tip.length = 0, hide.ns = TRUE) +
#   labs(
#     subtitle = get_test_label(m, detailed = TRUE),
#     caption = get_pwc_label(pwc)
#   )
# bxp

# if not normally distributed
library(ARTool)
## tutorial at:
### https://cran.r-project.org/web/packages/ARTool/readme/README.html
### https://depts.washington.edu/acelab/proj/art/
m_art = art(ErrorRate ~ Amp * Width * Cursor + Error(id), data = dat) # as we are using repeated measures Error(id) is used stead of (1|id)

## run anova
anova(m_art)

## effect size
# library(effectsize)
# eta_squared(m_art)

# post-hoc pairwise comparisons

## main effects
art.con(m_art, "Cursor", adjust="bonferroni")%>%
  summary()%>%
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

## if 2-way interaction
art.con(m_art, "Amp:Cursor", adjust="bonferroni")%>%
  summary()%>%
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))

## if 3-way interaction
art.con(m_art, "Amp:Width:Cursor", adjust="bonferroni")%>%
  summary()%>%
  mutate(sig. = symnum(p.value, corr=FALSE, na=FALSE,
                       cutpoints = c(0, .001, .01, .05, .10, 1),
                       symbols = c("***", "**", "*", ".", " ")))
