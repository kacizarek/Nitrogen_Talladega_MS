#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Coder: Kaci Zarek
#Date: 2024-Feb-9
#Purpose: Figures of Mar 22 to Jan 30 seasonal data (boxplots, regressions, distance to outlet)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#1.0 -- Set workspace ----------------------------------------------------------

#Install & load these packages for a time series plot
library(tidyverse)
library(lubridate)
library(patchwork)
library(readr)
library(scales)
library(ggpmisc)
#for basic statistical analyses 
library(rstatix)

#Import data
df <- read_csv("01_tidydata/ds02.csv")

#Filter out other seasonal sampling campaigns not analyzing right now
df1 <- df %>% 
  filter(date != "11/27/2023", date != "8/7/2023",
         date != "5/16/2023")

#manually reorder site names with 'fct_relevel'
df2 <- df1 %>% 
  mutate(site = fct_relevel(site, "TLM01", "TLC01", "TLM06", "TLM10", "TLA01",
                            "TLM16", "TLM19"))
df3 <- df2 %>% 
  mutate(date = fct_relevel(date, "3/29/2022", "8/11/2022", 
                            "1/30/2023"))

#2.0----Test Normality Plus Correlation Analyses--------------------------------

##Test temp and N2 excess for normality
par(mfrow = c(1,2))
shapiro_test(df3$temp)
qqnorm(df3$temp)
qqline(df3$temp)#temp non-normal
qqnorm(df3$n2excess)
qqline(df3$n2excess)#N2 excess normal distribution

##Test the linear regression relationship for N2excess~temp 
model <- lm(n2excess~temp, data = df3)
plot(model)

#Run correlation analyses for temp vs. n2excess
cor.test(df3$temp, df3$n2excess, method = c("pearson")) #r = 0.765, p < 0.001
cor.test(df3$temp, df3$n2excess, method = c("spearman")) #rho = 0.67; p < 0.001 ##use spearman

#Test O2 for normality
hist(df3$MIMS_O2mgL) ##visualize skewness 
qqplot(df3$MIMS_O2mgL)
qqnorm(df3$MIMS_O2mgL)
qqline(df3$MIMS_O2mgL)#slightly non-normal
shapiro_test(df3$MIMS_O2mgL) #barely normal
#Run correlation analyses for O2 vs. n2 excess
cor.test(df3$MIMS_O2mgL, df3$n2excess, method = c("spearman")) #rho = -0.67; p < 0.001
cor.test(df3$MIMS_O2mgL, df3$n2excess, method = c("pearson")) #r = -0.65; p = 0.0016

##Test the linear regression relationship for N2excess~O2 
model <- lm(n2excess~MIMS_O2mgL, data = df3)
plot(model)

##Test Q for normality
#Q DEFINITELY non-normal distribution & skewed right
shapiro_test(df3$Q_Ls)
qqnorm(df3$Q_Ls)
qqline(df3$Q_Ls)
Q <- log10(df3$Q_Ls)
shapiro_test(Q) #log transforming data makes Q normal
cor.test(Q, df3$n2excess, method = c("spearman")) #rho = -0.79; p < 0.001
cor.test(df3$Q_Ls, df3$n2excess, method = c("pearson")) #r = -0.40; p = 0.078

##Test the linear regression relationship for N2excess~Q 
model <- lm(n2excess~Q_Ls, data = df3) ##def not linear 
plot(model)

#Test NO3 for normality
#NO3 slightly normal but use spearman...
qqnorm(df3$NO3_uM)
qqline(df3$NO3_uM)
cor.test(df3$NO3_uM, df3$n2excess, method = c("spearman")) #rho = 0.38; p  = 0.085
cor.test(df3$NO3_uM, df3$n2excess, method = c("pearson")) #r = 0.45; p  = 0.04
shapiro_test(df3$NO3_uM) #normal according to shapiro test 

##Test the linear regression relationship for N2excess~NO3 
model <- lm(n2excess~NO3_uM, data = df3)
plot(model)

#Test DOC for normality
##DOC slightly normal but use spearman...
qqnorm(df3$DOC_uM)
qqline(df3$DOC_uM)
cor.test(df3$DOC_uM, df3$n2excess, method = c("spearman")) #rho = -0.066; p = 0.78
cor.test(df3$DOC_uM, df3$n2excess, method = c("pearson")) #r = -0.01; p = 0.96
shapiro_test(df3$DOC_uM) #non-normal according to shapiro test

DOC <- log10(df3$DOC_uM)
shapiro_test(DOC) #log transforming makes data normal

##Test the linear regression relationship for N2excess~DOC 
model <- lm(n2excess~DOC_uM, data = df3)
plot(model)

#Test NH4+ for normality
#not normally distributed
qqnorm(df3$NH4_uM)
qqline(df3$NH4_uM)
shapiro_test(df3$NH4_uM)
cor.test(df3$NH4_uM, df3$n2excess, method = c("spearman")) #rho = 0.61; p = 0.003
cor.test(df3$NH4_uM, df3$n2excess, method = c("pearson")) #r = 0.66; p = 0.001
NH4 <- log10(df3$NH4_uM)
shapiro_test(NH4) ##still not normal after log transforming 
##Most  parameters are non-normally distributed despite transformations -- use spearman

##Test the linear regression relationship for N2excess~NH4+ 
model <- lm(n2excess~NH4_uM, data = df3)
plot(model)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 ---- Plot by season (time) ------------------------------------------------
#Plot N2 excess by time (season) 
p1 <- df3 %>% ggplot(aes(season, n2excess)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  #scale_x_discrete(labels = c("Spring", "Summer", "Winter")) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  expand_limits(y = 4) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab(expression("∆ N"[2]~"Excess")) +
  #add horizontal line at 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey10") +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")
p1 + stat_poly_eq(use_label(c("n")))
#Run significance test between two groups 
pairwise_wilcox_test(df3, n2excess~season, paired = T)

#Plot Temp by time (season) 
p2 <- df3 %>% ggplot(aes(season, temp)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  #scale_x_discrete(labels = c("Spring", "Summer", "Winter")) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  expand_limits(y = 25) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab(expression("Temp ("*degree*C*")")) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")
p2 + stat_poly_eq(use_label(c("n"))) ##Number of observations 

#Run nonparametric test 
wilcox_test(df3, temp~season, paired = T) 


##Plot YSI DO by time (season)
df3 %>% ggplot(aes(season, YSI_DO_mgL)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                    name = "Season",
                    labels = c("Fall", "Spring", "Summer", "Winter")) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #change y-axis title label
  ylab(expression("O"[2]~"(mg L"^-1*")")) +
  expand_limits(y = c(2, 11.6)) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        legend.position = "none")

#Run nonparametric test
wilcox_test(df3, YSI_DO_mgL~season, paired = T)

#Plot MIMS dissolved O2 by time (season) 
p3 <- df3 %>% ggplot(aes(season, MIMS_O2mgL)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #change y-axis title label
  ylab(expression("O"[2]~"(mg L"^-1*")")) +
  expand_limits(y = 10.4) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")
p3 + stat_poly_eq(use_label(c("n")))

#Run nonparametric test
wilcox_test(df3, MIMS_O2mgL~season, paired = T) 
  #add_significance(symbols = c("A", "B", "C", "AB","ns")) %>% 

#Plot Q by time (season) 
p4 <- df3 %>% ggplot(aes(season, Q_Ls)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77")) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ylab(expression("Q (L s"^-1*")")) +
  expand_limits(y = 130) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")
p4 + stat_poly_eq(use_label("n"))
#Run nonparametric test
pairwise_wilcox_test(df3, Q_Ls~season, paired = T) 


#Plot NO3 uM by time (season) 
p6 <- df3 %>% ggplot(aes(season, NO3_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                    name = "Season",
                    labels = c("Spring", "Summer", "Winter")) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_y_continuous(limits = c(0, 2.2)) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #change y-axis title label
  ylab(expression("NO"[3]^{"-"}~"(µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        legend.position = "none")
p6 + stat_poly_eq(use_label(c("n"))) 

#Run nonparametric test 
wilcox_test(df3, NO3_uM~season, paired = T) 

#Plot DOC uM by time (season) 
p7 <- df3 %>% ggplot(aes(season, DOC_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                    name = "Season") +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_y_continuous(limits = c(0, 200)) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  expand_limits(y = 180) +
  #change y-axis title label
  ylab(expression("DOC (µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.text.x = element_text(color = "black", size = 14),
        axis.title.x = element_blank(),
        legend.position = "none")
p7 + stat_poly_eq(use_label(c("n")))

#Run nonparametric test 
wilcox_test(df3, DOC_uM~season, paired = T) 

#Plot NH4 uM by time (season) 
##NH4 below detection for all sites in rewet period, unfortunately
p8 <- df3 %>% ggplot(aes(season, NH4_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                    name = "Season",
                    labels = c("Spring", "Summer", "Winter")) +
  scale_y_continuous(limits = c(0, 2.2)) +
  scale_x_discrete(labels = c("Wet", "Dry-Down", "Rewet")) +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #change y-axis title label
  ylab(expression("NH"[4]^{"+"}~"(µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14), 
        axis.text.x = element_text(color = "black", size = 14),
        axis.title.x = element_blank(),
        legend.position = "none")
p8 + stat_poly_eq(use_label(c("n")))

#Run nonparametric test 
wilcox_test(df3, NH4_uM~season, paired = T)

##Patchwork plots across season for manuscript 
season_plots <- p1 + p2 + p3 + p4 + p6 + p8 + p7 + plot_layout(ncol =2)
print(season_plots)

#Add annotations to plot of seasonal data for manuscript
season_plots <- season_plots + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 14), plot.tag.position = c(0,1))

#Save plot with dimensions of interest for manuscript
ggsave("3seasons_figs_v4.png", season_plots, width = 8, 
       height = 10, units = "in",
       path = "/03_plots",
       dpi = 300)

#EXTRA Patchworked plots for talk ----------------------------------------------
#Patchwork plots across seasons together for talk
season_plots_talk <- p1 + p4 + p2 + p3 + plot_layout(nrow = 1)

#Add annotations to plot of seasonal data for talk
season_plots_talk <- season_plots_talk + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 14), plot.tag.position = c(0,1))

#Save plot with dimensions of interest for talk
ggsave("3seasons_boxplot_talk.png", season_plots_talk, width = 12, 
       height = 3, units = "in",
       path = "/Users/Kaci Zarek/Documents/MS_manuscript/TAL_Data/TAL_Data/03_plots")

#4.0 ----Regressions------------------------------------------------------------
#(1) N2 excess (response) vs. Temp (explanatory variable)
lm1 <- lm(data = df3, n2excess~temp)
summary(lm1) #R-squared: 0.56, p-value <<0.001

#Run correlation analyses for temp vs. n2excess
cor.test(df3$temp, df3$n2excess, method = c("pearson")) #r = 0.765, p < 0.001
cor.test(df3$temp, df3$n2excess, method = c("spearman")) #rho = 0.67; p < 0.001

p1 <- df3 %>% 
  ggplot(aes(x = temp, y = n2excess)) +
  geom_point(fill = "#a50f15", size =3, alpha = 0.8, shape = 21) +
  geom_smooth(method = 'lm',color = "#a50f15", se = TRUE, linewidth = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  ylab(expression("∆ N"[2]~"Excess")) +
  xlab(expression("Temperature ("*degree*C*")")) +
  ylim(-2, 4) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(2) N2 excess (response) vs. O2 (explanatory variable)
##W/ MIMS data
lm2 <- lm(data = df3, n2excess~MIMS_O2mgL)
summary(lm2)

#Run correlation analyses for O2 vs. n2excess
cor.test(df3$O2mgL_mean, df3$n2excess, method = c("spearman")) #rho = -0.67; p < 0.001
cor.test(df3$O2mgL_mean, df3$n2excess, method = c("pearson")) #r = -0.645; p = 0.0016

p2 <- df3 %>% 
  ggplot(aes(x = MIMS_O2mgL, y = n2excess)) +
  geom_point(fill = "#3182bd", size = 3, alpha = 0.8, shape = 21) +
  geom_smooth(method = 'lm', color = "#3182bd", se = TRUE, lwd = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  ylab(expression("∆ N"[2]~"Excess")) +
  xlab(expression("O"[2]~"(mg L"^-1*")")) +
  ylim(-2, 4) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_blank())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(3) N2 excess (response) vs. Q (explanatory variable)
##Q vs. N2excess not significant or high R^2
lm_Q <- lm(data = df3, n2excess~Q_Ls) 
summary(lm_Q) ##P-value = 0.076 > 0.05 Adjusted R2 = 0.11 ##Highest flows making non-sig

#Run correlation analyses for Q vs. n2excess
cor.test(df3$Q_Ls, df3$n2excess, method = c("spearman")) #rho = -0.78; p < 0.001
##if data was normal but it's not 
cor.test(df3$Q_Ls, df3$n2excess, method = c("pearson")) #r = -0.396; p = 0.078 

p3 <- df3 %>% 
  ggplot(aes(x = Q_Ls, y = n2excess)) +
  geom_point(fill = "#045a8d", size = 3, alpha = 0.8, shape = 21) +
  geom_smooth(color = "#045a8d", lwd = 1.0,  method = 'lm', se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  scale_y_continuous(limits = c(-2, 4)) +  
  scale_x_continuous(trans = "log10") +
  theme_classic() +
  ylab(expression("N"[2]~"Excess")) +
  xlab(expression("Flow (L s"^-1*")")) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_blank())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(4) N2 excess (response) vs. NH4+ (explanatory variable)
lm5 <- lm(n2excess ~ NH4_uM, data = df3)
summary(lm5) #Adj R-squared: 0.41, p-value = 0.001

#Run correlation analyses for NH4 vs. n2excess
cor.test(df3$NH4_uM, df3$n2excess, method = c("spearman")) #rho = 0.61; p = 0.003
cor.test(df3$NH4_uM, df3$n2excess, method = c("pearson")) #r = 0.66; p = 0.001

p4 <- df3 %>% 
  ggplot(aes(x = NH4_uM, y = n2excess)) +
  geom_point(fill = "#e6550d", size = 3, alpha = 0.8, shape = 21) +
  geom_smooth(method = "lm",
              color = "#e6550d",se = TRUE, lwd = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  ylab(expression("∆ N"[2]~"Excess")) +
  xlab(expression("NH"[4]^{"+"}~"(µM)")) +
  ylim(-2, 4) +  
  scale_x_continuous(limits = c(0.3,1.7))+
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_blank())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(5) N2excess~DOC Regression
lm7 <- lm(data = df3, n2excess~DOC_uM)
summary(lm7) ##not sig.#p=0.96, R2 = -0.05

#Run correlation analyses for DOC vs. n2excess
cor.test(df3$DOC_uM, df3$n2excess, method = c("spearman")) #rho = -0.066; p = 0.78
cor.test(df3$DOC_uM, df3$n2excess, method = c("pearson")) #r = -0.01; p = 0.96

p6 <- df3 %>% 
  ggplot(aes(x = DOC_uM, y = n2excess)) +
  geom_point(fill = "#8856a7", size = 3, alpha = 0.8, shape = 21) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  ylim(-2, 4) +
  ylab(expression("∆ N"[2]~"Excess")) +
  xlab(expression("DOC (µM)")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_blank())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#(6) N2excess~NO3 Regression
lm8 <- lm(data = df3, n2excess~NO3_uM)
summary(lm8) ##p-value = 0.04 sig R2 = 0.16

#Run correlation analyses for NO3 vs. n2excess
cor.test(df3$NO3_uM, df3$n2excess, method = c("spearman")) #rho = 0.48; p  = 0.028
cor.test(df3$NO3_uM, df3$n2excess, method = c("pearson")) #r = 0.45; p  = 0.04

p7 <- df3 %>% 
  ggplot(aes(x = NO3_uM, y = n2excess)) +
  geom_point(fill = "#ae017e", size = 3, alpha = 0.8, shape = 21) +
  geom_smooth(method = "lm",
             color = "#ae017e", se = TRUE, lwd = 1.0) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 0.75) +
  ylim(-2, 4) +
  ylab(expression("∆ N"[2]~"Excess")) +
  xlab(expression("NO"[3]^{"-"}~"(µM)")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.x = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 ----
#patchwork regressions 
regressions <- p1 + p3 + p2 + p7 + p6 + p4 + plot_layout(ncol = 3)
print(regressions)

#add annotations to regression
regressions <- regressions + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 14))

#Save figure
ggsave("regressions_3seasonals.png", regressions, width = 9, height = 7, 
       units = "in", 
       path = "/Users/Kaci Zarek/Documents/MS_manuscript/TAL_Data/TAL_Data/03_plots",
       dpi = 300)

#5.0 ----------Distance to Outlet Figures---------------------------------------
#Test relationship between N2excess and distance to outlet
cor.test(df3$streamDistanceFromOutlet_m, df3$n2excess, method = c("spearman"))
cor.test(df3$streamDistanceFromOutlet_m, df3$n2excess, method = c("pearson")) 
shapiro_test(df3$streamDistanceFromOutlet_m) #non-normal

#Distance to outlet spatial figures N2 Excess 
p1 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = n2excess)) +
  geom_point(aes(color = season), size = 4.5, alpha = 0.8) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                    name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.7, color = "grey20") +
  geom_line(data = df3, aes(color = season), linetype = "solid") +
  scale_shape_manual(values = c(17, 15, 19)) +
  scale_y_continuous(limits = c(-2, 4)) +
  scale_x_reverse() +
  ylab(expression("N"[2]~"Excess")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),legend.position = "none",
      axis.title.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test relationship between Temp and distance to outlet
cor.test(df3$streamDistanceFromOutlet_m, df3$temp, method = c("spearman")) 

#Distance to outlet spatial figures temp 
p2 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = temp)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_shape_manual(values = c(17, 15, 19)) +
  scale_y_continuous(breaks = c(14, 18, 22), limits =c(10, 24)) +
  scale_x_reverse() +
  ylab(expression("Temp ("*degree*C*")")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test relationship between oxygen and distance to outlet
cor.test(df3$streamDistanceFromOutlet_m, df3$MIMS_O2mgL, method = c("spearman"))

#Distance to outlet spatial figures O2 
p3 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = MIMS_O2mgL)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_shape_manual(values = c(17, 15, 19)) +
  expand_limits(y = 10.2, x = 8.2) +
  scale_x_reverse() +
  ylab(expression("O"[2]~"(mg L"^-1*")")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test relationship between Q and distance to outlet 
cor.test(df3$streamDistanceFromOutlet_m, df3$Q_Ls, method = c("spearman")) 

#Distance to outlet spatial figures Q 
p4 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = Q_Ls)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") + 
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_shape_manual(values = c(17, 15, 19), 
                     labels = c("Headwater", "Transitional Zone", "Valley Bottom")) +
  scale_y_continuous(trans = "log10") +
  expand_limits(y = 125) +
  scale_x_reverse() +
  ylab(expression("Q (L s"^-1*")")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test relationship between NO3 and distance to outlet
cor.test(df3$streamDistanceFromOutlet_m, df3$NH4_uM, method = c("spearman")) 

#Distance to outlet spatial figure for NH4
p6 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = NH4_uM)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") + 
  #geom_errorbar(aes(ymin= NH4_uM - NH4_uM_std, ymax = NH4_uM + NH4_uM_std, color = season), width = 0.2) + 
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_shape_manual(values = c(17, 15, 19)) +
  #reverse x axis to go from upstream to downstream 
  scale_y_continuous(limits = c(0, 2)) +
  scale_x_reverse() +
  ylab(expression("NH"[4]^{"+"}~"(µM)")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12), legend.position = "none")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test relationship between NO3 and distance to outlet
cor.test(df3$streamDistanceFromOutlet_m, df3$NO3_uM, method = c("spearman")) 
#Distance to outlet spatial figure for NO3
p7 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = NO3_uM)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") + 
  #geom_errorbar(aes(ymin= NO3_uM - NO3_uM_std, ymax = NO3_uM + NO3_uM_std, color = season), width = 0.2, 
                #size = 0.8) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_shape_manual(values = c(17, 15, 19)) +
  #reverse x axis to go from upstream to downstream 
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 2)) +
  ylab(expression("NO"[3]^{"-"}~"(µM)")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black", size = 12),legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Distance to outlet spatial figure for DOC
cor.test(df3$streamDistanceFromOutlet_m, df3$DOC_uM, method = c("spearman")) #rho = -0.71 p <<0.001
p8 <- df3 %>% ggplot(aes(x = streamDistanceFromOutlet_m, y = DOC_uM)) +
  geom_point(aes(color = season), size = 4, alpha = 0.8) +
  geom_line(data = df3, aes(color = season), linetype = "solid") + 
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"), 
                     name = "Season", labels = c("Wet", "Dry-Down", "Rewet")) +
  expand_limits(y = 175) +
  scale_x_reverse() +
  ylab(expression("DOC (µM)")) +
  xlab("Distance to Outlet (m)") +
  #change legend title
  labs(color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),legend.position = "bottom", 
        legend.direction = "horizontal", legend.background = element_rect(colour = "black", 
                                                                          linewidth= 0.5)) 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Patchwork distance to outlet figures
fig <- p1 + p2 + p3 + p4 + p7 + p8 + p6 + plot_layout(ncol = 2, nrow = 4) 
fig <- fig + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 12), 
        plot.tag.position = c(0, 1))

#Save figure
ggsave("DistanceToOutlet_SpatialFigs.png", fig, width = 7.5, height = 5.5, 
       units = "in", 
       path = "03_plots",
       dpi = 300)
