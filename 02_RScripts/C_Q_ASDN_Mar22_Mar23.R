#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Coder: Kaci Zarek
##Goal: Create C-Q and C-ASDN Figs 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 ---- Set up R environment -------------------------------------------------
#clear previous working environment
remove(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(patchwork)
library(ggpmisc)

#import C-Q continuous calibrated data
df <- read_csv("01_tidydata/ds01.csv",
               col_types = cols(grabNO3_mgL= col_double(),
                                grabNO3_uM = col_double()))
df <- df %>% select(datetime_local, datetime_UTC, Q_Ls, NO3_uM, grabNO3_uM, 
                    grabNO3_mgL, asdn_perm, seasonalPeriod, NO3_mgL)

#2.0 ---- Seasonal NO3-Q Stats w/ only Mar 22 to Mar 23 PLSR ran data ----------
#Calculate C-Q slope for wet period
wet <- df %>% filter(seasonalPeriod == "wet")
wet1 <- lm(data = wet, log(NO3_uM)~log(Q_Ls))
summary(wet1) 

#Calculate C-Q slope for dry-down period
dry <- df %>% filter(seasonalPeriod == "dry down")
dry1 <- lm(data = dry, log(NO3_uM)~log(Q_Ls))
summary(dry1) 

#Calculate C-Q slope for rewet period
rewet <- df %>% filter(seasonalPeriod == "rewet")
rewet1 <- lm(data = rewet, log(NO3_uM)~log(Q_Ls))
summary(rewet1)


#2.1 ---- C-Q plots for each seasonal period -----
p4 <- ggplot(df, aes(x = Q_Ls, y = NO3_uM)) + 
  geom_point(data = df, aes(color = seasonalPeriod), alpha = 0.6, shape = 21, size = 3) +
  geom_smooth(data = wet, method = "lm", color = "black", se = F, size = 1.5) +
  geom_smooth(data = dry, method = "lm", color = "black", se = F, size = 1.5) +
  geom_smooth(data = rewet, method = "lm", color = "black", se = F, size = 1.5) +
  scale_color_manual(values =  c("#d95f02", "#1b9e77", "#7570b3")) +
  scale_y_log10(labels = scales::comma, breaks = c(0.3, 1.0, 3)) +
  scale_x_log10(labels = scales::math_format(format = log10),
                     expand = c(0,1)) +
  labs(x = expression("Q (L s"^-1*")"), y = expression("NO"[3]^{"-"}~"(µM)")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 16),
        legend.position = "none")

p5 <- p4 + facet_wrap(~factor(seasonalPeriod, c('wet', 'dry down', 'rewet')), 
                      labeller = as_labeller(c("wet" = "Wet",
                                               "dry down" = "Dry-Down",
                                               "rewet" = "Rewet")),
                      nrow = 1,
                      scales = "fixed") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey", 
                                        color = "black"))
p6 <- p5 + 
  geom_point(data = rewet, aes(y = grabNO3_uM), color = "black", fill = "#1b9e77", 
             shape = 21, size = 3) +
  geom_point(data = wet, aes(y = grabNO3_uM), color = "black", fill = "#7570b3", shape = 21, 
             size = 3) +
  geom_point(data = dry, aes(y = grabNO3_uM), color = "black", fill = "#d95f02", 
             shape = 21, size = 3)
print(p6)

#determine number of observations in each seasonal period 
p6 +  stat_poly_eq(use_label(c("n")))

##3.0 ---- Determine log-log C-ASDN relationship -----
#Filter out wet period 
casdn_wet <- df %>% filter(seasonalPeriod == "wet")
wet1 <- lm(data = casdn_wet, log(NO3_uM)~log(asdn_perm))
summary(wet1) 

#Filter out dry down period
casdn_dry <- df %>% filter(seasonalPeriod == "dry down")
dry1 <- lm(data = casdn_dry, log(NO3_uM)~log(asdn_perm))
summary(dry1) 

#Filter out rewet period
casdn_rewet <- df %>% filter(seasonalPeriod == "rewet")
rewet1 <- lm(data = casdn_rewet, log(NO3_uM)~log(asdn_perm))
summary(rewet1)

##3.1 ---- Determine ADSN-Q relationship ----
ASDN_Q <- lm(data = df, log(asdn_perm)~log(Q_Ls))
summary(ASDN_Q)

##3.2 ---- ASDN-Q relationships for each seasonal period ----
wet_stat <- lm(data = wet, log(asdn_perm)~log(Q_Ls))
summary(wet_stat)
rewet_stat <- lm(data = rewet, log(asdn_perm)~log(Q_Ls))
summary(rewet_stat)
dry_stat <- lm(data = dry, log(asdn_perm)~log(Q_Ls))
summary(dry_stat)

#3.3 ---- C-ASDN plot for each seasonal period -----
p1 <- ggplot(df, aes(x = asdn_perm, y = NO3_uM)) + 
  geom_point(data = df, aes(color = seasonalPeriod), alpha = 0.4, shape = 19, size = 3) +
  geom_smooth(data = casdn_wet, method = "lm", color = "black", se = F, size = 1.5) +
  geom_smooth(data = casdn_dry, method = "lm", color = "black", se = F, size = 1.5) +
  geom_smooth(data = casdn_rewet, method = "lm", color = "black", se = F, size = 1.5) +
  scale_color_manual(values =  c("#d95f02", "#1b9e77", "#7570b3")) +
  scale_y_log10(labels = scales::comma) +
  scale_x_log10(breaks = c(2800, 3000, 3200, 3400)) +
  labs(x = expression("ASDN length (m)"), y = expression("NO"[3]^{"-"}~"(µM)")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title = element_text(color = "black", size = 16),
        legend.position = "none")

p2 <- p1 + facet_wrap(~factor(seasonalPeriod, c('wet', 'dry down', 'rewet')), 
                      labeller = as_labeller(c("wet" = "Wet",
                                               "dry down" = "Dry-Down",
                                               "rewet" = "Rewet")),
                      nrow = 1,
                      scales = "fixed") +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        strip.text = element_text(size = 14),
        strip.background = element_rect(fill = "grey", 
                                        color = "black"))
#add grab sample data points to C-ASDN figure
p3 <- p2 + 
  geom_point(data = casdn_rewet, aes(y = grabNO3_uM), color = "black", fill = "#1b9e77", 
             shape = 21, size = 3) +
  geom_point(data = casdn_wet, aes(y = grabNO3_uM), color = "black", fill = "#7570b3", shape = 21, 
             size = 3) +
  geom_point(data = casdn_dry, aes(y = grabNO3_uM), color = "black", fill = "#d95f02", 
             shape = 21, size = 3)
print(p3)

#determine number of seasonal observations for each period 
p3 + stat_poly_eq(use_label(c("n")))

#3.3 ----- Save Combined C-ASDN and C-Q plots -----
##Patchwork C-ASDN and C-Q figures 
C_ASDN_Q <- p6 / p3
##Add plot annotation
C_ASDN_Q <- C_ASDN_Q + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 14), plot.tag.position = c(0, 1))

##Save Plot 
ggsave("Figure3_C_Q_ASDN.png", C_ASDN_Q, 
       width = 8.5, height = 8, units = "in",
       path = "03_plots",
       dpi = 300)

#4.0 ----- Plot Q vs. ASDN together & create density plots ---------------------
#first, plot Q vs. ASDN 
p7 <- ggplot(df, aes(x = Q_Ls, y = asdn_perm)) + 
  geom_point(data = df, aes(color = seasonalPeriod), alpha = 0.8, shape = 19, size = 3) +
  geom_smooth(data = df, method = "lm", color = "black", se = F) +
  scale_color_manual(values =  c("#d95f02", "#1b9e77", "#7570b3"),
                     labels = c("Dry-Down", "Rewet", "Wet")) +
  scale_x_log10(labels = scales::math_format(format = log10)) +
  scale_y_log10() +
  labs(y = "ASDN length (m)", x = expression("Q (L s"^-1*")"), color = "Season") +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 12), 
        legend.background = element_rect(colour = "black",linewidth= 0.5),
        legend.position = "bottom")
print(p7)
#determine number of observations 
p7 + stat_poly_eq(use_label(c("n")))

#create a density plot for ASDN
asdn_fig <- ggplot(df) +
  geom_density(aes(x = asdn_perm, y = ..scaled.., color = seasonalPeriod), 
               alpha = 0.8, adjust = 4, lwd = 1.5) +
  scale_color_manual(values =  c("#d95f02", "#1b9e77", "#7570b3"),
                    labels = c("Dry-Down", "Rewet", "Wet")) +
  scale_y_continuous(labels = scales::label_number(0.1), breaks = c(0, 0.5, 1.0)) +
  scale_x_log10() +
  labs(x = "ASDN length (m)", y = "Density \n distribution", fill = "Season") +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(color = "black", size = 12), 
        legend.position = "none")+
  coord_flip()
print(asdn_fig)
#create a density plot for Q
q_fig <- ggplot(df) +
  geom_density(aes(x = Q_Ls, y = ..scaled.., color = seasonalPeriod), alpha = 0.5, 
               stat = "density", adjust = 3, lwd = 1.5) +
  scale_color_manual(values =  c("#d95f02", "#1b9e77", "#7570b3"),
                    labels = c("Dry-Down", "Rewet", "Wet")) +
  labs(x = expression("Q (L s"^-1*")"), y =  "Density \n distribution", fill = "Season") +
  scale_x_continuous(trans = "log10", labels = scales::math_format(format = log10)) +
  scale_y_continuous(labels = scales::label_number(0.1), breaks = c(0, 0.5, 1)) +
  theme_classic() +
  theme(axis.text.y = element_text(color = "black", size = 12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title = element_text(color = "black", size = 12), 
        legend.position = "none") 
print(q_fig)
#4.1 ---- Save Plots ---- 
#Patchwork Q and ASDN density plots w/ scatterplot
p8 <- q_fig + plot_spacer() + p7 + asdn_fig +
  plot_layout(
    ncol = 2, 
    nrow = 2, 
    widths = c(4, 1),
    heights = c(1, 4)
  ) 
print(p8)
ggsave("Q_ASDN_DensityPlots_Mar22_Mar23.png", p8, 
       width = 7, height = 6, units = "in",
       path = "03_plots",
       dpi = 300)

#5.0----Calculate flux per seasonal period--------------------------------------
#Mutate data frame into daily values
df1 <- df %>% 
  mutate(year = year(datetime_UTC), month = month(datetime_UTC), day = day(datetime_UTC)) %>% 
  group_by(year, month, day, seasonalPeriod) %>% 
  summarize(dailyQLs = mean(Q_Ls, na.rm = T), dailyNO3mgL = mean(NO3_mgL, na.rm = T)) 

df1 <- df1 %>% 
  filter(!is.na(dailyQLs)) %>% 
  filter(!is.na(dailyNO3mgL))

#Transform year, month, and day columns into one date column
df1 <- df1 %>% 
  mutate(date = make_date(year = year, month = month, day = day))

#Estimate daily flux 
flux <- df1 %>% mutate(flux = (dailyNO3mgL*dailyQLs*(1/1000)*(1/1000)*86400*0.226)) #N kg/day
#NO3 to NO3-N is either *0.226 or divide NO3mgL by 4.43 to get N 
#900 sec in 15 min

#Estimate each seasonal period flux
flux %>% 
  #mutate(year = year(date)) %>% 
  group_by(seasonalPeriod) %>% 
  summarise(
    #number of days with records per seasonal period
    n = n(), 
    #total flux accross year
    N_kg_season = sum(flux))

#Estimate annual flux
flux %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(
    #number of days with records per seasonal period
    n = n(), 
    #total flux accross year
    N_kg_year = sum(flux))

#6.0----Calculate avg NO3 and Q for each seasonal period------------------------
#Calculate avg NO3 and Q for wet period
df1 <- df %>%  
  filter(seasonalPeriod == "wet") %>% 
  group_by(seasonalPeriod)

df1 <- df1 %>% 
  summarize(across(Q_Ls:asdn_perm, na.rm = TRUE,
                   list(mean = mean, sd = sd)))

#Calculate avg NO3 and Q for dry down period
df2 <- df %>%  
  filter(seasonalPeriod == "dry down") %>% 
  group_by(seasonalPeriod)

dry_df <- df2 %>% 
  summarize(across(Q_Ls:asdn_perm, na.rm = TRUE,
                   list(mean = mean, sd = sd)))

#Calculate avg NO3 and Q for rewet period 
df3 <- df %>%  
  filter(seasonalPeriod == "rewet") %>% 
  group_by(seasonalPeriod)

rewet_df <- df3 %>% 
  summarize(across(Q_Ls:asdn_perm, na.rm = TRUE,
                   list(mean = mean, sd = sd)))

#Calculate annual C-Q 
year <- lm(data = df, log(NO3_uM)~log(Q_Ls)) 
summary(year) #R2 = 0.11 p < 0.001 B = -0.13

df1 <- df %>% 
  summarize(across(Q_Ls:NO3_uM, na.rm = TRUE, 
                   list(mean = mean, sd = sd)))

