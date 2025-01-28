#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Coder: Kaci Zarek
#Date: 2/11/2024
#Goal:Create DNF potential rate figure and boxplots of physicochemical data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remove(list = ls())

#Load required packages
library(ggbreak)
library(patchwork)
library(tidyverse)
#Compare means 
library(rstatix)
library(ggpmisc)
library(ggpubr)

#Import Data Set
file.path <- "TAL_Data/"
df <- read_csv("01_tidydata/ds02.csv")

#Filter out other seasonal sampling campaigns not analyzing right now
df1 <- df %>% 
  filter(date != "3/29/2022", date != "8/11/2022",
         date != "1/30/2023")
#Select the dnf potential rate data from the dataframe
df2 <- df1 %>% 
  select(DNF_umol_N_kg_hr, site, date, seasonalPeriod, temp, DOC_uM, NO3_uM, NH4_uM,
         YSI_DO_mgL, season, Q_Ls)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 ---- Plot DNF across seasons with other literature data values too --------
dnf_fig <- df2 %>% ggplot(aes(season, DNF_umol_N_kg_hr)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 4, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                   name = "Season") +
  geom_hline(yintercept = c(50, 67.5, 98.7), linetype = "dashed", color = "black", size = 0.75) +
  annotate("text", x = "fall", y = 50, label = "Forested Ditch", vjust = -0.9, size = 4.5) +
  annotate("text", x = "fall", y = 67.5, label = "Salt Marsh", vjust = -0.5, size = 4.5) +
  annotate("text", x = "fall", y = 98.7, label = "Urban River", vjust = -0.5, size = 4.5) +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  ylab(expression("Denitrification Potential (µmol N kg"^-1~"hr"^-1*")")) +
  scale_y_continuous(limits = c(0, 170)) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.title.x = element_blank(),
        legend.position = "blank")
plot(dnf_fig)

#save dnf potential fig 
ggsave("Figure7_potentialDNFrates_seasonal.png", dnf_fig, 
       height = 4, width = 5, units = "in", 
       path = "03_plots/",
       dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 ---- Plot other parameters for SI figure ----------------------------------
p4 <- df2 %>% ggplot(aes(season, temp)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  ylab(expression("Temp (°C)")) +
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

p5 <- df2 %>% ggplot(aes(season, DOC_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  ylab(expression("DOC (µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.title.x = element_blank(),
        legend.position = "none")

p6 <- df2 %>% ggplot(aes(season, NO3_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_y_continuous(limits = c(0, 3.5)) +
  ylab(expression("NO"[3]^{"-"}~"(µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")


p7 <- df2 %>% ggplot(aes(season, NH4_uM)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  scale_y_continuous(limits = c(0, 2)) +
    ylab(expression("NH"[4]^{"+"}~"(µM)")) +
  #Change axis numbers to darker color and text size
  theme(axis.text = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.title.x = element_blank(),
        legend.position = "none")

p8 <- df2 %>% ggplot(aes(season, Q_Ls)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  ylab(expression("Q (L s"^{"-"}*")")) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")


p9 <- df2 %>% ggplot(aes(season, YSI_DO_mgL)) +
  geom_boxplot(aes(fill = season), position = "dodge") +
  geom_jitter(aes(fill = season), width = 0.2, size = 3, alpha = 0.75, shape = 21) +
  scale_fill_manual(values = c( "#1b9e77", "#7570b3", "#d95f02"), 
                    name = "Season") +
  #change theme
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c("spring", "summer", "fall"),
                   labels = c("Wet", "Dry-Down", "Rewet")) +
  ylab(expression("O"[2]~"(mg L"^{"-"}*")")) +
  #Change axis numbers to darker color and text size
  theme(axis.text.y = element_text(color = "black", size = 14),
        axis.title.y = element_text(color = "black", size = 14),  
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

#Patchwork physicochemical data together for SI figure 
figs <-  p4 + p9 + p8 + p6 + p5 + p7 + plot_layout(ncol=2)

figs <- figs + plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 12),plot.tag.position = c(0, 1))

#Save SI figure
ggsave("FigureS6_physiochemicalVariables_2023.png", figs, 
       height = 5.5, width = 6.5, units = "in", 
       path = "03_plots",
       dpi = 300)
