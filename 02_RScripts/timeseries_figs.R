#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Coder: Kaci Zarek
##Goal: Make figs of time series data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
remove(list = ls())

#load packages
library(tidyverse)
library(lubridate)
library(patchwork)


#Import timeseries dataset 
df <- read_csv("01_tidydata/ds01.csv",
  col_types = cols(grabNO3_uM = col_double()))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 -- hydrograph -- highest recorded Q was 126 L/s
q <- df %>% 
  filter(Q_Ls >= 0.1) %>% 
  filter(datetime_UTC >= "2022-03-20 00:00:00" & datetime_UTC <= "2023-03-20 17:15:00")

#Continuous Q
continuousQ <- ggplot(q, aes(x = as.Date(datetime_UTC), y = Q_Ls)) +
  geom_line(aes(group = as.Date(datetime_UTC)), lwd = 1,color = "#3182bd") +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  labs(y = expression("Q (L s"^-1*")"), 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 14)) +
  #display each month
  scale_x_date(expand = c(0.01,0.01), breaks = "2 months", date_labels = "%b %y") 
plot(continuousQ)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 -- NO3 timeseries fig 
conc <- df %>% filter(NO3_uM >= 0) 
conc1 <- conc %>% 
  filter(NO3_uM <= 4)

##Continuous NO3
continuousN <- ggplot(conc1, aes(x = as.Date(datetime_UTC), y = NO3_uM)) +
  geom_line(aes(group = as.Date(datetime_UTC)), lwd = 1, alpha = 0.8) +
  #geom_point(aes(y = grabNO3_uM), color = "red", size = 2, alpha = 0.8, shape = 19)+
  labs(y = expression("NO"[3]^{"-"}~"(µM)"), 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #display each month
  scale_x_date(expand = c(0.01,0.01), breaks = "2 months", date_labels = "%b %y") 
print(continuousN)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.2 -- Temp timeseries fig ---

continuoustemp <- ggplot(df, aes(x = as.Date(datetime_UTC), y = temp_c)) +
  geom_line(aes(group = as.Date(datetime_UTC)),lwd = 1, alpha = 0.8, color = "darkred") +
  #geom_point(color = "darkred", size = 1, alpha = 0.8, shape = 19) +
  #geom_point(aes(y = grabNO3_uM), color = "red", size = 2, alpha = 0.8, shape = 19)+
  labs(y = expression("Temp (°C)"), 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 14)) +
  #display each month
  scale_x_date(expand = c(0.01,0.01), breaks = "2 months", date_labels = "%b %y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.3 -- hyetograph ----

continuousPrecip <- ggplot(df) +
  geom_col(aes(x = as.Date(datetime_UTC), y = rainfall_mm),  
           color = "#045a8d",
           linewidth = 1) +
  scale_y_reverse(position = "left",
                  expand=c(0,0), limits = c(120,0)) +
  labs(y = "Precip (mm)", 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #display each month
  scale_x_date(date_labels="%b",date_breaks  ="2 month",
               expand = c(0.01,0.01))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.4 -- ASDN timeseries fig ---

asdn <- ggplot(df) +
  geom_line(aes(x = as.Date(datetime_UTC), y = asdn_perm),  
            color = "black",
            linewidth = 1) +
  labs(y = "ASDN length (m)", 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_blank(), 
        axis.text.y = element_text(color = "black", size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  #display each month
  scale_x_date(date_labels="%b",date_breaks  ="2 month",
               expand = c(0.01,0.01))

#Highlight when sampling was done on hydrograph at least 
p1 <- continuousPrecip + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                                      "2023-01-30"))),
                                    alpha = 0.5, lwd = 4, 
                                    color = "grey")
p2 <- asdn + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                          "2023-01-30"))),
                        alpha = 0.5, lwd = 4, 
                        color = "grey")
p3 <- continuousQ + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                                 "2023-01-30"))),
                               alpha = 0.5, lwd = 4, 
                               color = "grey")
p4 <- continuousN + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                                 "2023-01-30"))),
                               alpha = 0.5, lwd = 4, 
                               color = "grey")

p5 <- continuoustemp + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                                    "2023-01-30"))),
                                  alpha = 0.5, lwd = 4, 
                                  color = "grey")

fig <- p1/p2/p3/p4/p5
fig <- fig + plot_annotation(tag_levels = 'a') & 
  theme(plot.tag = element_text(face = "bold", size = 14), plot.tag.position = c(0, 1))
print(fig)

#Save patchworked time series plot 
ggsave("FigureS3_timeseries.png", fig, width = 6, 
       height = 8, units = "in",
       path = "03_plots",
       dpi = 300)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#---Full record of study hydrograph from Mar 22 to Nov 23---
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create hydrograph w/ full Q record of study  
q <- read_csv("01_tidydata/ds03.csv")

#Filter for specific time frame
q2 <- q %>% 
  filter(Q_Ls >= 0.1) %>% 
  filter(datetime_UTC >= "2022-03-20 00:00:00" & datetime_UTC <= "2023-11-27 18:45:00")

#Plot full record of Q data 
p1 <- ggplot(q2, aes(x = as.Date(datetime_UTC), y = Q_Ls)) +
  geom_line(aes(group = as.Date(datetime_UTC)), lwd = 1,color = "#3182bd") +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  labs(y = expression("Q (L s"^-1*")"), 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 14)) +
  scale_x_date(expand = c(0.01,0.01))

#Add sampling campaigns onto the hydrograph
p1 <- p1 + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                        "2023-01-30", 
                                                        "2023-05-16", 
                                                        "2023-08-07",
                                                        "2023-11-27"))),
                      alpha = 0.5, lwd = 4, 
                      color = "grey")
print(p1)

#Create hydrograph w/ full Q record of study  
asdn <- read_csv("01_tidydata/ds03.csv")
ASDN <- asdn %>% 
  filter(datetime_UTC >= "2022-03-20 00:00:00" & datetime_UTC <= "2023-11-27 18:45:00")

p2 <- ggplot(ASDN, aes(x = as.Date(datetime_UTC), y = asdn_perm)) +
  geom_line(aes(x = as.Date(datetime_UTC), y = asdn_perm),  
            color = "black",
            linewidth = 1) +
  scale_y_continuous(trans = "log10", labels = scales::comma) +
  labs(y = "ASDN length (m)", 
       x= "Date") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.border = element_rect(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", size = 14)) +
  #display each month
  scale_x_date(expand = c(0.01,0.01), breaks = as.Date(c("2022-03-20", "2022-07-01",
                                                         "2022-11-01", 
                                                         "2023-03-01", 
                                                         "2023-07-01",
                                                         "2023-11-01")), date_labels = "%b %y") 

#Add sampling campaigns onto the hydrograph
p2 <- p2 + geom_vline(xintercept = as.numeric(as.Date(c("2022-03-29", "2022-08-11",
                                                        "2023-01-30", 
                                                        "2023-05-16", 
                                                        "2023-08-07",
                                                        "2023-11-27"))),
                      alpha = 0.5, lwd = 4, 
                      color = "grey")
print(p2)

#Patchwork ASDN and hydrograph 
fig <- (p1 / p2)

#Save ASDN and hydrograph together
ggsave("hydrograph_asdn_fullrecord.png", fig, width = 8, 
       height = 5, units = "in",
       path = "03_plots", dpi = 300)
