#Creating very dumb data to demonstrate potential ways of visualizing 
#scenario modeling results

#First, just scenario data for CC
scenarios <- as.factor(rep(c('S1','S2','S3', 'S4', 'S5'), each = 50))
time <- as.factor(rep(c('T1', 'T2', 'T3', 'T4', 'T5'), each = 10, times = 5))
cc <- rnorm(250, rep(c(65, 64, 63, 62, 61, 
                       40, 39, 38, 37, 36, 
                       20, 19, 18, 17, 16, 
                       35, 34, 33, 32, 31, 
                       63, 60, 57, 54, 41), each = 10), sd = 3)
row <- as.factor(rep(c(1:10), 25))
scenario <- data.frame(Scenario = scenarios, time = time, cc = cc, huc = row)
require(tidyr)
scenario.wide <- scenario %>%
  pivot_wider(names_from = c(Scenario, time), values_from = cc)

#differencing the data - differences between specific scenario comparisons across
#all time steps.
#I know that there is a faster way to do this, but it's functional.

diff1v2_T1 <- scenario.wide$S2_T1-scenario.wide$S1_T1
diff1v2_T2 <- scenario.wide$S2_T2-scenario.wide$S1_T2
diff1v2_T3 <- scenario.wide$S2_T3-scenario.wide$S1_T3
diff1v2_T4 <- scenario.wide$S2_T4-scenario.wide$S1_T4
diff1v2_T5 <- scenario.wide$S2_T5-scenario.wide$S1_T5

diff1v3_T1 <- scenario.wide$S3_T1-scenario.wide$S1_T1
diff1v3_T2 <- scenario.wide$S3_T2-scenario.wide$S1_T2
diff1v3_T3 <- scenario.wide$S3_T3-scenario.wide$S1_T3
diff1v3_T4 <- scenario.wide$S3_T4-scenario.wide$S1_T4
diff1v3_T5 <- scenario.wide$S3_T5-scenario.wide$S1_T5

diff1v4_T1 <- scenario.wide$S4_T1-scenario.wide$S1_T1
diff1v4_T2 <- scenario.wide$S4_T2-scenario.wide$S1_T2
diff1v4_T3 <- scenario.wide$S4_T3-scenario.wide$S1_T3
diff1v4_T4 <- scenario.wide$S4_T4-scenario.wide$S1_T4
diff1v4_T5 <- scenario.wide$S4_T5-scenario.wide$S1_T5

diff1v5_T1 <- scenario.wide$S5_T1-scenario.wide$S1_T1
diff1v5_T2 <- scenario.wide$S5_T2-scenario.wide$S1_T2
diff1v5_T3 <- scenario.wide$S5_T3-scenario.wide$S1_T3
diff1v5_T4 <- scenario.wide$S5_T4-scenario.wide$S1_T4
diff1v5_T5 <- scenario.wide$S5_T5-scenario.wide$S1_T5

diff2v3_T1 <- scenario.wide$S3_T1-scenario.wide$S2_T1
diff2v3_T2 <- scenario.wide$S3_T2-scenario.wide$S2_T2
diff2v3_T3 <- scenario.wide$S3_T3-scenario.wide$S2_T3
diff2v3_T4 <- scenario.wide$S3_T4-scenario.wide$S2_T4
diff2v3_T5 <- scenario.wide$S3_T5-scenario.wide$S2_T5

diff2v4_T1 <- scenario.wide$S4_T1-scenario.wide$S2_T1
diff2v4_T2 <- scenario.wide$S4_T2-scenario.wide$S2_T2
diff2v4_T3 <- scenario.wide$S4_T3-scenario.wide$S2_T3
diff2v4_T4 <- scenario.wide$S4_T4-scenario.wide$S2_T4
diff2v4_T5 <- scenario.wide$S4_T5-scenario.wide$S2_T5

diff3v4_T1 <- scenario.wide$S4_T1-scenario.wide$S3_T1
diff3v4_T2 <- scenario.wide$S4_T2-scenario.wide$S3_T2
diff3v4_T3 <- scenario.wide$S4_T3-scenario.wide$S3_T3
diff3v4_T4 <- scenario.wide$S4_T4-scenario.wide$S3_T4
diff3v4_T5 <- scenario.wide$S4_T5-scenario.wide$S3_T5

diff3v5_T1 <- scenario.wide$S5_T1-scenario.wide$S3_T1
diff3v5_T2 <- scenario.wide$S5_T2-scenario.wide$S3_T2
diff3v5_T3 <- scenario.wide$S5_T3-scenario.wide$S3_T3
diff3v5_T4 <- scenario.wide$S5_T4-scenario.wide$S3_T4
diff3v5_T5 <- scenario.wide$S5_T5-scenario.wide$S3_T5


diff <- data.frame(diff1v2_T1, diff1v2_T2, diff1v2_T3, diff1v2_T4, diff1v2_T5, 
                   diff1v3_T1, diff1v3_T2, diff1v3_T3, diff1v3_T4, diff1v3_T5,
                   diff1v4_T1, diff1v4_T2, diff1v4_T3, diff1v4_T4, diff1v4_T5,
                   diff1v5_T1, diff1v5_T2, diff1v5_T3, diff1v5_T4, diff1v5_T5,
                   diff2v3_T1, diff2v3_T2, diff2v3_T3, diff2v3_T4, diff2v3_T5,
                   diff2v4_T1, diff2v4_T2, diff2v4_T3, diff2v4_T4, diff2v4_T5,
                   diff3v4_T1, diff3v4_T2, diff3v4_T3, diff3v4_T4, diff3v4_T5,
                   diff3v5_T1, diff3v5_T2, diff3v5_T3, diff3v5_T4, diff3v5_T5)
diff.long <- 
  diff %>% pivot_longer(
  cols = diff1v2_T1:diff3v5_T5,
  names_to = c("diff", "time"),
  names_pattern = "diff?(.*)_(.*)")

#Example figures for mural
require(ggplot2)
library(paletteer)

require(viridis)

#Graph of scenarios over time

scenario.plot<-ggplot(data=scenario, aes(x=time, y=cc, fill = Scenario)) +
  geom_boxplot()+
  labs (x = bquote('Time Step'), y = expression('Canopy Cover (%)'), title = "**FAKE DATA** Canopy Cover by Scenario over Time")+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  theme (axis.text.x = element_text(size=12, angle = 90))+
  theme (axis.text.y = element_text (size = 12))+
  theme (axis.title.x = element_text (size =14))+  
  theme (axis.title.y = element_text (size =14))+
  theme (legend.text = element_text(size =12))+
  theme (legend.title = element_text (size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
require(tidyr)
require(dplyr)

#scatter plot of scenario means over time
scen.means <- scenario %>%
  group_by(Scenario, time) %>%
  summarise(mean = mean(cc), sd = sd(cc))

require(ggplot2)
scen.means.plot<-ggplot(data=scen.means, aes(x=time, y=mean)) +
  geom_point(aes(shape = Scenario, colour = Scenario))+
  scale_colour_viridis_d()+
  labs (x = bquote('Time Step'), y = expression('Canopy Cover (%)'), title = "**FAKE DATA** Mean Canopy Cover by Scenario over Time")+
  scale_y_continuous(labels = scales::comma)+
  theme (axis.text.x = element_text(size=12, angle = 90))+
  theme (axis.text.y = element_text (size = 12))+
  theme (axis.title.x = element_text (size =14))+  
  theme (axis.title.y = element_text (size =14))+
  theme (legend.text = element_text(size =12))+
  theme (legend.title = element_text (size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Focus on specific policy question 1 - comparing 1v2, 1v3, 1v4

Q1 <- filter(diff.long, diff=='1v2'| diff=='1v3' | diff == '1v4')

Q1scen <-filter(scenario, Scenario != 'S5')

#boxplot  that focuses on differences between scenarios 
Q1.plot<-ggplot(data=Q1, aes(x=time, y=value, fill = diff)) +
  geom_boxplot()+
  facet_grid(cols = vars(diff))+
  labs (x = bquote('Time Step'), y = expression('Difference in Canopy Cover'), title = "**FAKE DATA** Policy Question 1 - Canopy Cover Differences")+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  theme (axis.text.x = element_text(size=12, angle = 90))+
  theme (axis.text.y = element_text (size = 12))+
  theme (axis.title.x = element_text (size =14))+  
  theme (axis.title.y = element_text (size =14))+
  theme (legend.text = element_text(size =12))+
  theme (legend.title = element_text (size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#boxplot that focuses on actual values for each scenario (not difference)
Q1scen.plot<-ggplot(data=Q1scen, aes(x=time, y=cc, fill = Scenario)) +
  geom_boxplot()+
  facet_wrap(Scenario ~.)+
  labs (x = bquote('Time Step'), y = expression('Difference in Canopy Cover'), title = "**FAKE DATA** Policy Question 1 - Canopy Cover Differences")+
  scale_fill_viridis_d()+
  scale_y_continuous(labels = scales::comma)+
  theme (axis.text.x = element_text(size=12, angle = 90))+
  theme (axis.text.y = element_text (size = 12))+
  theme (axis.title.x = element_text (size =14))+  
  theme (axis.title.y = element_text (size =14))+
  theme (legend.text = element_text(size =12))+
  theme (legend.title = element_text (size = 14))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


S1 <- filter(scenario, Scenario == 'S1')
diff1v4 <- filter(diff.long, diff == '1v4')

meandiff <- diff1v4 %>%
  group_by(time) %>%
  summarise(mean = mean(value))

S1hist <- ggplot(data=S1, aes(x=cc))+
  geom_histogram(bins = 15, fill = '#440154FF',colour = "black" )+
  geom_vline(data=meanS1, aes(xintercept = mean), colour="#7AD151FF", linewidth =1) +
  facet_grid(time~.)+
  labs (y = bquote('Count'), x = expression('Canopy Cover'), title = "**FAKE DATA** Scenario 1 Canopy Cover averaged HUC")+
  scale_fill_viridis_d()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

diffhist <- ggplot(data=diff1v4, aes(x=value))+
  geom_histogram(bins = 15, fill = '#FDE725FF',colour = "black" )+
  geom_vline(data=meandiff, aes(xintercept = mean), colour="#7AD151FF", linewidth =1) +
  facet_grid(time~.)+
  labs (y = bquote('Count'), x = expression('Canopy Cover Difference'), title = "**FAKE DATA** Canopy Cover Differences Scenario 1v4 averaged across HUCS")+
  scale_fill_viridis_d()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

        

        