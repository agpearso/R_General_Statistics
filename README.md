## Set working directory ##

    setwd("##path on computer##")


## Load previous workspace if necessary ##

    load("##path on computer_File_name##")

    view(##File_name##)


## Descriptives: Example - Age ##

sum(##File_name##$Age)
(sum(##File_name##$Age)/length(##File_name##$Age))*100

mean(##File_name##$Age)

sd(##File_name##$Age)

sd(##File_name##$Age)/sqrt(length(##File_name##$Age))

max(##File_name##$Age)

min(##File_name##$Age)

range(##File_name##$Age)

shapiro.test(##File_name##$Age)


## Visualization ##

hist(##File_name##$Age)
qqnorm(##File_name##$Age)
boxplot(##File_name##$Age)


## Statistics: Example - Heart Rate; Young vs. Older adults ##

?var.test

?t.test

?cohen.d

var.test(x = Young_Adults$Heart_Rate, y = Older_Adults$Heart_Rate)

t.test(x = Young_Adults$Heart_Rate, y = Older_Adults$Heart_Rate, var.equal = TRUE, paired = FALSE)

cohen.d(Young_Adults$Heart_Rate, Older_Adults$Heart_Rate)

cohen.d(Young_Adults$Heart_Rate, Older_Adults$Heart_Rate, paired = FALSE, hedges.correction = TRUE)

cohen.d(Young_Adults$Heart_Rate, Older_Adults$Heart_Rate, paired = TRUE)


## ANOVA: Example - Heart rate across conditions of upright tilt in 100 adults ##

Heart_Rate <-data.frame(Subject=c(rep(1:100, 4)),
                            Condition=c(rep("0", times = 100), rep("30", times = 100), 
                                        rep("45", times = 100), rep("60", times = 100)),
                            Heart.Rate=c((##File_name##$Heart_Rate_Baseline), (##File_name##$Heart_Rate_30_Degrees), 
                                         (##File_name##$Heart_Rate_45_Degrees), (##File_name##$Heart_Rate_60_Degrees.)))

Heart_Rate
view(Heart_Rate)

?str
str(Heart_Rate)
Heart_Rate$Subject<-as.factor(Heart_Rate$Subject)
Heart_Rate$Condition<-as.factor(Heart_Rate$Condition)
str(Heart_Rate)

Heart_Rate %>%
  group_by(Condition) %>%
  get_summary_stats(Heart.Rate, type = "mean_sd")

?ggplot
Heart_Rate_Boxplot <- ggboxplot(Heart_Rate, x = "Condition", y = "Heart.Rate")
Heart_Rate_Boxplot

Heart_Rate_ggBoxplot <- ggplot(Heart_Rate, aes(x = Condition, y = Heart.Rate, fill = Condition))+
  geom_boxplot(position = position_dodge(width = 1), outlier.shape = NA, lwd = 1, fatten = 1, colour= "black")+
  theme_classic()+
  ylab("Heart Rate (bpm)")+
  theme(legend.position="top")+
  scale_y_continuous(breaks=c(40,60,80,100,120), labels = waiver(), limits=c(40,120))+
  theme(axis.text.y = element_text(colour = "Black", face="bold", size = 12),text = element_text(size=18),
        axis.text.x = element_text(colour = "Black", face = "bold", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "Black", face="bold", size = 16),
        axis.ticks.length = unit(4, "pt"), 
        axis.ticks.x = element_line(color = "black"), axis.ticks.y = element_line(color = "black"))

Heart_Rate_ggBoxplot

Heart_Rate_Violin <- ggplot(Heart_Rate, aes(x=Condition, y=Heart.Rate, fill=Condition)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_classic()+
  theme(
    legend.position="right",
    plot.title = element_text(size=0)
  ) +
  ggtitle("Violin chart") +
  xlab("Condition")
Heart.Rate_Violin

ggline(Heart_Rate, x = "Condition", y = "Heart.Rate",
       add = c("mean_sd"),
       palette = c("#00AFBB", "#E7B800"))

Heart_Rate %>%
  group_by(Condition) %>%
  identify_outliers(Heart.Rate)

Heart_Rate %>%
  group_by(Condition) %>%
  shapiro_test(Heart.Rate)
  
ggqqplot(Heart_Rate, "Heart.Rate", facet.by = "Condition")

gghistogram(Heart_Rate, "Heart.Rate", facet.by = "Condition")

?anova_test
Heart_Rate_aov <- anova_test(data = Heart_Rate, dv = Heart.Rate, wid = Subject, within = Condition, type = 3)

get_anova_table(Heart_Rate_aov)
print(Heart_Rate_aov)

?p.adjust.methods

Heart_Rate_aov<- Heart_Rate %>%
  pairwise_t_test(
    Heart.Rate ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni")
Heart_Rate_aov


## Two-way RMANOVA: Example - Blood pressure by sex and condition of upright tilt in 50 men and women##


Blood_Pressure <-data.frame(Subject=c(rep(1:100, 4)),
                                Sex=c(rep(Orthostatic_Tilt_Raw$Sex, 4)),
                                Condition=c(rep("0", times = 100), rep("30", times = 100), 
                                            rep("45", times = 100), rep("60", times = 100)),
                                Blood.Pressure=c((##File_name##$Blood_Pressure_Baseline), (##File_name##$Blood_Pressure_30_Degrees), 
                                             (##File_name##$Blood_Pressure_45_Degrees), (##File_name##$Blood_Pressure_60_Degrees)))


Blood_Pressure
view(Blood_Pressure)

str(Blood_Pressure)
Blood_Pressure$Subject<-as.factor(Raw_Blood_Pressure$Subject)
Blood_Pressure$Sex<-as.factor(Raw_Blood_Pressure$Sex)
Blood_Pressure$Condition<-as.factor(Raw_Blood_Pressure$Condition)
str(Blood_Pressure)

Blood_Pressure %>%
  group_by(Sex, Condition) %>%
  get_summary_stats(Blood.Pressure, type = "mean_sd")

bxp <- ggboxplot(Blood_Pressure, x = "Condition", y = "Blood.Pressure", fill = "Sex")
bxp

abxp <- ggplot(Blood_Pressure, aes(x = Condition, y = Blood.Pressure, fill = Sex))+
  geom_boxplot(position = position_dodge(width = 1), outlier.shape = NA, lwd = 1, fatten = 1, colour= "black")+
  theme_classic()+
  ylab("Blood Pressure (mmHg)")+
  theme(legend.position="right")+
  scale_y_continuous(breaks=c(60,70,80,90,100,110,120), labels = waiver(), limits=c(60,120))+
  theme(axis.text.y = element_text(colour = "Black", face="bold", size = 12),text = element_text(size=18),
        axis.text.x = element_text(colour = "Black", face = "bold", size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour = "Black", face="bold", size = 16),
        axis.ticks.length = unit(4, "pt"), 
        axis.ticks.x = element_line(color = "black"), axis.ticks.y = element_line(color = "black"))

abxp

Blood_Pressure_Violin <- ggplot(Blood_Pressure, aes(x=Condition, y=Blood.Pressure, fill=Sex)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=1, option="C") +
  theme_classic()+
  theme(
    legend.position="top",
    plot.title = element_text(size=12)
  ) +
  ggtitle("Blood Pressure During Orthostatic Tilt by Sex") +
  xlab("Condition")+
  scale_y_continuous(breaks=c(60,70,80,90,100,110,120), labels = waiver(), limits=c(60,120))
Raw_Blood_Pressure_Violin

ggline(Blood_Pressure, x = "Condition", y = "Blood.Pressure", color = "Sex",
       add = c("mean_se"),
       palette = c("#00AFBB", "#E7B800"))+
  scale_y_continuous(breaks=c(60,70,80,90,100,110,120), labels = waiver(), limits=c(60,120))

Blood_Pressure %>%
  group_by(Sex, Condition) %>%
  identify_outliers(Blood.Pressure)

Blood_Pressure %>%
  group_by(Sex, Condition) %>%
  shapiro_test(Blood.Pressure)

ggqqplot(Blood_Pressure, "Blood.Pressure", facet.by = "Condition")

ggqqplot(Blood_Pressure, "Blood.Pressure", ggtheme = theme_bw()) +
  facet_grid(Condition ~ Sex, labeller = "label_both")

Blood_Pressure_aov <- anova_test(data = Blood_Pressure, dv = Blood.Pressure, wid = Subject, between = Sex, within = Condition, type = 3)
get_anova_table(Blood_Pressure)
print(Blood_Pressure)

Blood_Pressure_pwc<- Blood_Pressure %>%
  pairwise_t_test(
    Blood.Pressure ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni")
Raw_Blood_Pressure_pwc

## Effect of Condition within Sex ##


Blood_Pressure_One_way <- Blood_Pressure %>%
  group_by(Sex) %>%
  anova_test(dv=Blood.Pressure, wid = Subject, within = Condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
Blood_Pressure_One_way

Blood_Pressure_pwc <- Blood_Pressure %>%
  group_by(Sex) %>%
  pairwise_t_test(
    Blood.Pressure ~ Condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
Blood_Pressure_pwc
view(Blood_Pressure_pwc)

## Effect of Sex within Condition ##


Blood_Pressure_pwc <- Blood_Pressure %>%
  group_by(Condition) %>%
  pairwise_t_test(
    Blood.Pressure ~ Sex, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
Blood_Pressure_pwc
view(Blood_Pressure_pwc)

Blood_Pressure_pwc <- Blood_Pressure_pwc %>% add_xy_position(x = "Condition")
bxp + 
  stat_pvalue_manual(Blood_Pressure_pwc, tip.length = 0, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(Blood_Pressure_pwc.aov, detailed = TRUE),
    caption = get_pwc_label(Blood_Pressure_pwc)
  )


## Correlations ##


?cor.test

cor.test(x = ##File_name##$Heart_Rate_Baseline, ##File_name##$Heart_Rate_60_Degrees)
cor.test(x = ##File_name##$Blood_Pressure_Baseline, ##File_name##$Blood_Pressure_60_Degrees)


?plot

plot(x = ##File_name##$Heart_Rate_Baseline, ##File_name##$Heart_Rate_60_Degrees)

plot(x = ##File_name##$Heart_Rate_Baseline, ##File_name##$Heart_Rate_60_Degrees, 
     xlab="Heart ate at baseline (bpm)", ylab= "Heart rate at 60 degrees (bpm)",
     col = ifelse(##File_name##$Sex == "1", "Red", "Blue"),
pch = ifelse(##File_name##$Sex == "1", 19, 15))
abline(lm(##File_name##$Heart_Rate_60_Degrees~##File_name##$Heart_Rate_Baseline), lwd = 2, col = "Black")
