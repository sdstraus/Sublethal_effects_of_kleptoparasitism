# Load Libraries ------------------
library(car)
library(effsize)
library(lme4)
library(cowplot)
library(ggplot2)
library(stats) #glms
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)
library(ggpubr)

# ADULTS ----------------------------
# body condition
df_all <- read.csv("data/eximius_body_measurements.csv")

# set measurement period as factor
df_all$Measurement <- factor(df_all$Measurement, levels= c("before", "after"))
levels(df_all$Measurement)

# remove Col_21 : Note says "Escapees"
df_all <- df_all %>% filter(Nest != "Col_21")

# convert weight to mg
adults <- df_all %>% filter(instar == "Adult")
adults$weight.mg <- adults$weight.grams * 1000


#### Body Condition ####

# BCI regression
y <- lm(log(weight.mg) ~ log(CT.W), data = adults)
adults$resids <- residuals(y)

Anova(y)
summary(y)

adult.resid.plot <- ggplot(data=adults, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        axis.line = element_line(size = 1.0))



#### Effect Sizes ####

# adults, before comparing treatments
adults.before <- adults %>% 
  filter(Measurement == "before")

y1 <- lmer(resids ~ Treatment + (1|Nest), data = adults.before)
Anova(y1, test.statistic = "F")

adult.p1 <- ggplot(data = adults.before, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

cohen.d(adults.before$resids, adults.before$Treatment)

# adults, after comparing treatments
adults.after <- adults %>% 
  filter(Measurement == "after")

y2 <- lmer(resids ~ Treatment + (1|Nest), data = adults.after)
Anova(y2, test.statistic = "F")

cohen.d(adults.after$resids, adults.after$Treatment)

adult.p2 <- ggplot(data = adults.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# adults, control, comparing before and after
adults.control <- adults %>% 
  filter(Treatment == "Control")

cohen.d(adults.control$resids, adults.control$Measurement)

y3 <- lmer(resids ~ Measurement + (1|Nest), data = adults.control)
Anova(y3, test.statistic = "F")

adult.p3 <- ggplot(data = adults.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# parasites, control, comparing before and after
adults.parasite <- adults %>% 
  filter(Treatment == "Parasite")

y4 <- lmer(resids ~ Measurement + (1|Nest), data = adults.parasite)
Anova(y4, test.statistic = "F")

cohen.d(adults.parasite$resids, adults.parasite$Measurement)

adult.p4 <- ggplot(data = adults.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()


# differences in residuals (before & after) between treatments

ad.bf.col <- adults.before %>% 
  group_by(Nest, Treatment) %>% 
  summarize(before.res = mean(resids))

ad.af.col <- adults.after %>% 
  group_by(Nest, Treatment) %>% 
  summarize(after.res = mean(resids)) %>% 
  mutate(instar = "Adult")

adults.col <- full_join(ad.bf.col, ad.af.col)
adults.col$diff.resids <- adults.col$after.res - adults.col$before.res

ggplot(data = adults.col, aes(x = Treatment, y = diff.resids))+
  geom_boxplot()

anova(lm(diff.resids ~ Treatment, data = adults.col))

cohen.d(adults.col$diff.resids ~ adults.col$Treatment)


# SUBADULT 2 -------------------


#### Body Condition ####
levels(df_all$instar)
#df_all <- read.csv("eximius_body_measurements.csv")
sub2 <- df_all %>% filter(instar == "Sub 2")
sub2$weight.mg <- sub2$weight.grams * 1000

# BCI regression
y <- lm(log(weight.mg) ~ log(CT.W), data = sub2)
sub2$resids <- residuals(y)

Anova(y)
summary(y)


sub2.resid.plot <- ggplot(data=sub2, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        axis.line = element_line(size = 1.0))


#### Effect Sizes ####
# sub2, before comparing treatments
sub2.before <- sub2 %>% 
  filter(Measurement == "before")

y1 <- lmer(resids ~ Treatment + (1|Nest), data = sub2.before)
Anova(y1, test.statistic = "F")

sub2.p0 <- ggplot(data = sub2.before, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

cohen.d(sub2.before$resids, sub2.before$Treatment)

# sub2, after comparing treatments
sub2.after <- sub2 %>% 
  filter(Measurement == "after")

cohen.d(sub2.after$resids, sub2.after$Treatment)

y2 <- lmer(resids ~ Treatment + (1|Nest), data = sub2.after)
Anova(y2, test.statistic = "F")

sub2.p1 <- ggplot(data = sub2.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# sub2, control, comparing before and after
sub2.control <- sub2 %>% 
  filter(Treatment == "Control")

cohen.d(sub2.control$resids, sub2.control$Measurement)

y3 <- lmer(resids ~ Measurement + (1|Nest), data = sub2.control)
Anova(y3, test.statistic = "F")

sub2.p2 <- ggplot(data = sub2.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# parasites, sub2, comparing before and after
sub2.parasite <- sub2 %>% 
  filter(Treatment == "Parasite")
cohen.d(sub2.parasite$resids, sub2.parasite$Measurement)

y4 <- lmer(resids ~ Measurement + (1|Nest), data = sub2.parasite)
Anova(y4, test.statistic = "F")

sub2.p3 <- ggplot(data = sub2.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()



# differences in residuals (before & after) between treatments

s2.bf.col <- sub2.before %>% 
  group_by(Nest, Treatment) %>% 
  summarize(before.res = mean(resids))

s2.af.col <- sub2.after %>% 
  group_by(Nest, Treatment) %>% 
  summarize(after.res = mean(resids)) %>% 
  mutate(instar = "Sub2")

sub2.col <- full_join(s2.bf.col, s2.af.col)
sub2.col$diff.resids <- sub2.col$after.res - sub2.col$before.res

ggplot(data = sub2.col, aes(x = Treatment, y = diff.resids))+
  geom_boxplot()

anova(lm(diff.resids ~ Treatment, data = sub2.col)) # p=0.051
cohen.d(sub2.col$diff.resids, sub2.col$Treatment, na.rm = TRUE) #medium

# SUBADULT 1 -------------------------------

###### Body Condition #########

sub1wts <- read.csv("data/sub1_weights.csv")
sub1wts$avg.wt <- (sub1wts$Weight / sub1wts$num.sub1)
sub1wts.before <- sub1wts[c(which(sub1wts$Measurement == "before")),]
sub1wts.after <- sub1wts[c(which(sub1wts$Measurement == "after")),]

sub1 <- df_all[-c(which(df_all$instar != "Sub 1")),]
sub1.before <- sub1[c(which(sub1$Measurement == "before")),]
sub1.after <- sub1[c(which(sub1$Measurement == "after")),]

#### sub average weight for each individual
## before
for(i in 1:length(sub1.before$Nest)){
  for(j in 1:length(sub1wts.before$Nest)){
    if(sub1.before$Nest[i] == sub1wts.before$Nest[j]){
      sub1.before$weight.grams[i] <- sub1wts.before$avg.wt[j]
    }}}

## after
for(i in 1:length(sub1.after$Nest)){
  for(j in 1:length(sub1wts.after$Nest)){
    if(sub1.after$Nest[i] == sub1wts.after$Nest[j]){
      sub1.after$weight.grams[i] <- sub1wts.after$avg.wt[j]
    }}}

#sub1.before <- sub1.before[,-10]
sub1.final <- rbind(sub1.after, sub1.before)
sub1.final$weight.mg <- sub1.final$weight.grams * 1000
write.csv(sub1.final, "data/Sub1wts.csv", row.names = FALSE)


#sub1.final <- read.csv("data/Sub1wts.csv")

# remove colony 21
sub1.final <- sub1.final %>% filter(Nest != "Col_21")

# create measurement factors
sub1.final$Measurement <- factor(sub1.final$Measurement, levels = c("before", "after"))

# BCI regression
y <- lm(log(weight.mg) ~ log(CT.W), data = sub1.final)
sub1.final$resids <- residuals(y)

Anova(y)
summary(y)
plot(log(weight.grams) ~ log(CT.W), data = sub1.final)
abline(y)


sub1.resid.plot <- ggplot(data=sub1.final, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 16, color = "black"),
        axis.title = element_text(size = 16),
        axis.line = element_line(size = 1.0))

resid.plots <- ggarrange(adult.resid.plot, sub2.resid.plot, sub1.resid.plot, labels = "auto", nrow = 1)
ggsave(resid.plots, filename = "figures/resid.plots.jpeg", dpi = "retina",
       width = 12, height = 4, units = "in")


#### Effect Sizes ####
# sub1, before comparing treatments
sub1.before <- sub1.final %>% 
  filter(Measurement == "before")

y1 <- lmer(resids ~ Treatment + (1|Nest), data = sub1.before)
Anova(y1, test.statistic = "F")

sub1.p1 <- ggplot(data = sub1.before, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

cohen.d(sub1.before$resids, sub1.before$Treatment)

# sub1, after comparing treatments
sub1.after <- sub1.final %>% 
  filter(Measurement == "after")

cohen.d(sub1.after$resids, sub1.after$Treatment)

y2 <- lmer(resids ~ Treatment + (1|Nest), data = sub1.after)
Anova(y2, test.statistic = "F")

sub1.p2 <- ggplot(data = sub1.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# sub1, control, comparing before and after
sub1.control <- sub1.final %>% 
  filter(Treatment == "Control")
cohen.d(sub1.control$resids, sub1.control$Measurement)

y3 <- lmer(resids ~ Measurement + (1|Nest), data = sub1.control)
Anova(y3, test.statistic = "F")

sub1.p3 <- ggplot(data = sub1.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# parasites, sub2, comparing before and after
sub1.parasite <- sub1.final %>% 
  filter(Treatment == "Parasite")
effsize::cohen.d(sub1.parasite$resids, sub1.parasite$Measurement)

y4 <- lmer(resids ~ Measurement + (1|Nest), data = sub1.parasite)
Anova(y4, test.statistic = "F")

sub1.p4 <- ggplot(data = sub1.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()



# differences in residuals (before & after) between treatments

s1.bf.col <- sub1.before %>% 
  group_by(Nest, Treatment) %>% 
  summarize(before.res = mean(resids))

s1.af.col <- sub1.after %>% 
  group_by(Nest, Treatment) %>% 
  summarize(after.res = mean(resids))

sub1.col <- full_join(s1.bf.col, s1.af.col)
sub1.col$diff.resids <- sub1.col$after.res - sub1.col$before.res

ggplot(data = sub1.col, aes(x = Treatment, y = diff.resids))+
  geom_boxplot()

anova(lm(diff.resids ~ Treatment, data = sub1.col))
cohen.d(sub1.col$diff.resids, sub1.col$Treatment)

# POOLED -----------------------

all.after.summary <- full_join(ad.af.col, s2.af.col, by = c("Nest", "Treatment"))
all.after.summary <- full_join(all.after.summary, s1.af.col, by = c("Nest", "Treatment"))

adults.col$instar <- "Adult"
sub2.col$instar <- "Sub 2"
sub1.col$instar <- "Sub 1"

all.col <- rbind(adults.col, sub2.col, sub1.col)
write.csv(all.col, "data/BCI_cleaned_all.csv", row.names = FALSE)
