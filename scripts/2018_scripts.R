####### Load Libraries #########
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

######## ADULTS  ##########
# body condition
df_all <- read.csv("../data/eximius_body_measurements.csv")
levels(df_all$Measurement)
df_all$Measurement <- factor(df_all$Measurement, levels= c("before", "after"))

# remove Col_21 : Note says "Escapees"
df_all <- df_all %>% filter(Nest != "Col_21")


adults <- df_all %>% filter(instar == "Adult")
adults$weight.mg <- adults$weight.grams * 1000

# 
# hist(adults$weight.grams)
# hist(sqrt(adults$weight.mg))
# hist(log10(adults$weight.grams))
# 
# z <- lm(log10(weight.mg) ~ Measurement, data = adults)
# summary(z)
# anova(z)
# plot(z)
# plot(weight.mg ~ Measurement, data = adults)

# adults.before <- adults[c(which(adults$Measurement == "before")),]

y <- lm(log(weight.mg) ~ log(CT.W), data = adults)
adults$resids <- residuals(y)

Anova(y)
summary(y)

ggplot(data=adults, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20),
        axis.line = element_line(size = 1.1))



#### adult effect sizes ####

# adults, before comparing treatments
adults.before <- adults %>% 
  filter(Measurement == "before")

y1 <- lmer(resids ~ Treatment + (1|Nest), data = adults.before)
Anova(y1, test.statistic = "F")

adult.p0 <- ggplot(data = adults.before, aes(x = resids, group = Treatment, fill = Treatment)) + 
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

adult.p1 <- ggplot(data = adults.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# adults, control, comparing before and after
adults.control <- adults %>% 
  filter(Treatment == "Control")

cohen.d(adults.control$resids, adults.control$Measurement)

y3 <- lmer(resids ~ Measurement + (1|Nest), data = adults.control)
Anova(y3, test.statistic = "F")

adult.p2 <- ggplot(data = adults.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# parasites, control, comparing before and after
adults.parasite <- adults %>% 
  filter(Treatment == "Parasite")

y4 <- lmer(resids ~ Measurement + (1|Nest), data = adults.parasite)
Anova(y4, test.statistic = "F")

cohen.d(adults.parasite$resids, adults.parasite$Measurement)

adult.p3 <- ggplot(data = adults.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()


####### Sub 2 body condition ########
levels(df_all$instar)
#df_all <- read.csv("eximius_body_measurements.csv")
sub2 <- df_all %>% filter(instar == "Sub 2")
sub2$weight.mg <- sub2$weight.grams * 1000

# hist(sub2$weight.grams)
# hist(sqrt(sub2$weight.mg))
# hist(log10(sub2$weight.grams)) #best
# 
# z <- lm(log10(weight.mg) ~ Measurement, data = sub2)
# summary(z)
# anova(z)
# plot(z)
# plot(weight.mg ~ Measurement, data = sub2)
# ## overall, sub2 weights greater after than before -- growth

y <- lm(log(weight.mg) ~ log(CT.W), data = sub2)
sub2$resids <- residuals(y)

Anova(y)
summary(y)


ggplot(data=sub2, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20),
        axis.line = element_line(size = 1.1))


#### sub 2 effect sizes ####
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


###### Sub 1 body condition #########

# !! keep, important code but don't need to rerun every time !!

# sub1wts <- read.csv("../data/sub1_weights.csv")
# sub1wts$avg.wt <- (sub1wts$Weight / sub1wts$num.sub1)
# sub1wts.before <- sub1wts[c(which(sub1wts$Measurement == "before")),]
# sub1wts.after <- sub1wts[c(which(sub1wts$Measurement == "after")),]
# 
# sub1 <- df_all[-c(which(df_all$instar != "Sub 1")),]
# sub1.before <- sub1[c(which(sub1$Measurement == "before")),]
# sub1.after <- sub1[c(which(sub1$Measurement == "after")),]
# 
# #### sub average weight for each individual
# ## before
# for(i in 1:length(sub1.before$Nest)){
#   for(j in 1:length(sub1wts.before$Nest)){
#       if(sub1.before$Nest[i] == sub1wts.before$Nest[j]){
#     sub1.before$weight.grams[i] <- sub1wts.before$avg.wt[j]
#       }}}
# 
# ## after
# for(i in 1:length(sub1.after$Nest)){
#   for(j in 1:length(sub1wts.after$Nest)){
#     if(sub1.after$Nest[i] == sub1wts.after$Nest[j]){
#       sub1.after$weight.grams[i] <- sub1wts.after$avg.wt[j]
#     }}}
# 
# #sub1.before <- sub1.before[,-10]
# sub1.final <- rbind(sub1.after, sub1.before)
# sub1.final$weight.mg <- sub1.final$weight.grams * 1000
# write.csv(sub1.final, "../data/Sub1wts.csv", row.names = FALSE)

### !! ^^ keep, important code, but don't need to rerun !! ## 

sub1.final <- read.csv("../data/Sub1wts.csv")

# remove colony 21
sub1.final <- sub1.final %>% filter(Nest != "Col_21")

sub1.final$Measurement <- factor(sub1.final$Measurement, levels = c("before", "after"))

y <- lm(log(weight.mg) ~ log(CT.W), data = sub1.final)
sub1.final$resids <- residuals(y)

Anova(y)
summary(y)
plot(log(weight.grams) ~ log(CT.W), data = sub1.final)
abline(y)


ggplot(data=sub1.final, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20),
        axis.line = element_line(size = 1.1))

#### sub 1 effect sizes ####
# sub1, before comparing treatments
sub1.before <- sub1.final %>% 
  filter(Measurement == "before")

y1 <- lmer(resids ~ Treatment + (1|Nest), data = sub1.before)
Anova(y1, test.statistic = "F")

sub1.p0 <- ggplot(data = sub1.before, aes(x = resids, group = Treatment, fill = Treatment)) + 
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

sub1.p1 <- ggplot(data = sub1.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# sub1, control, comparing before and after
sub1.control <- sub1.final %>% 
  filter(Treatment == "Control")
cohen.d(sub1.control$resids, sub1.control$Measurement)

y3 <- lmer(resids ~ Measurement + (1|Nest), data = sub1.control)
Anova(y3, test.statistic = "F")

sub1.p2 <- ggplot(data = sub1.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()

# parasites, sub2, comparing before and after
sub1.parasite <- sub1.final %>% 
  filter(Treatment == "Parasite")
cohen.d(sub1.parasite$resids, sub1.parasite$Measurement)

y4 <- lmer(resids ~ Measurement + (1|Nest), data = sub1.parasite)
Anova(y4, test.statistic = "F")

sub1.p3 <- ggplot(data = sub1.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)+
  xlab("Body Condition Index")+
  theme_cowplot()


#### plots ####
before.plots <- ggarrange(adult.p0, sub2.p0, sub1.p0, labels = c("Adult", "Sub 2", "Sub 1"), 
                         label.x = c(0.13, 0.115, 0.1), label.y = 1, nrow = 1, common.legend = TRUE, legend = 'right')

after.plots <- ggarrange(adult.p1, sub2.p1, sub1.p1, nrow = 1, common.legend = TRUE, legend = 'right')


measurement.plots <- ggarrange(before.plots, after.plots, nrow = 2, labels=c("Before", "After"),
                             label.x = -0.07, label.y = 0.8, common.legend = TRUE, legend = 'right')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "cm"))



ggsave(after.plots, filename = "../figures/after.density.jpeg", dpi = "retina", 
       units = "in", width = 14, height = 4.5)
ggsave(measurement.plots, filename = "../figures/measurement.density.jpeg", dpi = "retina", 
       units = "in", width = 12, height = 7)

control.plots <- ggarrange(adult.p2, sub2.p2, sub1.p2, labels = c("Adult", "Sub 2", "Sub 1"), 
                  label.x = c(-.13, -.14, -.13), label.y = 0.85, nrow = 3, 
                  common.legend = TRUE, legend = 'right')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "cm")) 

parasite.plots <-ggarrange(adult.p3, sub2.p3, sub1.p3, nrow = 3, 
          common.legend = TRUE, legend = 'right') +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")) 


treatment.plots <- ggarrange(control.plots, parasite.plots, nrow = 1, labels=c("Control", "Experimental"),
                             label.x = 0.25, common.legend = TRUE, legend = 'right')+
  theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "cm"))

ggsave(treatment.plots, filename = "../figures/treatments.density.jpeg", dpi = "retina", 
       units = "in", width = 12, height = 12)

########### ALL body condition ##########

df_new <- rbind(adults, sub2, sub1.final)

y <- lm(log(weight.mg) ~ log(CT.W), data = df_new)
df_new$resids <- residuals(y)
Anova(y)


ggplot(data=df_new, aes(x=CT.W, y = weight.mg))+
  geom_point()+
  geom_smooth(method = "lm")+
  xlab("Cephalothorax Width (mm)")+
  ylab("Spider Weight (mg)")+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20),
        axis.line = element_line(size = 1.1))

# order instar
df_new$instar <- factor(df_new$instar, order = TRUE, levels = c("Sub 1", "Sub 2", "Adult"))

# mega model
full.mod <- lmer(resids ~ Treatment + Measurement + Treatment:Measurement + (1|instar) + (1|Nest), data = df_new)
Anova(full.mod, test.statistic = "F")
summary(full.mod)


ggplot(full.mod, aes(x = Treatment, y = resids, color = Measurement, fill = instar))+
  geom_boxplot()+
  theme_classic()+
  scale_color_manual(values = c("black", "darkgrey"))+
  theme(text = element_text(size=20))+
  ylab("Body Condition Index")

control <- df_all[c(which(df_all$Treatment == "Control")),]
x <- lmer(resids ~ Measurement + instar+ (1|Nest), control)
Anova(x)
summary(x)
plot(resids ~ Measurement, control,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
Fig4a <- ggplot(control, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

parasite <- df_all[c(which(df_all$Treatment == "Parasite")),]
x <- lmer(resids ~ Measurement + (1|Nest), parasite)
Anova(x)
plot(resids ~ Measurement, parasite,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

Fig4b <- ggplot(parasite, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

df.after <- df_all[c(which(df_all$Measurement == "after")),]


y2 <- lmer(resids ~ Treatment + (1|Nest), data = df.after)
Anova(y2)
plot(resids ~ Treatment, data = df.after,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

y2 <- lmer(resids ~ Treatment + Measurement + Treatment:Measurement + (1|Nest), data = df_all)
Anova(y2)

label.df <- data.frame(Group = c("Control", "Parasite"),
                       Value = c(6, 9))

Fig4c <- ggplot(df.after, aes(x = Treatment, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

multiplot(Fig4a, Fig4b, Fig4c, cols = 3)





# all, after comparing treatments
all.after <- df_all %>% 
  filter(Measurement == "after")
cohen.d(all.after$resids, all.after$Treatment)

all.p1 <- ggplot(data = all.after, aes(x = resids, group = Treatment, fill = Treatment)) + 
  geom_density(alpha = 0.5)

# all, control, comparing before and after
all.control <- df_all %>% 
  filter(Treatment == "Control")
cohen.d(all.control$resids, all.control$Measurement)

all.p2 <- ggplot(data = all.control, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)

# parasites, all, comparing before and after
all.parasite <- df_all %>% 
  filter(Treatment == "Parasite")
cohen.d(all.parasite$resids, all.parasite$Measurement)

all.p3 <- ggplot(data = all.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
  geom_density(alpha = 0.5)





############ Mortality ##########
col.counts <- read.csv("../data/colony_counts.csv")

# remove Col_21 : Note says "Escapees"
col.counts <- col.counts %>% filter(Colony != "Col_21")


View(col.counts)
col.counts$num.all <- col.counts$num.adult + col.counts$num.sub2 + col.counts$num.sub1
num.after <- col.counts[c(which(col.counts$Measurement == "After")),]

num.after[,7] <- NULL

x <- lm(num.adult ~ Treatment, data =num.after)
x.2 <- glm(num.adult ~ Treatment, data =num.after, family = poisson(link = "identity"))
AIC(x, x.2)
Anova(x.2)
plot(num.adult ~ Treatment, num.after, ylab = "Number Adults", cex.axis = 1.5, cex.lab = 1.5)
cohen.d(num.after$num.adult, num.after$Treatment)
# diff in variance

y <- lm(num.sub2 ~ Treatment, data = num.after)
y.2 <- glm(num.sub2 ~ Treatment, data = num.after, family = poisson(link = "identity"))
AIC(y, y.2)
Anova(y.2)
plot(num.sub2 ~ Treatment, num.after, ylab = "Number Sub 2", cex.axis = 1.5, cex.lab = 1.5)  #...higher?
cohen.d(num.after$num.sub2, num.after$Treatment)

z <- glm(num.sub1 ~ Treatment, data =  num.after, family = poisson(link = "identity"))
Anova(z)
plot(num.sub1 ~ Treatment, num.after, ylab = "Number Sub 1", cex.axis = 1.5, cex.lab = 1.5)
cohen.d(num.after$num.sub1, num.after$Treatment)

w <- glm(num.all ~ Treatment, data = num.after, family = poisson(link = "identity"))
Anova(w)
plot(num.all ~ Treatment, num.after, ylab = "Total Number", cex.axis = 1.5, cex.lab = 1.5)
cohen.d(num.after$num.all, num.after$Treatment)

# make num.after long format
num.long <- num.after %>%
  pivot_longer(!c(Colony, Treatment, Measurement), names_to = "instar", values_to = "count")

num.long <- num.long %>% mutate(instar = recode_factor(instar, num.sub1  = "Sub 1", num.sub2 = "Sub 2", num.adult = "Adult"))
num.long$instar <- factor(num.long$instar, order = TRUE, levels = c("Sub 1", "Sub 2", "Adult"))

ggplot(data = filter(num.long, instar != "num.all"), aes(x = count, group = instar, fill = instar)) + 
  geom_density(alpha = 0.5)+
  xlab("Count")+
  theme_cowplot()

hist(num.after$num.adult)
hist(num.after$num.sub1)
hist(num.after$num.sub2)

num.long <-  num.long %>% filter(instar != "num.all")

instar.count <- glm(count ~ instar, data = num.long, family = poisson(link = "identity"))
Anova(instar.count)
summary(instar.count)

mod.av <- aov(instar.count)
TUKEY <- TukeyHSD(x=mod.av)
sig.letters <- TUKEY$instar[order(row.names(TUKEY$instar)), ]

value_max = num.long %>% group_by(instar) %>% summarize(max_value = max(count))
ggplot(data = filter(num.long, instar != "num.all"), aes(x = instar, y = count, fill = instar)) + 
  geom_boxplot(alpha = 0.5)+
  geom_text(data = value_max, aes(x = instar, y = max_value + 1, label = c("a", "ab", "b")) ,vjust=0)+
  xlab("Instar")+
  theme_cowplot()+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  theme(legend.position = "none")

# compare total number in after with instar specific body condition

# only looking at after
avg_BCI_after <- df_new %>% filter(Measurement == "after") %>% 
  group_by(Nest) %>% 
  summarise(BCI = mean(resids))

num.bci.after <- left_join(num.after, avg_BCI_after, by = c("Colony" = "Nest"))

bci.m1 <- lmer(BCI ~ num.all, data = num.bci.after)
summary(bci.m1)
plot(BCI ~ num.all, data = num.bci.after)

bci.m1.5 <- lm(BCI ~ num.all + I(num.all ^ 2), data = num.bci.after)
summary(bci.m1.5)

AIC(bci.m1, bci.m1.5) # quadratic preferred

bci.m2 <- lm(BCI ~ num.all + Treatment, data = num.bci.after)
summary(bci.m2)

bci.m3 <- lmer(BCI ~ Treatment, data = num.bci.after)
summary(bci.m3)
Anova(bci.m3)
plot(BCI ~ Treatment, data = num.bci.after)
cohen.d(num.bci.after$BCI, num.bci.after$Treatment)

ggplot(num.bci.after, aes(x = Treatment, y = BCI))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  stat_summary(fun=mean, geom="point", size=2, color="red")+
  theme_cowplot()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Index")


########### Find Lambda ##########
# classic ecology uses lambda for discreet population growth rate
# two measures: lambda = (ln(N0) - ln(Nt))/t ORR lambda = Nt+1/Nt

col.counts <- col.counts[,-7]

# make wide format to subtract before from after
wide.col.counts <- reshape(col.counts, idvar = "Colony", timevar = "Measurement", direction = 'wide')

wide.col.counts$lambda.all <- wide.col.counts$num.all.After / wide.col.counts$num.all.Before
wide.col.counts$lambda.adult <- wide.col.counts$num.adult.After / wide.col.counts$num.adult.Before
wide.col.counts$lambda.sub2 <- wide.col.counts$num.sub2.After / wide.col.counts$num.sub2.Before
wide.col.counts$lambda.sub1 <- wide.col.counts$num.sub1.After / wide.col.counts$num.sub1.Before

hist(wide.col.counts$lambda.all)

z <- lm(lambda.all ~ Treatment.After, wide.col.counts)
Anova(z)
plot(lambda.all ~ Treatment.After, wide.col.counts)

z <- lm(lambda.adult ~ Treatment.After, wide.col.counts)
Anova(z)
plot(lambda.adult ~ Treatment.After, wide.col.counts)

z <- lm(lambda.sub2 ~ Treatment.After, wide.col.counts)
Anova(z)
plot(lambda.sub2 ~ Treatment.After, wide.col.counts)

z <- lm(lambda.sub1 ~ Treatment.After, wide.col.counts)
Anova(z)
plot(lambda.sub1 ~ Treatment.After, wide.col.counts)



