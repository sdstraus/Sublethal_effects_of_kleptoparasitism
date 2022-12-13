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

setwd("/Users/sam/github/FitnessEffects/scripts")

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

anova(lm(diff.resids ~ Treatment, data = adults.col)) # p=0.41

cohen.d(adults.col$diff.resids ~ adults.col$Treatment) #small

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
effsize::cohen.d(sub1.parasite$resids, sub1.parasite$Measurement)

y4 <- lmer(resids ~ Measurement + (1|Nest), data = sub1.parasite)
Anova(y4, test.statistic = "F")

sub1.p3 <- ggplot(data = sub1.parasite, aes(x = resids, group = Measurement, fill = Measurement)) + 
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

write.csv(s1.af.col, "../data/sub1_bcis_after.csv")

all.after.summary <- full_join(ad.af.col, s2.af.col, by = c("Nest", "Treatment"))
all.after.summary <- full_join(all.after.summary, s1.af.col, by = c("Nest", "Treatment"))

write.csv(all.after.summary, "../data/all.after.summary.csv")

sub1.col <- full_join(s1.bf.col, s1.af.col)
sub1.col$diff.resids <- sub1.col$after.res - sub1.col$before.res

ggplot(data = sub1.col, aes(x = Treatment, y = diff.resids))+
  geom_boxplot()

anova(lm(diff.resids ~ Treatment, data = sub1.col)) # p=0.30
cohen.d(sub1.col$diff.resids, sub1.col$Treatment) #medium

adults.col$instar <- "Adult"
sub2.col$instar <- "Sub 2"
sub1.col$instar <- "Sub 1"

all.col <- rbind(adults.col, sub2.col, sub1.col)
diff.mod <- lm(diff.resids ~ instar + Treatment+instar:Treatment, data = all.col)
anova(diff.mod)

diff.mod.emm <- emmeans(diff.mod, "instar", data = all.col)
pairs(diff.mod.emm, adjust = "tukey")

anova(lm(diff.resids ~ instar + Treatment, data = all.col)) # p=0.06
summary(lm(diff.resids ~ instar + Treatment, data = all.col))

all.col$instar <- factor(all.col$instar, order = TRUE, levels = c("Sub 1", "Sub 2", "Adult"))


my_pal <- RColorBrewer::brewer.pal(n=3, name = "Dark2")
scales::show_col(viridis_pal(option = "C")(4))
my_pal <- viridis(option = "C", n = 4)
my_pal <- my_pal[c(4,3,2,1)]

diff.plot <- ggplot(data = all.col, aes(x = instar, y = diff.resids, fill = Treatment,                          fill = instar))+
  geom_boxplot(alpha = 0.7)+
  # geom_jitter(aes(group = Treatment))+
  geom_hline(yintercept = 0, color = 'red')+
  theme_classic()+
  scale_fill_manual(values = c("black", "darkgrey"))+
  # scale_fill_manual(values = c(paste(my_pal[2:4])))+
  theme(text = element_text(size=20))+
  ylab(expression(paste(Delta~"BCI")))+
  xlab("Instar")+
  labs(fill = "Treatment")

ggsave(diff.plot, filename = "../figures/diff_plot.jpeg", dpi = "retina", width = 10, height = 5.5, units = "in")

anova(lm(diff.resids ~ Treatment, data = all.col)) # p=0.06

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


ggplot(df_new, aes(x = Treatment, y = resids, color = Measurement, fill = instar))+
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

parasite <- df_new[c(which(df_all$Treatment == "Parasite")),]
x <- lmer(resids ~ Measurement + (1|Nest), parasite)
Anova(x)
plot(resids ~ Measurement, parasite,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

Fig4b <- ggplot(parasite, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

df.after <- df_new[c(which(df_new$Measurement == "after")),]


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
write.csv(col.counts, "../data/colony_counts_CLEANED.csv")

View(col.counts)
col.counts$num.all <- col.counts$num.adult + col.counts$num.sub2 + col.counts$num.sub1
num.after <- col.counts[c(which(col.counts$Measurement == "After")),]

num.after[,7] <- NULL


x <- glmer(num.all ~ Treatment + Measurement + Treatment*Measurement + (1|Colony), 
           family = poisson(link = "identity"), data = col.counts)
summary(x)
anova(x)
Anova(x)


y <- glm(num.all ~ Treatment, data = col.counts, family = poisson(link = "identity"))
Anova(y, test.statistic = "F")

mortality_plot <- ggplot(data = num.after, aes(x = Treatment, y = num.all, fill = Treatment))+
  geom_boxplot(alpha = 0.7)+
  ylab("Total number\n of spider remaining")+
  theme(text = element_text(size=20))+
  scale_fill_manual(values = c("black", "darkgrey"))+
  theme_cowplot()+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

plot_5.2 <- ggarrange(diff.plot, mortality_plot, nrow = 2, labels = "auto", common.legend = TRUE, legend = "right")
ggsave(plot_5.2, filename = "../figures/Plot_5.2.jpeg", dpi = "retina", width = 5, height = 7, units = 'in')

# create proportion columns
col.counts <- col.counts %>% 
  mutate(prop.adults = (num.adult/num.all),
         prop.sub2 = (num.sub2/num.all),
         prop.sub1 = (num.sub1/num.all)) %>% 
  mutate(prop.all = prop.adults + prop.sub2 + prop.sub1)

# need to re-order df
col.props <- col.counts[c(1,5,6,9:11)]

col.props.long <- col.props %>% pivot_longer(!c(Colony, Treatment, Measurement), names_to = "instar", values_to = "proportion")
col.props.long <- col.props.long %>% 
  mutate(instar = as.factor(instar))
levels(col.props.long$instar) <- c("Adult", "Sub2", "Sub1")
col.props.long$instar <- ordered(col.props.long$instar, levels = c("Sub1", "Sub2", "Adult"))





####### multinomial ####### 

# set baseline level
count.expanded$Treatment <- relevel(count.expanded$Treatment, ref = "Control")
count.expanded$Measurement <- relevel(count.expanded$Measurement, ref = "Before")

mod5 <- nnet::multinom(formula = instar ~ Treatment + Measurement + Treatment*Measurement,
                       data = count.expanded)
summary(mod5)
Anova(mod5)
coef(mod5)

#mixed effects multinomial logit model
library(mclogit)
mod6 <- mclogit(formula = cbind(count, instar) ~ Treatment + Measurement + Treatment*Measurement, random = ~1|Colony,
                data = counts.only.long)

mod6 <- mclogit(formula = cbind(count, instar) ~ Treatment, random = ~1|Colony,
                data = filter(counts.only.long, Measurement == "After"))

summary(mod6)
aov(mod6)

mod7 <- mclogit(formula = cbind(proportion, instar) ~ Treatment, random = ~1|Colony,
                data = filter(col.props.long, Measurement == "Before"))

summary(mod7)

col.props.long.after <- col.props.long %>% filter(Measurement == "After")
mean(col.props.long.after$proportion)
sd(col.props.long.after$proportion)

col.props.long.after %>% filter(Treatment == "Parasite") %>% summarize(mean(proportion))
col.props.long.after %>% filter(Treatment == "Control") %>% summarize(mean(proportion))
sd(col.props.long.after$proportion)

mod3 <- lm(proportion ~ instar + Measurement, 
           data = col.props.long)
summary(mod3)
anova(mod3)

mod4 <- glmer(instar ~ Treatment + Measurement + Treatment*Measurement + (1|Colony), 
           data = count.expanded, family = binomial)
summary(mod4)
anova(mod4)
Anova(mod4)

plot(instar ~ Treatment, data = filter(count.expanded, Measurement == "After"))

######## trying chi square #######

counts.only <- col.counts[,c(1:6)]

counts.only.long <- counts.only %>% pivot_longer(!c(Colony, Treatment, Measurement), names_to = "instar", values_to = "count")
counts.only.long <- counts.only.long %>% 
  mutate(instar = as.factor(instar))
levels(counts.only.long$instar) <- c("Adult", "Sub2", "Sub1")
counts.only.long$instar <- ordered(counts.only.long$instar, levels = c("Sub1", "Sub2", "Adult"))


counts.only.long %>% group_by(Colony, instar) %>% summarise(count, Measurement, Treatment)


df <- table(count.expanded)
table.3way <- ftable(count.expanded$Treatment, count.expanded$Measurement, count.expanded$instar)
mytable <- xtabs(~ Treatment+Measurement+instar, data=count.expanded)
mytable


mantelhaen.test(mytable)
mantelhaen.test(count.expanded$instar, count.expanded$Treatment, count.expanded$Measurement)
mantelhaen.test(count.expanded$instar, count.expanded$Treatment, count.expanded$Measurement)


mytable2 <- xtabs(~ Treatment+instar, data=filter(count.expanded, Measurement == "After"))
mod2 <- chisq.test(count.expanded$instar, count.expanded$Measurement)
mod2 <- fisher.test(mytable2)
mosaicplot(mytable2)

summary(mod2)
chisq.posthoc.test::chisq.posthoc.test(mytable2)
count.ex.after <- filter(count.expanded, Measurement == "After")
mod3 <- chisq.test(count.ex.after$instar, count.ex.after$Treatment)

mod4 <- mixtools::test.equality.mixed(x = count.ex.after$instar, y = count.ex.after$Colony, w = count.ex.after$Treatment)

mantelhaen.test(mytable)



# try with brms to add random effect
library(brms)
mod.brms <- brm(instar ~ Treatment + Measurement + Treatment*Measurement + (1|Colony), 
                data=count.expanded, family=cumulative("logit"))
summary(mod.brms)
loo(mod.brms)

mod.brms.fixed <- brm(instar ~ Treatment + Measurement + Treatment*Measurement, 
                data=count.expanded, family=cumulative("logit"))
summary(mod.brms.fixed)
loo(mod.brms, mod.brms.fixed)

install.packages("MCMCglmm")
library(MCMCglmm)

mod.mcmc <- MCMCglmm(instar ~ Treatment + Measurement + Treatment*Measurement, random = ~Colony, 
                data=count.expanded, family="ordinal")
######## ordinal ########

# ordinal logistic regression, no random


count.expanded <- counts.only.long[rep(seq(nrow(counts.only.long)), counts.only.long$count), 1:4]
library(MASS)
mod <- polr(instar ~ Treatment + Measurement + Measurement*Treatment, 
            data = count.expanded, Hess = TRUE)
summary(mod)
Anova(mod)

mod2 <- polr(instar ~ Treatment, 
             data = filter(count.expanded, Measurement == 'After'), Hess = TRUE)
summary(mod2)
Anova(mod2)


# try with ordinal package to include random effect - cumulative link mixed model (clmm)
library(ordinal)

count.expanded$Treatment <- as.factor(count.expanded$Treatment)
count.expanded$Measurement <- as.factor(count.expanded$Measurement)
count.expanded$Colony <- as.factor(count.expanded$Colony)

ord.null <- clmm(instar ~ 1 + (1|Colony), data = count.expanded, 
                 Hess = TRUE)
ord.mod1 <- clmm(instar ~ Treatment + (1|Colony), data = count.expanded, 
                 Hess = TRUE)
ord.mod2 <- clmm(instar ~ Treatment + Measurement + (1|Colony), data = count.expanded, 
                 Hess = TRUE)
ord.mod3 <- clmm(instar ~ Treatment*Measurement + (1|Colony), data = count.expanded, 
                 Hess = TRUE)
ord.mod3.2 <- clm(instar ~ Treatment*Measurement, data = count.expanded, 
                 Hess = TRUE)
RVAideMemoire::Anova.clmm(ord.mod3,type="II")
summary(ord.mod3)
anova(ord.mod3, ord.mod3.2)
summary(ord.mod1)$coefficients
AIC(ord.mod3,ord.mod3.2)


ord.mod2 <- clmm(instar ~ Treatment + (1|Colony), data = filter(count.expanded, Measurement == "After"))
summary(ord.mod2)
anova(ord.mod2)
RVAideMemoire::Anova.clmm(ord.mod2,type="II")



install.packages("buildmer")
ord.mod4 <- buildmer::buildclmm(instar ~ Treatment*Measurement + (1|Colony), data = count.expanded)
summary(ord.mod4)
anova(ord.mod4)

x.2 <- glm(num.adult ~ Treatment, data =num.after, family = poisson(link = "identity"))
AIC(x, x.2)
Anova(x.2)
plot(num.adult ~ Treatment, num.after, ylab = "Number Adults", cex.axis = 1.5, cex.lab = 1.5)
effsize::cohen.d(num.after$num.adult, num.after$Treatment)
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
effsize::cohen.d(num.after$num.sub1, num.after$Treatment)

w <- glm(num.all ~ Treatment, data = num.after, family = poisson(link = "identity"))
Anova(w)
plot(num.all ~ Treatment, num.after, ylab = "Total Number", cex.axis = 1.5, cex.lab = 1.5)
effsize::cohen.d(num.after$num.all, num.after$Treatment)

# make num.after long format
num.long <- num.after %>%
  pivot_longer(!c(Colony, Treatment, Measurement), names_to = "instar", values_to = "count")

num.long <- num.long %>% mutate(instar = recode_factor(instar, num.sub1  = "Sub 1", num.sub2 = "Sub 2", num.adult = "Adult"))
num.long$instar <- factor(num.long$instar, order = TRUE, levels = c("Sub 1", "Sub 2", "Adult"))

ggplot(data = filter(num.long, instar != "num.all", Treatment == "Control"), aes(x = count, group = instar, fill = instar)) + 
  geom_density(alpha = 0.5)+
  xlab("Count")+
  theme_cowplot()

ggplot(data = filter(num.long, instar != "num.all", Treatment == "Parasite"), aes(x = count, group = instar, fill = instar)) + 
  geom_density(alpha = 0.5)+
  xlab("Count")+
  theme_cowplot()

hist(num.after$num.adult)
hist(num.after$num.sub1)
hist(num.after$num.sub2)

num.long <-  num.long %>% filter(instar != "num.all")

instar.count <- glm(count ~ instar + Treatment + instar:Treatment, data = num.long, family = poisson(link = "identity"))
Anova(instar.count)
summary(instar.count)

mod.av <- aov(instar.count)
TUKEY <- TukeyHSD(x=mod.av)
sig.letters <- TUKEY$instar[order(row.names(TUKEY$instar)), ]

value_max = num.long %>% group_by(instar) %>% summarize(max_value = max(count))

my_pal <- viridis::viridis(option = "C", n = 4)
my_pal <- my_pal[c(4,3,2,1)]

final.count <- ggplot(data = filter(num.long, instar != "num.all"), aes(x = instar, y = count, fill = instar, color = Treatment)) + 
  geom_boxplot(alpha = 0.7)+
  # geom_text(data = value_max, aes(x = instar, y = max_value + 1, label = c("a", "ab", "b")) ,vjust=0)+
  xlab("Instar")+
  scale_color_manual(values = c("black", "grey"))+
  scale_fill_manual(values = paste(my_pal[2:4]))+
  theme_cowplot()+
  labs(fill = "Instar", y = "Final count")
  # stat_summary(fun=mean, geom="point", size=2, color="red")
  # theme(legend.position = "none")

ggsave(final.count, filename = "figures/count_plot.jpeg", dpi = "retina", width = 7.5, height = 4.5, units = "in")


### count diff
View(col.counts)
col.counts[,7] <- NULL

counts.wide <- col.counts %>% 
  pivot_wider(values_from = c(num.adult, num.sub2, num.sub1), names_from = c(Measurement))
# i could not get this to work

after <- col.counts %>% filter(Measurement == "After")
after <- after[,1:5]
after <- after %>% 
  rename(after.adult = num.adult,
         after.sub2 = num.sub2,
         after.sub1 = num.sub1)

before <- col.counts %>% filter(Measurement == "Before")
before <- before[,1:5]
before <- before %>% 
  rename(before.adult = num.adult,
         before.sub2 = num.sub2,
         before.sub1 = num.sub1)

counts.wide <- full_join(after, before)
counts.wide <- counts.wide %>% 
  mutate(diff.adult = after.adult - before.adult,
         diff.sub2 = after.sub2 - before.sub2,
         diff.sub1 = after.sub1 - before.sub1)

counts.wide <- counts.wide %>% 
  rename(Adult = diff.adult,
         Sub2 = diff.sub2, 
         Sub1 = diff.sub1)

counts.long <- counts.wide %>%
  select(Colony, Treatment, Adult, Sub2, Sub1) %>% 
  pivot_longer(!c(Colony, Treatment), names_to = "instar", values_to = "count.diff")

anova(lm(count.diff ~ instar + Treatment+instar:Treatment, data = counts.long)) # p=0.06
anova(lm(count.diff ~ instar + Treatment, data = counts.long)) # p=0.06
summary(lm(count.diff ~ instar + Treatment+ Treatment+instar:Treatment, data = counts.long))

counts.long$instar <- factor(counts.long$instar, order = TRUE, levels = c("Sub1", "Sub2", "Adult"))

my_pal <- viridis::viridis(option = "C", n = 4)
my_pal <- my_pal[c(4,3,2,1)]

diff.count <- ggplot(data = counts.long, aes(x = instar, y = count.diff, fill = instar, 
                                             color = Treatment)) + 
  geom_boxplot(alpha = 0.7)+
  # geom_text(data = value_max, aes(x = instar, y = max_value + 1, label = c("a", "ab", "b")) ,vjust=0)+
  xlab("Instar")+
  scale_color_manual(values = c("black", "grey"))+
  scale_fill_manual(values = paste(my_pal[2:4]))+
  theme_cowplot()+
  labs(fill = "Instar", y = "Final count")
# stat_summary(fun=mean, geom="point", size=2, color="red")
# theme(legend.position = "none")
# compare total number in after with instar specific body condition


# diff by instar
num.adult <- counts.long %>% filter(instar == "Adult")
anova(lm(count.diff ~ Treatment, data = num.adult)) #0.7121

num.sub2 <- counts.long %>% filter(instar == "Sub2")
anova(lm(count.diff ~ Treatment, data = num.sub2)) #0.08

num.sub1 <- counts.long %>% filter(instar == "Sub1")
anova(lm(count.diff ~ Treatment, data = num.sub1)) #0.229

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



