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

write.csv(s1.af.col, "data/sub1_bcis_after.csv")

# POOLED -----------------------

all.after.summary <- full_join(ad.af.col, s2.af.col, by = c("Nest", "Treatment"))
all.after.summary <- full_join(all.after.summary, s1.af.col, by = c("Nest", "Treatment"))

write.csv(all.after.summary, "data/all.after.summary.csv")

 
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

anova(lm(diff.resids ~ Treatment, data = all.col)) 



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
col.counts <- read.csv("data/colony_counts.csv")

# remove Col_21 : Note says "Escapees"
col.counts <- col.counts %>% filter(Colony != "Col_21")
write.csv(col.counts, "data/colony_counts_CLEANED.csv")

View(col.counts)
col.counts$num.all <- col.counts$num.adult + col.counts$num.sub2 + col.counts$num.sub1
num.after <- col.counts[c(which(col.counts$Measurement == "After")),]

num.after[,7] <- NULL


x <- glmer(num.all ~ Treatment + Measurement + Treatment*Measurement + (1|Colony), 
           family = poisson(link = "identity"), data = col.counts)
summary(x)
anova(x)
Anova(x)


mortality_plot <- ggplot(data = num.after, aes(x = Treatment, y = num.all, fill = Treatment))+
  geom_boxplot(alpha = 0.7)+
  ylab("Total number\n of spider remaining")+
  theme(text = element_text(size=20))+
  scale_fill_manual(values = c("black", "darkgrey"))+
  theme_cowplot()+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

plot_5.2 <- ggarrange(diff.plot, mortality_plot, nrow = 2, labels = "auto", common.legend = TRUE, legend = "right")
ggsave(plot_5.2, filename = "figures/Plot_5.2.jpeg", dpi = "retina", width = 5, height = 7, units = 'in')

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


