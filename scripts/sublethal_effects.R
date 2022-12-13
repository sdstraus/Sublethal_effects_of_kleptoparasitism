# Load Libraries ------------------
library(car)
library(lme4)
library(cowplot)
library(ggplot2)
library(stats)
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)
library(ggpubr)

all.col <- read.csv("data/BCI_cleaned_all.csv")

# ANALYSIS ---------------------

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
col.counts <- read.csv("data/colony_counts_RAW.csv")

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


