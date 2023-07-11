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
library(emmeans)

all.col <- read.csv("data/BCI_differences.csv")
all.col$instar <- factor(all.col$instar, order = TRUE, levels = c("Sub 1", "Sub 2", "Adult"))

# BCI Difference -------------
diff.mod <- lmer(diff.smi ~ instar + Treatment + Treatment:instar + (1|Nest), data = all.col)
Anova(diff.mod)
summary(diff.mod)

diff.mod.emm <- emmeans(diff.mod, c("instar", "Treatment"))
pairs(diff.mod.emm, adjust = "tukey")


diff.plot <- ggplot(data = all.col, aes(x = instar, y = diff.smi, fill = Treatment))+
  geom_boxplot(alpha = 0.7)+
  # geom_jitter(aes(group = Treatment))+
  geom_hline(yintercept = 0, color = 'red')+
  theme_classic()+
  scale_fill_manual(values = c("black", "darkgrey"))+
  # scale_fill_manual(values = c(paste(my_pal[2:4])))+
  theme(text = element_text(size=20))+
  ylab(expression(paste(Delta~"SMI")))+
  xlab("Instar")+
  labs(fill = "Treatment")



# Body Condition: LMER -------------------
df_new <- read.csv("data/BCI_final.csv")

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



## CHECK
# were there differences between treatments at the start of the experiment?

df_before <- df_new %>% 
  filter(Measurement == "before")

df_before_ad <- df_new %>% 
  filter(instar == "Adult")

before.mod.ad <- lmer(SMI ~ Treatment + (1|Nest), data = df_before_ad)
Anova(before.mod.ad)

df_before_s2 <- df_new %>% 
  filter(instar == "Sub 2")

before.mod.s2 <- lmer(SMI ~ Treatment + (1|Nest), data = df_before_s2)
Anova(before.mod.s2)

df_before_s1 <- df_new %>% 
  filter(instar == "Sub 1")

before.mod.s1 <- lmer(SMI ~ Treatment + (1|Nest), data = df_before_s1)
Anova(before.mod.s1)

# mega model
full.mod <- lmer(SMI ~ Treatment + Measurement + Treatment:Measurement + (1|Nest) + (1|instar), data = df_new)
Anova(full.mod)
summary(full.mod)
coef(full.mod)


# Mortality ------------------
col.counts <- read.csv("data/colony_counts_RAW.csv")

# remove Col_21 : Note says "Escapees"
col.counts <- col.counts %>% filter(Colony != "Col_21")
write.csv(col.counts, "data/colony_counts_CLEANED.csv", row.names = FALSE)

col.counts$num.all <- col.counts$num.adult + col.counts$num.sub2 + col.counts$num.sub1 ## sum of all individuals

col.counts_long <- col.counts %>% 
  pivot_longer(cols = c("num.adult", "num.sub2", "num.sub1"), 
               names_to = "instar",
               values_to = "count")

x.1 <- glmer(num.all ~ Treatment + Measurement + Treatment:Measurement + (1|Colony), 
             family = poisson(link = "identity"), data = col.counts_long)
summary(x.1)
Anova(x.1)

## number remaining
num.after <- col.counts[c(which(col.counts$Measurement == "After")),] ### only consider after measurement

num.after[,7] <- NULL # remove unneccessary column

## pivot long

num.after_wide <- num.after %>% 
  pivot_longer(cols = c("num.adult", "num.sub2", "num.sub1"), 
              names_to = "instar",
              values_to = "count")

x.2 <- glmer(count ~ Treatment + (1|Colony) + (1|instar), 
           family = poisson(link = "identity"), data = num.after_wide)
summary(x.2)
Anova(x.2)


mortality_plot <- ggplot(data = num.after, aes(x = Treatment, y = num.all, fill = Treatment))+
  geom_boxplot(alpha = 0.7)+
  ylab("Total number\n of spider remaining")+
  theme(text = element_text(size=20))+
  scale_fill_manual(values = c("black", "darkgrey"))+
  theme_cowplot()+
  theme(plot.margin = margin(1,1,1.5,1.2, "cm"))

plot_5.2 <- ggarrange(diff.plot, mortality_plot, nrow = 2, labels = "auto", common.legend = TRUE, legend = "right")
# ggsave(plot_5.2, filename = "figures/Plot_5.2.jpeg", dpi = "retina", width = 5, height = 7, units = 'in')

jpeg(file="figures/Figure_2.jpeg", width = 5, height = 7, units = "in", res = 1000)
plot_5.2
dev.off()
