####### Load Libraries #########
library(car)
library(effsize)
library(lme4)

library(ggplot2)
library(stats) #glms
library(ggthemes)
library(scales)
library(tidyr)
library(dplyr)

#### ADULTS body condition ##########
df_all <- read.csv("eximius_body_measurements.csv")
levels(df_all$Measurement)
df_all$Measurement <- factor(df_all$Measurement, levels= c("before", "after"))

adults <- df_all[-c(which(df_all$instar != "Adult")),]
adults$weight.mg <- adults$weight.grams * 1000

hist(adults$weight.grams)
hist(sqrt(adults$weight.mg))
hist(log10(adults$weight.grams))

z <- lm(log10(weight.mg) ~ Measurement, data = adults)
summary(z)
anova(z)
plot(z)
plot(weight.mg ~ Measurement, data = adults)

# adults.before <- adults[c(which(adults$Measurement == "before")),]

y <- lm(log(weight.mg) ~ log(CT.W), data = adults)
Anova(y)
summary(y)
plot(y)
plot(log(weight.mg) ~ log(CT.W), data = adults)
abline(y)
adults$resids <- residuals(y)

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

adults.after <- adults[c(which(adults$Measurement == "after")),]


y2 <- lmer(resids ~ Treatment + (1|Nest), data = adults.after)
summary(y2)
Anova(y2)
plot(resids ~ Treatment, data = adults.after, ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

y3 <- lmer(resids ~ Treatment + Measurement + Treatment:Measurement + (1|Nest), data = adults)
summary(y3)
Anova(y3)

# diff in variance
leveneTest(y2, center = mean) #non3

# y3 <- lm(log10(weight.mg) ~ Treatment, data = adults.after)
# summary(y3)
# plot(weight.mg ~ Treatment, data = adults.after)
# 
# x <- lm(weight.mg ~ PT.L, data = adults)
# summary(x)
# plot(weight.mg ~ PT.L, data = adults)
# abline(x)


control <- adults[c(which(adults$Treatment == "Control")),]
x <- lmer(resids ~ Measurement + (1|Nest), control)
Anova(x)
plot(resids ~ Measurement, control,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

Fig1a <- ggplot(control, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

parasite <- adults[c(which(adults$Treatment == "Parasite")),]
x <- lmer(resids ~ Measurement + (1|Nest), parasite)
Anova(x)
plot(resids ~ Measurement, parasite,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

Fig1b <- ggplot(parasite, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")


y2 <- lmer(resids ~ Treatment + (1|Nest), data = adults.after)
Anova(y2)

Fig1c <- ggplot(adults.after, aes(x = Treatment, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

multiplot(Fig1a, Fig1b, Fig1c, cols = 3)

####### Sub 2 body condition ########
levels(df_all$instar)
#df_all <- read.csv("eximius_body_measurements.csv")
sub2 <- df_all[-c(which(df_all$instar != "Sub 2")),]
sub2$weight.mg <- sub2$weight.grams * 1000

hist(sub2$weight.grams)
hist(sqrt(sub2$weight.mg))
hist(log10(sub2$weight.grams)) #best

z <- lm(log10(weight.mg) ~ Measurement, data = sub2)
summary(z)
anova(z)
plot(z)
plot(weight.mg ~ Measurement, data = sub2)
## overall, sub2 weights greater after than before -- growth

y <- lm(log(weight.mg) ~ log(CT.W), data = sub2)
Anova(y)
summary(y)
plot(log(weight.mg) ~ log(CT.W), data = sub2)
abline(y)
sub2$resids <- residuals(y)


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

sub2.after <- sub2[c(which(sub2$Measurement == "after")),]

y3 <- lmer(resids ~ Treatment + Measurement + Treatment:Measurement + (1|Nest), data = sub2)
summary(y3)
Anova(y3)

y2 <- lmer(resids ~ Treatment + (1|Nest), data = sub2.after)
Anova(y2)
summary(y2)
plot(resids ~ Treatment, data = sub2.after, ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)

#compare variance
leveneTest(y2)

# y3 <- lm(weight.mg ~ Treatment, data = sub2.after)
# summary(y3)
# plot(weight.mg ~ Treatment, data = sub2.after)

# x <- lm(weight.mg ~ PT.L, data = sub2)
# summary(x)
# plot(weight.mg ~ PT.L, data = sub2)
# abline(x)
# 
# v <- lm(weight.mg ~ Treatment, data = sub2)
# summary(z)
# plot(weight.mg ~ Treatment, data = sub2)

control <- sub2[c(which(sub2$Treatment == "Control")),]
x <- lmer(resids ~ Measurement + (1|Nest), control)
Anova(x)
plot(resids ~ Measurement, control,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
## in control, body condition improved for sub2s -- growth

Fig2a <- ggplot(control, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")


parasite <- sub2[c(which(sub2$Treatment == "Parasite")),]
x <- lmer(resids ~ Measurement + (1|Nest), parasite)
Anova(x)
plot(resids ~ Measurement, parasite,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
## in experimental treatment, body condition did not improve -- no growth -- slower time to moult?
Fig2b <- ggplot(parasite, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

Fig2c <- ggplot(sub2.after, aes(x = Treatment, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

multiplot(Fig2a, Fig2b, Fig2c, cols = 3)
###### Sub 1 body condition #########
setwd("/Volumes/straus/RProjects/FitnessEffects")
sub1wts <- read.csv("sub1_weights.csv")
sub1wts$avg.wt <- (sub1wts$Weight / sub1wts$num.sub1)
sub1wts.before <- sub1wts[c(which(sub1wts$Measurement == "before")),]
sub1wts.after <- sub1wts[c(which(sub1wts$Measurement == "after")),]

sub1 <- df_all[-c(which(df_all$instar != "Sub 1")),]
sub1.before <- sub1[c(which(sub1$Measurement == "before")),]
sub1.after <- sub1[c(which(sub1$Measurement == "after")),]

# average weight to average ct.w

# sub1wts.before$avg.CT.W <- (tapply(sub1.before$CT.W, sub1.before$Nest, mean))
# sub1wts.after$avg.CT.W <- (tapply(sub1.after$CT.W, sub1.after$Nest, mean))
# sub1.final <- rbind(sub1wts.after, sub1wts.before)
# 
# 
# y <- lm(log(avg.wt) ~ log(avg.CT.W), data = sub1.final)
# summary(y)
# plot(log(avg.wt) ~ log(avg.CT.W), data = sub1.final)
# abline(y)
# sub1.final$resids <- residuals(y)
# 
# sub1.after <- sub1.final[c(which(sub1.final$Measurement == "after")),]
# y2 <- lm(resids ~ Treatment, data = sub1.after)
# summary(y2)
# plot(resids ~ Treatment, data = sub1.after)
# 
# #compare variance
# leveneTest(y2)
# 
# control <- sub1.final[c(which(sub1.final$Treatment == "Control")),]
# anova(lm(resids ~ Measurement, control))
# plot(resids ~ Measurement, control)
# ## in control, body condition improved for sub1s -- growth
# 
# parasite <- sub1.final[c(which(sub1.final$Treatment == "Parasite")),]
# anova(lm(resids ~ Measurement, parasite))
# plot(resids ~ Measurement, parasite)
# ## in experimental treatment, body condition did not improve -- no growth -- slower time to moult?
# 
# plot(resids ~ Measurement, parasite)
# leveneTest(lm(resids ~ Measurement, parasite)) #var not diff

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
# sub1.final$weight.mg <- sub1.final$weight.grams * 1000
write.csv(sub1.final, "Sub1wts.csv")

y <- lm(log(weight.mg) ~ log(CT.W), data = sub1.final)
Anova(y)
summary(y)
plot(log(weight.grams) ~ log(CT.W), data = sub1.final)
abline(y)
sub1.final$resids <- residuals(y)

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

sub1.after <- sub1.final[c(which(sub1.final$Measurement == "after")),]
y2 <- lmer(resids ~ Treatment + (1|Nest), data = sub1.after)
Anova(y2)

y3 <- lmer(resids ~ Treatment + Measurement + Treatment:Measurement + (1|Nest), data = sub1.final)
Anova(y3)
summary(y3)

Anova(y2)
Anova(lm(resids ~ Treatment, sub1.after))
summary(y2)
plot(resids ~ Treatment, data = sub1.after, ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
# SIGNIFICANTLY DIFFERENT!!! ... but not when nest is a random effect...

control <- sub1.final[c(which(sub1.final$Treatment == "Control")),]
x <- lmer(resids ~ Measurement + (1|Nest), control)
Anova(x)
plot(resids ~ Measurement, control,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
## in control, body condition improved for sub1s -- growth
Fig3a <- ggplot(control, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

parasite <- sub1.final[c(which(sub1.final$Treatment == "Parasite")),]
x <- lmer(resids ~ Measurement + (1|Nest), parasite)
Anova(x)
## in experimental treatment, also better, also growth
plot(resids ~ Measurement, parasite,
     ylab = "Body Condition Index", cex.axis = 1.5, cex.lab = 1.5)
Fig3b <- ggplot(parasite, aes(x = Measurement, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")

Fig3c <- ggplot(sub1.after, aes(x = Treatment, y = resids))+
  geom_boxplot()+
  theme_classic()+
  theme(text = element_text(size=20))+
  ylab("Body Condition Residuals")


multiplot(Fig3a, Fig3b, Fig3c, cols = 3)
## effect sizes?? 
library(effsize)
cohen.d(sub1.final$resids, sub1.final$Treatment)

########### ALL body condition ##########
df_all <- read.csv("eximius_body_measurements.csv")
levels(df_all$Measurement)
df_all$Measurement <- factor(df_all$Measurement, levels= c("before", "after"))
df_all$weight.grams <- df_all$weight.grams * 1000

df_all <- df_all[-c(which(df_all$instar == "Sub 1")),]
sub1.final <- sub1.final[,-11]
df_all <- rbind(df_all, sub1.final)

# df_all$weight.mg <- df_all$weight.grams *1000

y <- lm(log(weight.mg) ~ log(CT.W), data = df_all)
Anova(y)
summary(y)
plot(y)
plot(log(weight.grams) ~ log(CT.W), data = df_all)
abline(y)
df_all$resids <- residuals(y)

ggplot(data=df_all, aes(x=CT.W, y = weight.mg))+
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
############ Mortality ##########
col.counts <- read.csv("colony_counts.csv")
View(col.counts)
col.counts$num.all <- col.counts$num.adult + col.counts$num.sub2 + col.counts$num.sub1
num.after <- col.counts[c(which(col.counts$Measurement == "After")),]

x <- glm(num.adult ~ Treatment, family = poisson, num.after)
Anova(x)
plot(num.adult ~ Treatment, num.after, ylab = "Number survivors", cex.axis = 1.5, cex.lab = 1.5)
# diff in variance
leveneTest(x, center = mean) #YES sig difference in variance

y <- glm(num.sub2 ~ Treatment, family= poisson, data = num.after)
Anova(y)
plot(num.sub2 ~ Treatment, num.after, ylab = "Number survivors", cex.axis = 1.5, cex.lab = 1.5)  #...higher?
leveneTest(y, center = mean)

z <- glm(num.sub1 ~ Treatment, family = poisson, num.after)
Anova(z)
plot(num.sub1 ~ Treatment, num.after, ylab = "Number survivors", cex.axis = 1.5, cex.lab = 1.5)
leveneTest(z, center = mean)

w <- glm(num.all ~ Treatment, family = poisson, num.after)
Anova(w)
plot(num.all ~ Treatment, num.after, ylab = "Number survivors", cex.axis = 1.5, cex.lab = 1.5)
leveneTest(w, center = mean)

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

######## Prey Capture #########

# need to bring in nest sizes to calc col size
nestMeas <- read.csv("2018_nestMeas.csv")
nestMeas <- nestMeas[,-11]

ex <- nestMeas[grep("Ex_", nestMeas$Nest),]
iy <- nestMeas[grep("Iy_", nestMeas$Nest),]

red <- rbind(ex, iy)
  
# area of ellipse : A = pi * a * b, where a=length/2, b=width/2
red$basket.cs <- pi * (red$BaskLen/2) * (red$BaskWid/2)
# Purcell & Aviles 2007 supp info: JS log # fem = -2.762 + 1.523(log CS), R^2 = 0.9
red$log.fem <- -2.762 + (1.523*log10(red$basket.cs))
red$num.fem <- 10^red$log.fem

# prey capture df
preycap_raw <- read.csv('2018_preycapture.csv') #remove notes

#isolate hosts
host.prey <- preycap_raw[c(which(preycap_raw$Code == "H")),]
#isolate column of interest
host.prey <- host.prey[,-c(7:9)]
host.prey <- host.prey[-c(which(is.na(host.prey$Interval) == TRUE)),]

host.prey$log.biomass <- rep(0, times = length(host.prey$Date))
host.prey$log.mm <- log10(host.prey$Size)

# for(i in 1:length(host.prey$Order)){
#   if(host.prey$Order[i] == "Coleoptera") {
#     host.prey$log.biomass[i] <- -0.611 + (1.141 * host.prey$log.mm[i])
#   }
# }

rows3 <- which(host.prey$Order == 'Hymenoptera') #-0.611	1.141
Hymenoptera <- host.prey[rows3,]
Hymenoptera$log.biomass <-  -0.611 + (1.141 * Hymenoptera$log.mm)

rows4 <- which(host.prey$Order =='Lepidoptera') #-3.169	3.624
Lepidoptera <- host.prey[rows4,]
Lepidoptera$log.biomass <- -3.169 + (3.624 * Lepidoptera$log.mm)

rows5 <- which(host.prey$Order == 'Coleoptera') #-1.960	2.966
Coleoptera <- host.prey[rows5,]
Coleoptera$log.biomass <- -1.960 + (2.966 * Coleoptera$log.mm)

rows6 <- which(host.prey$Order ==  'Orthoptera') #-1.928	2.680
Orthoptera <- host.prey[rows6,]
Orthoptera$log.biomass <-  -1.928 + (2.680 * Orthoptera$log.mm)

rows7 <- which(host.prey$Order == 'Hemiptera') #-1.749	2.529
Hemiptera <- host.prey[rows7,]
Hemiptera$log.biomass <- -1.749 + (2.529 * Hemiptera$log.mm)

rows8 <- which(host.prey$Order =='Diptera') #-1.361	2.026
Diptera <- host.prey[rows8,]
Diptera$log.biomass <- -1.361 + (2.026 * Diptera$log.mm)

biomassRaw <- rbind(Hymenoptera, Lepidoptera, Coleoptera, Orthoptera, Hemiptera, Diptera)
biomassRaw$biomass <- 10^biomassRaw$log.biomass

#sep treatments
control <- biomassRaw[which(biomassRaw$Treatment == "Control"),]

count <- tapply(control$Size, control$Nest, length)
tot.biomass <- tapply(control$biomass, control$Nest, sum)
Nest <- levels(control$Nest)
avg.size <- tapply(control$Size, control$Nest, mean)
df1 <-data.frame(cbind(Nest, count, tot.biomass, avg.size))

df1.fin <- merge(df1, red, by.x = 'Nest', by.y = 'Nest', all = F)
df1.fin <- df1.fin[,-c(5:14)]
df1.fin$treatment <- rep("Control", times = length(df1.fin$Nest))
rows <- grep("Ex_", df1.fin$Nest)
df1.fin$hours <- rep(0, times = length(df1.fin$Nest))
df1.fin$hours[rows] <- 18
rows <- grep("Iy_", df1.fin$Nest)
df1.fin$hours[rows] <- 16

experimental <- biomassRaw[which(biomassRaw$Treatment == "Experimental"),]

count <- tapply(experimental$Size, experimental$Nest, length)
tot.biomass <- tapply(experimental$biomass, experimental$Nest, sum)
Nest <- levels(experimental$Nest)
avg.size <- tapply(experimental$Size, experimental$Nest, mean)
df2 <-data.frame(cbind(Nest, count, tot.biomass, avg.size))

df2.fin <- merge(df2, red, by.x = 'Nest', by.y = 'Nest', all = F)
df2.fin <- df2.fin[,-c(5:14)]
df2.fin$treatment <- rep("Experimental", times = length(df2.fin$Nest))
rows <- grep("Ex_", df2.fin$Nest)
df2.fin$hours <- rep(0, times = length(df2.fin$Nest))
df2.fin$hours[rows] <- 16
# except Ex_11 only had 14
which(df2.fin$Nest == "Ex_11") #11
df2.fin$hours[11] <- 14
rows <- grep("Iy_", df2.fin$Nest)
df2.fin$hours[rows] <- 16

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

df.fin <- rbind(df1.fin, df2.fin)
df.fin$count <- as.integer(df.fin$count)
df.fin$treatment <- as.factor(df.fin$treatment)
df.fin$avg.size <- as.numeric.factor(df.fin$avg.size)
df.fin$tot.biomass <- as.numeric.factor(df.fin$tot.biomass)

rows <- which(is.na(df.fin$count==T))
df.fin$count[rows] <- 0
df.fin <- df.fin[-rows,]
rows <- which(is.na(df.fin$tot.biomass==T))
df.fin$tot.biomass[rows] <- 0

df.fin$num.fem <- 10^df.fin$log.fem
df.fin$rate <- df.fin$tot.biomass / df.fin$hours / df.fin$num.fem
## put together
x <- lm(log10(rate) ~ log.fem + I(log.fem^2) + treatment, data = df.fin)
Anova(x, type = "III")
plot(log10(avg.size)~log.fem, df.fin)

x <- lm(log10(avg.size) ~ treatment, data = df.fin)
Anova(x, type = "III")
summary(x)
plot((count/hours/num.fem)~treatment, df.fin)
tapply(df.fin$avg.size, df.fin$treatment, mean)

## count
p <- lm(log10(count/hours/num.fem) ~ log10(num.fem) + treatment, data = df.fin)
Anova(p, type = "III") 
p <- lm(log10(avg.size) ~ treatment, data = df.fin)
Anova(p, type = "III") 
p <- lm(log10(rate) ~ log10(num.fem) + treatment, data = df.fin)
Anova(p, type = "III") 

# df.fin <- df.fin[-which(df.fin$Nest=="Ex_02"),]
# df.fin <- df.fin[-which(df.fin$Nest=="Ex_03"),]

ggplot(df.fin, aes(x =num.fem, y = (rate), colour = factor(treatment)))+
  geom_point(size=4) + 
  stat_smooth(method="lm", se=F, size = 1.5) +
  scale_linetype_manual(values = c("solid", "dashed"))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))+
  labs(x = "number of spiders", 
       y = "Dry mg per h per fem (Log10)",
       colour = "Treatment") +
  theme_base()+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20),
        legend.text=element_text(size=16),
        legend.title = element_text(size=20))+
  theme(legend.key.width = unit(3, 'line'))


