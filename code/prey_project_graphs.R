library(ggplot2)
library(scales)
library(colortools)

wheel("lightskyblue", num = 12)
tetradic("lightskyblue")
complementary("darkolivegreen3")
splitComp("darkolivegreen3")
adjacent("darkolivegreen3")
adjacent("#BE5ACD")
adjacent("#5A69CD")
wheel("yellowgreen", num = 12)
adjacent("#FAB387")
sequential("#FAB387")

nest_stats <- read.csv("nest_statistics.csv", row.names = 1)
prey_capture_perhr <- read.csv("prey_capture_perhr.csv", row.names = 1)

#Tangle volume vs spider density
nest_stats$Type <- as.factor(nest_stats$Type)
nest_stats$Type <- relevel(nest_stats$Type, "Host")
ggplot(nest_stats, aes(TangleVol, LogSpidDens, colour = Type, shape = Type)) + geom_point(size = 2.5) + 
  geom_smooth(method = lm, size = 1, se = FALSE) +
  theme_classic(base_size = 17) + 
  scale_color_manual(values = c("#87CEFA", "#FAB387", "#FAEC87")) + 
  labs(x = "Tangle Volume", y = "Spider Density \n (log10)") +
  theme(aspect.ratio = 1) +
  theme(aspect.ratio = 1, axis.text = element_text(colour = "black", size = 9), axis.title = element_text(size = 12)) +
  scale_x_log10(label = comma) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))
 
  
lm_density <- lm(LogSpidDens ~ LogTangleVol + Type + LogTangleVol:Type, data = nest_stats)
summary_dens<- summary(lm_density, level = 0.95)
confidence_dens <- confint(lm_density)
anova_dens <- anova(lm_density)

# avg prey size vs CS (log prey size)
which(colnames(nest_stats) == "AvgPreySze_NoOut")
which(rownames(nest_host) == "Ex_07")
nest_stats[20, 25] <- NA
ggplot(nest_stats, aes(NumSpiders, AvgPreySze, colour = Treatment)) + geom_point(size = 2.5) + 
  geom_smooth(method = lm, formula = y ~ x + I(x^2), size = 1, se = FALSE) +
  theme_classic(base_size = 17) + 
  xlab ("Number of Spiders") + 
  ylab(expression(paste("Mean Prey Size \n (log10, mm)"))) +
  theme(aspect.ratio = 1, axis.text = element_text(colour = "black", size = 11), axis.title = element_text(size = 12)) +
  scale_y_log10() +
  scale_x_log10(labels = comma) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm")) 
nest_host <- subset(nest_stats, Type == "Host")
min(nest_host$AvgPreySze, na.rm = TRUE)


hist(nest_stats$AvgPreySze[nest_stats$Type=="Host"])
hist(nest_stats$AvgPreySze[nest_stats$Type=="ActPar"], breaks = 12)
hist(nest_stats$AvgPreySze[nest_stats$Type=="PassPar"], breaks = 12)

#Log num prey per hr per capita
ggplot(subset(nest_stats, LogNumPrey_Hr_Capita != "-Inf"), aes(NumSpiders, LogNumPrey_Hr_Capita, colour = Treatment) + geom_point(size = 2)) + 
  geom_smooth(method = lm, size = 1, se = FALSE) +
  theme_classic(base_size = 17) + 
  xlab ("Number of Spiders") + 
  ylab(expression(paste("Number of Prey/ Hr/ Capita"))) +
  theme(aspect.ratio = 1, axis.text = element_text(colour = "black", size = 11), axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 11)) +
  scale_x_log10(labels = comma) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

#biomass per hr per capita !!!!PHIL PLEASE HELP!!!!
ggplot(nest_stats, aes(NumSpiders, LogBiomass_hr_capita, colour = Treatment)) + 
  geom_point(size = 2) + 
  geom_smooth(method = lm, size = 1, se = FALSE, formula = y ~ x) +
  theme_classic(base_size = 17) + 
  xlab ("Number of Spiders") + 
  ylab(("Biomass (mg)")) +
  theme(aspect.ratio = 1, axis.text = element_text(colour = "black", size = 12), axis.title = element_text(size = 12)) +
  scale_x_log10(labels = comma) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))


#proportion biomass graph
ggplot(subset(nest_stats, Type!="Host"), aes(LogTangleVol, LogPropBiom, colour = Type, shape = Type)) + 
  geom_point(size = 2) + 
  geom_smooth(method = lm, size = 1, se = FALSE) +
  theme_classic(base_size = 17) + 
  scale_color_hue(l=40, c=35)

#bar graphs
ggplot(nest_stats, aes(x = quintile, y = TLbiom_perspid, fill = Type)) + 
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 17) + 
  labs(x = "Colony Size", y = "Total Biomass") +
  scale_fill_manual(values=c("lightgoldenrod4", "darkseagreen4", "lightsteelblue3"))

ggplot(subset(nest_stats, Type!="Host"), aes(x = quintile, y = TLbiom_perspid, fill = Type)) + 
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 17) + 
  labs(x = "Colony Size", y = "Total Biomass") +
  scale_fill_manual(values=c("darkseagreen4", "lightsteelblue3"))

# percent prey larger than caught by parasites !!!! PHIL PLEASE HELP!!! 
nest_stats_ActPar <- subset(nest_stats, Type == "ActPar")
nest_stats_PassPar <- subset(nest_stats, Type == "PassPar")
nest_stats_host <- subset(nest_stats, Type == "Host")
max(nest_stats_ActPar$Biomass_mg, na.rm = TRUE)
max(nest_stats_PassPar$Biomass_mg, na.rm = TRUE)


for (i in 1:length(nest_stats_host$Biomass_mg)){
 if (nest_stats_host$Biomass_mg[i] > 1.89178 & !is.na(nest_stats_host$Biomass_mg[i])){
    nest_stats_host$percent[i] <- 1
  } else if (nest_stats_host$Biomass_mg[i] <= 1.89178 & !is.na(nest_stats_host$Biomass_mg[i])){
    nest_stats_host$percent[i] <- 2
  } else {
    nest_stats_host$percent[i] <- NA
  }
}

as.factor(nest_stats_host$percent)

ggplot(subset(nest_stats_host, !is.na(Biomass_mg)), aes(x = quintile, y = Biomass_mg, fill = percent)) + 
  geom_bar(stat = "identity") + 
  theme_classic(base_size = 17) +
  scale_fill_manual(values=c("darkseagreen4", "lightsteelblue3"))

write.csv(nest_stats, "stats_for_graphs.csv")
