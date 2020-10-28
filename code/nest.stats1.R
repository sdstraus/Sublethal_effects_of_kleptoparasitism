####nest characteristics####
nest_measure <- read.csv("2018_nestMeas.csv")
library(dplyr)
library(tidyr)

#delete unused rows -- some data was just collected on nests for another project
nest_measure <- nest_measure[ !(nest_measure$Nest %in% c("ChichiPatch.1", "ChichiPatch.2", "ChichiPatch.3", "Ahuano5.1", "Ahuano.5.2", "Ahuano.5.3", "JardinBotanico", "Ahuano11.2.1", "Ahuano11.2.2", "ChichiPatch.4", "ChichiPatch.5", "JS-T11.6.1", "JS-T11.6.2", "JS-T11.6.3", "Archidona.1", "Archidona.2", "Archidona.3", "Ex_08", "Iyarina_01_OLD", "Iyarina__02_OLD", "Iyarina_03_OLD", "Iyarina_04_OLD", "Iy_09", "Iy_10")),]
#deleted the initial nest measurement for ex_11 because the nest was in very rough shape until they rebuil it and most of it was unused
nest_measure <- nest_measure[ !(nest_measure$Nest %in% c("Ex_11_End")),]
#also deleted the final nest measurements for ex_10 because the nest size changed later when observations were not used for this study
nest_measure <- nest_measure[-(20),]
#basket cross section area
nest_measure$BasketCrossSection_cm2 <- (0.5*nest_measure$BaskLen)*(0.5*nest_measure$BaskWid)*pi
nest_measure$BasketCrossSection_cm2 <- round(nest_measure$BasketCrossSection_cm2, digits=2)
# #tangle volume
# tanglevol <- c()
# for(i in 1:length(nest_measure$BaskLen)){
#   if(nest_measure$Nest[i] == "Iy_11"){
#     tanglevol[i] <- (0.5*nest_measure$BaskLen[i])*(0.5*nest_measure$BaskWid[i])*nest_measure$TangleHeight[i]*pi*(0.5)
#   } else{
#     tanglevol[i] <- (0.5*nest_measure$BaskLen[i])*(0.5*nest_measure$BaskWid[i])*nest_measure$TangleHeight[i]*pi  
#   }
# }
# nest_measure$TangleVol <- tanglevol
# #tangle surface area save as csv in excel and merge the files onto nest measure
# nest_measure$Tangle_SA <- c(42505.69, 5861.86, 2258.50, 16409.30, 12033.56, 15405.15, 19597.44,
#                             70979.81,
#                             4833.46,
#                             4062.66,
#                             82025.40,
#                             20207.17,
#                             5804.44,
#                             24177.61,
#                             24005.67,
#                             2703.51,
#                             16210.84,
#                             6867.85,
#                             182402.94)
#number of host spiders
nest_measure$NumHostSpid <- 10^(-2.762+1.523*(log10(nest_measure$BasketCrossSection_cm2)))
#number of commensals. prey capture data removed "experimental" rows, this data was for another project
prey_capture <- read.csv("2018_preycapture.csv", stringsAsFactors = FALSE)
# prey_capture <- prey_capture[ !(prey_capture$Treatment %in% "Experimental"),]
# prey_capture <- subset(prey_capture, select = -Treatment) 
# names(prey_capture)[names(prey_capture) == "X..Seen"] <- "NumSeen"
prey_capture$Nest[prey_capture$Nest=="Ex06"] <- "Ex_06"
prey_capture <- prey_capture[c(which(prey_capture$Code == "H")),]
# num_seen_comm <- prey_capture[ which(prey_capture$Code == "C"), ]
# nest_measure$AvgNumComm <- tapply(num_seen_comm$NumSeen, INDEX = num_seen_comm$Nest, FUN = mean, na.rm=TRUE)
# #number of kleptos
# num_seen_klept <- prey_capture[ which(prey_capture$Code == "K"), ]
# nest_measure$AvgNumKlept <- tapply(num_seen_klept$NumSeen, INDEX = num_seen_klept$Nest, FUN = mean, na.rm=TRUE)

#change to long format
# colnames(nest_measure)
# nest_measure <- gather(nest_measure, "Type", "AvgNumSpid", c(15,16,17))
# #densities
# nest_measure$Density <- (nest_measure$AvgNumSpid)/ (nest_measure$TangleVol)
# #log10 number of spiders
# nest_measure$log10NumSpid <- log10(nest_measure$AvgNumSpid)
# #change type names
# nest_measure$Type[which(nest_measure$Type == "NumHostSpid")] <- "Host"
# nest_measure$Type[which(nest_measure$Type == "AvgNumComm")] <- "PassPar"
# nest_measure$Type[which(nest_measure$Type == "AvgNumKlept")] <- "ActPar"

write.csv(nest_measure, "nest_statistics.csv")

####Prey Capture####
#Prey Size
# prey_capture$Code[which(prey_capture$Code == "H")] <- "Host"
# prey_capture$Code[which(prey_capture$Code == "C")] <- "PassPar"
# prey_capture$Code[which(prey_capture$Code == "K")] <- "ActPar"
colnames(prey_capture)
# names(prey_capture)[6]<- paste("Type")
size <- group_by(prey_capture, Nest, Treatment)
PreySze <- summarize(size, AvgPreySze = mean(Size, na.rm = TRUE))
nest_stats <- merge(nest_measure, PreySze, by=c("Nest"))
nest_stats$LogPreySze <- log10(nest_stats$AvgPreySze)

#preysze1 has NA = 0
prey_capture$SizeNoNA <- prey_capture$Size
prey_capture$SizeNoNA[is.na(prey_capture$SizeNoNA)] <- 0
size1 <- group_by(prey_capture, Nest)
PreySze1 <- summarize(size1, AvgPreySze_incl0 = mean(SizeNoNA))
nest_stats <- merge(nest_stats, PreySze1, by=c("Nest"))
nest_stats$LogAvgPreySzeIncl0 <- log10(nest_stats$AvgPreySze_incl0)

#prey size excl EX03 measurement, preysze2 has no outlier
which(colnames(prey_capture) == "Size")
prey_capture[24, 5] <- NA
size <- group_by(prey_capture, Nest)
PreySze2 <- summarize(size, AvgPreySze_NoOut = mean(Size, na.rm = TRUE))
nest_stats <- merge(nest_stats, PreySze2, by=c("Nest"))
nest_stats$LogAvgPreySze_NoOut <- log10(nest_stats$AvgPreySze_NoOut)

####total prey number - deleting first hour of observation data####
is.character(prey_capture$Time)
prey_capture_perhr <- prey_capture[!(prey_capture$Time == "12:58" & prey_capture$Nest == "Ex_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:59" & prey_capture_perhr$Nest == "Ex_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "17:00" & prey_capture_perhr$Nest == "Ex_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:55" & prey_capture_perhr$Nest == "Ex_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "21:00" & prey_capture_perhr$Nest == "Ex_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:52" & prey_capture_perhr$Nest == "Ex_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:51" & prey_capture_perhr$Nest == "Ex_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:54" & prey_capture_perhr$Nest == "Ex_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:48" & prey_capture_perhr$Nest == "Ex_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:55" & prey_capture_perhr$Nest == "Ex_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:53" & prey_capture_perhr$Nest == "Ex_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:52" & prey_capture_perhr$Nest == "Ex_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:55" & prey_capture_perhr$Nest == "Ex_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:49" & prey_capture_perhr$Nest == "Ex_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:56" & prey_capture_perhr$Nest == "Ex_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:20" & prey_capture_perhr$Nest == "Ex_04"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:12" & prey_capture_perhr$Nest == "Ex_04"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:15" & prey_capture_perhr$Nest == "Ex_04"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:11" & prey_capture_perhr$Nest == "Ex_04"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:17" & prey_capture_perhr$Nest == "Ex_04"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:20" & prey_capture_perhr$Nest == "Ex_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:21" & prey_capture_perhr$Nest == "Ex_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:26" & prey_capture_perhr$Nest == "Ex_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:20" & prey_capture_perhr$Nest == "Ex_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:27" & prey_capture_perhr$Nest == "Ex_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:15" & prey_capture_perhr$Nest == "Ex_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "07:59" & prey_capture_perhr$Nest == "Ex_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:04" & prey_capture_perhr$Nest == "Ex_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:03" & prey_capture_perhr$Nest == "Ex_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:04" & prey_capture_perhr$Nest == "Ex_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:17" & prey_capture_perhr$Nest == "Ex_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:01" & prey_capture_perhr$Nest == "Ex_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:06" & prey_capture_perhr$Nest == "Ex_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:04" & prey_capture_perhr$Nest == "Ex_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:07" & prey_capture_perhr$Nest == "Ex_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:37" & prey_capture_perhr$Nest == "Ex_09"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:32" & prey_capture_perhr$Nest == "Ex_09"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:38" & prey_capture_perhr$Nest == "Ex_09"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:32" & prey_capture_perhr$Nest == "Ex_09"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:38" & prey_capture_perhr$Nest == "Ex_09"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "13:01" & prey_capture_perhr$Nest == "Ex_10"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "09:04" & prey_capture_perhr$Nest == "Ex_10"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "17:06" & prey_capture_perhr$Nest == "Ex_10"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "05:01" & prey_capture_perhr$Nest == "Ex_10"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "21:08" & prey_capture_perhr$Nest == "Ex_10"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "13:14" & prey_capture_perhr$Nest == "Ex_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "09:16" & prey_capture_perhr$Nest == "Ex_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "15:57" & prey_capture_perhr$Nest == "Ex_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "03:55" & prey_capture_perhr$Nest == "Ex_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "19:57" & prey_capture_perhr$Nest == "Ex_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "12:19" & prey_capture_perhr$Nest == "Ex_12"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:04" & prey_capture_perhr$Nest == "Ex_12"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:09" & prey_capture_perhr$Nest == "Ex_12"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:05" & prey_capture_perhr$Nest == "Ex_12"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:10" & prey_capture_perhr$Nest == "Ex_12"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "07:57" & prey_capture_perhr$Nest == "Iy_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:20" & prey_capture_perhr$Nest == "Iy_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "03:55" & prey_capture_perhr$Nest == "Iy_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:00" & prey_capture_perhr$Nest == "Iy_01"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:00" & prey_capture_perhr$Nest == "Iy_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:22" & prey_capture_perhr$Nest == "Iy_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "03:59" & prey_capture_perhr$Nest == "Iy_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:02" & prey_capture_perhr$Nest == "Iy_02"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:01" & prey_capture_perhr$Nest == "Iy_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:24" & prey_capture_perhr$Nest == "Iy_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:00" & prey_capture_perhr$Nest == "Iy_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:03" & prey_capture_perhr$Nest == "Iy_03"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:06" & prey_capture_perhr$Nest == "Iy_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:28" & prey_capture_perhr$Nest == "Iy_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:04" & prey_capture_perhr$Nest == "Iy_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:07" & prey_capture_perhr$Nest == "Iy_05"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:08" & prey_capture_perhr$Nest == "Iy_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:30" & prey_capture_perhr$Nest == "Iy_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:06" & prey_capture_perhr$Nest == "Iy_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:09" & prey_capture_perhr$Nest == "Iy_06"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:10" & prey_capture_perhr$Nest == "Iy_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:31" & prey_capture_perhr$Nest == "Iy_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:07" & prey_capture_perhr$Nest == "Iy_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:10" & prey_capture_perhr$Nest == "Iy_07"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:14" & prey_capture_perhr$Nest == "Iy_08"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:34" & prey_capture_perhr$Nest == "Iy_08"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:09" & prey_capture_perhr$Nest == "Iy_08"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:11" & prey_capture_perhr$Nest == "Iy_08"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "08:20" & prey_capture_perhr$Nest == "Iy_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "20:36" & prey_capture_perhr$Nest == "Iy_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "04:10" & prey_capture_perhr$Nest == "Iy_11"),]
prey_capture_perhr <- prey_capture_perhr[!(prey_capture_perhr$Time == "16:15" & prey_capture_perhr$Nest == "Iy_11"),]

write.csv(prey_capture, "prey_capture.csv")

####summary stats on prey capture per hour####
#total number of prey per hour per capita
NumPrey <- group_by(prey_capture_perhr, Nest)
TLNumPrey <- summarize(NumPrey, TLNumPrey = sum(!is.na(Order)))
nest_stats <- merge(nest_stats, TLNumPrey, by=c("Nest"))
nest_stats$NumObsHours <- c(18, 18, 18, 18, 18, 18, 18, 18, 18, 20, 20, 20, 18, 18, 18, 20, 20, 20, 20, 20, 20, 18, 18, 18, 18, 18, 18, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20)
nest_stats$NumPrey_Hr_Cap <- (nest_stats$TLNumPrey.x)/(nest_stats$NumObsHours)/(nest_stats$NumHostSpid)

#prey biomass 
prey_capture_biomass <- prey_capture_perhr
prey_capture_biomass[is.na(prey_capture_biomass)] <- 0
biomass_b <- c()
for (i in 1:length(prey_capture_biomass$Size)){
  if (prey_capture_biomass$Order[i] == "Coleoptera"){
    biomass_b[i] <- (-1.960 + (2.966*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Diptera") {
    biomass_b[i] <- (-1.361 + (2.026*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Hemiptera") {
    biomass_b[i] <- (-1.749 + (2.529*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Hymenoptera") {
    biomass_b[i] <- (-0.611 + (1.141*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Lepidoptera") {
    biomass_b[i] <- (-3.169 + (3.624*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Orthoptera") {
    biomass_b[i] <- (-1.928 + (2.680*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Spider") {
    biomass_b[i] <- (-2.643 + (4.011*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Unknown"){
    biomass_b[i] <- (-1.412 + (2.085*log10(prey_capture_biomass$Size[i])))
  } else if (prey_capture_biomass$Order[i] == "Larva") {
    biomass_b[i] <- (-1.412 + (2.085*log10(prey_capture_biomass$Size[i])))
  } else { biomass_b[i] <- 0 
  }
}

prey_capture_perhr$LogBiomass_mg <- biomass_b
prey_capture_perhr$LogBiomass_mg[prey_capture_perhr$LogBiomass_mg == 0] <- NA
prey_capture_perhr$Biomass_mg <- 10^(prey_capture_perhr$LogBiomass_mg)

PreyBio <- group_by(prey_capture_perhr, Nest)
PreyBiom <- summarize(PreyBio, PreyBiom = mean(Biomass_mg, na.rm = TRUE))
nest_stats <- merge(nest_stats, PreyBiom, by=c("Nest"))
colnames(nest_stats)[25] <- "Biomass_mg"

#biomass per hr per spider
nest_stats$Biomass_hr_capita <- (nest_stats$Biomass_mg) / (nest_stats$NumObsHours) / (nest_stats$NumHostSpid)
nest_stats$LogBiomass_hr_capita <- log10(nest_stats$Biomass_hr_capita)
nest_stats$LogBiomass_hr_capita[is.nan(nest_stats$LogBiomass_hr_capita)] <- NA

#change column names
# colnames(nest_stats)[17] <- "SpidDensity"
colnames(nest_stats)[13] <- "NumSpiders"

#natural log columns 
# nest_stats$LogSpidDens <- log10(nest_stats$SpidDensity)
nest_stats$LogBaskCrossSect_cm2 <- log10(nest_stats$BasketCrossSection_cm2)
# nest_stats$LogTangleVol <- log10(nest_stats$TangleVol)
nest_stats$LogNumPrey_Hr_Capita <- log10(nest_stats$NumPrey_Hr_Cap)

#proportion biomass
TLbiomass <- group_by(prey_capture_perhr, Nest)
TLbiomass1 <- summarise(TLbiomass, TLbiomass1 = sum(Biomass_mg, na.rm = TRUE))
nest_stats <- merge(TLbiomass1, nest_stats, by="Nest")
colnames(nest_stats)[2] <- "TLbiom"

# TLbiomassCK <- group_by(prey_capture_perhr, Nest, Type)
# TLbiomass2 <- summarise(TLbiomassCK, TLbiomass2 = sum(Biomass_mg, na.rm = TRUE))
# TLbiomass2 <- as.data.frame(TLbiomass2)
# nest_stats <- merge(TLbiomass2, nest_stats, by=c("Nest","Type"))
# colnames(nest_stats)[3] <- "TLbiom_perspid"
# nest_stats$propbiom <- (nest_stats$TLbiom_perspid)/ (nest_stats$TLbiom)
# nest_stats$LogPropBiom <- log10(nest_stats$propbiom)

#for the bar graph
nest_stats %>% mutate(quintile = ntile(TangleVol, 4)) -> nest_stats

write.csv(prey_capture_perhr, "prey_capture_perhr.csv")
write.csv(nest_stats, "nest_statistics.csv")
write.csv(prey_capture, "prey_capture.csv")







