
#### INITIALS ####

ParkCode = pCode
OutDir = "./data/park-specific/output"

#### END INITIALS ####

AllAudubonData <- readRDS("./data/general/Audubon.Rds")
ParkData <- subset(AllAudubonData, unit_code == ParkCode)

TrendSummary <- ddply(ParkData, ~season+climate_trend, summarise, 
                             NSpecies = length(common_name),
                             SpeciesList = paste(unique(as.character(common_name)), collapse = ", "))

NSpecies <- dcast(TrendSummary, season ~ climate_trend, value.var="NSpecies")
SpeciesList <- dcast(TrendSummary, season ~ climate_trend, value.var="SpeciesList")

TrendSummary.final <- rbind(NSpecies, SpeciesList)

write.csv(TrendSummary.final, paste(OutDir, "AudubonTrends.csv", sep="/"))