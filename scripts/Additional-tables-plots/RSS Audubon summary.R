
#### INITIALS ####

AudubonDataPath = "./data/raw-data/Audubon-NPS_study-all_species_trends_filtered.csv"
ParkCode = pCode
OutDir = "./data/derived-data"

#### END INITIALS ####

AllAudubonData <- read.csv(AudubonDataPath)
ParkData <- subset(AllAudubonData, unit_code == ParkCode)

TrendSummary <- ddply(ParkData, ~season+climate_trend, summarise, 
                             NSpecies = length(common_name),
                             SpeciesList = paste(unique(as.character(common_name)), collapse = ", "))

NSpecies <- dcast(TrendSummary, season ~ climate_trend, value.var="NSpecies")
SpeciesList <- dcast(TrendSummary, season ~ climate_trend, value.var="SpeciesList")

TrendSummary.final <- rbind(NSpecies, SpeciesList)

write.csv(TrendSummary.final, paste(OutDir, "AudubonTrends.csv", sep="/"))