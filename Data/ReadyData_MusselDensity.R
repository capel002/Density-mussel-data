
# Author: Tony Wilkes
################################################################################
# SET-UP ====

rm(list=ls())
set.seed(1)
source("MyFunctions_MusselDensity.R")
if(!require(easypackages)) {install.packages("easypackages")}
library(easypackages)

mypackages <- c("openxlsx", "corrgram", "svglite", "car", "readxl", "lubridate",
                'autoimage')
packages(mypackages, prompt = FALSE)
libraries(mypackages)

dataPath <- "C:/Data Tony Wilkes/MyWorkDesktop/Data/Mussel Density/"

################################################################################
# Read & correct data ====
#

# reading data
DM <- read_excel(paste0(dataPath, "Data Tony.xlsx"), na="NA")
CI <- read_excel(paste0(dataPath, "Data_CI_Tony.xlsx"), na="NA")
EnvironmentalVar <- read.csv(paste0(dataPath, "EnvironmentalVar_ZK.csv"), row.names=1,
                             stringsAsFactors = F, na = "NA")
Meta <- read_excel(paste0(dataPath, "MetaData_MusZk.xlsx"))

colnames(DM)[10] <- "Xcode"
colnames(Meta)[1] <- "Xcode"


# translate dates
CI$date <- as.Date(as.character(CI$Period), format = "%Y-%m-%d")
DM$date <- as.Date(as.character(DM$Month), format = "%Y-%m-%d")
DM$month.num <- month(DM$date)
DM$Month <- NULL
DM$Xcode <- format(as.Date(as.character(DM$Xcode), format = "%Y-%m-%d"),
                   "%Y-%m")

Meta$Xcode <- format(as.Date(as.character(Meta$Xcode), format= "%Y-%m-%d"),
                             "%Y-%m")
Meta$`date in` <- as.Date(as.character(Meta$`date in`), format = "%Y-%m-%d")
Meta$`date out` <- as.Date(as.character(Meta$`date out`), format = "%Y-%m-%d")

EnvironmentalVar$Date <- as.Date(EnvironmentalVar$Date, format = "%d/%m/%Y")


################################################################################
# Summarise original data ====
#
write.summary(CI, "Supplement/raw data exploration/Summary CI data.xlsx")
write.summary(DM, "Supplement/raw data exploration/Summary DensityMussel.xlsx")
write.summary(EnvironmentalVar, "Supplement/raw data exploration/Summary EnvironmentalVar.xlsx")

reset.par()

pdf("Supplement/raw data exploration/frequency plots CI data.pdf", paper='a4r',
    width=12, height=8)
par(mfrow=c(2,3))
df_frequencyplots(CI)
dev.off()

pdf("Supplement/raw data exploration/frequency plots DensityMussel.pdf", paper='a4r',
    width=12, height=8)
par(mfrow=c(3,3))
df_frequencyplots(DM)
dev.off()

pdf("Supplement/raw data exploration/frequency plots EnvironmentalVar.pdf", paper='a4r',
    width=12, height=8)
par(mfrow=c(2,3))
df_frequencyplots(EnvironmentalVar)
dev.off()


reset.par()
svglite("Supplement/raw data exploration/correlogram CI data.svg", width=12, height=8)
corrgram(CI, lower.panel = panel.cor, upper.panel = panel.ellipse, order = 'PCA')
dev.off()

reset.par()
svglite("Supplement/raw data exploration/correlogram DensityMussel.svg", width=12, height=8)
corrgram(DM, lower.panel = panel.cor, upper.panel = panel.ellipse, order = 'PCA')
dev.off()

svglite("Supplement/raw data exploration/correlogram EnvironmentalVar.svg", width=12, height=8)
corrgram(EnvironmentalVar, lower.panel = panel.cor, upper.panel = panel.ellipse, order = 'PCA')
dev.off()


################################################################################
# Link Environment data with Meta Data ====
#

sort(unique(Meta$Xcode))

EnvironmentalVar$Xcode <- NA

for (i in 1:length(Meta$Xcode)) {
  ind <- which(
    EnvironmentalVar$Date >= Meta$`date in`[i] & EnvironmentalVar$Date <= Meta$`date out`[i]
  )
  EnvironmentalVar$Xcode[ind] <- Meta$Xcode[i]
}


EnvironmentAggr <- aggregate(cbind(meanTemp, meanChla, meanTurb) ~ Xcode,
                             FUN=mean, data = EnvironmentalVar)

EnvironmentAggr$TempDirection <- "0"
ind <- which(EnvironmentAggr$Xcode %in% c("2017-03", "2017-05", "2017-06"))
EnvironmentAggr$TempDirection[ind] <- "+"
ind <- which(EnvironmentAggr$Xcode %in% c("2017-10", "2017-11", "2018-03"))
EnvironmentAggr$TempDirection[ind] <- "-"
EnvironmentAggr$TempDirection <- factor(EnvironmentAggr$TempDirection,
                                        levels = c("0", "-", "+"), ordered = F)


################################################################################
# Create main merged dataset ====
#

ExperimentData <- merge(DM, Meta, by = "Xcode", all.x = T)
d <- merge(ExperimentData, EnvironmentAggr, by="Xcode", all=TRUE)

d$N1 <- ifelse(d$N1 > d$N0, d$N0, d$N1)

d$DensityStart <- d$Density_start
d$Density_start <- NULL


################################################################################
# Create merged CI dataset ====
#

CI$Xcode <- paste(lubridate::year(CI$date),
                  formatC(lubridate::month(CI$date), width = 2, flag = "0"),
                  sep = "-")
sort(unique(CI$Xcode))
sort(unique(Meta$Xcode))

CIData <- merge(CI, Meta, by = "Xcode", all.x = T)

d.CI <- merge(CIData, EnvironmentAggr, by="Xcode", all=TRUE)
d.CI <- d.CI[complete.cases(d.CI),]

d.CI$L.cm <- (d.CI$`length^3 (cm^3)`)^(1/3)
d.CI$AFDW.mg <- d.CI$`AFDW (mg)`
all(round(d.CI$AFDW.mg / d.CI$L.cm^3, 5) == round(d.CI$`CI (mg/cm^3)`, 5))

d.CI$DensityStart <- d.CI$Density_start
d.CI$Density_start <- d.CI$`length^3 (cm^3)` <- d.CI$`AFDW (mg)` <- NULL


################################################################################
# Summarise Merged data ====

write.summary(d, "Supplement/merged data exploration/Summary Merged Main Data.xlsx")
write.summary(d.CI, "Supplement/merged data exploration/Summary Merged CI Data.xlsx")

reset.par()
pdf("Supplement/merged data exploration/frequency plots Merged Main Data.pdf",
    paper='a4r', width=12, height=8)
par(mfrow=c(3,3))
df_frequencyplots(d)
dev.off()

pdf("Supplement/merged data exploration/frequency plots Merged CI Data.pdf",
    paper='a4r', width=12, height=8)
par(mfrow=c(3,3))
df_frequencyplots(d.CI)
dev.off()

reset.par()
svglite("Supplement/merged data exploration/correlogram Merged Main Data.svg",
        width=12, height=8)
corrgram(d, lower.panel = panel.cor, upper.panel = panel.ellipse, order = 'PCA')
dev.off()

svglite("Supplement/merged data exploration/correlogram Merged CI Data.svg",
        width=12, height=8)
corrgram(d.CI, lower.panel = panel.cor, upper.panel = panel.ellipse, order = 'PCA')
dev.off()

################################################################################
# Save RData image

save.image("Data/DataMerged.RData")

################################################################################
