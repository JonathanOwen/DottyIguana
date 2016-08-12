# EXPLORATORY ANALYSIS OF U.S. FATALITY ANALYSIS REPORTING SYSTEM DATA 2010-2014
# Jonathan Owen,    April 18, 2016
# Data source http://www-fars.nhtsa.dot.gov//QueryTool/QuerySection/SelectYear.aspx
# Above URL is beginning of data selection for query 
# FARS code lookup tables manually created from selection criteria
# GLC codes obtained from http://www.gsa.gov/portal/content/102761
#
# The following criteria were selected for the FARS queries for the years
# 2010-2014
# (Obs. = Sequential observation number within each year)
# statenum = integer representing GLC code for state
# casenum = sequential case number within each state and each year
# vnumber = sequential vehicle number within each case
# pnumber = person number within each vehicle
# atmcond & atmcond2 = integer code for atmospheric conditions
# city = integer representing GLC code for city
# county = integer representing GLC code for county
# acchr = integer representing accident hour, e.g. 0 for 0:00 to 0:59am
# accmon = integer representing accident month, e.g. 1 for January
# caseyear = integer for year of accident in YYYY format
# dayofweek = integer representing day of week, e.g. 1 for Sunday
# fhevent & mhevent = integer codes for first & most harmful events in accident sequence
# latitude & longitude = numeric for decimal form of location
# numfatal = integer number of fatalities in accident
# rfun = integer code for road function type, e.g. rural interstate
# injury = integer for severity of injury to person on a 0-8 scale
# vfatcount  = integer number of fatalities in vehicle
# drf1 = integer code for primary driver related factor
# crshavoid = integer code for maneuvers made by driver to avoid crash
# contribcirc = integer code for contributing circumstances
# crashtype = integer code for vehicle movement during crash
# criticalevent cat & criticalevent = integer codes for critical events prior to crash and their category
# devfunc = integer code for functional state of traffic control devices
# dridistract = integer code for causes of driver distraction
# driavoidmanvr = integer code for driver trying to avoid something as cause of crash
# drivisobs = integer code for visual obstruction for driver 
# preimploc = integer code for pre-impact location
# preimpstab = integer code for pre-impact stability
# vehmanvr = integer code for vehcile movement prior to event
# roadalgn = integer code for road alignment, e.g, curve, left
# roadprof = integer code for roadway grade
# surfcond = integer code for road surface condition
# spdlim = integer of posted speed limit in miles per hour
# traflane = integer code for traffic lane arrangement
# contdev = integer code for types of traffic control device present
# trafflow = integer code for traffic description


library(dplyr)
library(ggplot2)
library(reshape)
library(stringi)
library(Hmisc)

# READ GLC CODES
locGSA <- read.table("GLCs_for_the_USA_and_DC.txt", sep = "\t", header = TRUE)
locGSA <- read.csv("GLCs_for_the_USA_and_DC.csv", skip = 1)
txtCritEvent <- read.table("Critical_Event_Codes.txt", sep = "\t")
txtRoadFn <- read.table("Road_Function_Codes.txt", sep = "\t")
vm2 <- read.table("nhtsa_vm_2.txt", sep = "\t", header = TRUE)

# READ 2014 TABLE
classCol <- rep("character", 42)
nameCol <- read.table("fars_nhtsa_2010.txt", sep = "\t", nrows = 1, colClasses = classCol)
classCol[c(6, 9:14, 19:21, 25:28, 32, 37, 40:41)] <- "numeric"
fars14 <- read.table("fars_nhtsa_2014.txt", sep = "\t", skip = 2, colClasses = classCol)

# FIX USE OF PERIOD IN 2014 TABLE
fars14[grepl("^\\.$", fars14[,7], perl = TRUE), 7] <- "99" #fars code for Atmospheric Condition 2 UNKNOWN = 99
fars14[grepl("^\\.$", fars14[,8], perl = TRUE), 8] <- "9999" #GLC code for city UNKNOWN = 9999
fars14[grepl("^\\.$", fars14[,15], perl = TRUE), 15:16] <- "0.0"
fars14[grepl("^\\.$", fars14[,18], perl = TRUE), 18] <- "99" #fars code for Roadway Function Class UNKNOWN = 99
fars14[grepl("^\\.$", fars14[,23], perl = TRUE), 23] <- "99" #fars code for Attempted Avoidance Maneuver UNKNOWN = 99
fars14[grepl("^\\.$", fars14[,33], perl = TRUE), 33] <- "9" #fars code for Pre-Impact Stability UNKNOWN = 9
fars14[grepl("^\\.$", fars14[,34], perl = TRUE), 34] <- "99" #fars code for Pre-Event Movement UNKNOWN = 99
fars14[grepl("^\\.$", fars14[,35], perl = TRUE), 35] <- "9" #fars code for Road Alignment UNKNOWN = 9
fars14[grepl("^\\.$", fars14[,36], perl = TRUE), 36] <- "9" #fars code for Roadway Grade UNKNOWN = 9
fars14[grepl("^\\.$", fars14[,38], perl = TRUE), 38] <- "9" #fars code for Surface Type UNKNOWN = 9
fars14[grepl("^\\.$", fars14[,39], perl = TRUE), 39] <- "99" #fars code for Speed Limit UNKNOWN = 99
fars14[grepl("^\\.$", fars14[,42], perl = TRUE), 42] <- "9" #fars code for Trafficway Description UNKNOWN = 9
for (i in c(7:8, 15:18, 23, 33:36, 38:39, 42)) {fars14[,i] <- as.numeric(fars14[,i])}

# READ TABLES FOR OTHER YEARS, COMBINE, CLEAN UP
classCol[c(7:8, 15:18, 23, 33:36, 38:39, 42)] <- "numeric"
fars10 <- read.table("fars_nhtsa_2010.txt", sep = "\t", skip = 2, colClasses = classCol)
fars11 <- read.table("fars_nhtsa_2011.txt", sep = "\t", skip = 2, colClasses = classCol)
fars12 <- read.table("fars_nhtsa_2012.txt", sep = "\t", skip = 2, colClasses = classCol)
fars13 <- read.table("fars_nhtsa_2013.txt", sep = "\t", skip = 2, colClasses = classCol)
fars <- rbind(fars10, fars11, fars12, fars13, fars14)
fars <- fars[,1:42]
names(fars) <- nameCol[1:42]
rm(fars10); rm(fars11); rm(fars12); rm(fars13); rm(fars14)
fars[grepl("^\\.$", fars[,22], perl = TRUE), 22] <- "99" #fars code for Driver Related Factor 1  UNKNOWN = 99
fars[grepl("^\\.$", fars[,29], perl = TRUE), 29] <- "99" #fars code for Driver Distracted By UNKNOWN = 99
fars[,22] <- as.numeric(fars[,22])
# save(fars, file = "save_object/fars.Rdata")

# RESHAPE vm2
varVm2 <- names(vm2)
idVm2 <- varVm2[1:3]
measVm2 <- varVm2[4:10] 
meltVm2 <- melt(vm2, id = idVm2, measured = measVm2)
names(meltVm2) <- c(idVm2, "fnRoad", "vehMile")
vm2 <- meltVm2
rm(meltVm2)
vm2$nameState <- as.factor(toupper(as.character(vm2$nameState)))
vm2$nameState <- as.factor(gsub("DIST OF", "DISTRICT OF", as.character(vm2$nameState)))
vm2$fnRoad <- as.factor(gsub("other_freeway", "freeway", as.character(vm2$fnRoad)))

# EDIT txtRoadFn
names(txtRoadFn) <- c("idRoadFn", "fullRoadFn")
txtRoadFn$typeArea <- stri_extract_first_words(txtRoadFn$fullRoadFn)
txtRoadFn$fnRoad <- stri_replace_first(txtRoadFn$fullRoadFn,  replacement = "", fixed = txtRoadFn$typeArea)
txtRoadFn$fnRoad <- gsub("^-", "", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub("\\w*\\s\\w*-Interstate", "interstate", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub("\\w*\\s\\w*-Other", "other_principal_arterial", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub(" ", "_", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub("^Local(_\\w*)*", "local", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub("(\\w*_)*Freeways_or_Expressways", "freeways_and_expressways", txtRoadFn$fnRoad)
txtRoadFn$fnRoad <- gsub("Unknown(_\\w*)*", "unknown", txtRoadFn$fnRoad)
txtRoadFn$fnRoad[15] <- "unknown"
txtRoadFn$fnRoad <- tolower(txtRoadFn$fnRoad)
txtRoadFn$typeArea <- tolower(txtRoadFn$typeArea)
txtRoadFn <- txtRoadFn[,c(1:2, 5:6)]

# EDIT locGSA
locGSA <- locGSA[1:46087,]
fars$abbrState <- locGSA$State.Abbreviation[match(fars$statenum, locGSA$State.Code)]
fars$nameState <- locGSA$State.Name[match(fars$statenum, locGSA$State.Code)]

# GET TEXT
fars$fnRoad <- as.factor(txtRoadFn$fnRoad[match(fars$rfun, txtRoadFn$idRoadFn)])
fars$typeArea <- as.factor(txtRoadFn$typeArea[match(fars$rfun, txtRoadFn$idRoadFn)])
fars$fnRoad <- as.factor(gsub("\\w*or_collector", "collector", as.character(fars$fnRoad)))

# GET VEHICLE MILES
getVM <- function(x){
  subVm <- subset(vm2, yyyy == x[12])
  subVm <- subset(subVm, nameState == x[44])
  subVm <- subset(subVm, typeArea == x[47])
  if (x[46] != "collector"){
    subVm <- subset(subVm, fnRoad == x[46])
    } else {
      subVm <- subset(subVm, fnRoad %in% c("major_collector", "minor_collector"))
      }
  return(sum(subVm$vehMile))
}

fars$vehMile[1:100000] <- apply(fars[1:100000,], 1, getVM)
fars$vehMile[100001:200000] <- apply(fars[100001:200000,], 1, getVM)
fars$vehMile[200001:300000] <- apply(fars[200001:300000,], 1, getVM)
fars$vehMile[300001:341436] <- apply(fars[300001:341436,], 1, getVM)
fars$vehMile <- unlist(fars$vehMile)
fars$rateFatal <- 1e6 * fars$vehMile/fars$numfatal

write.csv(fars, file = "fars.csv")

save(fars, file = "save_object/fars_vm.Rdata")

# CREATE UNIQUE OBSERVATION AND CASE ID 
fars$"Obs." <- paste0(fars$caseyear, "-", fars$"Obs.")
fars$casenum <- paste0(fars$caseyear, "-", fars$statenum, "-", fars$casenum)
names(fars)[1] <- "idObs"

# CONVERT TO FACTORS
# NOTE: prior conversion to numeric was used to check for valid coding
for (i in c(2, 6:14, 18:20, 22:42)) {fars[,i] <- as.factor(fars[,i])}

byRoadFn <- fars %>%
  group_by(fnRoad, typeArea, spdlim, abbrState, vehMile, caseyear, casenum) %>%
  summarise(caseFatal = sum(numfatal)/n()) %>%
  arrange(desc(caseFatal))

byRoadFn <- byRoadFn %>%
  group_by(fnRoad, typeArea, spdlim, abbrState, vehMile, caseyear) %>%
  summarise(fnRoadFatal = sum(caseFatal)) %>%
  arrange(desc(fnRoadFatal))

byRoadFn$rateFatal <- 1e6 * byRoadFn$fnRoadFatal/byRoadFn$vehMile

save(byRoadFn, file = "save_object/road_fn.Rdata")

subRoadFn <- byRoadFn[byRoadFn$fnRoad != "unknown",]
subRoadFn <- byRoadFn[byRoadFn$rateFatal < 10000,]

# PLOT byRoadFn
pdf("road_function_plot.pdf", width = 11, height = 8)
plotRF <- ggplot(data = subRoadFn, aes(rateFatal)) + theme_bw(base_size = 10)
plotRF <- plotRF + geom_density(aes(color = caseyear))  + facet_grid(fnRoad ~ typeArea)
plotRF
dev.off()

pdf("road_function_plot.pdf", width = 11, height = 8)
plotPoiss <- ggplot(data = subRoadFn, aes(x = vehMile, y = fnRoadFatal)) + theme_bw(base_size = 10)
plotPoiss <- plotPoiss + geom_point(aes(color = caseyear))  + facet_grid(fnRoad ~ typeArea)
plotPoiss
dev.off()



subPoisson <- byRoadFn[byRoadFn$fnRoad != "unknown",]

dfPoisson <- subPoisson %>%
  group_by(fnRoad, typeArea, caseyear) %>%
  summarise(tmpCol = n())
dfPoisson <- dfPoisson[,1:3]
namesDf <- names(dfPoisson)

fitPoisson <-  function(x){
  fitX <- glm(fnRoadFatal ~ vehMile, data = subPoisson[subPoisson$fnRoad == x[1] & subPoisson$typeArea == x[2] &
                                                 subPoisson$caseyear == x[3], ], family = "poisson")
  return(coef(summary(fitX))[,1:4])
}

fits <- apply(dfPoisson, 1, fitPoisson)
fits <- t(fits)
dfPoisson <- cbind(dfPoisson, fits)
names(dfPoisson) <- c(namesDf[1:3], "intercept", "rateFit", "stdErrInt", "stdErrRate", "zInt", "zRate", "pInt", "pRate")


# LOOK AT CRITICAL PRE-CRASH EVENTS
byPreCrash <- fars %>%
  group_by(fnRoad, typeArea, criticalevent, abbrState, vehMile, caseyear, casenum) %>%
  summarise(caseFatal = sum(numfatal)/n()) %>%
  arrange(desc(caseFatal))

byPreCrash <- byPreCrash %>%
  group_by(fnRoad, typeArea, criticalevent, abbrState, vehMile, caseyear) %>%
  summarise(fnRoadFatal = sum(caseFatal)) %>%
  arrange(desc(fnRoadFatal))

byPreCrash$rateFatal <- 1e6 * byPreCrash$fnRoadFatal/byPreCrash$vehMile

save(byPreCrash, file = "save_object/pre_crash.Rdata")

subPreCrash <- byPreCrash[byPreCrash$fnRoad != "unknown",]
subPreCrash <- byPreCrash[byPreCrash$rateFatal != Inf,]

plotPC <- ggplot(data = subPreCrash, aes(rateFatal)) + theme_bw(base_size = 10)
plotPC <- plotPC + geom_density(aes(color = typeArea))  + facet_grid(fnRoad ~ criticalevent)
plotPC

hiPreCrash <- subPreCrash[subPreCrash$criticalevent %in% c(2, 16, 70, 71, 88, 89), ]
hiPreCrash$descCriticalEvent <- txtCritEvent$V2[match(hiPreCrash$criticalevent, txtCritEvent$V1)]
hiPreCrash <- hiPreCrash[hiPreCrash$rateFatal < 1000,]

plotHPC <- ggplot(data = hiPreCrash, aes(rateFatal)) + theme_bw(base_size = 10)
plotHPC <- plotHPC + geom_density(aes(color = typeArea))  + facet_grid(fnRoad ~ descCriticalEvent)
plotHPC









# 
tCriticalEvents <- Fars %>%
  group_by(criticalevent, casenum) %>%
  summarise(CaseFatal = sum(numfatal)/n()) %>%
  arrange(desc(CaseFatal))

tCriticalEvents2 <- tCriticalEvents %>%
  group_by(criticalevent) %>%
  summarise(EventFatal = sum(CaseFatal)) %>%
  arrange(criticalevent)

tCriticalEvents2$CriticalEvent <-txtCritEvent[tCriticalEvents2$criticalevent,2]

tCriticalEvents <- arrange(tCriticalEvents2, desc(EventFatal))

pEvent <- ggplot(data = subset(tCriticalEvents, EventFatal > 10000),
                 aes(x = CriticalEvent, y = EventFatal, label = CriticalEvent)) + 
  theme_bw(base_size = 10)
pEvent <- pEvent + geom_point() + geom_text(vjust = -0.5, size = 3) + 
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank()) +
ggtitle("U.S. Vehicle Accident Fatalities by Critical Events Prior to Crash, 2010-2014") +
  labs(y = "Total Fatalities 2010-2014")
pdf("critcal_event_plot.pdf", width = 11, height = 8)
pEvent
dev.off()

#
tRoadFn <- Fars %>%
  group_by(rfun, spdlim, casenum) %>%
  summarise(CaseFatal = sum(numfatal)/n()) %>%
  arrange(desc(CaseFatal))

tRoadFn2 <- tRoadFn %>%
  group_by(rfun, spdlim) %>%
  summarise(RoadFatal = sum(CaseFatal)) %>%
  arrange(rfun)

tRoadFn2$RoadFunction <-txtRoadFn[tRoadFn2$rfun,2]

SpeedRange <- c("0-25", "30-35", "40-45", "50-55", "60-65", "70+")
lSpdlim <- levels(Fars$spdlim)
lSpdRng <- c(rep(1, 6), rep(2, 2), rep(3, 2), rep(4, 2), rep(5, 2), rep(6, 5))
tSpeeds <- data.frame(cbind(lSpdlim, lSpdRng))
tRoadFn2$SpeedLimit <-tSpeeds[tRoadFn2$spdlim,2]
tRoadFn2$SpeedLimit <-SpeedRange[tRoadFn2$SpeedLimit]
tRoadFn <- arrange(tRoadFn2, desc(RoadFatal)) 
tRoadFn3 <- tRoadFn %>%
  filter(spdlim != "98" & spdlim != "99"  ) %>%
  group_by(rfun, RoadFunction, SpeedLimit) %>%
  summarise(RoadFatal = sum(RoadFatal))
tRoadFn3$SpeedLimit <- as.factor(tRoadFn3$SpeedLimit)
tRoadFn3 <- arrange(tRoadFn3, desc(RoadFatal))
subRoadFn <- tRoadFn3[tRoadFn3$rfun %in% as.character(c(1:5)), ]


pRoadFn <- ggplot(data = subRoadFn, aes(x = SpeedLimit, y = RoadFatal)) + theme_bw(base_size = 10)
pRoadFn <- pRoadFn + geom_point(aes(color = RoadFunction), alpha = 0.5, size = 3) + 
  facet_grid(. ~ RoadFunction)
pRoadFn <- pRoadFn + theme(legend.position = "none") +
  ggtitle("U.S. Vehicle Accident Fatalities by Speed Limit and Road Function, 2010-2014") +
  labs(x = "Posted Speed Limit, mph", y = "Total Fatalities 2010-2014")
pdf("road_function_plot.pdf", width = 11, height = 8)
pRoadFn
dev.off()


