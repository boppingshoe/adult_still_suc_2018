
# 2003 - 2018 BON Fall Chinooka (modified from palmer EPA data request)
# this file queries, combines and saves the dataset

# section 1: fishy data
rm(list=ls())
library(RODBC)
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
# Obtain data from SQL Server (by known fall chinook srrt=13h,13w) ----
channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=PITTAG_SQL6;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=PITGLOBAL;Network=DBMSSOCN")
x <- sqlQuery(channel, "
SELECT tag_id, DimTagID, srrt,
'rearType'=SUBSTRING(srrt,3,3),
length, migr_yr, tag_date,
tag_site, rel_site, rel_date, river_km, capture_di,
'migHis' = case
WHEN CHARINDEX('2', capture_di) = 0 THEN 'river'
else 'trans'
end,
boa_obs, mca_obs,

'iha_obs' = case
WHEN DATEPART(YYYY, boa_obs) = DATEPART(YYYY, iha_obs) THEN iha_obs
WHEN DATEPART(YYYY, boa_obs) = DATEPART(YYYY, ich_obs) THEN ich_obs
else NULL
end,

gra_obs, pra_obs, ria_obs, wea_obs,
grj_obs, goj_obs, lmj_obs,

'ihj_obs' = case
WHEN DATEPART(YYYY, boa_obs) > DATEPART(YYYY, iha_obs) THEN iha_obs
WHEN DATEPART(YYYY, boa_obs) > DATEPART(YYYY, ich_obs) THEN ich_obs
else NULL
end,

rrj_obs, mcj_obs, jdj_obs, bon_obs

FROM   PITGLOBAL.dbo.[INTERROGATION BY SITE]                   

WHERE  srrt IN ('13H', '13W') AND
year(boa_obs) >= 2003 AND
--SUBSTRING(river_km,1,3) > 470
CONVERT(INT,
        CASE
        WHEN IsNumeric(SUBSTRING(river_km,1,3)) = 1 THEN SUBSTRING(river_km,1,3)
        ELSE 0 END) > 470 
ORDER BY boa_obs
")

odbcCloseAll()

# clean up fishy data ----
# Rename columns and convert factors to characters
names(x) <- c("tag_id", "dim_id", "srrt", "rear", "length", "mig_yr", "tag_date", "tag_site", "rel_site", "rel_date", "rel_km", "capture_di", "mig_his", "boa_obs", "mca_obs", "iha_obs", "gra_obs", "pra_obs", "ria_obs", "wea_obs", "grj_obs", "goj_obs", "lmj_obs", "ihj_obs", "rrj_obs", "mcj_obs", "jdj_obs", "bon_obs")

   i <- sapply(x, is.factor)
   x[i] <- lapply(x[i], as.character)
   rm(i)

# Strip white space from variables
   # trim <- function (x) gsub("^\\s+|\\s+$", "", x)
   x$rel_km <- trimws(x$rel_km)
   x$tag_site <- trimws(x$tag_site)
   x$rel_site <- trimws(x$rel_site)

# Corrections to data
# Release date can't be less than tag date - assign the tag date instead
   sum(x$rel_date < x$tag_date & !is.na(x$rel_date))
   x$rel_date[x$rel_date < x$tag_date & !is.na(x$rel_date)] <- x$tag_date[x$rel_date < x$tag_date & !is.na(x$rel_date)]

# If release date missing, assume tag date
   sum(is.na(x$rel_date))
   # x$rel_date[is.na(x$rel_date)] <- x$tag_date[is.na(x$rel_date)]

# create month and year variables
   # Date and time formating
   x$tag_date <- as.POSIXlt(x$tag_date, format="%Y-%m-%d %H:%M:%S")
   x$rel_date <- as.POSIXlt(x$rel_date, format="%Y-%m-%d %H:%M:%S")
   
   x$boa_obs <- as.POSIXlt(x$boa_obs, format="%Y-%m-%d %H:%M:%S")
   x$mca_obs <- as.POSIXlt(x$mca_obs, format="%Y-%m-%d %H:%M:%S")
   x$iha_obs <- as.POSIXlt(x$iha_obs, format="%Y-%m-%d %H:%M:%S")
   x$gra_obs <- as.POSIXlt(x$gra_obs, format="%Y-%m-%d %H:%M:%S")
   x$pra_obs <- as.POSIXlt(x$pra_obs, format="%Y-%m-%d %H:%M:%S")
   x$ria_obs <- as.POSIXlt(x$ria_obs, format="%Y-%m-%d %H:%M:%S")
   x$wea_obs <- as.POSIXlt(x$wea_obs, format="%Y-%m-%d %H:%M:%S")
   
   x$grj_obs <- as.POSIXlt(x$grj_obs, format="%Y-%m-%d %H:%M:%S")
   x$goj_obs <- as.POSIXlt(x$goj_obs, format="%Y-%m-%d %H:%M:%S")
   x$lmj_obs <- as.POSIXlt(x$lmj_obs, format="%Y-%m-%d %H:%M:%S")
   x$ihj_obs <- as.POSIXlt(x$ihj_obs, format="%Y-%m-%d %H:%M:%S")
   x$rrj_obs <- as.POSIXlt(x$rrj_obs, format="%Y-%m-%d %H:%M:%S")
   x$mcj_obs <- as.POSIXlt(x$mcj_obs, format="%Y-%m-%d %H:%M:%S")
   x$jdj_obs <- as.POSIXlt(x$jdj_obs, format="%Y-%m-%d %H:%M:%S")
   x$bon_obs <- as.POSIXlt(x$bon_obs , format="%Y-%m-%d %H:%M:%S")
   
   x$boa_jul <- x$boa_obs$yday + 1 # Julian day
   x$boa_yr <- as.numeric(format(x$boa_obs, "%Y"))
   x$mca_jul <- x$mca_obs$yday + 1 # Julian day
   x$mca_yr <- as.numeric(format(x$mca_obs, "%Y"))
   
   x$rel_mon <- as.numeric(format(x$rel_date, "%m"))
   x$rel_yr <- as.numeric(format(x$rel_date, "%Y"))

# check if there migration years less than the tag year
   sum(x$mig_yr < x$rel_yr)   # If > 0, need to correct
    x[x$mig_yr < x$rel_yr,]$mig_yr<-  x[x$mig_yr < x$rel_yr,]$rel_yr
    any(x$mig_yr < x$rel_yr) # should be FALSE now

# If fish released BEFORE May 31 - Migration Year = Tag Year
# If fish released AFTER  May 31 - Migration Year = Tag Year + 1
    table(x$rel_mon)

# Only need to modify fish released after May 31
   x$mig_yr[x$mig_yr == x$rel_yr & x$rel_mon > 5] <- x$mig_yr[x$mig_yr == x$rel_yr & x$rel_mon > 5] + 1
   x$dif_yr <- x$mig_yr - x$rel_yr
    table(x$rel_mon, x$dif_yr)

# Fork length check
   # sort(unique(x$length))
   # Got a few fish at 700 +
   sum(na.omit(x$length)>400)
   x <- subset(x, x$length < 400 | is.na(x$length))
   nrow(x)

# Remove juveniles and minijacks ----
# Take a look at juvs and minis
    # tmp <- subset(x, x$boa_yr == x$mig_yr)
    # nrow(tmp)
    # head(tmp)
    # table(format(tmp$boa_obs, "%m"))
    # rm(tmp)

# Remove juvs and mini's
   x <- subset(x, x$boa_yr != x$mig_yr)

# Jacks and Adults
   x$salt <- x$boa_yr - x$mig_yr
    table(x$salt)
   # salt = -1 (juv went down adult ladder? - so instead of being a jack is actually a minijack / juv)

   x <- subset(x, x$salt > 1) # Remove Jacks (and some juv that got through filter)

   x$esu <- ifelse(substr(x$rel_km,1,3) == "522", "Snake", "Upper Columbia")
   x$esu[substr(x$rel_km,1,3) == "539"] <- "Middle Columbia"
   # You need to exclude Tucannon and Lyon's Ferry origin fish.
   # Some of thse fish stray, but most will return to their origin below Granite
   # Hence they are not available for detection at Lower Granite which would bias CJS results
   fc1 <- subset(x, !x$rel_site %in% c("LYFE","TUCR","CURP"))
   
# (stop it now) ----
# Assign Origin ----

x$pop <- NA
x$pop[substr(x$relKm,1,7) == "522.016"] <- "Ice Harbor"
x$pop[substr(x$relKm,1,7) == "522.016"] <- "Ice Harbor"
x$pop[substr(x$relKm,1,7) == "522.018"] <- "Ice Harbor"
x$pop[substr(x$relKm,1,7) == "522.109"] <- "Lower Monument"
x$pop[substr(x$relKm,1,7) == "522.173"] <- "Lower Granite"
x$pop[substr(x$relKm,1,7) == "522.224"] <- "Clearwater"
x$pop[substr(x$relKm,1,7) == "522.225"] <- "Snake River Trap"
x$pop[substr(x$relKm,1,7) == "522.241"] <- "Snake River (km 224-223)"
x$pop[substr(x$relKm,1,7) == "522.258"] <- "Snake River (km 224-223)"
x$pop[substr(x$relKm,1,7) == "522.291"] <- "Snake River (km 224-223)"
x$pop[substr(x$relKm,1,11) == "522.303.103"] <- "Salmon Trap"
x$pop[substr(x$relKm,1,11) == "522.303.215"] <- "South Fork Salmon"
x$pop[substr(x$relKm,1,11) == "522.303.416"] <- "Upper Salmon"
x$pop[substr(x$relKm,1,11) == "522.303.489"] <- "Upper Salmon"
x$pop[substr(x$relKm,1,7) == "522.308"] <- "Imnaha"
x$pop[substr(x$relKm,1,7) == "522.348"] <- "Snake River (km 224-223)"
x$pop[substr(x$relKm,1,7) == "522.357"] <- "Snake River (km 303-397)"
x$pop[substr(x$relKm,1,3) == "539"] <- "Yakima"
x$pop[substr(x$relKm,1,3) == "639"] <- "Priest Rapids"
x$pop[substr(x$relKm,1,3) == "669"] <- "Wanapum"
x$pop[substr(x$relKm,1,3) == "730"] <- "Rock Island"
x$pop[substr(x$relKm,1,3) == "754"] <- "Wenatchee"
x$pop[substr(x$relKm,1,3) == "763"] <- "Rocky Reach"
x$pop[substr(x$relKm,1,3) == "765"] <- "Rocky Reach" # Actually Ringold fish released at Turtle Rock
x$pop[substr(x$relKm,1,3) == "778"] <- "Entiat"
x$pop[substr(x$relKm,1,3) == "810"] <- "Chelan River"
x$pop[substr(x$relKm,1,3) == "830"] <- "Wells Dam"
x$pop[substr(x$relKm,1,3) == "843"] <- "Methow"
x$pop[substr(x$relKm,1,3) == "858"] <- "Okanogan River"

# There's about 900 that now unassigned
# This because the querry includes more individuals now (11X's and 13X's)
# I don't think you're going to drill to the MPG level so ignore for now
   # sum(is.na(x$pop))
   # x[is.na(x$pop),]

# table(x$pop)

# export data ----
# write.csv(x$tagId, file=paste0(wd, "fc_data/data_compile/fc_tag_id.csv"), row.names=FALSE, quote=TRUE)
save(fc1, file=paste0(wd, "data_compile/fc_data/fc1.Rdata"))
rm(x)
# ----


# section 2: temperature data
rm(list=ls())
library(RODBC)
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
# BON forebay temp (only go until sept something)
# 2003 to 2013 ----
channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=SQL2;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=fpc;Network=DBMSSOCN")

tdMost <- sqlQuery(channel, "
  
  SET NOCOUNT ON
  SET ANSI_WARNINGS OFF
  
  -- Clean data
  SELECT [date], [site],
  'temp_tdg' = CASE WHEN temp_tdg BETWEEN 0 AND 30 THEN temp_tdg ELSE NULL END,
  'tdgs' =    CASE WHEN tdgs BETWEEN 95 AND 140 THEN tdgs ELSE NULL END
  INTO ##temp              
  FROM [fpc].[dbo].[tdgs_hourly_historic]
  WHERE DATEPART(YYYY, [date]) BETWEEN 2003 AND 2013 AND
  [site] = 'bon' 
  
  -- Compute daily averages
  SELECT 'date' = CONVERT(date,[date]),
  [site],
  'temp'=AVG(temp_tdg),
  'tdgs'=AVG(tdgs)
  FROM ##temp
  WHERE DATEPART(YYYY, [date]) BETWEEN 2003 AND 2013
  GROUP BY [date], [site] ORDER BY [date]  
  
  DROP TABLE ##temp
  
  ")

# odbcCloseAll()

tdMost$date <- as.Date(tdMost$date)

# Aug 2013 and up ----
# channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=SQL2;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=fpc;Network=DBMSSOCN")

tdRest <- sqlQuery(channel, "
  
  SET NOCOUNT ON
  SET ANSI_WARNINGS OFF
  
  -- Clean data
  SELECT [date], [site],
  'degf' = CASE WHEN degf BETWEEN 32 AND 86  THEN (degf - 32)/1.8 ELSE NULL END,
  'tdgs' = CASE WHEN tdgs BETWEEN 95 AND 140 THEN tdgs            ELSE NULL END
  INTO ##temp              
  FROM [fpc].[dbo].[tdg_spill_archive]
  WHERE [date] >  '2013-08-31 00:00:00' AND
  [site] = 'bon' 
  
  SELECT 'date' = CONVERT(date,[date]),
  [site],
  'temp'=AVG(degf),
  'tdgs'=AVG(tdgs)
  FROM ##temp
  GROUP BY [date], [site] ORDER BY [date]  
  
  DROP TABLE ##temp
  
  ")

odbcCloseAll()

tdRest$date <- as.Date(tdRest$date)

# Bring temper datasets together ----
# Avoid overlapping datasets
tdMost <- subset(tdMost, tdMost$date < min(tdRest$date))

# Stack 2 temperature datasets
td <- rbind(tdMost, tdRest)
td <- td[order(td$date),] 

# missing dates?
allDates <- seq(min(td$date), max(td$date), 1)
(missingDates <- allDates[!allDates %in% td$date]) # uh...
tmp <- data.frame(date=missingDates, site="BON", temp=NA, tdgs=NA)
td <- rbind(td, tmp)
td <- td[order(td$date),]
all(diff(td$date) == 1) # Should be true

td$site<- NULL
names(td)[1]<- 'obs_date'
# write.csv(td, file=paste0(wd, 'fc_data/data_compile/bon_temp_2003_2018.csv'), row.names = F)
save(td, file=paste0(wd, "data_compile/fc_data/bon_temp_2003_2018.Rdata"))
# ----

# lower Snake tailrace temp
# 2003 to 2012 ----
channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=SQL2;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=fpc;Network=DBMSSOCN")

tdMost <- sqlQuery(channel, "
  
  SET NOCOUNT ON
  SET ANSI_WARNINGS OFF
  
  SELECT [date], [site],
  'temp_tdg' = CASE WHEN temp_tdg BETWEEN 0 AND 30 THEN temp_tdg ELSE NULL END,
  'tdgs' =    CASE WHEN tdgs BETWEEN 95 AND 140 THEN tdgs ELSE NULL END
  INTO ##temp              
  FROM [fpc].[dbo].[tdgs_hourly_historic]
  WHERE DATEPART(YYYY, [date]) BETWEEN 2003 AND 2012 AND
  [site] in ('mcpw','idsw','lmnw','lgsw','lgnw')
  -- idsw (ice tail), lmnw (lomo tail), lgsw (goose tail), lgnw (granite tail)
  -- [site] = 'mcpw' -- mcn tail
  
  -- Compute daily averages
  SELECT 'date' = CONVERT(date,[date]),
  [site],
  'temp'=AVG(temp_tdg),
  'tdgs'=AVG(tdgs)
  FROM ##temp
  WHERE DATEPART(YYYY, [date]) BETWEEN 2003 AND 2012
  GROUP BY [date], [site] ORDER BY [date]  
  
  DROP TABLE ##temp
  
  ")

tdMost$date <- as.Date(tdMost$date)

# Aug 2012 and up ----

tdRest <- sqlQuery(channel, "
  
  SET NOCOUNT ON
  SET ANSI_WARNINGS OFF
  
  SELECT [date], [site],
  'degf' = CASE WHEN degf BETWEEN 32 AND 86  THEN (degf - 32)/1.8 ELSE NULL END,
  'tdgs' = CASE WHEN tdgs BETWEEN 95 AND 140 THEN tdgs            ELSE NULL END
  INTO ##temp              
  FROM [fpc].[dbo].[tdg_spill_archive]
  WHERE [date] >  '2012-08-31 00:00:00' AND
  [site] in ('mcpw','idsw','lmnw','lgsw','lgnw') 
  -- [site] = 'mcpw' 
  
  SELECT 'date' = CONVERT(date,[date]),
  [site],
  'temp'=AVG(degf),
  'tdgs'=AVG(tdgs)
  FROM ##temp
  GROUP BY [date], [site] ORDER BY [date]  
  
  DROP TABLE ##temp
  
  ")

odbcCloseAll()

tdRest$date <- as.Date(tdRest$date)

# Bring temper datasets together ----
# Avoid overlapping datasets
tdMost <- subset(tdMost, tdMost$date < min(tdRest$date))

# Stack 2 temperature datasets
tdall <- rbind(tdMost, tdRest)
sn<- c('MCPW','IDSW','LMNW','LGSW','LGNW')
tn<- c('mcn','ihr','lmn','lgs','lgr')
td<- list()
for (i in 1:5){
  td[[i]] <- subset(tdall, site==sn[i])[,-2]
  allDates <- seq(min(td[[i]]$date), max(td[[i]]$date), 1)
  missingDates <- allDates[!allDates %in% td[[i]]$date]
  tmp <- data.frame(date=missingDates, temp=NA, tdgs=NA)
  td[[i]] <- rbind(td[[i]], tmp)
  td[[i]] <- td[[i]][order(td[[i]]$date),]
  colnames(td[[i]])<- c('date', paste0(tn[i],'_temp'), paste0(tn[i],'_tdgs'))
  if (any(diff(td[[i]]$date) != 1)) stop (paste('epic fail at', i))
  if (i>1) td[[i]]<- merge(td[[i-1]], td[[i]], by='date')
}
tdls<- td[[5]]
tdls <- tdls[order(tdls$date),]
names(tdls)[1]<- 'obs_date'

save(tdls, file=paste0(wd, "data_compile/low_snake_temp_2003_2018.Rdata"))
# ----


# section 3: flow data
rm(list=ls())
library(RODBC)
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
# MCN, IHR and LGR ----
channel <- odbcDriverConnect("case=nochange;Description=Global;
  DRIVER=SQL Server;SERVER=SQL2;UID=sa;PWD=frznool;
  WSID=CUTTHROAT;DATABASE=pittag;Network=DBMSSOCN")

fdall <- sqlQuery(channel, "
  SELECT site, date, AvgTotalDischarge, AvgTotalSpill, AvgTurbineDischarge, AvgUnitsOnLine, AvSpillPct = round(((AvgTotalSpill/AvgTotalDischarge) * 100),0)
  FROM [fpc].[dbo].[tbl_coe_mean_flow]
  where site in ('MCN', 'IHR', 'LGR')
  --and date between '2003-01-01' and '2018-12-31'
  and datepart(YYYY,date) between '2003' and '2018'
  order by site, date
  ")

odbcCloseAll()

fdall$date<- as.Date(fdall$date, format='%Y-%m-%d')
names(fdall)<- c('site', 'date', 'dis', 'spill', 'turb', 'unit', 'spct')

sn<- c('MCN','IHR','LGR')
fn<- c('mcn','ihr','lgr')
fd<- list()
for (i in 1:3){
  fd[[i]] <- subset(fdall, site==sn[i])[,-1]
  allDates <- seq(min(fd[[i]]$date), max(fd[[i]]$date), 1)
  missingDates <- allDates[!allDates %in% fd[[i]]$date]
  tmp <- data.frame(date=missingDates, dis=NA, spill=NA, turb=NA, unit=NA, spct=NA)
  fd[[i]] <- rbind(fd[[i]], tmp)
  fd[[i]] <- fd[[i]][order(fd[[i]]$date),]
  colnames(fd[[i]])<- c('date', paste0(fn[i],'_dis'), paste0(fn[i],'_spill'),
    paste0(fn[i],'_turb'), paste0(fn[i],'_unit'), paste0(fn[i],'_spct'))
  if (any(diff(fd[[i]]$date) != 1)) stop (paste('epic fail at', i))
  if (i>1) fd[[i]]<- merge(fd[[i-1]], fd[[i]], by='date')
}
fdls<- fd[[3]]
fdls <- fdls[order(fdls$date),]
names(fdls)[1]<- 'obs_date'

save(fdls, file=paste0(wd, "data_compile/low_snake_flow_2003_2018.Rdata"))
# ----


# section 4: bring all data together
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'

load(file=paste0(wd, "data_compile/fc_data/fc1.Rdata")) # fishy data
fcls<- subset(fc1, !is.na(mca_obs)) # start at mcn
fcls$obs_date<- as.Date(fcls$mca_obs)
load(file=paste0(wd, 'data_compile/low_snake_temp_2003_2018.Rdata')) # temper
load(file=paste0(wd, "data_compile/low_snake_flow_2003_2018.Rdata")) # flow

fclsdat<- merge(fcls, tdls, by='obs_date')
fclsdat<- merge(fclsdat, fdls, by='obs_date')
fclsdat[4468:4482, 'mcn_temp']<- 18.135 # replace NA with average beteen 9/30 and 10/3
fclsdat[4508:4511, 'mcn_temp']<- 16.325 # replace NA with average beteen 10/10 and 10/14
fclsdat[36:47, 'ihr_temp']<- fclsdat[36:47, 'mcn_temp'] # replace weird mcn_temp with ihr_ temp
# plot(fclsdat$lgr_temp, fclsdat$mcn_temp, xlim=c(0,30), ylim=c(0,30))
# plot(fclsdat$mcn_temp, fclsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))
# plot(fclsdat$lgr_temp, fclsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))
# LGR temp is messed up
# with(fclsdat, plot(mca_jul, mcn_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))
# with(fclsdat, plot(mca_jul, ihr_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))
# with(fclsdat, plot(mca_jul, lgr_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))

save(fclsdat, file=paste0(wd, "data_compile/fc_data/fclsdat.Rdata"))



























