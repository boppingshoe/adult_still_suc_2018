
# 2003-2018 spring and summer Chinooka

rm(list=ls())
library(RODBC)

# rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
# Obtain data from SQL Server (by known spring/summer chinooka) ----

channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=PITTAG_SQL6;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=PITGLOBAL;Network=DBMSSOCN")

x <- sqlQuery(channel, "
SELECT tag_id, DimTagID, srrt,
'runType'= case
  WHEN SUBSTRING(srrt,2,1) = 1 THEN 'spring'
  ELSE 'summer'
end,
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

WHERE  srrt IN ('11H', '11W', '12H', '12W') AND
DATEPART(YYYY, boa_obs) >= 2003 AND
substring(river_km, 1, 3) = '522' AND
cast(substring(river_km, 5, 3) as int) > 173

ORDER BY boa_obs  

")

odbcCloseAll()

# clean up fishy data ----
# Rename columns and convert factors to characters
names(x) <- c("tag_id", "dim_id", "srrt", "run", "rear", "length", "mig_yr", "tag_date", "tag_site", "rel_site", "rel_date", "rel_km", "capture_di", "mig_his", "boa_obs", "mca_obs", "iha_obs", "gra_obs", "pra_obs", "ria_obs", "wea_obs", "grj_obs", "goj_obs", "lmj_obs", "ihj_obs", "rrj_obs", "mcj_obs", "jdj_obs", "bon_obs")

i <- sapply(x, is.factor)
x[i] <- lapply(x[i], as.character)
rm(i)

# Strip white space from variables
x$rel_km <- trimws(x$rel_km)
x$tag_site <- trimws(x$tag_site)
x$rel_site <- trimws(x$rel_site)

# Corrections to data
# Release date can't be less than tag date - assign the tag date instead
sum(x$rel_date < x$tag_date & !is.na(x$rel_date))
x$rel_date[x$rel_date < x$tag_date & !is.na(x$rel_date)] <- x$tag_date[x$rel_date < x$tag_date & !is.na(x$rel_date)]

# If release date missing, assume tag date
sum(is.na(x$rel_date))
x$rel_date[is.na(x$rel_date)] <- x$tag_date[is.na(x$rel_date)]

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
tmp <- subset(x, x$boa_yr == x$mig_yr)
nrow(tmp)
head(tmp)
table(format(tmp$boa_obs, "%m"))
rm(tmp)

# Remove juvs and mini's
x <- subset(x, x$boa_yr != x$mig_yr)

# Jacks / Adults
x$salt <- x$boa_yr - x$mig_yr
table(x$salt)

# salt = -1 (juv went down adult ladder? - so instead of being a jack is actually a minijack / juv)
# salt = -6 (A tag must have been reused ??)
x <- subset(x, x$salt > 1) # Remove Jacks

x$esu <- ifelse(substr(x$rel_km,1,3) == "522", "Snake", "Upper Columbia")
x$esu[substr(x$rel_km,1,3) == "539"] <- "Middle Columbia"
# You need to exclude Tucannon and Lyon's Ferry origin fish.
# Some of thse fish stray, but most will return to their origin below Granite
# Hence they are not available for detection at Lower Granite which would bias CJS results
ssc1 <- subset(x, !rel_site %in% c("LYFE","TUCR","CURP") &
    !tag_site %in% c("LYFE","TUCR","CURP"))

# export data ----
save(ssc1, file=paste0(wd, "data_compile/ssc_data/ssc1.Rdata"))
rm(x)
# ----


# section 4: bring all data together
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'

load(file=paste0(wd, "data_compile/ssc_data/ssc1.Rdata")) # fishy data
sscls<- subset(ssc1, !is.na(mca_obs) & mca_yr!=2018) # start at mcn and exclude 2018
sscls$obs_date<- as.Date(sscls$mca_obs)
load(file=paste0(wd, 'data_compile/low_snake_temp_2003_2018.Rdata')) # temper
load(file=paste0(wd, "data_compile/low_snake_flow_2003_2018.Rdata")) # flow

ssclsdat<- merge(sscls, tdls, by='obs_date')
ssclsdat<- merge(ssclsdat, fdls, by='obs_date')

# check temp data
plot(ssclsdat$lgr_temp, ssclsdat$mcn_temp, xlim=c(0,30), ylim=c(0,30))
plot(ssclsdat$mcn_temp, ssclsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))
plot(ssclsdat$lgr_temp, ssclsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))

with(ssclsdat, plot(mca_jul, mcn_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))
with(ssclsdat, plot(mca_jul, ihr_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))
with(ssclsdat, plot(mca_jul, lgr_temp, pch=20, cex=2, col=mig_yr, ylim=c(8, 23)))

save(ssclsdat, file=paste0(wd, "data_compile/ssc_data/ssclsdat.Rdata"))










