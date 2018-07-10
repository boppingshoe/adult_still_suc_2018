rm(list=ls())
library(RODBC)
wd <- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'

# section 1: wild ----
channel <- odbcDriverConnect("case=nochange; Description=Global; DRIVER=SQL Server; SERVER=PITTAG_SQL6; UID=sa; PWD=frznool; WSID=CUTTHROAT; Network=DBMSSOCN")
x <- sqlQuery(channel=channel, stringsAsFactors=FALSE,
  query="
  
  SELECT a.tag_id, a.DimTagID, a.srrt, a.migr_yr, a.length, a.tag_date, a.tag_site, a.rel_site, a.rel_date, a.river_km,
  a.boa_obs,
  
  'tda_obs' =
  CASE
  WHEN td1_obs IS NOT NULL THEN td1_obs
  WHEN td2_obs IS NOT NULL THEN td2_obs
  ELSE NULL
  END, 
  
  a.mca_obs,
  
  'iha_obs' = case
  WHEN DATEPART(YYYY, a.boa_obs) <= 2004 THEN a.iha_obs
  else a.ich_obs
  end,
  
  a.lma_obs, a.goa_obs, a.gra_obs, a.pra_obs, a.ria_obs, a.wea_obs,
  a.grj_obs,a.goj_obs,a.lmj_obs,
  
  'ihj_obs' = case
  WHEN DATEPART(YYYY, a.boa_obs) > DATEPART(YYYY, a.iha_obs) THEN a.iha_obs
  WHEN DATEPART(YYYY, a.boa_obs) > DATEPART(YYYY, a.ich_obs) THEN a.ich_obs
  else NULL
  end,
  
  a.rrj_obs, a.mcj_obs, a.jdj_obs, a.bon_obs,
  c.SessionMessage, a.capture_di,
  
  'trans' = case
  WHEN CHARINDEX('2',a.capture_di) = 0 THEN 'River'
  else 'Trans'
  end,
  
  'MPG' = case
  WHEN SUBSTRING(a.river_km,1,3) = '509' THEN 'Walla Walla'
  WHEN SUBSTRING(a.river_km,1,7) = '522.100' THEN 'Lower Snake'
  WHEN SUBSTRING(a.river_km,1,7) = '522.016' THEN 'Unknown (Ice Harbor)'
  WHEN SUBSTRING(a.river_km,1,7) = '522.067' THEN 'Unknown (Lower Monumental)'
  WHEN SUBSTRING(a.river_km,1,7) = '522.173' THEN 'Unknown (Lower Granite)'
  WHEN SUBSTRING(a.river_km,1,7) = '522.224' THEN 'Clearwater'
  WHEN SUBSTRING(a.river_km,1,7) = '522.225' THEN 'Clearwater'
  WHEN SUBSTRING(a.river_km,1,7) = '522.234' THEN 'Lower Snake'
  WHEN SUBSTRING(a.river_km,1,7) = '522.271' THEN 'Grande Ronde'
  WHEN SUBSTRING(a.river_km,1,7) = '522.303' THEN 'Salmon'
  WHEN SUBSTRING(a.river_km,1,7) = '522.308' THEN 'Imnaha'
  WHEN SUBSTRING(a.river_km,1,3) = '539' THEN 'Yakima'
  WHEN SUBSTRING(a.river_km,1,3) = '639' THEN 'Unknown (Priest Rapids)'
  WHEN SUBSTRING(a.river_km,1,3) = '730' THEN 'Unknown (Rock Island)'
  WHEN SUBSTRING(a.river_km,1,3) = '754' THEN 'Wenatchee'
  WHEN SUBSTRING(a.river_km,1,3) = '778' THEN 'Entiat'
  WHEN SUBSTRING(a.river_km,1,3) = '843' THEN 'Methow'
  WHEN SUBSTRING(a.river_km,1,3) = '858' THEN 'Okanogan'
  else 'Unknown'
  end,
  
  'ESU' = case
  WHEN SUBSTRING(river_km,1,3) = '502' THEN 'Middle Columbia'
  WHEN SUBSTRING(river_km,1,3) = '509' THEN 'Middle Columbia'
  WHEN SUBSTRING(river_km,1,3) = '522' THEN 'Snake'
  WHEN a.tag_site = 'RINH' THEN 'Ringold'
  WHEN SUBSTRING(river_km,1,3) >= '639' THEN 'Upper Columbia'
  else 'Unknown'
  end,
  
  'realStock' = case
  WHEN (substring(river_km, 5, 3) = '271' or
  substring(river_km, 5, 3) = '308' or
  (substring(river_km, 5, 3) = '224' and 
  cast(substring(river_km, 9, 3) as int) < 120) or
  (substring(river_km, 5, 3) = '303' and 
  substring(river_km, 9, 3) <> '319' and 
  substring(river_km, 9, 3) <> '215')) 
  and a.tag_site not in ('clwtrp','saltrp','snktrp') THEN 'A'
  
  WHEN ((substring(river_km, 5, 3) = '224' and 
  cast(substring(river_km, 9, 3) as int) >= 120) or
  (substring(river_km, 5, 3) = '303' and
  (substring(river_km, 9, 3) = '319' or 
  substring(river_km, 9, 3) = '215')))
  and tag_site not in ('clwtrp','saltrp','snktrp') THEN 'B'
  else ''
  end
  
  FROM   PITGLOBAL.dbo.[INTERROGATION BY SITE] a                    
  INNER JOIN PITGLOBAL.dbo.FactMarkHistory b ON a.DimTagID = b.DimTagID
  LEFT JOIN  PITGLOBAL.dbo.DimSessionMessage c ON b.DimSessionMessageID = c.DimSessionMessageID
  
  WHERE  a.srrt = '32W' AND
  -- Exclude fish tagged as adults
  (a.length IS NULL OR a.length < 300) AND
  SUBSTRING(a.river_km,1,3) > 470 AND
  a.boa_obs IS NOT NULL AND
  -- Can't distinguish ESU
  a.tag_site NOT IN ('MCN') AND
  DATEPART(YYYY, a.boa_obs) BETWEEN 2003 AND 2017
  
  ORDER BY boa_obs   
  
  ")

odbcCloseAll()

# Rename columns and convert factors to characters
names(x) <- c("tagId", "dimTagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaObs", "tdaObs", "mcaObs", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "sesMes", "ch", "migHis", "mpg", "esu", "realStock")

i <- sapply(x, is.factor)
x[i] <- lapply(x[i], as.character)
rm(i)

# Check that MPG assigned for all fish -- should be FALSE
any(x$mpg == "Unknown") 

# Formating
# Date and time formating
x$tagDate <- as.POSIXlt(x$tagDate, format="%Y-%m-%d %H:%M:%S")
x$relDate <- as.POSIXlt(x$relDate, format="%Y-%m-%d %H:%M:%S")

x$boaObs <- as.POSIXlt(x$boaObs, format="%Y-%m-%d %H:%M:%S")
x$tdaObs <- as.POSIXlt(x$tdaObs, format="%Y-%m-%d %H:%M:%S")
x$mcaObs <- as.POSIXlt(x$mcaObs, format="%Y-%m-%d %H:%M:%S")
x$ihaObs <- as.POSIXlt(x$ihaObs, format="%Y-%m-%d %H:%M:%S")
x$lmaObs <- as.POSIXlt(x$lmaObs, format="%Y-%m-%d %H:%M:%S")
x$goaObs <- as.POSIXlt(x$goaObs, format="%Y-%m-%d %H:%M:%S")
x$graObs <- as.POSIXlt(x$graObs, format="%Y-%m-%d %H:%M:%S")
x$praObs <- as.POSIXlt(x$praObs, format="%Y-%m-%d %H:%M:%S")
x$riaObs <- as.POSIXlt(x$riaObs, format="%Y-%m-%d %H:%M:%S")
x$weaObs <- as.POSIXlt(x$weaObs, format="%Y-%m-%d %H:%M:%S")

x$grjObs <- as.POSIXlt(x$grjObs, format="%Y-%m-%d %H:%M:%S")
x$gojObs <- as.POSIXlt(x$gojObs, format="%Y-%m-%d %H:%M:%S")
x$lmjObs <- as.POSIXlt(x$lmjObs, format="%Y-%m-%d %H:%M:%S")
x$ihjObs <- as.POSIXlt(x$ihjObs, format="%Y-%m-%d %H:%M:%S")
x$rrjObs <- as.POSIXlt(x$rrjObs, format="%Y-%m-%d %H:%M:%S")
x$mcjObs <- as.POSIXlt(x$mcjObs, format="%Y-%m-%d %H:%M:%S")
x$jdjObs <- as.POSIXlt(x$jdjObs, format="%Y-%m-%d %H:%M:%S")
x$bonObs <- as.POSIXlt(x$bonObs , format="%Y-%m-%d %H:%M:%S")

x$boaYr <- as.numeric(format(x$boaObs, "%Y"))
x$tdaYr <- as.numeric(format(x$tdaObs, "%Y")) 
x$mcaYr <- as.numeric(format(x$mcaObs, "%Y"))
x$ihaYr <- as.numeric(format(x$ihaObs, "%Y"))
x$lmaYr <- as.numeric(format(x$lmaObs, "%Y"))
x$goaYr <- as.numeric(format(x$goaObs, "%Y"))
x$graYr <- as.numeric(format(x$graObs, "%Y"))

x$tagMon <- as.numeric(format(x$tagDate, "%m"))
x$tagYr <- as.numeric(format(x$tagDate, "%Y"))
x$relMon <- as.numeric(format(x$relDate, "%m"))
x$relYr <- as.numeric(format(x$relDate, "%Y"))

# Strip the trialing space
# trim.trailing <- function (x) sub("\\s+$", "", x)
x$tagSite <- trimws(x$tagSite)
x$relSite <- trimws(x$relSite)
x$riverKM <- trimws(x$riverKM)

# Get rid of known juveniles (tag year = bon adult year) 
nrow(x[x$relYr==x$boaYr,])
# some observations have missing release dates (assign tag dates)
x$relDate[is.na(x$relDate)] <- x$tagDate[is.na(x$relDate)]
x$relYr <- as.numeric(format(x$relDate, "%Y"))

x <- subset(x, x$relYr != x$boaYr)
sum(x$relYr == x$boaYr)

# Coil detection data
# Only those that weren't detected at MCA are in question
j <- x[is.na(x$mcaObs) & is.na(x$lmaObs) & is.na(x$goaObs) & is.na(x$graObs) & is.na(x$weaObs) & is.na(x$riaObs)  ,]
write.csv(paste0(j$tagId, "<>"), paste0(wd, "steelyhead/temporary_data/32W_potentialJuv.csv"), row.names=FALSE, quote=FALSE)

# Info to connect to database
channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=PITTAG_SQL6;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=PITGLOBAL;Network=DBMSSOCN")
# Obtain Coil specific detections from all BON sites
y <- sqlQuery(channel, stringsAsFactors=FALSE,
  query="
  
  SET NOCOUNT ON
  
  -- Get PIT tag codes to query
  IF OBJECT_ID('tempdb..##pitTagCodes') IS NOT NULL BEGIN DROP TABLE ##pitTagCodes END
  CREATE TABLE ##pitTagCodes ([tagId] varchar(16))
  BULK INSERT  ##pitTagCodes
  FROM '//Mobydick/CurrentData/STAFF/BOBBY/css/adult_still_suc_2018/steelyhead/temporary_data/32W_potentialJuv.csv'
  WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '<>',  FIRSTROW = 2)
  
  UPDATE ##pitTagCodes 
  SET tagId = SUBSTRING(tagId, 3,16)
  
  -- Obtain Coil specific detections from all BON sites
  SELECT b.Tag, a.ObsDateTime, c.Site, c.AntennaId
  FROM PITGLOBAL.dbo.FactObs a,
  PITGLOBAL.dbo.DimTag b, 
  PITGLOBAL.dbo.DimAntennaGroup c,
  ##pitTagCodes d
  WHERE b.Tag = d.tagId AND
  a.DimTagID = b.DimTagID AND
  a.DimAntennaGroupID = c.DimAntennaGroupID AND
  c.Site IN ('BO1','BO2','BO3','BO4')
  ORDER BY b.Tag, a.ObsDateTime
  
  ")

odbcCloseAll()

names(y) <- c("tagId", "obsTime", "obsSite", "antID")

# One tag wasn't "515F571A41    " - safely ignore - won't be able to get 1 record of coil data
length(unique(j$tagId)) == length(unique(y$tagId))
# unique(j$tagId)[!unique(j$tagId) %in% unique(y$tagId)]

# Merge with your coil sort key that you created
k <- read.csv(paste0(wd, "steelyhead/temporary_data/bon_coil_sort_key.csv"), header=TRUE, stringsAsFactor=FALSE)
k$startDate <- as.Date(k$startDate, format="%m/%d/%Y")
k$endDate <- as.Date(k$endDate, format="%m/%d/%Y")

# Loop below takes a bit
y$sort <- NA
for(i in 1:nrow(y)) {
  
  y$sort[i] <- k$sort[y$obsSite[i] == k$obsSite & y$antID[i] == k$antID &
      as.Date(y$obsTime[i]) >= k$startDate &
      as.Date(y$obsTime[i]) <= k$endDate]
  
}

y <- y[order(y$tagId, y$obsTime),]

# Determine Juveniles
x$age <- "Adult"

# Check 1 - MCA Obs > BOA Obs ###
# These look like juvenile detections in the adult ladder at McNary
# Don't want to do crazy coil thing for McNary
subset(x, x$boaObs > x$mcaObs)
tempId <- subset(x, x$boaObs > x$mcaObs)$tagId
x$mcaObs[x$tagId %in% tempId] <- NA

# (Check 2 - MCA year != BOA year)
# OK - expected w steelhead
# Also could be kelt going down adult ladder ?
# Either way, this is correctly classified as a "success"
nrow(subset(x, x$boaYr < x$mcaYr)) # a lot

# (Check 3 - juvs going through adult ladder
# Strictly decreasing anntennae sort values
monoDec <- function(dat) {
  if(length(dat) == 1) { out <- FALSE } else        # Detected at single coil once
    if(length(unique(dat)) == 1) out <- FALSE         # Detected at single coil more than once
    else out <- all(dat == cummin(dat))               # Multiple coil detections
    return(out)
}

temp <- tapply(y$sort, y$tagId, monoDec)
juv1 <- names(temp[temp])
# subset(x, x$tagId %in% juv1)

x$age[x$tagId %in% juv1] <- "Juv"

# (Check 4 - detected at a single coil - rely on previous detection history)
tempId <- names(which(tapply(y$antID, y$tagId, function(x) length(unique(x))) == 1))
temp <- x[x$tagId %in% tempId,]

# These can be inferred to be juvs
year <- function(x) as.numeric(format(x, "%Y"))

tempId <- subset(temp, temp$boaYr == year(temp$grjObs) |
    temp$boaYr == year(temp$gojObs) |
    temp$boaYr == year(temp$lmjObs) |
    temp$boaYr == year(temp$ihjObs) |                        
    temp$boaYr == year(temp$rrjObs) |                        
    temp$boaYr == year(temp$mcjObs) |
    temp$boaYr == year(temp$jdjObs) |
    temp$boaYr == year(temp$bonObs))$tagId
x$age[x$tagId %in% tempId] <- "Juv"

# These can't be determined - leave in, but could remove if you wanted to be cautious
tempId <- subset(temp, is.na(temp$grjObs) &
    is.na(temp$gojObs) &
    is.na(temp$lmjObs) &
    is.na(temp$ihjObs) &
    is.na(temp$rrjObs) &
    is.na(temp$mcjObs) &
    is.na(temp$jdjObs) &
    is.na(temp$bonObs))$tagId

x[x$tagId %in% tempId,]

# (Check 5)
# Juvenille bypass detection year equals boa year
#  GRJ    # GRJ Year = BOA Year       # MCA not NA     # GRJ before BOA
subset(x, year(x$grjObs) == x$boaYr & is.na(x$mcaObs) & x$grjObs < x$boaObs & x$age == "Adult")
x<- x[!x$tagId%in%c('3D9.1C2DA0E9CF','3D9.1C2D9C75D7'),]
# GOJ
subset(x, year(x$gojObs) == x$boaYr & is.na(x$mcaObs) & x$gojObs < x$boaObs & x$age == "Adult")
x<- x[!x$tagId%in%c('3D9.1C2DA07780','3D9.1C2D546351'),]
# LMJ
subset(x, year(x$lmjObs) == x$boaYr & is.na(x$mcaObs) & x$lmjObs < x$boaObs & x$age == "Adult")
x<- x[!x$tagId%in%c('3D9.1BF22DF252','3D9.1C2D551AE4'),]
# MCJ
subset(x, year(x$mcjObs) == x$boaYr & is.na(x$mcaObs) & x$mcjObs < x$boaObs & x$age == "Adult")
# JDJ
subset(x, year(x$jdjObs) == x$boaYr & is.na(x$mcaObs) & x$jdjObs < x$boaObs  & x$age == "Adult")
# BON
subset(x, year(x$bonObs) == x$boaYr & is.na(x$mcaObs) & x$bonObs < x$boaObs & x$age == "Adult") # keep these

# Remove juveniles
table(x$age)
x <- subset(x, x$age  == "Adult")

# Write data file
# x <- x[, c("tagId", "dimTagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaYr","boaObs", "tdaObs", "mcaObs", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "sesMes", "ch", "migHis", "mpg", "esu")]

save(x, file=paste0(wd, "steelyhead/temporary_data/32W_final.Rdata"))
#####

# section 2: hatchery ----
rm(list=ls())
wd <- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'
channel <- odbcDriverConnect("case=nochange; Description=Global;
  DRIVER=SQL Server;
  SERVER=PITTAG_SQL6;
  UID=sa;
  PWD=frznool;
  WSID=CUTTHROAT;
  DATABASE=PITGLOBAL;
  Network=DBMSSOCN")

x <- sqlQuery(channel=channel, stringsAsFactors=FALSE,
  query="
  
  SELECT a.tag_id, a.DimTagID, a.srrt, a.migr_yr, a.length, a.tag_date, a.tag_site, a.rel_site, a.rel_date, a.river_km,
  a.boa_obs,
  
  'tda_obs' =
  CASE
  WHEN td1_obs IS NOT NULL THEN td1_obs
  WHEN td2_obs IS NOT NULL THEN td2_obs
  ELSE NULL
  END, 
  
  a.mca_obs,
  
  'iha_obs' = case
  WHEN DATEPART(YYYY, a.boa_obs) <= 2004 THEN a.iha_obs
  else a.ich_obs
  end,
  
  a.lma_obs, a.goa_obs, a.gra_obs, a.pra_obs, a.ria_obs, a.wea_obs,
  a.grj_obs,a.goj_obs,a.lmj_obs,
  
  'ihj_obs' = case
  WHEN DATEPART(YYYY, a.boa_obs) > DATEPART(YYYY, a.iha_obs) THEN a.iha_obs
  WHEN DATEPART(YYYY, a.boa_obs) > DATEPART(YYYY, a.ich_obs) THEN a.ich_obs
  else NULL
  end,
  
  a.rrj_obs, a.mcj_obs, a.jdj_obs, a.bon_obs,
  c.SessionMessage, a.capture_di,
  
  'trans' = case
  WHEN CHARINDEX('2',a.capture_di) = 0 THEN 'River'
  else 'Trans'
  end,
  
  'MPG' = case
  WHEN SUBSTRING(a.river_km,1,3) IN ('502','509') THEN 'Walla Walla'
  WHEN SUBSTRING(a.river_km,1,7) IN ('522.095', '522.100', '522.113') THEN 'Lower Snake'
  WHEN SUBSTRING(a.river_km,1,7) = '522.224' THEN 'Clearwater'
  WHEN SUBSTRING(a.river_km,1,7) = '522.271' THEN 'Grande Ronde'
  WHEN SUBSTRING(a.river_km,1,7) = '522.303' THEN 'Salmon'
  WHEN SUBSTRING(a.river_km,1,7) = '522.308' THEN 'Imnaha'
  WHEN SUBSTRING(a.river_km,1,7) IN ('522.395', '522.397') THEN 'Below HCD'
  WHEN SUBSTRING(a.river_km,1,3) = '567' THEN 'Ringold'
  WHEN SUBSTRING(a.river_km,1,3) = '639' THEN 'Unknown (Priest Rapids)'
  WHEN SUBSTRING(a.river_km,1,3) = '730' THEN 'Unknown (Rock Island)'
  WHEN SUBSTRING(a.river_km,1,3) = '754' THEN 'Wenatchee'
  WHEN SUBSTRING(a.river_km,1,3) = '763' THEN 'Unknown (Rocky Reach)'
  WHEN SUBSTRING(a.river_km,1,3) = '830' THEN 'Wells'
  WHEN SUBSTRING(a.river_km,1,3) = '843' THEN 'Methow'
  WHEN SUBSTRING(a.river_km,1,3) = '858' THEN 'Okanogan'
  else 'Unknown'
  end,
  
  'ESU' = case
  WHEN SUBSTRING(river_km,1,3) = '502' THEN 'Middle Columbia'
  WHEN SUBSTRING(river_km,1,3) = '509' THEN 'Middle Columbia'
  WHEN SUBSTRING(river_km,1,3) = '522' THEN 'Snake'
  WHEN a.tag_site = 'RINH' THEN 'Ringold'
  WHEN SUBSTRING(river_km,1,3) >= '639' THEN 'Upper Columbia'
  else 'Unknown'
  end, 
  
  'realStock' = case
  -- Mutually exclusive tag sites
  -- B
  -- Clearwater MPG
  WHEN a.tag_site IN ('CLWH', 'CLWR', 'DWOR') THEN 'B'
  -- Salmon MPG, SQUAWP is an acclimation pond, 12 total observations
  -- For 7 observations no stock is given, rest are B's, assuming so
  WHEN a.tag_site = 'SQUAWP' THEN 'B'
  
  -- A
  -- Grande Ronde MPG
  WHEN a.tag_site IN ('GRANDR', 'GRNTRP', 'IRRI') THEN 'A'
  -- Lower Snake MPG
  WHEN a.tag_site IN ('TUCH', 'TUCR') THEN 'A'
  -- Hells Canyon Dam MPG
  WHEN a.tag_site = 'HCD' THEN 'A'
  -- Imnaha MPG
  WHEN a.tag_site = 'IMNTRP' THEN 'A'
  -- Salmon and Below HCD MPGs
  WHEN a.tag_site = 'NISP' THEN 'A'
  
  -- Tag sites with both A and B runs ('HAGE' and 'MAVA'), rely on stock column
  -- B
  WHEN (a.tag_site IN ('HAGE', 'MAVA') AND d.Stock IN ('DWOR', 'DWOR B', 'DWORB', 'DWORSHAK B', 'DWROSHAK B')) THEN 'B'
  WHEN (a.tag_site IN ('HAGE', 'MAVA') AND d.Stock IN ('UPPER SALMON B', 'USALB', 'USB')) THEN 'B'
  
  -- A
  WHEN (a.tag_site IN ('HAGE', 'MAVA') AND d.Stock IN ('E. F. NATURAL', 'E.F. NATURAL', 'E.F. NATURALS', 'EAST FORK',  'EAST FORK B', 'EAST FORK NAT', 'EAST FORK NAT.', 'EF NAT', 'EFNAT')) THEN 'A'
  WHEN (a.tag_site IN ('HAGE', 'MAVA') AND d.Stock IN ('LEMHI', 'PAH', 'PAH A', 'PAH A HAMMER', 'PAH A LEMHI', 'PAH A RED ROCK', 'PAH A STINKY', 'PAH A/SAW A', 'PAH/SAW', 'PAHA', 'PAHA / SAWA',
  'PAHSAMEROI A', 'PAHSIMERIO A', 'PAHSIMEROI A', 'PAW / SAW', 'SAW', 'SAW A', 'SAW A / PAH A', 'SAW A /PAH A', 'SAW A PAH A', 'SAW A VAL CK',
  'SAW A/PAH A', 'SAWA', 'SAWOOTH A', 'SAWT A', 'SAWTOOH A', 'SAWTOORH A', 'SAWTOOTH', 'SAWTOOTH A')) THEN 'A'
  
  -- Other special cases
  -- 'LYFE' hatchery fish go to Grande Ronde, Lower Snake, and Walla Walla MPGs. Only want to differentiate A and B for Snake River
  WHEN (a.tag_site = 'LYFE' AND SUBSTRING(a.river_km,1,7) IN ('522.095', '522.100', '522.113', '522.271')) THEN 'A'
  
  -- Clearwater B run from 'HAGE' not identified by stock
  WHEN (a.tag_site = 'HAGE' AND d.Stock = '' AND SUBSTRING(a.river_km,1,7) = '522.224') THEN 'B'
  
  -- Salmon A run from 'HAGE' not identified by stock (Salmon B runs are never released at 'SAWTR' and 'YANKFK', but these 4 fish with missing stock info are)
  WHEN (a.tag_site = 'HAGE' AND d.Stock = '' AND SUBSTRING(a.river_km,1,7) = '522.303') THEN 'A'
  
  else ''
  end,
  
  d.Stock
  
  FROM   PITGLOBAL.dbo.[INTERROGATION BY SITE] a
  -- Session Message
  INNER JOIN PITGLOBAL.dbo.FactMarkHistory b ON a.DimTagID = b.DimTagID
  LEFT JOIN  PITGLOBAL.dbo.DimSessionMessage c ON b.DimSessionMessageID = c.DimSessionMessageID
  -- Stock
  INNER JOIN PITGLOBAL.dbo.DimStock d ON d.DimStockID = b.DimStockID
  
  
  WHERE  a.srrt = '32H' AND
  -- Exclude fish tagged as adults
  (a.length IS NULL OR a.length < 300) AND
  SUBSTRING(a.river_km,1,3) > '470'  AND
  a.boa_obs IS NOT NULL AND
  DATEPART(YYYY, a.boa_obs) BETWEEN 2003 AND 2016 AND
  -- Cannot distinguish 'A' from 'B' run at these sites
  a.tag_site NOT IN ('IHR', 'LGR', 'LGRLDR', 'LGRRRR', 'LGS', 'LMN', 'MCN', 'SALTRP', 'SNKTRP')
  
  ORDER BY boa_obs
  
  ")

odbcCloseAll()

# Rename columns and convert factors to characters
names(x) <- c("tagId", "dimTagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaObs", "tdaObs", "mcaObs", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "sesMes", "ch", "migHis", "mpg", "esu", "realStock", "stock")

i <- sapply(x, is.factor)
x[i] <- lapply(x[i], as.character)
rm(i)

# Check that run (A or B) assigned for all Snake River origin fish - should be TRUE
all(x$realStock[x$ESU == "Snake" %in% c("A","B")])

# Check that MPG assigned for all fish -- should be FALSE
any(x$MPG == "Unknown") 

# Formating
# Date and time formating
x$tagDate <- as.POSIXlt(x$tagDate, format="%Y-%m-%d %H:%M:%S")
x$relDate <- as.POSIXlt(x$relDate, format="%Y-%m-%d %H:%M:%S")

x$boaObs <- as.POSIXlt(x$boaObs, format="%Y-%m-%d %H:%M:%S")
x$tdaObs <- as.POSIXlt(x$tdaObs, format="%Y-%m-%d %H:%M:%S")
x$mcaObs <- as.POSIXlt(x$mcaObs, format="%Y-%m-%d %H:%M:%S")
x$ihaObs <- as.POSIXlt(x$ihaObs, format="%Y-%m-%d %H:%M:%S")
x$lmaObs <- as.POSIXlt(x$lmaObs, format="%Y-%m-%d %H:%M:%S")
x$goaObs <- as.POSIXlt(x$goaObs, format="%Y-%m-%d %H:%M:%S")
x$graObs <- as.POSIXlt(x$graObs, format="%Y-%m-%d %H:%M:%S")
x$praObs <- as.POSIXlt(x$praObs, format="%Y-%m-%d %H:%M:%S")
x$riaObs <- as.POSIXlt(x$riaObs, format="%Y-%m-%d %H:%M:%S")
x$weaObs <- as.POSIXlt(x$weaObs, format="%Y-%m-%d %H:%M:%S")

x$grjObs <- as.POSIXlt(x$grjObs, format="%Y-%m-%d %H:%M:%S")
x$gojObs <- as.POSIXlt(x$gojObs, format="%Y-%m-%d %H:%M:%S")
x$lmjObs <- as.POSIXlt(x$lmjObs, format="%Y-%m-%d %H:%M:%S")
x$ihjObs <- as.POSIXlt(x$ihjObs, format="%Y-%m-%d %H:%M:%S")
x$rrjObs <- as.POSIXlt(x$rrjObs, format="%Y-%m-%d %H:%M:%S")
x$mcjObs <- as.POSIXlt(x$mcjObs, format="%Y-%m-%d %H:%M:%S")
x$jdjObs <- as.POSIXlt(x$jdjObs, format="%Y-%m-%d %H:%M:%S")
x$bonObs <- as.POSIXlt(x$bonObs , format="%Y-%m-%d %H:%M:%S")

x$boaYr <- as.numeric(format(x$boaObs, "%Y"))
x$tdaYr <- as.numeric(format(x$tdaObs, "%Y")) 
x$mcaYr <- as.numeric(format(x$mcaObs, "%Y"))
x$ihaYr <- as.numeric(format(x$ihaObs, "%Y"))
x$lmaYr <- as.numeric(format(x$lmaObs, "%Y"))
x$goaYr <- as.numeric(format(x$goaObs, "%Y"))
x$graYr <- as.numeric(format(x$graObs, "%Y"))

x$tagMon <- as.numeric(format(x$tagDate, "%m"))
x$tagYr <- as.numeric(format(x$tagDate, "%Y"))
x$relMon <- as.numeric(format(x$relDate, "%m"))
x$relYr <- as.numeric(format(x$relDate, "%Y"))

# Strip the trialing space
x$tagSite <- trimws(x$tagSite)
x$relSite <- trimws(x$relSite)
x$riverKM <- trimws(x$riverKM)

# Create more specific populations
x$group <- trimws(paste(x$mpg, x$realStock))

# Get rid of known juveniles (tag year = bon adult year)
sum(x$relYr == x$boaYr)
x <- subset(x, x$relYr != x$boaYr)

# Coil detection data 
# Only those that weren't detected at MCA are in question
j <- x[is.na(x$mcaObs) & is.na(x$lmaObs) & is.na(x$goaObs) & is.na(x$graObs) & is.na(x$weaObs) & is.na(x$riaObs)  ,]
write.csv(paste0(j$tagId, "<>"), paste0(wd, "steelyhead/temporary_data/32H_potentialJuv.csv"), row.names=FALSE, quote=FALSE)

# Info to connect to database
channel <- odbcDriverConnect("case=nochange;Description=Global;DRIVER=SQL Server;SERVER=PITTAG_SQL6;UID=sa;PWD=frznool;WSID=CUTTHROAT;DATABASE=PITGLOBAL;Network=DBMSSOCN")

# Obtain Coil specific detections from all BON sites

y <- sqlQuery(channel, stringsAsFactors=FALSE,
  query="
  
  SET NOCOUNT ON
  
  -- Get PIT tag codes to query
  IF OBJECT_ID('tempdb..##pitTagCodes') IS NOT NULL BEGIN DROP TABLE ##pitTagCodes END
  CREATE TABLE ##pitTagCodes ([tagId] varchar(16))
  BULK INSERT  ##pitTagCodes
  FROM '//Mobydick/CurrentData/STAFF/BOBBY/css/adult_still_suc_2018/steelyhead/temporary_data/32H_potentialJuv.csv'
  WITH (FIELDTERMINATOR = ',', ROWTERMINATOR = '<>',  FIRSTROW = 2)
  
  UPDATE ##pitTagCodes 
  SET tagId = SUBSTRING(tagId, 3,16)
  
  -- Obtain Coil specific detections from all BON sites
  SELECT b.Tag, a.ObsDateTime, c.Site, c.AntennaId
  FROM PITGLOBAL.dbo.FactObs a,
  PITGLOBAL.dbo.DimTag b, 
  PITGLOBAL.dbo.DimAntennaGroup c,
  ##pitTagCodes d
  WHERE b.Tag = d.tagId AND
  a.DimTagID = b.DimTagID AND
  a.DimAntennaGroupID = c.DimAntennaGroupID AND
  c.Site IN ('BO1','BO2','BO3','BO4')
  ORDER BY b.Tag, a.ObsDateTime
  
  ")

odbcCloseAll()

names(y) <- c("tagId", "obsTime", "obsSite", "antID")

# One tag wasn't "3D9.1BF1862ABA" - safely ignore - won't be able to get 1 record of coil data
length(unique(j$tagId)) == length(unique(y$tagId))
unique(j$tagId)[!unique(j$tagId) %in% unique(y$tagId)]

# Merge with your coil sort key that you created
k <- read.csv(paste0(wd, "steelyhead/temporary_data/bon_coil_sort_key.csv"), header=TRUE, stringsAsFactor=FALSE)
k$startDate <- as.Date(k$startDate, format="%m/%d/%Y")
k$endDate <- as.Date(k$endDate, format="%m/%d/%Y")

# !!! Loop below takes 2ish minutes !!! - couldn't come up with a slicker way
y$sort <- NA
for(i in 1:nrow(y)) {
  y$sort[i] <- k$sort[y$obsSite[i] == k$obsSite & y$antID[i] == k$antID & as.Date(y$obsTime[i]) >= k$startDate & as.Date(y$obsTime[i]) <= k$endDate]
}

y <- y[order(y$tagId, y$obsTime),]

# Determine Juveniles
x$age <- "Adult"

### Check 1 - MCA Obs > BOA Obs ###
# These look like juvenile detections in the adult ladder at McNary
# Don't want to do crazy coil thing for McNary
subset(x, x$boaObs > x$mcaObs)
tempId <- subset(x, x$boaObs > x$mcaObs)$tagId
x$mcaObs[x$tagId %in% tempId] <- NA

# (Check 2 - MCA year != BOA year)
# OK - expected w steelhead
# Also could be kelt going down adult ladder ?
x$boaYr <- as.numeric(format(x$boaObs, "%Y"))
x$mcaYr <- as.numeric(format(x$mcaObs, "%Y"))
head(subset(x, x$boaYr < x$mcaYr))

# (Check 3 - juvs going through adult ladder
# Strictly decreasing anntennae sort values
monoDec <- function(dat) {
  if(length(dat) == 1) { out <- FALSE } else        # Detected at single coil once
    if(length(unique(dat)) == 1) out <- FALSE         # Detected at single coil more than once
    else out <- all(dat == cummin(dat))               # Multiple coil detections
    return(out)
}

temp <- tapply(y$sort, y$tagId, monoDec)
juv1 <- names(temp[temp])
# subset(x, x$tagId %in% juv1)[,c("relDate","boaObs")]

x$age[x$tagId %in% juv1] <- "Juv"

# (Check 4 - detected at a single coil - rely on previous detection history)
tempId <- names(which(tapply(y$antID, y$tagId, function(x) length(unique(x))) == 1))
temp <- x[x$tagId %in% tempId,]

# These can be inferred to be juvs
year <- function(x) as.numeric(format(x, "%Y"))

tempId <- subset(temp, temp$boaYr == year(temp$grjObs) |
    temp$boaYr == year(temp$gojObs) |
    temp$boaYr == year(temp$lmjObs) |
    temp$boaYr == year(temp$ihjObs) |                        
    temp$boaYr == year(temp$rrjObs) |                        
    temp$boaYr == year(temp$mcjObs) |
    temp$boaYr == year(temp$jdjObs) |
    temp$boaYr == year(temp$bonObs))$tagId
x$age[x$tagId %in% tempId] <- "Juv"

# These can't be determined - leave in, but could remove if you wanted to be cautious
tempId <- subset(temp, is.na(temp$grjObs) &
    is.na(temp$gojObs) &
    is.na(temp$lmjObs) &
    is.na(temp$ihjObs) &
    is.na(temp$rrjObs) &
    is.na(temp$mcjObs) &
    is.na(temp$jdjObs) &
    is.na(temp$bonObs))$tagId

x[x$tagId %in% tempId,]

# (Check 5)
# Juvenille bypass detection year equals boa year
#  GRJ    # GRJ Year = BOA Year       # MCA not NA     # GRJ before BOA
subset(x, year(x$grjObs) == x$boaYr & is.na(x$mcaObs) & x$grjObs < x$boaObs & x$age == "Adult")
# GOJ
subset(x, year(x$gojObs) == x$boaYr & is.na(x$mcaObs) & x$gojObs < x$boaObs & x$age == "Adult")
# LMJ
subset(x, year(x$lmjObs) == x$boaYr & is.na(x$mcaObs) & x$lmjObs < x$boaObs & x$age == "Adult")
# MCJ
subset(x, year(x$mcjObs) == x$boaYr & is.na(x$mcaObs) & x$mcjObs < x$boaObs & x$age == "Adult")
# JDJ
subset(x, year(x$jdjObs) == x$boaYr & is.na(x$mcaObs) & x$jdjObs < x$boaObs  & x$age == "Adult")
# BON
subset(x, year(x$bonObs) == x$boaYr & is.na(x$mcaObs) & x$bonObs < x$boaObs & x$age == "Adult") # keep it

# Remove juveniles
table(x$age)
x <- subset(x, x$age  == "Adult")

# Write data file

#x <- x[, c("tagId", "dimTagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaYr","boaObs", "tdaObs", "mcaObs", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "sesMes", "ch", "trans", "MPG", "ESU", "realStock", "stock")]

save(x, file=paste0(wd, "steelyhead/temporary_data/32H_final.Rdata"))
#####

# section 3:combine ----
load(file=paste0(wd, "steelyhead/temporary_data/32H_final.Rdata"))
h <- x[, c("tagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaYr", "boaObs", "tdaObs", "mcaObs", "mcaYr", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "ch", "migHis", "mpg", "esu", "realStock")] # hatch

load(file=paste0(wd, "steelyhead/temporary_data/32W_final.Rdata"))
w <- x[, c("tagId", "srrt", "migYr", "length", "tagDate", "tagSite", "relSite", "relDate", "riverKM", "boaYr","boaObs", "tdaObs", "mcaObs", "mcaYr", "ihaObs", "lmaObs", "goaObs", "graObs", "praObs", "riaObs", "weaObs", "grjObs", "gojObs", "lmjObs", "ihjObs", "rrjObs", "mcjObs", "jdjObs", "bonObs", "ch", "migHis", "mpg", "esu", "realStock")] # wild

rm(x)
mykiss <- rbind(h,w)
mykiss$rear <- substr(mykiss$srrt, 3,3)
mykiss$mcaJul <- mykiss$mcaObs$yday + 1

# kelt that were overlooked during initial data query (wah wah) edited 5/1/2017
mykiss$boaObs<- as.Date(mykiss$boaObs)
mykiss$mcaObs<- as.Date(mykiss$mcaObs)
mykiss$ihaObs<- as.Date(mykiss$ihaObs)
mykiss$graObs<- as.Date(mykiss$graObs)
xfish_iha<- subset(mykiss, mcaObs>ihaObs)$tagId
xfish_gra<- subset(mykiss, mcaObs>graObs)$tagId
mykiss$ihaObs[mykiss$tagId %in% xfish_iha]<- NA
mykiss$graObs[mykiss$tagId %in% xfish_gra]<- NA

# detections for CJS
mykiss$ihaDet<- ifelse(is.na(mykiss$ihaObs), 0, 1)
mykiss$graDet<- ifelse(is.na(mykiss$graObs), 0, 1)

# filter by date 6/1 to 9/15 (to avoid holdovers, but I don't think is effective)
#mykiss$boaMon <- as.numeric(format(mykiss$boaObs, "%m"))
#mykiss$boaDay <- as.numeric(format(mykiss$boaObs, "%d"))
#mykiss<- subset(mykiss, boaMon>5 & boaMon<10)
#mykiss<- subset(mykiss, boaMon!=9 | boaDay<16)

names(mykiss) <- c("tag_id", "srrt", "mig_yr", "length", "tag_date", "tag_site", "rel_site", "rel_date", "rel_km", "boa_yr","boa_obs", "tda_obs", "mca_obs", "mca_yr", "iha_obs", "lma_obs", "goa_obs", "gra_obs", "pra_obs", "ria_obs", "wea_obs", "grj_obs", "goj_obs", "lmj_obs", "ihj_obs", "rrj_obs", "mcj_obs", "jdj_obs", "bon_obs", "capture_di", "mig_his", "mpg", "esu", "stock", "rear", "mca_jul", "iha_det", "gra_det")

save(mykiss, file=paste0(wd, "data_compile/st_data/mykiss.Rdata")) # just fishy
#####

# section 4: bring all data together
# dataset
rm(list=ls())
wd<- 'G:/STAFF/Bobby/css/adult_still_suc_2018/'

load(file=paste0(wd, "data_compile/st_data/mykiss.Rdata")) # fishy data
mykiss<- subset(mykiss, !is.na(mca_obs) & mca_yr!=2017) # start at mcn and exclude 2017
mykiss$obs_date<- as.Date(mykiss$mca_obs)
load(file=paste0(wd, 'data_compile/low_snake_temp_2003_2018.Rdata')) # temper
load(file=paste0(wd, "data_compile/low_snake_flow_2003_2018.Rdata")) # flow

stlsdat<- merge(mykiss, tdls, by='obs_date')
stlsdat<- merge(stlsdat, fdls, by='obs_date')
stlsdat[279:351, 'ihr_temp']<- stlsdat[279:351, 'mcn_temp'] # replace weird ihr_temp with mcn_ temp
a<- (17.42917-16.65417)/ (16.058333-15.941667)
b<- 16.65417- 6.642895*15.941667
stlsdat[3904:3927, 'ihr_temp']<- (stlsdat[3904:3927, 'mcn_temp']-b)/a # replace with regression
stlsdat[stlsdat$mcn_temp>25&!is.na(stlsdat$mcn_temp),]$mcn_temp<- subset(stlsdat, mcn_temp>25)$ihr_temp

# check temp data
# plot(stlsdat$lgr_temp, stlsdat$mcn_temp, xlim=c(0,30), ylim=c(0,30))
# plot(stlsdat$mcn_temp, stlsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))
# plot(stlsdat$lgr_temp, stlsdat$ihr_temp, xlim=c(0,30), ylim=c(0,30))

# with(stlsdat, plot(mca_jul, mcn_temp, pch=20, cex=2, col=mig_yr, ylim=c(0, 23)))
# with(stlsdat, plot(mca_jul, ihr_temp, pch=20, cex=2, col=mig_yr, ylim=c(0, 23)))
# with(stlsdat, plot(mca_jul, lgr_temp, pch=20, cex=2, col=mig_yr, ylim=c(0, 23)))

save(stlsdat, file=paste0(wd, "data_compile/st_data/stlsdat.Rdata"))
# load(file=paste0(wd, "data_compile/st_data/stlsdat.Rdata"))















