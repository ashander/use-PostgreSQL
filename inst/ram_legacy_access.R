#
# Example code using this usePostgreSQL wrapper for RPostgreSQL to connect to RAM legacy DB
# Jaime Ashander


# install this package
# if(require(devtools))
# install_github("ashander/use-PostgreSQL") #without devtools library need to download manually from github
# or can install from root locally
library(usePostgreSQL)

options(pg.auth =  alist(host="nautilus-vm.mathstat.dal.ca", user="srdbuser", password="srd6us3r!", dbname="srdb", port="5432")) 
## set global option using connection info from RAM legacy website
readCitationFile('inst/RAM_CITATION.txt')

con <- pgCon()

## get info from stock table for South Africa LME
sa.stocks <- pgQry("select * from srdb.stock where stockid in (select stockid from srdb.lmetostocks where lme_number=29)", con)

library(stringr) # to build queries based on the table of SA stocks
library(reshape)
library(ggplot2) # for nifty plots 


## get assessid/stockid lookup
ql.key <- paste("select assessid, stockid from srdb.assessment where stockid in ('", str_c(sa.stocks$stockid,collapse="', '"),"')", sep="")
sa.assess.stock.key<-pgQry(ql.key, con)

## get timeseries associated with SA
ql.ts <- paste("select * from srdb.timeseries where assessid in ('", str_c(sa.assess.stock.key$assessid,collapse="', '"),"')", sep="")
sa.ts<-pgQry(ql.ts, con)

## grab the whole tsmetrics table and pair it down to SA
tsm <- pgTab("srdb.tsmetrics", con)
sa.tsmetrics <- tsm[tsm$tsunique %in% unique(sa.ts$tsid), ]

## manual inspection revealed plenty of SSB estimate coverage...
sa.ts.ssb <- sa.ts[sa.ts$tsid =="SSB-MT", ] # all the SA time series with SSB-MT estimated

pgDiscon(con)

## pull in stock common names, doing a JOIN in the query might be preferable
sa.spp <-sa.ts.ssb
sa.spp$species.SSB<-sa.stocks$commonname[match(sa.assess.stock.key$stockid[match(sa.ts.ssb$assessid, sa.assess.stock.key$assessid)], sa.stocks$stockid)]
sa.spp$assessid <- NULL
sa.spp$tsid <- NULL
## use melt/cast to combine assessments across years per common-name ... note REMOVING NAs so proceed with caution 
sa.gg.w<-cast(sa.spp, tsyear~species.SSB, fun.aggregate=function(x){sum(x, na.rm=TRUE)}, value='tsvalue')
sa.gg <- melt(sa.gg.w, id=c('tsyear','species.SSB'))

p1 <- ggplot(sa.gg, aes(tsyear, value, fill=species.SSB))+geom_area()+scale_color_brewer()+ylab("Metric tons")+xlab("Year")
p1
