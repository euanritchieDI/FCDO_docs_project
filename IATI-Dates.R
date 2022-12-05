########################################################################################

## SORT OUT DATES 

########################################################################################


dates = collectdate[c(1,3:6)]
dates = dates[!grepl("GB-GOV-3",dates$id),]
dates = dates[grepl("GB-(GOV-)?1-\\d{6}",dates$id),]

## IF ACTUAL START MISSING, REPLACE WITH PLANNED START
#dates$dates.2[is.na(dates$dates.2)] = dates$dates.1[is.na(dates$dates.2)]
dates = dates[!duplicated(dates),]
#dates = dates[-2]
#dates = dates[-3]	#THIS PICKS PLANNED, INSTEAD OF ACTUAL STARTS
names(dates) = c("id","start.plan","start.actual","end.plan","end.actual")
for (i in 2:5){dates[,i] = as.Date(dates[,i])}

dates$parent = gsub("\\-\\d{3}$","",dates$id)
dates$isparent = (dates$parent==dates$id)*1
dates$StartyearIATI = year(dates$start.plan)

## CREATE DIFFERENCE BETWEEN ACTUAL/PLANNED END. IF NOT AVAILABLE AT PROJECT, BUT IS FOR SUB-PROJECT, THEN TAKE MAX
## ACROSS SUB-PROJECTS
dates$diff   = with(dates,as.numeric(end.actual - end.plan))
dates$diffab = with(dates,abs(diff))
dates$diffab[dates$diffab==-Inf] = NA
dates$maxdiff = with(dates,ave(diffab,parent,FUN = function(x) max(x,na.rm=T)))
dates$diffab[dates$isparent==1 & is.na(dates$diffab)] = dates$maxdiff[dates$isparent==1 & is.na(dates$diffab)]
dates$maxdiff = NULL

dates = dates[dates$isparent==1,]
dates[c("id","isparent")] = NULL	
	
## THESE WERE SOME MISSING ONES, CHECKED MANUALLY
dates = rbind(dates,c(NA,NA,NA,NA,"GB-1-114287",2012,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-1-114488",2009,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-1-202042",2012,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-1-202297",2011,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-1-202733",2011,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-GOV-1-300505",2017,NA,NA))
dates = rbind(dates,c(NA,NA,NA,NA,"GB-GOV-1-300291",2016,NA,NA))
	

dates = dates[!duplicated(dates$parent,fromLast=T),]








