library(data.table)   
library(dplyr)
library(xml2)
library(XML)
library(rvest)
library(stringr)
library(stringi)
library(openxlsx)
library(readtext)
library(ggplot2)
library(tidyr)
library(rddensity)
library(rdd)

setwd("H:/My Documents/Devtracker Code/")
#otherpath = "C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/"

findna = function(x){return(sapply(x,function(y) sum(is.na(y))))}
finder = function(x){return(sapply(x,function(y) sum(!is.na(y))))}

### GET LINKS TO ALL THE IATI XML PAGES FOR DFID/FCDO

fcdolinks = NULL
for (i in 1:7){
	fcdo = sprintf("https://iatiregistry.org/publisher/fcdo?page=%s",i)
	fcdo = read_html(fcdo)
	toadd = fcdo %>% html_nodes(xpath="//ul//div/p/a[contains(text(),'Download')]") %>% html_attr('href')
	fcdolinks = c(fcdolinks,toadd)
}

fcdolinks = fcdolinks[!grepl("United-Arab",fcdolinks)]  #UAE EMPTY AND MESSES WITH CODE

### THIS FIRST BIT SCRAPES ALL THE LINKS TO DOCUMENTS FROM IATI XML PAGES (AND OTHER INFO

collectdocs = NULL
collectinfo = NULL
collectbval = NULL
collectdate = NULL
collecttrns = NULL
collecttype = NULL

for (lnk in fcdolinks){
  proj = gsub(".*/","",lnk)
  proj = gsub(".xml","",proj)

  if (grepl("organisation",proj)){
	next
  }	
  link = tryCatch(read_xml(lnk),error=function(e) "fail")
  if (link[1]=="fail"){next}
  if (length(xml_find_all(link,xpath="//iati-activity"))==0) {
	next
  }
  docnodes = link %>% xml_find_all("//document-link/title/narrative")
  if (length(docnodes)==0) {
	  docs = data.frame("narrative"="none","link"="none","parent"="none","proj"=proj)
  } else {
	docnarr  = docnodes %>% xml_text
	doclink  = docnodes %>% xml_parent %>% xml_parent %>% xml_attr("url")
	docid    = sapply(docnodes, function(x) x %>% xml_parent %>% xml_parent %>% xml_parent %>% xml_child() %>% xml_text)
	docs = data.frame("narrative"=docnarr,"link"=doclink,"parent"=docid,"proj"=proj)

 	 titnodes    = link %>% xml_find_all("//iati-activity/title") 
 	 titles      = titnodes %>% html_text()
 	 id          = sapply(titnodes,FUN = function(x) x %>% xml_parent %>% xml_child() %>% xml_text)
 	 info	  = data.frame('titles' = titles,'parent'=id)
 	    info[setdiff(names(collectinfo),names(info))] = NA
 	    collectinfo[setdiff(names(info),names(collectinfo))] = NA
 	    collectinfo = rbind(collectinfo,info)
  }
  collectdocs = rbind(collectdocs,docs)

  ##BUDGET DATA
  budgetnodes = link %>% xml_find_all("//budget") 
	if(length(budgetnodes)==0){next}
  bstart      = data.table('budgetstart' = budgetnodes %>% xml_find_all("period-start") %>% html_attrs)
  bend        = data.table('budgetend' = budgetnodes %>% xml_find_all("period-end") %>% html_attrs)
  bvalue      = budgetnodes %>% xml_find_all("value") %>% html_text()
  id          = sapply(budgetnodes,FUN = function(x) x %>% xml_parent %>% xml_child() %>% xml_text)
  bvalues     = as.data.frame(cbind(id,bstart,bend,bvalue))
  bvalues = cbind(bvalues,proj)
     bvalues[setdiff(names(collectbval),names(bvalues))] = NA
     collectbval[setdiff(names(bvalues),names(collectbval))] = NA
     collectbval = rbind(collectbval,bvalues)
  

  ##TRANSACTION DATA
  tnodes    = link %>% xml_find_all("//transaction") 
	if(length(tnodes)==0){next}
  trec	= tnodes %>% xml_find_first("receiver-org") %>% xml_text() 
  tdesc	= tnodes %>% xml_find_first("description") %>% xml_text()
  tvalnodes = link %>% xml_find_all("//transaction/value")
  ttype     = data.table('ttype' = unlist(tnodes %>% xml_find_all("transaction-type") %>% html_attrs))
  tattr     = data.table(tvalnodes %>% html_attrs)
  		  tattr[, c("currency","date") := tstrsplit(V1, ",", fixed=TRUE)]
  tvalue    = tvalnodes %>% xml_text()
  id        = sapply(tnodes,FUN = function(x) x %>% xml_parent %>% xml_child() %>% xml_text)
  tvalues   = as.data.frame(cbind(id,ttype,tattr,tvalue,proj,tdesc,trec))
     tvalues[setdiff(names(collecttrns),names(tvalues))] = NA
     collecttrns[setdiff(names(tvalues),names(collecttrns))] = NA
     collecttrns = rbind(collecttrns,tvalues)

  ##AID TYPE (FOR LATERALITY: B02 IS MULTILATERAL)
  tnodes    = link %>% xml_find_all("//iati-activity/default-aid-type")
  ttype     = unlist(tnodes %>% html_attrs())
  id        = sapply(tnodes,FUN = function(x) x %>% xml_parent %>% xml_child() %>% xml_text)
  aidtype = data.frame('ttype' = ttype,'id'=id,'proj'=proj)
     aidtype[setdiff(names(collecttype),names(aidtype))] = NA
     collecttype[setdiff(names(aidtype),names(collecttype))] = NA
     collecttype = rbind(collecttype,aidtype)

  ## ACTIVITY DATES
  datenodes = link %>% xml_find_all("//activity-date") 
  id   = sapply(datenodes,FUN = function(x) x %>% xml_parent %>% xml_child() %>% xml_text)
  dates = unlist(datenodes %>% html_attrs())
  dates = data.frame("id"=id,"type"=dates[names(dates)=="type"],"dates"=dates[names(dates)=="iso-date"],"proj"=proj)
  dates = reshape(dates,direction="wide",idvar=c("id","proj"),timevar="type")
	dates[setdiff(names(collectdate),names(dates))] = NA
	collectdate[setdiff(names(date),names(collectdate))] = NA
	collectdate = rbind(collectdate,dates)

}

rm(fcdolinks,fcdo,titnodes,id,info,doclink,docid,docs,docnarr,titles,proj,lnk,toadd,i,link,aidtype,ttype,
   tnodes,trec,tdesc,tvalue,tvalnodes,tvalues,bvalues,bend,bstart,budgetnodes,bvalue,dates,datenodes,tattr)
  
  collecttrns$currency = gsub(".*?= ","",collecttrns$currency)
  collecttrns$currency = gsub('"',"",collecttrns$currency)
  collecttrns$date = gsub(".*?= ","",collecttrns$date)
  collecttrns$date = gsub('"|)',"",collecttrns$date)
  collecttrns$V1 = NULL
#------------------------------------------------------


### SORT OUT DOCUMENTS
collectdocs = collectdocs[1:3]
collectdocs = collectdocs[!duplicated(collectdocs),]
collectdocs$narrative = tolower(collectdocs$narrative)
collectdocs$LF  = grepl("logical ?framework",collectdocs$narrative)*1
collectdocs$PCR = grepl("project ?completion ?review",collectdocs$narrative)*1
collectdocs$AR = grepl("annual ?review",collectdocs$narrative)*1
collectdocs$BC = grepl("business ?case",collectdocs$narrative)*1
collectdocs$IS = grepl("intervention ?summary",collectdocs$narrative)*1

## SELECT ONLY DFID DOCS 
dfiddocs = collectdocs[!grepl("GOV-3",collectdocs$parent),]

## SELECT BUSINESS CASES/ADDENDUMS
BC = dfiddocs[dfiddocs$BC==1,]
BC$year = str_extract(BC$narrative,"(?<=, )\\d{4}")
BC$ADD = grepl("addendum",BC$narrative)*1

### READ IN BUSINESS CASES AS TEXT 
bc = BC[BC$ADD!=1,]
bc = bc[grepl("\\.odt",bc$link),]						# non-addendum business cases
bctext  =NULL
for (i in bc$link){
	toadd = tryCatch(tolower(readtext(i)),error=function(e) "fail")
	bctext = c(bctext,toadd)
}


##----------------------------------------------------------------------
## CODE THAT SORTS OUT DATA FROM IATI

## MULTI DUMMY
## ASSUMING THAT IF ANY BI COMPONENT, IT IS ALL BI (AND MULTI COMPONENT SHOULD REALLY BE MULTI-BI)
lat        = collecttype
lat$parent = gsub("\\-\\d{3}$","",lat$id)
lat$multi  = (lat$ttype=="B02")*1
lat$multi = with(lat,ave(multi,parent,FUN=min))
lat = lat[!duplicated(lat$parent),]
lat[c("ttype","id","proj")] = NULL

## SORT BUDGETS
bud = collectbval
bud = bud[!is.na(bud$id),]
bud$parent = gsub("\\-\\d{3}$","",bud$id)
bud$budgetstart = unlist(bud$budgetstart)
bud$budgetstart = as.Date(bud$budgetstart)
bud$past20  = (bud$budgetstart>"2021-01-01")*1
bud$budto20 = ifelse(bud$past20==1,0,bud$bvalue)
bud$bvalue = as.numeric(bud$bvalue)
bud$budto20 = as.numeric(bud$budto20)
bud = as.data.frame(bud %>% group_by(parent) %>% summarize(budget=sum(bvalue/10^6),budtto20=sum(budto20/10^6)))

## SORT TRANSACTIONS
tran = collecttrns
tran = tran[!is.na(tran$id),]
tran$parent = gsub("\\-\\d{3}$","",tran$id)
tran$tvalue = as.numeric(tran$tvalue)
tran$date   = as.Date(tran$date)
tran = tran[tran$ttype %in% c(3,4,8),]	#remove commitments, loan/interest repayments
tran$spendto20 = ifelse(tran$date>"2021-01-01",0,tran$tvalue)
tran = as.data.frame(tran %>% group_by(parent) %>% summarize(spend=sum(tvalue/10^6),spendtto20=sum(spendto20/10^6)))



##----------------------------------------------------------------------------------------
## STUFF READ DIRECTLY FROM TEXT FILES DOWNLOADED ABOVE

### CODE THAT PULLS BCRS (WITH REGEX AND SOME MANUAL CHECKS ON REGEX RESULTS: MORE ADDED LATER (DATA OUTPUT: "BCRs")
source("BusinessCases-BCRs.R")
BCRs = BCRs[2:4]
names(BCRs) = c("parent","manualBcr","bcrR")

### CODE THAT PULLS PROG VALS FROM BCS (WITH REGEX)(DATA OUTPUT: "bcpv")
source("BusinessCases-ProgVals.R")
bcpv = bcpv[-1]
names(bcpv) = c("parent","BCprogvalR")
bcpv$BCprogvalR[bcpv$BCprogvalR>1000000] = bcpv$BCprogvalR[bcpv$BCprogvalR>1000000]/10^6 # nothing should be >1trn, misplaced "m"

### CODE THAT PULLS START YEAR FROM BCS (WITH REGEX)(DATA OUTPUT: "BCyear")
source("BusinessCases-StartDate.R")
BCyear = BCyear[!duplicated(BCyear),]
BCyear = BCyear[-1]
names(BCyear) = c("parent","BCyearR")
BCyear$BCyearR = as.numeric(BCyear$BCyearR)

### CODE THAT SORTS THE DATE INFORMATION FROM IATI (DATA OUTPUT: "dates")
source("IATI-Dates.R")

### CODE THAT SORTS THE DATE INFORMATION FROM IATI (DATA OUTPUT: "costdf")
source("CostExtensions.R")

##----------------------------------------------------------------------------------------
## STUFF ENTERED MANUALLY PREPARED IN SEPARATE SHEETS/EXTRACTED FROM AR XML FILES

### ANNUAL REVIEW STUFF NEEDS TO BE SORT SEPARATELY, READING IN HERE:
ar = read.csv(paste0(otherpath,"AR-analysis-CLEAN.csv"))
ar$val3 = ar$val3/10^6
names(ar) = c("parent","ReviewYear","ARyear","Ytoohigh","risk","score","ARprogval")

## BCRS FIRST BATCH MANUALLY CHECKED BY RANIL
bcrM	= read.csv("C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/For_BCR_search - with BCRs added.csv")
names(bcrM)[names(bcrM)=="BCR"] = "bcrM"
names(bcrM)[names(bcrM)=="NProgVal"] = "BCprogvalM1"
bcrM = bcrM[!duplicated(bcrM[c("parent","bcrM")]),]	# two duplicates but neither have any info
bcrM = bcrM[c("parent","bcrM","BCprogvalM1")]

## 'ALL REMAINING' (CONTAINS SOME MANUALLY CHECKED BCRS/PVS FROM BUSINESS CASES
allr = read.csv("C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/All_remaining-WITH-REGEX.csv")
allr = allr[!duplicated(allr),]
names(allr)[names(allr)=="ManualBCR"] = "bcrM2"
names(allr)[names(allr)=="ManualProgVal"] = "BCprogvalM2"


## FUNCTION THAT CHECKS IF PARENT DUPLICATED BUT ONLY ONE BCR (ETC) FILLED IN, AND THEN FILLS FOR BOTH
filler = function(x){
	if (length(unique(x[!is.na(x)]))>1)  {stop('more than one value')}
	if (length(x[!is.na(x)])==0) {return(x)}
	else {
		x[is.na(x)] = x[!is.na(x)][1]
	}
	return(x)
}	
allr[grep("Notes|SRO",names(allr))] = NULL
for (i in 3:ncol(allr)){
	allr[,i] = with(allr,ave(allr[,i],parent,FUN = filler))
}
allr = allr[!duplicated(allr),]
allr = allr[c("parent","bcrM2","BCprogvalM2","Checked")]
##---------------------------------------------------------------------------------------------------





# THE DATES DF HAS NEARLY COMPLETE SET OF PROJECT CODES, WHETHER THEY HAVE DOCUMENTS, WHETHER THEY WERE CORRECTLY DOWNLOADED ETC
# IT IS EVERYTHING ON IATI FROM DFID, APART FROM A FEW IN THE AR THAT HAVE SINCE BEEN REMOVED BY IATI

allp = c(ar$parent,dates$parent)
allp = allp[!duplicated(allp)]
allp = data.frame("parent"=allp)

## START TO MERGE EVERYTHING SO FAR (AFTER WILL READ IN AND MERGE THE MANUAL SEARCHES

allp = left_join(allp,bcpv,by="parent")
allp = left_join(allp,BCyear,by="parent")
allp = left_join(allp,BCRs,by="parent")
allp = left_join(allp,allr,by="parent")
allp = left_join(allp,bcrM,by="parent")

## SORT BCRS
allp$bcrM[is.na(allp$bcrM)] = allp$bcrM2[is.na(allp$bcrM)]
allp$BCR = allp$bcrM
allp$BCRmanual = ifelse(is.na(allp$BCR),0,1)
allp$BCR[is.na(allp$BCR)] = allp$bcrR[is.na(allp$BCR)] 
allp$BCRmanual[allp$manualBcr==1] = 1
allp[c("bcrR","bcrM","bcrM2","manualBcr")] = NULL

## SORT BC PROGRAMME VALUES
allp$BCprogvalM1[is.na(allp$BCprogvalM1)] = allp$BCprogvalM2[is.na(allp$BCprogvalM1)]
allp$BCprogval = allp$BCprogvalM1
allp$BCprogval[is.na(allp$BCprogval)] = allp$BCprogvalR[is.na(allp$BCprogval)]
allp[c("BCprogvalM1","BCprogvalM2","BCprogvalR")] = NULL

### MERGING ON AR DATA. BUT BECAUSE WE AREN'T REALLY USING AR SCORES CURRENTLY, MERGING ON DATA WITH JUST THE PV. 
ar  = ar[order(ar$parent,ar$ReviewYear),]
ar1 = ar[!duplicated(ar$parent),]

allp = left_join(allp,ar1[c("parent","ARyear","ARprogval")],by="parent")

### MERGING ON COST EXTENSIONS 
allp = left_join(allp,costdf,by="parent")

allp$ExtensionVal.2 = as.numeric(allp$ExtensionVal.2)
allp$ExtensionVal.3 = as.numeric(allp$ExtensionVal.3)
allp$ExtensionVal.4 = as.numeric(allp$ExtensionVal.4)
allp$ExtensionVal.1[is.na(allp$ExtensionVal.1)] = 0
allp$ExtensionVal.2[is.na(allp$ExtensionVal.2)] = 0
allp$ExtensionVal.3[is.na(allp$ExtensionVal.3)] = 0
allp$ExtensionVal.4[is.na(allp$ExtensionVal.4)] = 0
allp$year.1 = as.numeric(allp$year.1) 	## for checking years, if 1st extension year is before any start year, probably earlier start year correct

## ASSUMING THAT IF ORIGINAL VALUE ACCORDING TO CE < AR PROG VAL, THEN THE LATTER INCLUDES EXTENSIONS (WHETHER OR NOT THOSE EXT HAVE BEEN FOUND BY CODE)
allp$ARCEprogval = with(allp,ifelse(ARprogval>OriginalVal.1 & !is.na(ARprogval) & !is.na(OriginalVal.1),OriginalVal.1,ARprogval))

allp$ProgVal = allp$BCprogval
allp$ProgVal[is.na(allp$ProgVal)] = allp$ARCEprogval[is.na(allp$ProgVal)]
allp$ARCEprogval = NULL

## MERGING ON IATI DATES
allp = left_join(allp,dates,by="parent")
allp$StartyearIATI = as.numeric(allp$StartyearIATI)

# CREATED a flag to check those for which some sources says project started>2011, and some <2011
allp$maxyear = with(allp,sapply(1:nrow(allp),function(x) max(c(ARyear[x],BCyearR[x],StartyearIATI[x]),na.rm=T)))
allp$minyear = with(allp,sapply(1:nrow(allp),function(x) min(c(ARyear[x],BCyearR[x],StartyearIATI[x]),na.rm=T)))
allp$flag = with(allp,ifelse(!is.na(maxyear) & !is.na(minyear) & is.na(allp$BCyearR) & minyear<=2011 & maxyear>=2011 & minyear!=2011,1,0))

allp$StartYear = allp$BCyearR
allp$StartYear[is.na(allp$StartYear)] = allp$ARyear[is.na(allp$StartYear)]	#ARyear has better correlation with BCyearR than IATI year
allp$StartYear[is.na(allp$StartYear)] = allp$StartyearIATI[is.na(allp$StartYear)]
allp$postQAU = ifelse(allp$StartYear>=2011,1,0)


## MERGE ON LATERALITY, BUDGET, TRANSACTIONS
allp = left_join(allp,lat ,by="parent")
allp = left_join(allp,bud ,by="parent")
allp = left_join(allp,tran,by="parent")

## DIFFERENCE BETWEEN PROGRAMME VALUE AND SPEND 
allp$spenddiff = with(allp,spend - ProgVal) 	## we should create a dummy variable here for whether has cost extension
allp$spenddiffabs = with(allp,abs(spenddiff))


### MAKE A SEPARATE DATASET WITH AR INFORMATION
allpAR = left_join(allp,ar[c("parent","ReviewYear","Ytoohigh","risk","score")],by="parent")



### WRITE TO CSV (THIS IS COMMENTED OUT FOR NOW)

#write.csv(allp,paste0(otherpath,"MASTER-one-per-proj.csv"))
#write.csv(allpAR,paste0(otherpath,"MASTER-many-per-proj.csv"))


#################################################################################################################################

## AT THIS STAGE, DATA IS ALL COMPILED (EXCLUDING AR SCORES ETC NOT CURRENTLY USED). BELOW IS ANALYSIS

#################################################################################################################################

allp = read.csv(paste0(otherpath,"MASTER-one-per-proj.csv"))

## MCCRARY
DCdensity(allp[allp$postQAU==1 & allp$ProgVal<100,]$ProgVal,40)
DCdensity(allp[allp$postQAU==0 & allp$ProgVal<100,]$ProgVal,40)

## (PLACEBO)
DCdensity(allp[allp$postQAU==1 & allp$ProgVal<100,]$ProgVal,20)
DCdensity(allp[allp$postQAU==0 & allp$ProgVal<100,]$ProgVal,20)



## CJM
summary(rddensity(allp[allp$postQAU==1,]$ProgVal,40))
summary(rddensity(allp[allp$postQAU==0,]$ProgVal,40))

summary(rddensity(dat[dat$post=="Post 2011",]$ProgVal,20))
summary(rddensity(dat[dat$post=="Pre 2011",]$ProgVal,20))



## CREATING HISTOGRAM BY MANUALLY SPLITTING DATA BECAUSE COULDN'T GET TICK MARKS TO BE AFTER RATHER THAN MID DATA POINT (SO 40m LINE IN 
## WRONG PLACE)

spl = 1
df1 <-  transform(allp[allp$postQAU==1,], group=cut(ProgVal,breaks=seq(0,100,spl)))
df1 = df1[!is.na(df1$group),]
df1 = as.data.frame(df1 %>% group_by(group) %>% summarize(count=n()))
df1$type = "Post 2011"
df2 <-  transform(allp[allp$postQAU==0,], group=cut(ProgVal,breaks=seq(0,100,spl)))
df2 = df2[!is.na(df2$group),]
df2 = as.data.frame(df2 %>% group_by(group) %>% summarize(count=n()))
df2$type = "Pre 2011"

df = rbind(df1,df2)
df$pct = with(df,ave(count,type,FUN = function(x) x*100/sum(x)))
df = as.data.frame(df %>% complete(type,group))
df$group = rep(seq(spl,100,spl),2)
df[is.na(df)] = 0
df$group = df$group-spl/2
df$pct = df$pct/100

n1 = sum(df[df$type=="Post 2011",]$count)
n2 = sum(df[df$type=="Pre 2011",]$count)

df$type[df$type=="Post 2011"] = paste0(df$type[df$type=="Post 2011"]," (n=",n1,")")
df$type[df$type=="Pre 2011"]  = paste0(df$type[df$type=="Pre 2011"], " (n=",n2,")")

lvls = c("Pre 2011 (n=166)","Post 2011 (n=1645)")

ggplot(df) + geom_bar(aes(x=group,y=pct),width=spl,stat="identity",fill="grey60",color="grey30",
	position="dodge")+facet_wrap(~factor(type,levels=lvls))+
	geom_vline(xintercept=40,color="red",linetype="dashed")+
	#geom_vline(xintercept=5,color="red",linetype="dashed")+ 
	theme_bw() + scale_x_continuous(breaks=seq(0,100,10))+xlab("Programme Value")+ylab("")


###-----------------------------------------------------------------------------------------------------------------------

## EXPLORING COUNTERFACTUAL !!!! IN PROGRESS !!!!


## POLYNOMIAL OF DEGREE 4
post = df[grepl("Post 2011",df$type),]
post$group = post$group + spl/2
post$dum = (post$group%%5==0)*1
post$dum1 = (post$group %in% c(0,5,10,15,20,25,30,35,40))*1
post$dum2 = (post$group %in% c(45,50,55,60,65,70,75,80,85,90,95))*1

## POLYNOMIAL AND ONE DUMMY FOR ALL ROUND NUMBES
mod1 = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum,post)


## EXCLUDING THE REGION AROUND 40
lwr = 36
upr = 45
silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum+I(dum*group),post[!post$group %in% c(lwr:upr),])
gap = predict(silly,post[post$group %in% c(lwr:upr),])
post$fitpct = c(c(silly$fitted.values)[1:(lwr-1)],c(gap),c(silly$fitted.values)[lwr:(100+lwr-upr-1)])

#post$dum = (post$group%%5==0)*1
#post$dum[post$group==40] = 0

#silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum,post)
#silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4),post)

#post$fitpct = c(silly$fitted.values)

#names(post)[names(post)=="count"] = "num"

post$diff = with(post,pct/fitpct*100-100)

P5 = ggplot(post[post$group<70,]) + geom_line(aes(x=group,y=diff),color="black") + 
	geom_hline(yintercept=0,linetype="dashed")+theme_bw() + xlab("")+	
	ggtitle("Percent diff between obs in each group, and estimated obs according to counterfactual distribution")+
	scale_x_continuous(breaks=seq(0,100,5))
P5	



ggplot(post) + geom_line(aes(x=group,y=fitpct))+ geom_bar(aes(x=group,y=pct),stat="identity",alpha=0.5)























