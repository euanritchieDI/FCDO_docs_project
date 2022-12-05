library(data.table)
library(stringr)
library(dplyr)
library(openxlsx)
library(xml2)
library(readtext)


setwd("C:/Users/eritchie/Downloads/devtracker_ar_latestodt/")
filelist = list.files()
projects = gsub("\\.odt","",filelist)

searchterm = "[Pp]rogramme ? ?[Ss]core|(Overall|Aggregate)? ?[Oo]utput ?[Ss]core|Project ?[Ss]core"


fails2 = NULL
collectdf = NULL
for (i in 1:length(filelist)) {
#for (i in 1) {		
	f = filelist[i]
 	tmpd <- tempdir()
  	tmpf <- tempfile(tmpdir=tmpd, fileext=".zip")
	file.copy(f, tmpf)
	tryCatch(unzip(tmpf, exdir=paste0(tmpd,"/",projects[i])), error=function(e) {})           #sprintf("%s/docdata", tmpd))
	doc = tryCatch(read_xml(paste0(tmpd,"/",projects[i],"/content.xml")),error=function(e) filelist[i])   				

	unlink(tmpf)
	unlink(paste0(tmpd,"/",projects[i])) 		                       					#sprintf("%s/docdata", tmpd), recursive=TRUE)
	print(filelist[i])
	if (!doc[1]==filelist[i]){
		tble = doc %>% xml_find_all("//table:table")
		ind  = grepl(paste0("(Year.*(",searchterm,").*Risk ?[Rr]ating)"),tble %>% xml_text)
		ind = which(ind)[1]
		vtble = xml_text(xml_find_all(doc,".//table:table-cell"))
		val   = vtble[grep("[Pp]rogramme ? ?[Vv]alue",vtble)]
		if(!is.na(ind)){
			dat  = tble[ind] %>% xml_children %>% xml_children %>% xml_text
			yearloc = grep("[Yy]ear",dat)[1]
			dat[yearloc] = "year"
			progloc = grep(searchterm,dat)[1]
			riskloc = grep("[Rr]isk",dat)[1]
			rows    = progloc-yearloc-1
			years   = dat[(yearloc+1):(yearloc+rows)]
			progs   = dat[(progloc+1):(progloc+rows)]
			risks   = dat[(riskloc+1):(riskloc+rows)]
			dfs = data.frame("year"=years,"score"=progs,"risk"=risks)
		} else { 
			dfs = data.frame("year"="notable","score"="notable","risk"="notable")
		}
		dfs$value  = ifelse(length(val)==0,"notfound",val)
		dfs$proj   = filelist[i]
		collectdf = rbind(collectdf,dfs)
	} else {
		fails2 = c(fails2,doc)
	}
}
rm(dfs,years,progs,rows,risks,riskloc,progloc,dat,yearloc,tble,val,vtble,ind,i)

#THERE ARE THREE PROJECTS WITH NO INFO IN THE TABLE THAT THIS GETS RID OF
collectdf = collectdf[!is.na(collectdf$proj),]
PRESERVE = collectdf


### THIS FILLS IN SOME GAPS - PROJECTS THAT HAVE AR BUT FOR WHICH THE FRONT TABLE FILLED IN DIFFERENT
### THESE ARE ALL 5M AND ABOVE. THERE ARE A FEW OTHERS BELOW 5m BUT LIFE IS TOO SHORT

gb202939 = data.frame("proj"="GB-1-202939.odt",
			    "year"=2020:2013,
			    "score"="A",
			    "risk"="medium",
			    "value"=157900000)
gb203804 = data.frame("proj"="GB-1-203804.odt",
			    "year"=2018:2014,
			    "score"=c("A+","A++","A+","A","A"),
			    "risk"="missing",
			    "value"=38897334)
gb300124 = data.frame("proj"="GB-GOV-1-300124.odt",
			    "year"=2020:2019,
			    "score"="A",
			    "risk"="medium",
			    "value"=24650000)
gb300714 = data.frame("proj"="GB-GOV-1-300714.odt",
			    "year"=2020,
			    "score"="A",
			    "risk"="missing",
			    "value"=30000000)
gb300239 = data.frame("proj"="GB-GOV-1-300239.odt",
			    "year"=2019:2017,
			    "score"=c("A","A","B"),
			    "risk"=rep("severe",3),
			    "value"=93000000)
gb300815 = data.frame("proj"="GB-GOV-1-300815.odt",
			    "year"=2020,
			    "score"="A+",
			    "risk"="medium",
			    "value"=19800000)
gb300550 = data.frame("proj"="GB-GOV-1-300550.odt",
			    "year"=2020,
			    "score"="A",
			    "risk"="medium",
			    "value"=16000000)
gb203292 = data.frame("proj"="GB-1-203292.odt",
			    "year"=2019:2012,
			    "score"=c("A","A+","A+","A+","A","A+","A+","A"),
			    "risk"=c(rep("medium",5),rep("low",3)),
			    "value"=39636308)
gb300059 = data.frame("proj"="GB-GOV-1-300059.odt",
			    "year"=2020:2017,
			    "score"="A",
			    "risk"=c("severe","medium","medium","severe"),
			    "value"=106000000)

collectdf = rbind(collectdf,gb202939,gb203804,gb300124,gb300714,gb300239,gb300815,gb300550,gb203292,gb300059)
rm(gb202939,gb203804,gb300124,gb300714,gb300239,gb300815,gb300550,gb203292,gb300059)


# THE YEAR VALUE HAS BEEN FILLED OUT IN LOTS OF DIFFERENT WAYS IN THE ARS, AND IS A COMPLETE MESS. THIS ATTEMPTS TO SORT
collectdf$y2 = str_extract(collectdf$year,"\\d{4}")
collectdf$y2 = with(collectdf,ifelse(grepl("[0-9]{2}/[0-9]{2}",year) & is.na(y2),paste0("20",str_extract(year,"\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("[0-9]{2}-[0-9]{2}",year) & is.na(y2),paste0("20",str_extract(year,"\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("[A-Za-z]{3} [0-9]{2}",year) & is.na(y2),paste0("20",str_extract(year,"\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("[A-Za-z]{3}-[0-9]{2}",year) & is.na(y2),paste0("20",str_extract(year,"\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("FY[0-9]{2}",year) & is.na(y2),paste0("20",str_extract(year,"\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("\\d{2}/\\d{1}/\\d{2}",year) & is.na(y2),paste0("20",str_extract(year,"(?<=\\d/)\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("Year\\d{5}",year) & is.na(y2),str_extract(year,"(?<=Year\\d)\\d{4}"),y2))
collectdf$y2 = with(collectdf,ifelse(grepl("April\\d{2}",year) & is.na(y2),paste0("20",str_extract(year,"(?<=April)\\d{2}")),y2))
collectdf$y2 = with(collectdf,ifelse(y2>2100,str_extract(year,"(?<= \\d)[0-9]{4}"),y2))
collectdf$y2 = with(collectdf,ifelse(year=="table not present","notable",y2))
collectdf$y2 = with(collectdf,ifelse(is.na(y2),str_extract(year,"(?<=Year\\d)[0-9]{4}"),y2))

collectdf$y2[collectdf$year=="notable"] = "notable"

###  AD HOC ADJUSTMENTS FOR PROJECTS WITH YEARS LISTED BY NUMBER
collectdf[collectdf$proj=="GB-GOV-1-300855.odt" & collectdf$year==1,]$y2 = 2019
collectdf[collectdf$proj=="GB-GOV-1-300815.odt" & collectdf$year==1,]$y2 = 2019
collectdf[collectdf$proj=="GB-GOV-1-300788.odt" & collectdf$year==1,]$y2 = 2019
collectdf[collectdf$proj=="GB-GOV-1-300788.odt" & collectdf$year==2,]$y2 = 2020
collectdf[collectdf$proj=="GB-GOV-1-300760.odt" & collectdf$year==1,]$y2 = 2019


# AD HOC ADJUSTMENT FOR A PROJECT THAT HAS YEAR AND RISK/PROGRAMME SCORES MISALIGNED (NEED TO CHECK STILL APPLICABLE FOR FUTURE DLS)
collectdf[collectdf$proj=="GB-1-202992.odt",]$y2[1:6] = 2013:2018
collectdf = collectdf[!(collectdf$proj=="GB-1-202992.odt" & is.na(collectdf$risk)),]
collectdf = collectdf[!(collectdf$proj=="GB-1-202992.odt" & is.na(collectdf$y2)),]

## THIS PROJECT PICKS UP TWO WRONG OBSVERATIONS, GETTING RID OF THEM
collectdf = collectdf[!(collectdf$proj=="GB-1-205165.odt" & !collectdf$y2 %in% c(2018,2019)),]

collectdf$y2[grepl("Year ?12013/14",collectdf$year)] = 2013


### MERGE ON START YEAR ACCORDING TO ANNUAL REVIEW. SOME ARE HIGHER THAN REVIEW YEARS - PROJECTS BEING EXTENDED?
filelistred = setdiff(filelist,c("GB-1-203292.odt","GB-GOV-1-300656.odt"))	#these don't work, corrupted files or something
startdate = sapply(filelistred,function(x) str_extract(readtext(x),"[Ss]tart.{5,60}"))
starts    = data.frame("proj"=filelistred,"starts"=startdate)
starts$starts = gsub("Date review.*","",starts$starts)
starts$starts = gsub(".*: ?","",starts$starts)
starts$starts[!grepl("[0-9]",starts$starts)] = ""
starts$SYear = str_extract(starts$starts,"20[0-9]{2}")
starts$starts = gsub("\\.","/",starts$starts)
starts$starts = gsub("\\-","/",starts$starts)
starts$SYear[is.na(starts$SYear)]   = str_extract(starts$starts,"(?<=/.{1,10}/)[0-9]{2}")[is.na(starts$SYear)]
starts$SYear[is.na(starts$SYear)] = ""
starts$SYear[nchar(starts$SYear)==2] = paste0("20",starts$SYear[nchar(starts$SYear)==2])
starts = starts[c("proj","SYear")]

collectdf = left_join(collectdf,starts,by="proj")
collectdf$Ytoohigh = (collectdf$SYear>collectdf$y2)*1
collectdf$Ytoohigh = with(collectdf,ave(Ytoohigh,proj,FUN=function(x) max(x,na.rm=T)))
rm(starts,filelistred,startdate)



# WHAT FOLLOWS ARE SEVERAL LONG, TEDIOUS CHUNKS OF CODE THAT ATTEMPT TO MAKE THE RISK/SCORES CONSISTENT

collectdf$risk2 = trimws(tolower(collectdf$risk))
collectdf$risk2 = with(collectdf,gsub("moderate|\\bmed\\b|\\bmod\\b|\\bmid\\b","medium",risk2))
collectdf$risk2 = with(collectdf,gsub("\\bh\\b|major|maj\\b","high",risk2))
collectdf$risk2 = with(collectdf,gsub("\\bl\\b|\\bmin\\b|minor","low",risk2))
collectdf$risk2[grepl("2.*medium",collectdf$risk2)] = "medium"
collectdf$risk2 = with(collectdf,gsub("\\d.*","",risk2))
collectdf$risk2 = with(collectdf,gsub("\\(.*","",risk2))
terms = "(low|medium|high)"
collectdf$risk2 = with(collectdf,ifelse(grepl(paste0(terms,"[a-z'’]{10,}"),risk2),str_extract(risk2,terms),risk2))
collectdf$risk2[collectdf$risk2=="substantial"] = "high"
collectdf$risk2[collectdf$risk2=="substantive"] = "high"
collectdf$risk2 = with(collectdf,gsub(" ?- ?| to | ?/ ?|\\b \\b","/",risk2))
collectdf$risk2 = with(collectdf,gsub("\\bm\\b","medium",risk2)) # I think this is right, abbrev "m" is very unhelpful given use of "moderate", "major", "minor"
collectdf$risk2 = trimws(collectdf$risk2)
collectdf$risk2 = with(collectdf,gsub("medium..?$","medium",risk2))
collectdf$risk2[grepl("[^/]medium",collectdf$risk2)] = "medium"

collectdf$risk2[collectdf$risk2=="high/medium"] = "medium/high"
collectdf$risk2[grepl("medium/high",collectdf$risk2)] = "medium/high"
collectdf$risk2[collectdf$risk2=="medium/low"] = "low/medium"
collectdf$risk2[collectdf$risk2=="medium/medium"] = "medium"
collectdf$risk2[!grepl("medium",collectdf$risk2) & !grepl("severe",collectdf$risk2) & grepl("high",collectdf$risk2)] = "high"

preservedf = collectdf
collectdf = collectdf[!(collectdf$year=="" & collectdf$risk==""),]
collectdf = collectdf[!grepl("Programme ?Score",collectdf$year),]
collectdf = collectdf[!grepl("Overall ?Output",collectdf$year),]
collectdf = collectdf[collectdf$y2<2022,]

collectdf = na.omit(collectdf) 


collectdf$score2 = trimws(collectdf$score)
collectdf$score2[collectdf$score2=="[A+]"] = "A+"
collectdf$score2[collectdf$score2=="A*"] = "A+"
collectdf$score2[collectdf$score2=="A +"] = "A+"
collectdf$score2[substr(collectdf$score2,1,3)=="A+-"] = "A+-"
collectdf$score2[collectdf$score2=="A+ (tendency toward A)"] = "A+-"
collectdf$score2[grepl("A\\+[^\\+-]$",collectdf$score2)] = "A+"
collectdf$score2[grepl(".A\\+$",collectdf$score2)] = "A+"

collectdf$score2[collectdf$score2=="(A)"] = "A"
collectdf$score2[collectdf$score2=="A "] = "A"
collectdf$score2[collectdf$score2=="A -"] = "A-"
collectdf$score2[collectdf$score2=="A-B"] = "A-"
collectdf$score2[grepl("A,",collectdf$score2)] = "A"
collectdf$score2[grepl("A[^\\+-]$",collectdf$score2)] = "A"
collectdf$score2[grepl("A: ?O.*",collectdf$score2)] = "A"
collectdf$score2[grepl("A.outputs.*",collectdf$score2)] = "A"
collectdf$score2[grepl("A[0-9].*",collectdf$score2)] = "A"
collectdf$score2[grepl("AOutput.*",collectdf$score2)] = "A"
collectdf$score2[grepl("A ?\\(.*",collectdf$score2)] = "A"
collectdf$score2[grepl("[0-9]A",collectdf$score2)] = "A"
collectdf$score2[grepl("A ?\\(tendency towards B",collectdf$score2)] = "A-"
collectdf$score2[grepl("[Ll]ikely ?to ?be ?largely",collectdf$score2)] = "A-"

collectdf$score2[grepl("B: ?O.*",collectdf$score2)] = "B"
collectdf$score2[grepl("BOutput.*",collectdf$score2)] = "B"
collectdf$score2[grepl("B ?\\(.*",collectdf$score2)] = "B"
collectdf$score2[grepl("B[0-9].*",collectdf$score2)] = "B"
collectdf$score2[grepl("[Ll]ikely ?to ?be ?partially",collectdf$score2)] = "B"
collectdf$score2[grepl("B[^\\+-]$",collectdf$score2)] = "B"
collectdf$score2[grepl("C-output",collectdf$score2)] = "C"

collectdf$score2[grepl("(Na|NA|N/A|n/a|Too early|not scored)",collectdf$score2)] = "No score"

##-------------------------------------------------------
## THERE ARE STILL LOTS OF SCORES THAT ARE NUMERIC. THIS IS ANNOYING, BUT THEY ARE MAINLY FOR EARLIER YEARS SO LEAVING FOR NOW

preservedf = collectdf

pv = collectdf[!duplicated(collectdf$proj),c("proj","value")]
pv$value = gsub(":$","",pv$value)
pv$val2 = sub(".*?:","",pv$value)
pv$val2 = gsub(",","",pv$val2)
pv$val2 = gsub("[Uu]p ?to","",pv$val2)
pv$val2 = gsub("[Pp]rogramme ?[Vv]alue","",pv$val2)

expres  = "(£|GBP) ?[0-9\\.]+ ?(m|M)?"
pv$val2 = with(pv,ifelse(str_count(val2,expres)==1,str_extract(val2,expres),val2))

# THIS IS APPROXIMATE: WHEN VALUE DOESN'T CONTAIN REFERENCE TO DEPARTMENT, MORE THAN ONE NUMBER GENERALLY MEANS THAT THE PROJECT
# IS SPLIT BY TYPE OF ASSISTANCE (E.G. TA v FINANCIAL) AND FIRST NUMBER USUALLY TOTAL (UNLESS "TOTAL" INDICATES OTHERWISE)
expres2 = "(DFID|Department for|FCDO|UK|[Tt]otal)"
pv$val2 = with(pv,ifelse(str_count(val2,expres)>1 & str_count(val2,expres2)==0,str_extract(val2,expres),val2))

expres3 = "(£|GBP) ?[0-9\\.]+ ?(m|M)? ?from ?(DFID|FCDO|UK)"
pv$val2 = with(pv,ifelse(str_count(pv$val2,expres3)>0,str_extract(pv$val2,expres3),val2))

expres4 = "DFID ?([Cc]ontribution|funding)? ?(- )?(£|GBP) ?[0-9\\.]+ ?(m|M)?"
pv$val2 = with(pv,ifelse(str_count(val2,expres4)>0,str_extract(val2,expres4),val2))

expres5 = "(£|GBP) ?[0-9\\.]+ ?(m|M|million)? ?(as |– |– )?(DFID|Department for ?Internation)"
pv$val2 = with(pv,ifelse(str_count(pv$val2,expres5)>0,str_extract(pv$val2,expres5),val2))

expres6 = "(£|GBP) ?[0-9\\.,]+ ?(m|M|[Mm]illion)? ?\\(DFID ?([Cc]ontribution)?\\)"
pv$val2 = with(pv,ifelse(str_count(val2,expres6)>0,str_extract(val2,expres6),val2))

# AT THIS STAGE, INSPECTING THE REST, IT LOOKS LIKE THE FIRST NUMBER IS MOST RELEVENT ONE
pv$val2 = with(pv,ifelse(str_count(val2,expres)>1,str_extract(val2,expres),val2))
pv$val2 = gsub("\\(.*","",pv$val2)
pv$val2 = gsub("(£|GBP| )","",pv$val2)
pv$val2 = tolower(pv$val2)
pv$val2 = gsub("million","m",pv$val2)
pv$val2 = gsub("dfid","",pv$val2)
pv$val2 = gsub("contribution\\-?","",pv$val2)
pv$val2 = gsub("from","",pv$val2)
pv$val2 = gsub("[a-ln-z\\–]","",pv$val2)

pv[str_count(pv$val2,"/")>0,]$val2 = ""
pv$val2 = str_trim(pv$val2)
pv$val2 = ifelse(str_count(pv$val2,"\\.")>1,gsub("\\.","",pv$val2),pv$val2)
pv$val3 = ifelse(grepl("m",pv$val2),as.numeric(gsub("m","",pv$val2))*10^6,as.numeric(pv$val2))
pv$val3[pv$val3>10^10 & !is.na(pv$val3)] = pv$val3[pv$val3>10^10 & !is.na(pv$val3)]/10^6
pv$val3[grepl("billion",pv$value) & !is.na(pv$val3)] = pv$val3[grepl("billion",pv$value) & !is.na(pv$val3)]*10^9
pv$val3[pv$val3<100 & !is.na(pv$val3)] = pv$val3[pv$val3<100 & !is.na(pv$val3)]*10^6

## THIS USES SEPARATE CODE TO CHECK THROUGH OLDER AR DOCUMENTS TO TRY AND FILL GAPS
source("C:/Users/eritchie/OneDrive - Center for Global Development/IATI/missingPROGvalues.R")

pv = left_join(pv,collectall,by="proj")
pv$val3[is.na(pv$val3)] = pv$missing[is.na(pv$val3)]
pv$missing = NULL


# CHECKING SOME THAT SEEM OBVIOUSLY WRONG AGAINST THE AR DOCUMENTS/WEBSITE
# FIRST 6 WERE WRONG (IN ORDER OF SIZE). NEXT FEW WERE RIGHT SO STOPPING THERE
# POSSIBLE THAT SOME PROJECTS ARE OUT BY FACTOR OF 1000 (hundreds of thousands, not hundreds of millions) BUT HARDER TO IDENTIFY
pv = pv[c("proj","val3")]
pv$val3[pv$proj=="GB-1-204823.odt"] = pv$val3[pv$proj=="GB-1-204823.odt"]*1000
pv$val3[pv$proj=="GB-1-203445.odt"] = pv$val3[pv$proj=="GB-1-203445.odt"]*10^6
pv$val3[pv$proj=="GB-1-204180.odt"] = pv$val3[pv$proj=="GB-1-204180.odt"]*1000
pv$val3[pv$proj=="GB-1-203885.odt"] = pv$val3[pv$proj=="GB-1-203885.odt"]*1000 #website says this should be around 1.5m
pv$val3[pv$proj=="GB-1-202506.odt"] = 6541808 # space meant last three digits lost
pv$val3[pv$proj=="GB-1-204337.odt"] = pv$val3[pv$proj=="GB-1-204337.odt"]*1000 # a "." should have been a ","

preservedf = collectdf
collectdf = left_join(collectdf,pv,by="proj")
collectdf$val3[is.na(collectdf$val3)] = as.numeric(collectdf$value[is.na(collectdf$val3)]) # one of the manually added obs went wrong

rm(expres,expres2,expres3,expres4,expres5,expres6,f,fails,pv,pg,tmpd,tmpf,thename,thepath,terms,tble,searchterm,scor,risk,ptitle,
   i,j,link,lnk,progindex,year,yearindex,riskindex,filelist,files,value,diff,doc,doccount,docnodes)


#-----------------------------------------------------------
## SOME 'ANNUAL REVIEWS' ARE ACTUAL PROJECT COMPLETION REVIEWS. AS SUCH, DIFFERENT FORMAT FOR PROG VALUES SO THIS TRYING TO FILL GAPS
wrongname = unique(collectdf[is.na(collectdf$val3) | 
				     grepl("Original ?([Pp]rogramme|[Pp]roject) ?[Vv]alue",collectdf$value),]$proj)

setwd("C:/Users/eritchie/Downloads/devtracker_ar_latestodt/")
orig = sapply(wrongname,function(x) str_extract(readtext(x),"(?<=Original ?([Pp]rogramme|[Pp]roject) ?[Vv]alue):?\n?.{0,30}"))
orig = gsub("(,|£|\n)","",orig)
orig = gsub(" ?\\(.*","",orig)
orig = gsub(" m","m",orig)
orig = gsub("(illion|GBP)","",orig)
orig = gsub(".*: ","",orig)
orig = gsub("of which","",orig)
orig = gsub("(PSD - |Tranche 1-|:)","",orig)
orig = gsub(" - .*","",orig)
orig = ifelse(grepl("[Mm]",orig),as.numeric(gsub("[Mm]","",orig))*10^6,as.numeric(orig))

collectdf$val3[collectdf$proj %in% wrongname] = orig[match(collectdf$proj[collectdf$proj %in% wrongname],wrongname)]
rm(orig,wrongname)

#---------------------------------------------------------

collectdf$proj = gsub("\\.odt","",collectdf$proj)
names(collectdf)[names(collectdf)=="proj"] = "parent"


write.csv(collectdf,"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/AR-analysis.csv",row.names=F)

arred = collectdf[5:11]
arred = arred[arred$score %in% c("A++","A+","A+-","A","A-","B","C"),]
arred = arred[grepl("low|medium|high|severe",arred$risk2),]
arred = arred[order(arred$id,-as.numeric(arred$y2)),]

names(arred) = c("parent","year","Syear","Ytoohigh","risk","score","progval")

write.csv(arred,"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/AR-analysis-CLEAN.csv",row.names=F)


###############################################################################

### EVERYTHING ONWARDS IS UNUSED, CAN'T REMEMBER WHAT IT IS FOR





## RUN FCDO-IATI FIRST
collectdate$proj = NULL
collectstat$proj = NULL

collectdfj = left_join(collectdf,collectinfo[!duplicated(collectinfo$id),],by="id")
collectdfj = left_join(collectdfj,collectdate[!duplicated(collectdate$id),],by="id")
collectdfj = left_join(collectdfj,collectstat[!duplicated(collectstat$id),],by="id")

write.csv(collectdfj,"C:/Users/eritchie/Documents/ARnew-analysis.csv",row.names=F)


collectdfj = left_join(collectdfj,allt,by="id")


## GIVE AR SCORES A NUMERIC VALUE
nums = data.frame("score2"=c("A++","A+","A+-","A","A-","B","C"),numscore=7:1)
collectdfj = full_join(collectdfj,nums,by="score2")
collectdfj$counter  = with(collectdfj,ave(y2,id,FUN = seq_along))
collectdfj$maxcount = with(collectdfj,ave(counter,id,FUN = max))
collectdfj$avescore = with(collectdfj,ave(numscore,id,FUN = mean))
collectdfj = collectdfj %>%  mutate(lagrisk = lag(risk2), diffscore = numscore - lag(numscore))
collectdfj$negchange = ifelse(collectdfj$diffscore<0,1,0)
collectdfj$risksimple = collectdfj$risk2
collectdfj$risksimple[grepl("low",collectdfj$risksimple)] = "low"
collectdfj$risksimple[grepl("high|severe",collectdfj$risksimple)] = "high"

write.csv(collectdfj,"C:/Users/eritchie/Documents/ARnew-analysis.csv",row.names=F)




