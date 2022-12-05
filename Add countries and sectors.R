library(stringr)
library(readtext)


## SOMETHING HAS CHANGED SO THAT THIS HAS FEWERR OBS THAN SOMETHING PREVIOUSLY DOWNLOADED THE OTHER DAY
## SO USING LATTER BUT VERY ANNOYING
#iati = read.csv("https://datastore.codeforiati.org/api/1/access/activity.csv?reporting-org=GB-GOV-1&stream=True&ref=qb")

setwd("C:/Users/eritchie/Downloads/")	## BOTH FILE FROM RANIL AND PRE-DOWNLOADED IATI ACTIVITIES IN HERE
iatiD = read.csv("activity (1).csv")
allp = read.csv("MASTER-one-per-proj.csv")
allpM = read.csv("MASTER-many-per-proj.csv")

## MISSING 40 ACTIVITIES
missing = setdiff(allp$parent,iatiD$iati.identifier)
length(missing)

iatiD = iatiD[iatiD$iati.identifier %in% allp$parent,]	
iatiD$firstRec = gsub(";.*","",iatiD$recipient.country.code)	#MAIN COUNTRY (FIRST LISTED BIGGEST)
iatiD$firstSec = gsub(";.*","",iatiD$sector.code)			#MAIN SECTOR  (FIRST LISTED BIGGEST)
iatiD$firstReg = gsub(";.*","",iatiD$recipient.region)		#MAIN REGION (SOMETIMES LISTED INSTEAD OF COUNTRY)

names(iatiD)[names(iatiD)=="iati.identifier"] = "parent"

allpM = left_join(allpM,iatiD[c("parent","firstSec","firstRec","firstReg")],by="parent")
allpM$firstRec[allpM$firstRec=="" & !is.na(allpM$firstRec)] = allpM$firstReg[allpM$firstRec=="" & !is.na(allpM$firstRec)]
allpM$firstRec[allpM$parent %in% c("GB-1-113779","GB-1-114287","GB-1-114488")] = "AF" 	# FROM MANUALLY CHECKING


allp = left_join(allp,iatiD[c("parent","firstSec","firstRec","firstReg")],by="parent")
allp$firstRec[allp$firstRec=="" & !is.na(allp$firstRec)] = allp$firstReg[allp$firstRec=="" & !is.na(allp$firstRec)]
allp$firstRec[allp$parent %in% c("GB-1-113779","GB-1-114287","GB-1-114488")] = "AF"		# FROM MANUALLY CHECKING

allp$firstSecBroad = substr(allp$firstSec,1,3)


#----------------------------------------------------------------------------
## SEARCHING THROUGH ANNUAL REVIEWS DOWNLOADED FOR MISSING 40. FOUND 25

missing = paste0(missing,".odt")
setwd("C:/Users/eritchie/Downloads/devtracker_ar_latestodt/")
finder = NULL
for (i in missing[missing %in% list.files()]){
	txt = readtext(i)
	nme = i
	tmp = data.frame("file"=nme,"text"=txt)
	finder = rbind(finder,tmp)
}

## ADD RECIPIENT (MOST SEEM TO BE AFGHANISTAN WITH A FEW EXCEPTIONS)
finder$af = str_count(finder$text.text,"[Aa]fghanistan|[Aa]fghans")
finder$firstRec = NA
finder$firstRec[finder$af>4] = "AF"
finder$firstRec[finder$file=="GB-1-203454.odt"] = "MM"
finder$firstRec[finder$file=="GB-1-203778.odt"] = "Developing countries, unspecified"
finder$firstRec[finder$file=="GB-1-204661.odt"] = "MM"
finder$firstRec[finder$file=="GB-1-204672.odt"] = "MM"
finder$firstRec[finder$file=="GB-1-204792.odt"] = "UA"
finder$firstRec[finder$file=="GB-1-204793.odt"] = "UA"
finder$firstRec[finder$file=="GB-GOV-1-300291.odt"] = "Africa, regional"
finder$firstRec[finder$file=="GB-GOV-1-300561.odt"] = "UA"
finder$firstRec[finder$file=="GB-GOV-1-300250.odt"] = "UA"


## FOUND SECTORS MANUALLY
finder$firstSec=NA
finder$firstSec[finder$file=="GB-1-113779.odt"] = NA
finder$firstSec[finder$file=="GB-1-114287.odt"] = 23630
finder$firstSec[finder$file=="GB-1-114488.odt"] = NA
finder$firstSec[finder$file=="GB-1-201935.odt"] = 15142
finder$firstSec[finder$file=="GB-1-202042.odt"] = 31120
finder$firstSec[finder$file=="GB-1-202109.odt"] = 15130
finder$firstSec[finder$file=="GB-1-202297.odt"] = 43010
finder$firstSec[finder$file=="GB-1-202733.odt"] = 21020
finder$firstSec[finder$file=="GB-1-203180.odt"] = 43010
finder$firstSec[finder$file=="GB-1-203454.odt"] = 15150
finder$firstSec[finder$file=="GB-1-203501.odt"] = 15170
finder$firstSec[finder$file=="GB-1-203629.odt"] = 15130
finder$firstSec[finder$file=="GB-1-203748.odt"] = 15250
finder$firstSec[finder$file=="GB-1-203778.odt"] = 43010
finder$firstSec[finder$file=="GB-1-203904.odt"] = NA
finder$firstSec[finder$file=="GB-1-203951.odt"] = NA
finder$firstSec[finder$file=="GB-1-204122.odt"] = NA
finder$firstSec[finder$file=="GB-1-204260.odt"] = 74020
finder$firstSec[finder$file=="GB-1-204394.odt"] = NA
finder$firstSec[finder$file=="GB-1-204661.odt"] = NA
finder$firstSec[finder$file=="GB-1-204672.odt"] = 25030
finder$firstSec[finder$file=="GB-1-204792.odt"] = 15113
finder$firstSec[finder$file=="GB-1-204793.odt"] = 72010
finder$firstSec[finder$file=="GB-1-204952.odt"] = 15130
finder$firstSec[finder$file=="GB-GOV-1-300291.odt"] = 15113
finder$firstSec[finder$file=="GB-GOV-1-300505.odt"] = 11120
finder$firstSec[finder$file=="GB-GOV-1-300561.odt"] = 73010
finder$firstSec[finder$file=="GB-GOV-1-300250.odt"] = 72010


##SOME MORE FOR BROAD SECTORS
finder$firstSecBroad=NA
finder$firstSecBroad[finder$file=="GB-1-113779.odt"] = 240
finder$firstSecBroad[finder$file=="GB-1-114287.odt"] = 236
finder$firstSecBroad[finder$file=="GB-1-114488.odt"] = 311
finder$firstSecBroad[finder$file=="GB-1-201935.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-202042.odt"] = 311
finder$firstSecBroad[finder$file=="GB-1-202109.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-202297.odt"] = 430
finder$firstSecBroad[finder$file=="GB-1-202733.odt"] = 210
finder$firstSecBroad[finder$file=="GB-1-203180.odt"] = 430
finder$firstSecBroad[finder$file=="GB-1-203454.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-203501.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-203629.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-203748.odt"] = 152
finder$firstSecBroad[finder$file=="GB-1-203778.odt"] = 430
finder$firstSecBroad[finder$file=="GB-1-203904.odt"] = 720
finder$firstSecBroad[finder$file=="GB-1-203951.odt"] = 250
finder$firstSecBroad[finder$file=="GB-1-204122.odt"] = 311
finder$firstSecBroad[finder$file=="GB-1-204260.odt"] = 740
finder$firstSecBroad[finder$file=="GB-1-204394.odt"] = 720
finder$firstSecBroad[finder$file=="GB-1-204661.odt"] = 152
finder$firstSecBroad[finder$file=="GB-1-204672.odt"] = 250
finder$firstSecBroad[finder$file=="GB-1-204792.odt"] = 151
finder$firstSecBroad[finder$file=="GB-1-204793.odt"] = 720
finder$firstSecBroad[finder$file=="GB-1-204952.odt"] = 151
finder$firstSecBroad[finder$file=="GB-GOV-1-300291.odt"] = 151
finder$firstSecBroad[finder$file=="GB-GOV-1-300505.odt"] = 111
finder$firstSecBroad[finder$file=="GB-GOV-1-300561.odt"] = 730
finder$firstSecBroad[finder$file=="GB-GOV-1-300250.odt"] = 720


finder = finder[c("file","firstRec","firstSec","firstSecBroad")]
finder$file = gsub("\\.odt","",finder$file)
names(finder)[names(finder)=="file"] = "parent"
names(finder)[2:4] = paste0(names(finder)[2:4],"M")

#----------------------------------------------------------------------------


allp = left_join(allp,finder,by="parent")
allp$firstRec[is.na(allp$firstRec)] = allp$firstRecM[is.na(allp$firstRec)]
allp$firstSec[is.na(allp$firstSec)] = allp$firstSecM[is.na(allp$firstSec)]
allp$firstSecBroad[is.na(allp$firstSecBroad)] = allp$firstSecBroadM[is.na(allp$firstSecBroad)]
allp[c("firstSecM","firstRecM","firstSecBroadM")] = NULL
allp$firstRec[allp$parent=="GB-1-114276"] = "NA"

### FROM DIFFERENT FOLDER
allp$firstRec[allp$parent=="GB-1-204973"] = "UA"
allp$firstRec[allp$parent=="GB-GOV-1-300561"] = "UA"
allp$firstRec[allp$parent=="GB-GOV-1-300250"] = "UA"

allp$firstSec[allp$parent=="GB-1-204973"] = 72010
allp$firstSec[allp$parent=="GB-GOV-1-300561"] = 73010
allp$firstSec[allp$parent=="GB-GOV-1-300250"] = 72010

allp$firstSecBroad[allp$parent=="GB-1-204973"] = 720
allp$firstSecBroad[allp$parent=="GB-GOV-1-300561"] = 730
allp$firstSecBroad[allp$parent=="GB-GOV-1-300250"] = 720



missing = allp[is.na(allp$firstRec),]$parent
allp[allp$parent %in% missing,]

setwd("C:/Users/eritchie/Downloads/")
write.csv(allp,"MASTER-one-per-proj-REC-SEC.csv",row.names=F)
write.csv(allpM,"MASTER-many-per-proj-REC-SEC.csv",row.names=F)










setwd("C:/Users/eritchie/Downloads/")


test1  = read.csv("MASTER-one-per-proj.csv")
test11 = read.csv("MASTER-one-per-proj-REC-SEC.csv")

test2  = read.csv("MASTER-many-per-proj.csv")
test22 = read.csv("MASTER-many-per-proj-REC-SEC.csv")

all.equal(test1,test11[,1:40])
all.equal(test2,test22[,1:44])

allpM = read.csv("MASTER-many-per-proj.csv")

missing = allp[is.na(allp$firstRec),]$parent


missing %in% budgetsr$parent

