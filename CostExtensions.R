###########################################################################################

#### COST EXTENSIONS FROM ADDENDUMS TO BUSINESS CASES

###########################################################################################

addendum = BC[BC$ADD==1,]
addendum = addendum[grepl("\\.odt",addendum$link),] # Select only odt ones (code below won't work on others)

##### SELECT ADDENDUMS THAT CONTAIN THE PHRASE "COST EXTENSION"
dum =NULL
for (i in addendum$link){
	toadd = tryCatch((!is.na(str_extract(tolower(readtext(i)),"cost ?extension")))*1,error=function(e) "fail")
	if(length(toadd)>1) {break}
	dum = c(dum,toadd)
}
addendum$costE = dum
coste = addendum[addendum$costE==1,]


### FIND TEXT THAT PERTAINS TO EXTENSION VALUE (OCCURS RIGHT AT FRONT OF DOC USUALLY) AND EXTRACT THE NUMBER
costtext = sapply(coste$link,
	FUN = function(x) str_extract(tolower(readtext(x)),"cost ?extension ?value(.|\n|\t){50}"))

costtext = gsub("cost ?extension ?value( \\(if ?applicable\\))?","",costtext)
costtext = gsub("ifapplicable)","",costtext)
costtext =  sub("\n","",costtext)
costtext = gsub("\\(if applicable\\)","",costtext)
costtext = gsub("\n(.|\n)*","",costtext)
costdf = data.frame("link"=names(costtext),"text"=costtext)
rownames(costdf) = NULL
costdf$text = gsub(",","",costdf$text)
costdf$text = gsub(" ?million","m",costdf$text)
costdf$text = gsub("£","",costdf$text)
costdf$text = gsub("","",costdf$text)
costdf$text = paste0(" ",costdf$text)
costdf$text = sub("[^0-9\\.]+(?=[0-9])","",costdf$text,perl=T)
costdf$text = sub(" (?=m)","",costdf$text,perl=T)
costdf$text = gsub("(?<![0-9])m","",costdf$text,perl=T)
costdf$text = gsub("(\\(| ).*","",costdf$text)  ######
costdf$text = gsub("[^0-9m\\.]","",costdf$text,perl=T)
costdf$text = gsub("m.*","m",costdf$text,perl=T)
costdf$text = gsub("^2019","",costdf$text)
costdf$extval = as.numeric(gsub("[^0-9\\.]","",costdf$text))
costdf$extval = with(costdf,ifelse(grepl("m",text),extval*10^6,extval))

costdf = left_join(costdf,coste[c("link","parent")],by="link")

costdf$extval[costdf$link=="http://iati.fcdo.gov.uk/iati_documents/54103956.odt"] = 38808000
costdf$extval[costdf$link=="http://iati.fcdo.gov.uk/iati_documents/61887576.odt"] = 62100000
costdf = costdf[order(-costdf$extval),]
costdf = costdf[c("parent","extval","link")]
costdf = na.omit(costdf)

### NOW ADD ON ORIGINAL PROJECT VALUE IF POSSIBLE

costdf$orig = sapply(costdf$link,
	FUN = function(x) str_extract(tolower(readtext(x)),"(current|original|previous) ?(dfid )? ?(project|programme) ?(value|budget)(.|\n|\t){80}"))
costdf$remove = str_extract(costdf$orig,".*\n")
costdf$orig   = with(costdf,sapply(1:nrow(costdf),FUN = function(x) gsub(remove[x],"",orig[x])))
costdf$remove = NULL
costdf$Sub    = str_extract(costdf$orig,".*")
costdf$findno = str_count(costdf$Sub,"[0-9\\.,£]+")
costdf$orig[costdf$findno==0] = sub("original[^\n]*\n","",costdf$orig[costdf$findno==0])
costdf$Sub    = str_extract(costdf$orig,".*")
costdf$Sub    = gsub("\\(.*\\)","",costdf$Sub)
costdf$Sub    = gsub("( |,)","",costdf$Sub)
costdf$Sub    = gsub("million","m",costdf$Sub)
costdf$Sub    = gsub("\\.$","",costdf$Sub)
costdf$Sub    = gsub("(over3years|phase1)","",costdf$Sub)
costdf$findno = str_count(costdf$Sub,"[0-9\\.,£]+")
costdf$Sub    = ifelse(costdf$findno>1,gsub(".*:","",costdf$Sub),costdf$Sub)
costdf$num    = str_extract(costdf$Sub,"£?[0-9\\.]+m?")
costdf$num    = gsub("£","",costdf$num)
costdf$mill   = grepl("m",costdf$num)*1
costdf$num    = as.numeric(gsub("m","",costdf$num))
costdf$num    = ifelse(costdf$mill==1,costdf$num*10^6,costdf$num)
costdf[c("mill","findno","Sub")] = NULL

## A FEW MANUAL CHECKS/ADJUSTMENTS NEEDED
costdf$num[costdf$parent=="GB-1-202211"] = 120*10^6			#missing "million"
costdf$num[costdf$parent=="GB-1-204133"] = 995000			#missing "thousand"
costdf$num[costdf$parent=="GB-1-205183"] = 1.1*10^9
costdf$num[costdf$num<100] = costdf$num[costdf$num<100]*10^6	#missing "billion"
costdf$num[costdf$num>10^12] = costdf$num[costdf$num>10^12]/10^6  #"million" stated when it clearly shouldn't have been
## next is bit more complicated: basically there were three components and only first picked up
costdf$num[costdf$parent %in% c("GB-1-202814","GB-1-202820","GB-1-202830","GB-1-202834")]=771850775
costdf$orig = NULL
costdf$sum = with(costdf,extval+num)
costdf = costdf[c("parent","link","num","extval","sum")]
names(costdf) = c("parent","link","OriginalVal","ExtensionVal","sum")


### IT IS NOT CLEAR WHETHER SOME COST EXTENSIONS REFER TO THE INCREMENTAL, OR TOTAL COST
### THIS TRIES TO RESOLVE THAT, BY 1) ELIMINATING IMPLAUSIBLE/IRRELEVANT VALUES 2) TEXT SEARCH FOR MORE INFO
costdf$pct = with(costdf,sum/OriginalVal*100-100)

addreg = "(total )?additional(\n|.){0,10}£?[0-9]+[\\.,0-9]* ?m?" 

costdf$check = sapply(costdf$link,
	FUN = function(x) str_extract(tolower(readtext(x)),addreg))
costdf$check = gsub("(total|additional|£|\n|,|grant|fund(ing)?|\\.$|^\\.)","",costdf$check)
costdf$check = gsub("[^0-9\\.m]","",costdf$check)
costdf$checkmil = grepl("m",costdf$check)*1
costdf$check = as.numeric(gsub("m","",costdf$check))
costdf$check = ifelse(costdf$checkmil==1,costdf$check*10^6,costdf$check); costdf$checkmil = NULL

costdf$check2 = (costdf$check==costdf$ExtensionVal)*1
costdf$check3 = with(costdf,check==(ExtensionVal-OriginalVal)*1)
costdf$newval = with(costdf,ExtensionVal-OriginalVal)
costdf$ExtensionVal[costdf$check3==T & !is.na(costdf$check3)] = costdf$newval[costdf$check3==T & !is.na(costdf$check3)]
costdf[c("check3","newval")] = NULL
costdf$sum = with(costdf,ExtensionVal+OriginalVal)
costdf$pct = with(costdf,sum/OriginalVal*100-100)

## AT THIS STAGE:
## There are 456 cost extensions, some of which might be the total, rather than incremental value. 
## There are 54 that have a higher cost extension than original value (if less, must mean incremental, not total)
## For 8 of these, a text search suggested that the CE was incremental (leaving 38). 
## For 28 of these, the sum of original value and cost extension was less than 40m, meaning we don't care whether total or incremental
## Also, if the difference between original and CE is >40m, then it doesn't matter whether incremental or total
##
## We also want to check those where CE>40m and CE>original value, but these are all included in the ten
costdf$MANUAL = (costdf$pct>100 & (costdf$check2!=1 | is.na(costdf$check2)) & costdf$sum>=40*10^6)*1
costdf$MANUAL[(costdf$ExtensionVal-costdf$OriginalVal)>40000000] = 0

## GB-1-204564:     don't change
## GB-GOV-1-300113: CE is total: change
## GB-1-204270: 	  don't change
## GB-1-205231:     don't change
## GB-1-203808:	  don't change (It is incremental, but need to think about dates: in this case it is extending project)
## GB-1-205161:	  don't change
## GB-1-205268:	  don't change (been extended many many times)
## GB-1-204564:	  don't change
## GB-1-204322:	  CE is total: change

costdf$ExtensionVal[costdf$parent %in% c("GB-GOV-1-300113","GB-1-204322")] = 
	with(costdf[costdf$parent %in% c("GB-GOV-1-300113","GB-1-204322"),],ExtensionVal-OriginalVal)

costdf$sum = with(costdf,ExtensionVal+OriginalVal) # recalculate
costdf[c("check","pct","check2","MANUAL")] = NULL

costdf = left_join(costdf,addendum[c("narrative","link")],by="link")
costdf$year = str_extract(costdf$narrative,"(?<=, )\\d{4}")

for (i in 3:5){ costdf[,i] = as.numeric(costdf[,i])/10^6}


## BECAUSE SOME PROJECTS HAVE MULTIPLE EXTENSIONS, THIS NEEDS RESHAPING
costdf = costdf[-2]
costdf = costdf[!duplicated(costdf[-c(5,6)]),]
costdf = costdf[order(costdf$parent,costdf$OriginalVal,costdf$sum),]
costdf$id = with(costdf,ave(narrative,parent,FUN = seq_along))
costdf = costdf[-c(4,5)]
costdf = reshape(costdf,direction="wide",idvar="parent",timevar="id")
costdf[is.na(costdf)] = ""
costdf = costdf[!grepl("5|6",names(costdf))] ## this is justifiable because no extensions after the fourth make a different to threshold and columns are pain in arse

rm(addendum,dum,i,toadd,coste,costtext)

write.csv(costdf,"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/DFID-Cost-extensions.csv",row.names=F)







