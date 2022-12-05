########################################################################################

## BUSINESS CASES: PROGRAMME VALUES 
## (this had been in separate code, but only applied to "FullDataset" so misses some projects without ARs)

########################################################################################



bcdf = data.frame("link"=bc$link,"text"=bctext)				
rownames(bcdf) = NULL
bcdf = left_join(bcdf,bc[c("link","parent")],by="link")


num = "[£$0-9,]{1,12}\\.?[0-9]{0,2} ?m?"

pv1 = "programme ?value ?:? ?"
pv2 = "the ?(uk|united ?kingdom) ?will ?(provide|allocate|contribute) ?(up ?to ?)?"
pv3 = "(department ?for ?international ?development ?\\(dfid\\)?|dfid) ?will ?(provide|allocate) ?(up ?to ?)?"
 
bcdf$text = gsub(",","",bcdf$text)

bcdf$pv1 = str_extract(bcdf$text,paste0(pv1,num))
bcdf$pv2 = str_extract(bcdf$text,paste0(pv2,num))
bcdf$pv3 = str_extract(bcdf$text,paste0(pv3,num))

# REMOVE STUB
bcdf$pv1 = gsub(pv1,"",bcdf$pv1)
bcdf$pv2 = gsub(pv2,"",bcdf$pv2)
bcdf$pv3 = gsub(pv3,"",bcdf$pv3) 

#REMOVE SPACE
bcdf$pv1 = gsub(" ","",bcdf$pv1)
bcdf$pv2 = gsub(" ","",bcdf$pv2)
bcdf$pv3 = gsub(" ","",bcdf$pv3) 


strip = function(x){
	m = ifelse(str_detect(x,"m"),10^6,1)
	d = ifelse(str_detect(x,"\\$"),0.7,1)
	y = as.numeric(gsub("[^0-9\\.]","",x))
	y = y*m*d
	return(y)
}

bcdf$pv1n = sapply(bcdf$pv1,strip)
bcdf$pv2n = sapply(bcdf$pv2,strip)
bcdf$pv3n = sapply(bcdf$pv3,strip)	

bcdf$pv1n[bcdf$pv1n<1000 & !is.na(bcdf$pv1n)] = bcdf$pv1n[bcdf$pv1n<1000 & !is.na(bcdf$pv1n)]*10^6
bcdf$pv2n[bcdf$pv2n<1000 & !is.na(bcdf$pv2n)] = bcdf$pv2n[bcdf$pv2n<1000 & !is.na(bcdf$pv2n)]*10^6
bcdf$pv3n[bcdf$pv3n<1000 & !is.na(bcdf$pv3n)] = bcdf$pv3n[bcdf$pv3n<1000 & !is.na(bcdf$pv3n)]*10^6


bcdf$flag1 = (!is.na(bcdf$pv1n) & !is.na(bcdf$pv2n) & bcdf$pv1n!=bcdf$pv2n)*1

# DIFFERENCES
#GB-GOV-1-300257: pv1 correct
#GB-GOV-1-300686: pv1 correct
#GB-GOV-1-300587: pv2 correct
#GB-1-204809:	pv2 correct
#GB-1-204854:	pv1 correct
#GB-GOV-1-300163:	pv2 correct
#GB-1-202737:	pv1 correct
#GB-GOV-1-300695: Says "around 200m at current e rates"

#Will just change so that pv1 is main variable
bcdf$pv1n[bcdf$parent=="GB-GOV-1-300587"] = bcdf$pv2n[bcdf$parent=="GB-GOV-1-300587"]
bcdf$pv1n[bcdf$parent=="GB-1-204809"] 	= bcdf$pv2n[bcdf$parent=="GB-1-204809"]
bcdf$pv1n[bcdf$parent=="GB-GOV-1-300163"] = bcdf$pv2n[bcdf$parent=="GB-GOV-1-300163"]
bcdf$pv1n[bcdf$parent=="GB-GOV-1-300695"] = 200*10^6

bcdf$flag2 = (!is.na(bcdf$pv1n) & !is.na(bcdf$pv3n) & bcdf$pv1n!=bcdf$pv3n)*1

#GB-GOV-1-300418: use pv3
#GB-GOV-1-300280: use pv1
#GB-GOV-1-300379: use pvl
#GB-GOV-1-300561: use pv1
#GB-1-204695:	use pv1
#GB-1-204954:	use pv1
#GB-GOV-1-300262: use pv1
#GB-GOV-1-300395: use pv1
#GB-GOV-1-300632: use pv1

bcdf$pv1n[bcdf$parent=="GB-GOV-1-300418"] = bcdf$pv3n[bcdf$parent=="GB-GOV-1-300418"]


bcdf$flag3 = (!is.na(bcdf$pv2n) & !is.na(bcdf$pv3n) & bcdf$pv2n!=bcdf$pv3n & is.na(bcdf$pv1))*1


#GB-1-203583: use pv2
#GB-1-202775: use pv2
#GB-1-201913: use pv2
#GB-1-202656: use pv2
#GB-1-203913: use pv2
#GB-1-203996: use pv2
#GB-1-203213: use pv2 
#GB-1-203722: use pv2
#GB-1-203152: use pv3
#GB-1-204114: use pv2

bcdf$pv2n[bcdf$parent=="GB-1-203152"] = bcdf$pv3n[bcdf$parent=="GB-1-203152"]

bcdf$pv = bcdf$pv1n
bcdf$pv[is.na(bcdf$pv)] = bcdf$pv2n[is.na(bcdf$pv)]
bcdf$pv[is.na(bcdf$pv)] = bcdf$pv3n[is.na(bcdf$pv)]

bcdf = bcdf[!is.na(bcdf$pv),]
bcdf = bcdf[c("link","parent","pv")]
bcdf$pv = bcdf$pv/10^6

bcdf = bcdf[!duplicated(bcdf),]

rm(pv1,pv2,pv3,num,strip)

bcpv = bcdf
rm(bcdf)



#write.csv(bcdf,"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/BusinessCase_ProgVal.csv",row.names=F)
