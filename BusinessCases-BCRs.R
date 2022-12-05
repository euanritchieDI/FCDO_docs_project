########################################################################################################

#### BUSINESS CASES: BCRS

########################################################################################################


chars = sapply(bctext,nchar)
bc$chars = chars
write.csv(bc[c("parent","chars")],"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/BCcharacters.csv",row.names=F)


bcdf = data.frame("link"=bc$link,"text"=bctext)				
rownames(bcdf) = NULL
bcdf = left_join(bcdf,bc[c("link","parent")],by="link")		# this produces duplicates as same BCs uploaded for different projects

############ CREATE REGEX TO EXTRACT BCRS
base1 = "((benefit( |-|/){0,2}(to)?( |-)cost( |-|/){0,2}ratio ?( \\(bcr\\))?)|(bcr ?))(of|is|(is )?equal to|comes out at)( around| about| at least)? ?"
num1  = "£?[0-9]{1,3}\\.?[0-9]{0,4}:?1?%?"
num2  = "£?[0-9]{1,3} ?to ?(one|1)"
num3  = "£?(one|1) ?to ?£?[0-9]{1,3}"
num   = paste0("(",num3,"|",num2,"|",num1,")")


base2 = "((benefit( |-|/){0,2}cost( |-|/){0,2}ratio ?( \\(bcr\\))?)|(bcr ?)).{0,40}(between|from) ?"
num4  = "£?[0-9]{1,3}\\.?[0-9]{0,2}%? ?(to|and) ?£?[0-9]{1,3}\\.?[0-9]{0,2}%?"

re1 = paste0(base1,num)
re2 = paste0(base2,num4)
###############

bcdf$bclist = str_extract_all(bcdf$text,re1)

  #### FUNCTION THAT EXTRACTS NUMERICAL VALUE FROM TEXT (REMOVING THE :1 IF INCLUDED IN BCR)
  strip1 = function(x){
  	txt = str_extract(x,"[0-9\\.:]+")
	gsub(":1","",txt)
  }
  strip2 = function(x){
	txt = str_extract_all(x,"[0-9\\.:]+")
	sapply(txt,FUN = function(x) as.numeric(gsub(":1","",x)))
  }
  #####


bcdf$test = sapply(bcdf$bclist,FUN = function(x) length(unique(strip1(x))))
bcdf$BCR  = sapply(bcdf$bclist,FUN = function(x) unique(strip1(x)))

### In all cases so far checked where "bcr of 1" has been picked up, this is a sensitivity test 
### (asking "what needs to be true to achieve a bcr of 1". So removing those from lists of BCRs found

funk = function(x){
	if(is.na(match(1,x))){
		return(x)
	} else {
		return(x[-match(1,x)])
	}
}

bcdf$BCR = sapply(bcdf$BCR,funk)
bcdf$pctrnge  = sapply(bcdf$BCR,FUN = function(x) max(as.numeric(x))/min(as.numeric(x)))
bcdf$rnge     = sapply(bcdf$BCR,FUN = function(x) max(as.numeric(x))-min(as.numeric(x)))
bcdf$aver     = sapply(bcdf$BCR,FUN = function(x) mean(as.numeric(x)))
bcdf$aver[bcdf$pctrnge>1.3] = NA
bcdf$manual = (is.na(bcdf$aver))*1
bcdf$manual[bcdf$test==0] = 0

## The above finds cases with one BCR quoted, or cases where several are quoted but they are all similar. 
## Ones that have very different BCRs listed will be checked manually. However, another regex throws up different numbers, and so
## below code flags a few more to check manually. 

bcdf$bclist2 = str_extract_all(bcdf$text,re2)
bcdf$test2   = sapply(bcdf$bclist2,FUN = length) 
bcdf$BCR2    = sapply(bcdf$bclist2,FUN = function(x) unique(strip2(x)))
bcdf$BCR2max = sapply(bcdf$BCR2,FUN = function(x)  max(unlist(x)))
bcdf$BCR2min = sapply(bcdf$BCR2,FUN = function(x)  min(unlist(x)))

bcdf$manual[bcdf$test2!=0 & (bcdf$aver>bcdf$BCR2max | bcdf$aver<bcdf$BCR2min)] = 1
bcdf[c("BCR2max","BCR2min","test","test2","rnge")] = NULL

bcdf$FinalBCR = bcdf$aver

##### MANUAL CHECKS !!!!

bcdf$FinalBCR[bcdf$parent=="GB-1-201724"] = 52			#this is central BCR, no caveats mentioned
bcdf$FinalBCR[bcdf$parent=="GB-1-202580"] = 1.27		#this is overall, other values referred to separate parts within project
bcdf$FinalBCR[bcdf$parent=="GB-1-204624"] = 2.8			#this is central, low scenario 1.2, high scenario 6.3
bcdf$FinalBCR[bcdf$parent=="GB-1-203491"] = 1.7			#this is central, low scenario 1.39, high scenario 1.92 (there are others too)
bcdf$FinalBCR[bcdf$parent=="GB-1-203582"] = 4.1			#"analysis suggests minimum of 4.1" is quoted first. Later says "a very high BCR of 5.6"
bcdf$FinalBCR[bcdf$parent=="GB-1-203864"] = 6.2			#this is BCR of preferred option, other option 4.2
bcdf$FinalBCR[bcdf$parent=="GB-1-203870"] = 3.15		#this is central BCR, higher d rate:2.1, lower d rate: 4.1
bcdf$FinalBCR[bcdf$parent=="GB-1-204916"] = -99			# not clear, but probably could work out REVISIT
bcdf$FinalBCR[bcdf$parent=="GB-1-202921"] = 1.9			# from table 15, bcr of preferred option 3
bcdf$FinalBCR[bcdf$parent=="GB-1-203161"] = -99			# not clear, but probably could work out REVISIT
bcdf$FinalBCR[bcdf$parent=="GB-1-202610"] = 3.19		# overall BCR from summary
bcdf$FinalBCR[bcdf$parent=="GB-1-202893"] = 4.4			# this is bcr of preferred option (I think: said option 2 and this is "overall BCR for 2(a) and 2(b)
bcdf$FinalBCR[bcdf$parent=="GB-1-202900"] = 3.19		# overall BCR from summary
bcdf$FinalBCR[bcdf$parent=="GB-1-203396"] = 1.9			# Option 1 preferred, 1a had BCR of 1.89, option 1b had 1.92
bcdf$FinalBCR[bcdf$parent=="GB-1-203330"] = 1.26		# Estimate presented first (based on conservative assumptions, 3.0 claimed later)
bcdf$FinalBCR[bcdf$parent=="GB-1-203536"] = 2.27		# this is BCR of preferred option 3
bcdf$FinalBCR[bcdf$parent=="GB-1-203648"] = 10.7		# this is overall BCR of preferred option 1 (bcrs also presented for each part)
bcdf$FinalBCR[bcdf$parent=="GB-1-203679"] = 1.2			# this is overall BCR of preferred option 2
bcdf$FinalBCR[bcdf$parent=="GB-1-204330"] = 5.5			# this is overall BCR of preferred option 3
bcdf$FinalBCR[bcdf$parent=="GB-1-204582"] = NA			# Only gives some crazy BCR under daft assumptions (400) or break even assumptions
bcdf$FinalBCR[bcdf$parent=="GB-1-205058"] = 3.46		# this is central, low = 2.32, high = 5.32
bcdf$FinalBCR[bcdf$parent=="GB-1-205238"] = 2.4			# This seems most sensible. Other options are 1.9 (not accounting for preserved/returned capital) or 4.2 (ignoring that capital entirely)
bcdf$FinalBCR[bcdf$parent=="GB-1-114293"] = 2.33		# There is some weird reasoning as to why actually it should be 55, but I think this sounds more sensible
bcdf$FinalBCR[bcdf$parent=="GB-1-201388"] = 2.46		# this is BCR of preferred option 2
bcdf$FinalBCR[bcdf$parent=="GB-1-202736"] = 2.22		# this is BCR of preferred option 1
bcdf$FinalBCR[bcdf$parent=="GB-1-203484"] = 1.48		# this is BCR of preferred option 1. Option 2 had slightly higher BCR (1.52) but riskier apparently
bcdf$FinalBCR[bcdf$parent=="GB-1-204135"] = 2.05		# preferred option 1. Sensitivity analysis lowers to 1.02
bcdf$FinalBCR[bcdf$parent=="GB-1-204176"] = 2.2 		# overall BCR, none others given
bcdf$FinalBCR[bcdf$parent=="GB-1-204237"] = 2.03 		# overall BCR, none others given
bcdf$FinalBCR[bcdf$parent=="GB-1-201913"] = -99 		# Lots mentioned for different parts, might be possible to piece together REVISIT
bcdf$FinalBCR[bcdf$parent=="GB-1-203574"] = 2.9 		# overall BCR, 4.2 for mitigation bit and 2.5 for adaptation bit
bcdf$FinalBCR[bcdf$parent=="GB-1-201196"] = -99 		# says evidence suggests BCR between 1 and 37 which is pretty useless REVISIT?
bcdf$FinalBCR[bcdf$parent=="GB-1-202623"] = 3.7 		# overall BCR for preferred option
bcdf$FinalBCR[bcdf$parent=="GB-1-203924"] = 6.1 		# overall BCR for preferred option 4
bcdf$FinalBCR[bcdf$parent=="GB-GOV-1-300113"] = 5.2 		# This is average of the two components of preferred option (3) (from table on p.32)
bcdf$FinalBCR[bcdf$parent=="GB-1-204325"] = 5.7 		# this is base case, high = 15.1, low = 2.2
bcdf$FinalBCR[bcdf$parent=="GB-1-202824"] = 7.6 		# this is bcr for preferred option 3 
bcdf$FinalBCR[bcdf$parent=="GB-1-202349"] = -99 		# bcr of 1.37 for outputs 2 and 3, and 0.65 for output 4 (I think?) but not clear overall REVISIT
bcdf$FinalBCR[bcdf$parent=="GB-1-203687"] = 7.7 		# this is bcr for overall but numbers 5.2 and 5.34 are mentioned for what look like different bits (p.32)
bcdf$FinalBCR[bcdf$parent=="GB-1-205074"] = 3.0 		# this baseline, low = 1.5, high = 3.1
bcdf$FinalBCR[bcdf$parent=="GB-1-201239"] = 3.5 		# this is overall. No other figures given but lots of BCRs quoted from international evidence
bcdf$FinalBCR[bcdf$parent=="GB-1-203187"] = 2.9 		# REVISIT Looks like two options under preferred options, and this is highest (Other is 1.8)
bcdf$FinalBCR[bcdf$parent=="GB-1-203264"] = NA	 		# International evidence on similar presented, but large range and not sure how it translates
bcdf$FinalBCR[bcdf$parent=="GB-GOV-1-300166"] = NA 		# International evidence on similar presented, but large range and not sure how it translates
bcdf$FinalBCR[bcdf$parent=="GB-1-202169"] = 2.2 		# This is BCR with just "direct" benefits. Also quoted is with "indirect" benefits: 3.9
bcdf$FinalBCR[bcdf$parent=="GB-GOV-1-300102"] = 1.6 		# this is BCR for preferred option 3
bcdf$FinalBCR[bcdf$parent=="GB-1-205171"] = 1.1 		# this is bcr quoted in summary, possibility of higher suggested in text
bcdf$FinalBCR[bcdf$parent=="GB-1-203106"] = 70	 		# this is BCR quoted. NO caveats quoted but seems high!
bcdf$FinalBCR[bcdf$parent=="GB-1-202534"] = 2	 		# !! This might not be for entire project (assumed based on "adaptation type engagements" in SSA
bcdf$FinalBCR[bcdf$parent=="GB-1-203539"] = 4	 		# this is baseline BCR, low = 3, high = 7.7
bcdf$FinalBCR[bcdf$parent=="GB-1-203631"] = 3.17 		# this is baseline sensitivity analysis gives low estimate of 1.6
bcdf$FinalBCR[bcdf$parent=="GB-1-205055"] = NA	 		# No overall bcr given, only for components (2, and 3.3 are given for different parts)
bcdf$FinalBCR[bcdf$parent=="GB-1-203603"] = NA	 		# International evidence on similar presented, but large range and not sure how it translates
bcdf$FinalBCR[bcdf$parent=="GB-1-204861"] = 1.76 		# this is BCR for preferred option 3
bcdf$FinalBCR[bcdf$parent=="GB-1-204797"] = 1.7 		# this is BCR presented in summary
bcdf$FinalBCR[bcdf$parent=="GB-1-201564"] = 1.75 		# This is average of high and low case presented (2.3 and 1.2)
bcdf$FinalBCR[bcdf$parent=="GB-1-202293"] = 1.85 		# this is base case for preferred option 1
bcdf$FinalBCR[bcdf$parent=="GB-1-203292"] = 7.1 		# this is base case, sensitivity analysis give low estimate of 5.3
bcdf$FinalBCR[bcdf$parent=="GB-1-203666"] = 2.68  		# this is bcr of preferred option 2
bcdf$FinalBCR[bcdf$parent=="GB-1-203803"] = 7.6  		# this is baseline bcr, sensitivity analysis gives low estimate of 3.6
bcdf$FinalBCR[bcdf$parent=="GB-1-205045"] = 5.83  		# This is central BCR but unclear if it is considered base case. High = 6.6, low = 4.4
bcdf$FinalBCR[bcdf$parent=="GB-1-201733"] = 1.1  		# this is presented as minimum but quoted upfron, lots of higher numbers quoted in tables
bcdf$FinalBCR[bcdf$parent=="GB-1-202647"] = 2.3  		# this is bcr for preferred option 2
bcdf$FinalBCR[bcdf$parent=="GB-1-202737"] = 6.3  		# this is baseline bcr. Also, interestingly, includes figure of £18 per tonne CO2e abated
bcdf$FinalBCR[bcdf$parent=="GB-1-202884"] = 2.3  		# this is baseline bcr, no others were obvious
bcdf$FinalBCR[bcdf$parent=="GB-1-202935"] = 8.6  		# this is middle bcr, high = 13, low = 3.8
bcdf$FinalBCR[bcdf$parent=="GB-1-203034"] = 6  			# this is bcr for preferred option 2
bcdf$FinalBCR[bcdf$parent=="GB-1-204056"] = 3.5			# this is BCR based on "10% attribution" and if 25% assumed, rises to 8.7
bcdf$FinalBCR[bcdf$parent=="GB-1-204239"] = 2.57		# this is BCR based on "ten-year hoziron", based on "full working life" rises to 7.15
bcdf$FinalBCR[bcdf$parent=="GB-1-204240"] = 8.4			# gives range of 8.4 to 10.5 but then gives some caveats, and concludes likely that order of magnitude. So taking lower end of range
bcdf$FinalBCR[bcdf$parent=="GB-1-204637"] = 1.86		# central bcr given for phase 1 and 2

bcdf = bcdf[order(-bcdf$FinalBCR),]

bcdf$FinalBCR[bcdf$parent=="GB-GOV-1-300036"] = 16		# The BCR picked up footnote number 22 (so was 1622 instead of 16)


# Other high values:
# GB-1-202759 has BCR of 202. This is what the BC claims, "largely because of the global benefits associated with stopping 
#	the spread of resistance to artemisinin". For country specific BCR, gives (more reasonable sounding) BCR of 5.1
# GB-1-202759 has BCR of 126. This is genuinely what is claimed, not an error. Cost per life saved of £31 apparently...
# GB-1-203106 claims BCR of 70, no caveats mentioned
# GB-1-201724 claims BCR of 52, no caveats mentioned
# GB-1-202211 This BCR (39) is actually lower bound estimate of BCR from previous activities of org being funded (CGIAR)
# GB-1-203539 claims BCR of 38, no caveats mentioned

BCRset1 = bcdf[!is.na(bcdf$FinalBCR),]
BCRset2 = bcdf[is.na(bcdf$FinalBCR) & sapply(bcdf$bclist2,length)>0,]	#forgot about these: those with BCR range given
BCRset2$FinalBCR = sapply(BCRset2$BCR2,function(x) mean(unlist(x[x<2000])))
BCRset2$aveOfRange = 1

BCRset2$FinalBCR[BCRset2$parent=="GB-1-205027"]       = (1.1 + 1.35)/2
BCRset2$FinalBCR[BCRset2$parent=="GB-GOV-1-300372"]   = 4.29 			# one of the ranges was for "do nothing", the other included sensitivity analysis
BCRset2$FinalBCR[BCRset2$parent=="GB-1-202907"]       = 2.1			# this is central case, the other end of range is sensitivity analysis
BCRset2$aveOfRange[BCRset2$parent=="GB-GOV-1-300372"] = 0			
BCRset2$aveOfRange[BCRset2$parent=="GB-1-202907"]     = 0			

BCRset1$aveOfRange = 0
BCRs = rbind(BCRset1,BCRset2)
BCRs = BCRs[c("link","parent","manual","FinalBCR","aveOfRange")]
BCRs = BCRs[!duplicated(BCRs),]

BCRspres = BCRs

BCRs = left_join(BCRs,BC[c("link","parent","narrative","year")],by=c("link","parent"))

rm(BCRspres,BCRset2,BCRset1,bcdf,funk,chars,re1,re2,num,num1,num2,num3,num4,base1,base2,strip1,strip2)



#write.csv(BCRs,"C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/BCR-database-17th-Sept.csv",row.names=F)

