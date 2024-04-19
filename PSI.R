library(ggplot2)		# for graphs
library(dplyr)		# for collapsing/merging data easily
library(gridExtra)	# arranging graphs
library(WDI)		# downloading world bank data
library(ggpubr)		# arranging graphs
library(countrycode)	# merging on iso codes
library(tidyr)		# some tidy up functions
library(data.table)	# much faster than base r
library(openxlsx)		# flexible for reading/writing excel files

source("DoCRS.R")


daclist    = c(1,2,3,4,5,6,7,8,9,10,11,12,18,20,21,22,40,50,61,68,69,75,76,84,301,302,701,742,801,820)

options(timeout=250)
#------------------------------ DOWNLOAD TABLE 1              
table1link   = "https://stats.oecd.org/FileView2.aspx?IDFile=570b240e-df9c-4586-b53d-99946fac437d"

tmpd = tempdir()
tmpf = tempfile(tmpdir=tmpd, fileext=".zip")
download.file(url=table1link,destfil=tmpf,mode="wb")
unzip(tmpf, exdir=paste0(tmpd,"/","tab1"))
setwd(paste0(tmpd,"/","tab1"))
table1 = read.csv(paste0(tmpd,"/","tab1/",list.files()[1]))  
do.call(unlink, list(list.files()))	

dac1 = as.data.frame(table1 %>% filter(DONOR %in% daclist & Year>2016))
dac1[c("Flags","Part","PART","Time")] = NULL
dac1 = dac1[dac1$FLOWS %in% c(1120,1140,1160,1130),]
dac1 = dac1[order(dac1$FLOWS,dac1$Year),]

names(dac1)[names(dac1)=="Aid.type"] = "type"

dac1$type[dac1$type=="I.A.11.2. Private sector instruments - instrument approach"] = "instrument"
dac1$type[dac1$type=="I.A.11.1. Private sector instruments - institutional approach"] = "institutional"

ger = as.data.frame(dac %>% filter(AMOUNTTYE=="D" & 



dac1[dac1$DONOR==12 & dac1$Year==2022 & dac1$type=="institutional",]








######################################################################


N = 1000	  # number of simulated loans
princ = 100	  # principal
dur = 10	  # duration of loan
gra = 5	  # grace period	
int = 0.05	  # interest rate
D = 0.1	  # discount rate (0.1 for LDC)
R = 0.03	  # risk of default each period
per = dur-gra # repayment period 

repay_p = c(rep(0,gra),rep(princ/per,per))
repay_i = int*(princ-c(0,cumsum(repay_p)[1:(dur-1)]))
repay_t = repay_i + repay_p

disc = (1+D)^-(1:dur)
risk = (1-R)^(1:dur)

GEQ = princ - sum(disc*repay_t)
EPV = sum(risk*repay_t) - princ

sched = c(-princ,repay_t)
mat = t(matrix(rep(sched,N),nrow=length(sched)))

funk = function(v,x) {
	newv = v
	if(x<length(v)){newv[x:length(newv)]=0}
	return(newv)
}

test = t(replicate(n=N,funk(sched,rgeom(1,0.03)+2)))
sum(test[,11]==0)


testdf = as.data.frame(data.frame(test) %>% pivot_longer(cols=everything()) )
testdf$loan = rep(rep(1:(N/10),each=10),each=length(sched))-1
testdf$period = as.numeric(str_extract(testdf$name,"[0-9].*")) + testdf$loan

result = as.data.frame(testdf %>% group_by(period) %>% summarize(val=sum(value)))
result$cumul = with(result,cumsum(val))

###############################################################

## FRANCE

crs$usd_commitment[is.na(crs$usd_commitment)] = 0
crs$usd_commitment_defl[is.na(crs$usd_commitment_defl)] = 0

fra = as.data.frame(crs %>% filter(donorcode==4))

frapsi = fra[fra$psiflag==2 & !is.na(fra$psiflag),]

as.data.frame(crs %>% filter(donorcode==4 & agencycode==44) %>% group_by(year) %>% 
	summarize(disb=sum(usd_disbursement,na.rm=T)) %>% arrange(year) #%>%
#	pivot_wider(names_from=finance_t,values_from=disb))

as.data.frame(crs %>% filter(donorcode==918 & floor(parentchannelcode/10000)==6) %>% 
	group_by(agencycode,psiflag,year) %>% 
	summarize(disb=sum(usd_commitment_defl,na.rm=T)) %>% arrange(year) %>%
	pivot_wider(names_from=psiflag,values_from=disb))

as.data.frame(crs %>% filter(donorcode==918 & psiflag==2) %>% group_by(agencyname,year) %>% 
	summarize(disb=sum(usd_disbursement_defl,na.rm=T)) %>% arrange(year) %>%
	pivot_wider(names_from=year,values_from=disb))

eupsi =crs[crs$donorcode==918 & crs$agencycode==3 & crs$psiflag==2 & !is.na(crs$psiflag),]




