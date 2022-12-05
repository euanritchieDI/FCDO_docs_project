library(rddensity)
library(rdd)
library(ggplot2)
library(ggpubr)

library(kable)

dat = read.csv("C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData/FullDataset.csv")
dat = dat[order(dat$parent,dat$ReviewYear),]
dat = dat[!duplicated(dat$parent),]
## REPLACE NAS WITH 0 FOR NUMERIC VALUES (NOT YEARS, SO I CAN GO OTHER WAY)
dat[c(15,16,18,19,21,22,24,25)][is.na(dat[c(15,16,18,19,21,22,24,25)])] = 0

dat = dat[!is.na(dat$ProgVal),]
dat$near = (round(dat$ProgVal,0) == round(dat$OriginalVal.1 + dat$ExtensionVal.1+ dat$ExtensionVal.2 + dat$ExtensionVal.3,0))*1
dat$ProgVal[dat$near==1] = dat$OriginalVal.1[dat$near==1]

dat$StartyearIATI[is.na(dat$StartyearIATI)] = dat$StartYear[is.na(dat$StartyearIATI)]
dat$StartyearIATI[dat$parent=="GB-1-114287"] = 2012
dat$StartyearIATI[dat$parent=="GB-1-114488"] = 2009
dat$StartyearIATI[dat$parent=="GB-1-202042"] = 2012
dat$StartyearIATI[dat$parent=="GB-1-202297"] = 2011
dat$StartyearIATI[dat$parent=="GB-1-202733"] = 2011
dat$StartyearIATI[dat$parent=="GB-GOV-1-300175"] = 2016
dat$StartyearIATI[dat$parent=="GB-GOV-1-300505"] = 2017

dat$StartyearIATI[dat$StartyearIATI<=2000]  = dat$StartYear[dat$StartyearIATI<=2000]
dat$post = ifelse(dat$StartyearIATI>=2011,"Post 2011","Pre 2011")

########################
datt = dat
datt$hasBCR = (!is.na(datt$BCR))*1
ggplot(datt[datt$ProgVal<100,]) + geom_boxplot(aes(x=factor(hasBCR),y=ProgVal))
summary(glm(hasBCR ~ProgVal,data=datt,family = "binomial"))
#############################


## MCCRARY
DCdensity(dat[dat$post=="Post 2011" & dat$ProgVal<100,]$ProgVal,40)
DCdensity(dat[dat$post=="Pre 2011" & dat$ProgVal<100,]$ProgVal,40)

DCdensity(dat[dat$post=="Post 2011" & dat$ProgVal<100,]$ProgVal,20)
DCdensity(dat[dat$post=="Pre 2011" & dat$ProgVal<100,]$ProgVal,20)

## CJM
summary(rddensity(dat[dat$post=="Post 2011",]$ProgVal,40))
summary(rddensity(dat[dat$post=="Pre 2011",]$ProgVal,40))

summary(rddensity(dat[dat$post=="Post 2011",]$ProgVal,20))
summary(rddensity(dat[dat$post=="Pre 2011",]$ProgVal,20))


#dens = ggplot(dat[dat$ProgVal<100,]) + 
#	geom_histogram(aes(x=ProgVal,y=..density..),fill="grey60",color="grey30",binwidth=4) + theme_bw()+
#	geom_vline(xintercept=40,color="red",linetype="dashed")+theme_bw()+
#	facet_wrap(~post) + scale_x_continuous(breaks=seq(0,100,10)) + ggtitle("")
#dens 


####################################################################
## Bins
spl = 1
df1 <-  transform(dat[dat$post=="Post 2011",], group=cut(ProgVal,breaks=seq(0,100,spl)))
df1 = df1[!is.na(df1$group),]
df1 = as.data.frame(df1 %>% group_by(group) %>% summarize(count=n()))
df1$type = "Post 2011"
df2 <-  transform(dat[dat$post=="Pre 2011",], group=cut(ProgVal,breaks=seq(0,100,spl)))
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

ggplot(df) + geom_bar(aes(x=group,y=pct),width=spl,stat="identity",fill="grey60",color="grey30",
	position="dodge")+facet_wrap(~type)+
	geom_vline(xintercept=40,color="red",linetype="dashed")+
	#geom_vline(xintercept=5,color="red",linetype="dashed")+ 
	theme_bw() + scale_x_continuous(breaks=seq(0,100,10))+xlab("Programme Value")+ylab("")

#post = dat[dat$ProgVal<100 & dat$post=="Post 2011",]

###########################################################
## POLYNOMIAL OF DEGREE 4
post = df[grepl("Post 2011",df$type),]
post$group = post$group + spl/2
post$dum = (post$group%%5==0)*1
post$dum1 = (post$group %in% c(0,5,10,15,20,25,30,35,40))*1
post$dum2 = (post$group %in% c(45,50,55,60,65,70,75,80,85,90,95))*1

## POLYNOMIAL AND ONE DUMMY FOR ALL ROUND NUMBES
mod1 = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum,post)

	

## EXCLUDING THE REGION 36:39
lwr = 36
upr = 45
silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum + I(dum*group),post[!post$group %in% c(lwr:upr),])
gap = predict(silly,post[post$group %in% c(lwr:upr),])
post$fitpct = c(c(silly$fitted.values)[1:(lwr-1)],c(gap),c(silly$fitted.values)[lwr:(100+lwr-upr-1)])

post$diff = with(post,pct/fitpct*100-100)

ggplot(post) + geom_line(aes(x=group,y=fitpct))+ geom_bar(aes(x=group,y=pct),stat="identity",alpha=0.5)+ 
	geom_vline(xintercept=lwr,linetype="dashed") + geom_vline(xintercept=upr,linetype="dashed") + theme_bw()


with(post,sum(group*pct))
with(post,sum(group*fitpct))

post2 = post
silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum + I(dum*group),post2)
gap = predict(silly,post2)
post2$fitpct = gap

post2$diff = with(post2,pct/fitpct*100-100)

ggplot(post) + geom_line(aes(x=group,y=fitpct))+ geom_bar(aes(x=group,y=pct),stat="identity",alpha=0.5)+ 
	geom_vline(xintercept=lwr,linetype="dashed") + geom_vline(xintercept=upr,linetype="dashed") + theme_bw()




#post$dum = (post$group%%5==0)*1
#post$dum[post$group==40] = 0

#silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4)+dum,post)
#silly = lm(pct ~ group + I(group^2) + I(group^3)+ I(group^4),post)

#post$fitpct = c(silly$fitted.values)

#names(post)[names(post)=="count"] = "num"


P5 = ggplot(post[post$group<70,]) + geom_line(aes(x=group,y=diff),color="black") + 
	geom_hline(yintercept=0,linetype="dashed")+theme_bw() + xlab("")+	
	ggtitle("Percent diff between obs in each group, and estimated obs according to counterfactual distribution")+
	scale_x_continuous(breaks=seq(0,100,5))
P5	



















P4 = ggplot(post) + geom_bar(aes(x=group,y=pct),stat="identity",width=spl,fill="grey60",color="grey30",
	position="dodge") + geom_line(aes(x=group,y=fitpct),color="dodgerblue4",size=1)+theme_bw()+
	geom_vline(xintercept=40,color="red",linetype="dashed")+ scale_x_continuous(breaks=seq(0,100,5))+
	xlab("Programme Value")
P4 

silly1 = lm(count ~ group + I(group^2) + I(group^3)+ I(group^4),post)$fitted.values
post$fitpct = c(silly)


   
## POLYNOMIAL OF DEGREE 3
silly = lm(count ~ group + I(group^2) + I(group^3)+dum +dum2,post)$fitted.values
post$fit = c(silly)

P3 = ggplot(post) + geom_bar(aes(x=group,y=count),width=spl,stat="identity",fill="grey60",color="grey30",
	position="dodge") + geom_line(aes(x=group,y=fit),color="dodgerblue4",size=1)+theme_bw()+
	geom_vline(xintercept=40,color="red",linetype="dashed")
P3	







post = dat[dat$ProgVal<100 & dat$post=="Post 2011",]$ProgVal
kink_est <- bunch(post, zstar = 40, t1 = 0, t2 = 0.2, Tax = 0,
                   cf_start = 19, cf_end = 30,
                   exclude_before = 5, exclude_after = 5, binw = 2,poly_size = 3)













pre = dat[dat$post==0,]$ProgVal
pos = dat[dat$post==1,]$ProgVal

summary(rddensity(pre,40))
summary(rddensity(pos,40))

par(mfrow=c(1,2))
hist(pos[pos<100],breaks=50)
hist(pre[pre<100],breaks=50)

dens = ggplot(dat[dat$ProgVal<100,]) + geom_density(aes(x=ProgVal,color=factor(post)),size=1) 
dens + theme_bw()

posG = ggplot(dat[dat$post==1 & dat$ProgVal<100,]) + 
	geom_histogram(aes(x=ProgVal,y=..density..),binwidth=1,fill="grey50",color="grey20") + 
	theme_bw() + 
	geom_vline(xintercept=40,linetype="dashed") + geom_vline(xintercept=5,linetype="dashed")

preG = ggplot(dat[dat$post==0 & dat$ProgVal<100,]) + 
	geom_histogram(aes(x=ProgVal,y=..density..),binwidth=1,fill="grey50",color="grey20") + 
	theme_bw() + 
	geom_vline(xintercept=40,linetype="dashed") + geom_vline(xintercept=5,linetype="dashed")

ggarrange(posG,preG,nrow=1)


pos40 = rddensity(pos,40,p=3)$test$p_jk
pos20 = rddensity(pos,30,p=3)$test$p_jk
pos5  = rddensity(pos,5,p=3)$test$p_jk

funk = function(x){
	return(rddensity(pos,x,p=3)$test$t_jk)
}
Test = sapply(1:50,funk)


pos40 = rddensity(pos,40)




