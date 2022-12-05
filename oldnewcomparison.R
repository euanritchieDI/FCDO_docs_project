


dat = read.csv("C:/Users/eritchie/OneDrive - Center for Global Development/DevtrackerData-old/FullDataset.csv")

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


dat = dat[c("parent","StartyearIATI","post","BC_link")]

new = allp[!is.na(allp$ProgVal),]
new = new[c("parent","BCprogval","ARprogval","ARyear","BCyearR","ProgVal","StartYear")]


summary(rddensity(new[new$StartYear>=2011 &  new$parent %in% dat$parent,]$ProgVal,40,h=20))
summary(rddensity(new[new$StartYear>=2011 & !new$parent %in% dat$parent,]$ProgVal,40,h=20))
summary(rddensity(new[new$StartYear>=2011,]$ProgVal,40,h=20))

new$inold = (new$parent %in% dat$parent)*1

write.csv(new,"C:/Users/eritchie/Documents/DELETE.csv",row.names=F)

newp = new[new$StartYear>=2011,]

## Bins
spl = 5
df1 <-  transform(newp[newp$inold==1,], group=cut(ProgVal,breaks=seq(0,100,spl)))
df1 = df1[!is.na(df1$group),]
df1 = as.data.frame(df1 %>% group_by(group) %>% summarize(count=n()))
df1$type = "In Old"
df2 <-  transform(newp[newp$inold==0,], group=cut(ProgVal,breaks=seq(0,100,spl)))
df2 = df2[!is.na(df2$group),]
df2 = as.data.frame(df2 %>% group_by(group) %>% summarize(count=n()))
df2$type = "New"
df3 <-  transform(newp, group=cut(ProgVal,breaks=seq(0,100,spl)))
df3 = df3[!is.na(df3$group),]
df3 = as.data.frame(df3 %>% group_by(group) %>% summarize(count=n()))
df3$type = "All"



df = rbind(df1,df2,df3)
df$pct = with(df,ave(count,type,FUN = function(x) x*100/sum(x)))
df = as.data.frame(df %>% complete(type,group))
df$group = rep(seq(spl,100,spl),3)
df[is.na(df)] = 0
df$group = df$group-spl/2
df$pct = df$pct/100

n1 = sum(df[df$type=="In Old",]$count)
n2 = sum(df[df$type=="New",]$count)


#df$type[df$type=="In Old"] = paste0(df$type[df$type=="In Old"]," (n=",n1,")")
#df$type[df$type=="New"]  = paste0(df$type[df$type=="New"], " (n=",n2,")")

ggplot(df) + geom_bar(aes(x=group,y=pct),width=spl,stat="identity",fill="grey60",color="grey30",
	position="dodge")+facet_wrap(~type)+
	geom_vline(xintercept=40,color="red",linetype="dashed")+
	#geom_vline(xintercept=5,color="red",linetype="dashed")+ 
	theme_bw() + scale_x_continuous(breaks=seq(0,100,10))+xlab("Programme Value")+ylab("")















