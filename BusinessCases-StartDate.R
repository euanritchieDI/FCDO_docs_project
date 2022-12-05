########################################################################################

## BUSINESS CASES: START DATES
## (this had been in separate code, but only applied to "FullDataset" so misses some projects without ARs)

########################################################################################


bcdf = data.frame("link"=bc$link,"text"=bctext)				
rownames(bcdf) = NULL
bcdf = left_join(bcdf,bc[c("link","parent")],by="link")


datereg = "start ?date:? ?.{0,25}\\d{4}" 
bcdf$getstart = str_extract(bcdf$text,datereg)
bcdf$getstart = str_extract(bcdf$getstart,"\\d{4}$")

BCyear = bcdf[-2]
rm(bcdf)







