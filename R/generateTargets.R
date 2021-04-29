setwd("...")

# read in, add basin, zone, stratum
hill_ID=scan(file="../auxdata/hill.jc.h77.asc", skip=6, na.strings="*")
patch_ID=scan(file="../auxdata/patch.jc.h77.asc", skip=6, na.strings="*")
zone_ID = patch_ID
stratum_ID = patch_ID
LAI=scan(file="../auxdata/lai2007.lan.h77.asc", skip=6, na.strings="*")
#LAI=round(LAI, digits=5)

x=rep(1,length(hill_ID)) # uses the number of cases
basin_ID=x

#combine them
tmp = as.data.frame(cbind(basin_ID,hill_ID,zone_ID,patch_ID,stratum_ID,LAI))
tmp2 = subset(tmp, is.na(tmp$patch_ID)==F)
tmp = aggregate(tmp2, by=list(tmp2$patch_ID, tmp2$zone_ID, tmp2$hill_ID, tmp2$basin_ID), mean)
#tmp = aggregate(tmp2, by=list(tmp2$basin_ID, tmp2$hill_ID, tmp2$zone_ID,tmp2$patch_ID), mean)

#reorder them, remove subset_key
tmp=tmp[,c(5:10)]

# remove null values
tmp = subset(tmp, is.na(tmp$LAI)==F)
tmp = subset(tmp, !tmp$LAI==-9999) # if user has another null value, input it here
tmp = subset(tmp, !tmp$LAI==Inf) # if user has another null value, input it here
tmp = format(tmp, scientific=FALSE)

#Export and merge with a file header and target list
newheader = sprintf("%d num_stratum\n%d num_targets", nrow(tmp), length(tmp)-5)
write(newheader, file="../tecfiles/spinup_thresholds2007.jc.h77.txt")
targets = colnames(tmp)
targets = subset(targets, !targets=="basin_ID") 
targets = subset(targets, !targets=="hill_ID") 
targets = subset(targets, !targets=="zone_ID") 
targets = subset(targets, !targets=="patch_ID")
targets = subset(targets, !targets=="stratum_ID") 
write(targets, file="../tecfiles/spinup_thresholds2007.jc.h77.txt", append=T)
write.table(tmp, file="../tecfiles/spinup_thresholds2007.jc.h77.txt", append=T, quote=F, row.names=F)
