events <- read.csv("database/All events.csv")
mv <- read.csv("database/mechanicalventilation.csv")
sofa <- read.csv("SOFA_all_patients.csv")
pats <- read.csv("Patients_hospital3.csv")
copd <- read.csv("copd.csv")
kf <- read.csv("kidneyfunction.csv")
dem <- read.csv("dementia.csv")
chf <- read.csv("chronicheartfailure.csv")
char <- read.csv("charlson.csv")

comor <- data.frame(
  a_patientid = pats$a_patientid,
  copd = NA,
  chf = NA,
  dem = NA,
  kf = NA,
  char = NA
)

comor$copd <- copd$choicecode[match(comor$a_patientid, copd$a_patientid)]
comor$chf <- chf$choicecode[match(comor$a_patientid, chf$a_patientid)]
comor$kf <- kf$choicecode[match(comor$a_patientid, kf$a_patientid)]
comor$dem <- dem$choicecode[match(comor$a_patientid, dem$a_patientid)]
comor$char <- char$choicecode[match(comor$a_patientid, char$a_patientid)]


sofa$sofa24h <- sofa$time <= 1440
sofa$sofa48h <- sofa$time <= 2880 & sofa$time > 1440

sofa$sofa36h <- sofa$time <= 1440*3

ids <- unique(sofa$a_patientid)

sofas <- data.frame(cbind(
  a_patientid=ids,
  sofa24h=unlist(lapply(ids,function(id) mean(sofa[sofa$a_patientid == id & sofa$sofa24h,]$SOFA.score))),
  sofa48h=unlist(lapply(ids,function(id) mean(sofa[sofa$a_patientid == id & sofa$sofa48h,]$SOFA.score))),
  sofa.max=unlist(lapply(ids,function(id) max(sofa[sofa$a_patientid == id,]$SOFA.score))),
  sofa.avg=unlist(lapply(ids,function(id) mean(sofa[sofa$a_patientid == id,]$SOFA.score)))
))

pats$sofa24h <- NA
pats$sofa48h <- NA
pats$sofa.max <- NA
pats$sofa.avg <- NA


pats$sofa24h <- sofas$sofa24h[match(pats$a_patientid,sofas$a_patientid)]
pats$sofa48h <- sofas$sofa48h[match(pats$a_patientid,sofas$a_patientid)]
pats$sofa.max <- sofas$sofa.max[match(pats$a_patientid,sofas$a_patientid)]
pats$sofa.avg <- sofas$sofa.avg[match(pats$a_patientid,sofas$a_patientid)]

write.csv(cbind(pats,comor),"Tab1 H3 comor.csv")



