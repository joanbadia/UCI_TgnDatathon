
cleaned_data <- read.csv("Tab1 H3 comor.csv")

PatsIDs <-subset(cleaned_data, select=c(age,a_patientid,
                                        patientsex,bmi,
                                        sofa.max,sofa.avg,
                                        hospital_outcome
))
PatsIDs <- na.omit(PatsIDs) 
PatsSel <- cleaned_data[cleaned_data$a_patientid %in% PatsIDs$a_patientid,]

cleaned_data$sofa_cat <- cut(cleaned_data$sofa.max,breaks=5)
cleaned_data$sofa_cat <- ifelse(is.na(cleaned_data$sofa_cat),"no SOFA score",cleaned_data$sofa_cat)
PatsSel$sofa_cat <- cut(PatsSel$sofa.max,breaks=5)
PatsSel$sofa_cat <- ifelse(is.na(PatsSel$sofa_cat),"no SOFA score",PatsSel$sofa_cat)

PatsSel$AgeCat <- cut(PatsSel$age,breaks=10)


tblpct(cleaned_data$patientsex, cleaned_data$sofa_cat)
tblpct(cleaned_data$sofa_cat,cleaned_data$patientsex)

tblpct(cleaned_data$patientsex, cleaned_data$copd,nasT = "ifany")
tblpct(cleaned_data$patientsex, cleaned_data$char,nasT = "ifany")
tblpct(cleaned_data$patientsex, cleaned_data$chf,nasT = "ifany")
tblpct(cleaned_data$patientsex, cleaned_data$dem,nasT = "ifany")
tblpct(cleaned_data$patientsex, cleaned_data$kf,nasT = "ifany")

tblpct(factor(cleaned_data$copd), cleaned_data$patientsex, nasT = "ifany")
tblpct(factor(cleaned_data$char), cleaned_data$patientsex, nasT = "ifany")
tblpct(factor(cleaned_data$chf), cleaned_data$patientsex, nasT = "ifany")
tblpct(factor(cleaned_data$dem), cleaned_data$patientsex, nasT = "ifany")
tblpct(factor(cleaned_data$kf), cleaned_data$patientsex, nasT = "ifany")


tblpct(factor(PatsSel$copd), PatsSel$patientsex, nasT = "ifany")
tblpct(factor(PatsSel$char), PatsSel$patientsex, nasT = "ifany")
tblpct(factor(PatsSel$chf), PatsSel$patientsex, nasT = "ifany")
tblpct(factor(PatsSel$dem), PatsSel$patientsex, nasT = "ifany")
tblpct(factor(PatsSel$kf), PatsSel$patientsex, nasT = "ifany")
tblpct(factor(PatsSel$sofa_cat), PatsSel$patientsex, nasT = "ifany")
tblpct(PatsSel$AgeCat, PatsSel$patientsex, nasT = "ifany")

PatsM <- PatsSel[PatsSel$patientsex == "M",]
PatsF <- PatsSel[PatsSel$patientsex == "F",]
tblpct(factor(PatsM$sofa_cat), PatsM$hospital_outcome, nasT = "ifany")
tblpct(factor(PatsF$sofa_cat), PatsF$hospital_outcome, nasT = "ifany")
tblpct(factor(PatsM$AgeCat), PatsM$hospital_outcome, nasT = "ifany")
tblpct(factor(PatsF$AgeCat), PatsF$hospital_outcome, nasT = "ifany")


tblpct(PatsM$sofa_cat >= 3, PatsM$hospital_outcome, nasT = "ifany")
tblpct(PatsF$sofa_cat >= 3, PatsF$hospital_outcome, nasT = "ifany")


ggarrange(
  ggdensity(PatsM,"age",color="hospital_outcome"),  
  ggdensity(PatsF,"age",color="hospital_outcome")
)








# There are more sick males?

# copd
# charlson
# chronicalheartfailure
# dementia
# kidney function