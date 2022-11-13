library(arrow)
library(tidyverse)
library(purrr)
library(parallel)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dvar <- read_parquet("database/d_variables/d_variables.parquet")
dvar <- dvar[dvar$hospital_coded == 3,]
pat <- read.csv("Table1_Patients.csv")
# sed <- read.csv("all_levels_sedation.csv")

readbysplits <- function(n=100,b=10){
  sp <- split(seq_len(n),sort(rep(1:10,b)[1:n]))
  res <- lapply(sp, function(sl){
    res10 <- readparquet_byid(lis[sl])
    cnames <- Reduce(intersect, lapply(res10,colnames))
    res10 <- do.call(rbind, lapply(res10, function(patvals) 
      patvals[,cnames]))
  })
  
}

readparquet_byid <- function(l){
  lapply(1:length(l), function(v){
    if(length(l[[v]]) > 0) {
      chunk <- read_parquet(paste0("database/monitored_numeric/",names(l[v])))
      chunk$var_name <- dvar$name[match(chunk$a_variableid,dvar$a_variableid)]
      chunk <- chunk[chunk$a_patientid %in% as.numeric(names(l[[v]])),]
      chunk <- spread(chunk[,c("var_name","time","value","a_patientid")],
                      key = c("var_name"),value="value")
    }
  })
}

lis <- local({
  nf <- list.files("database/monitored_numeric/")
  lf <- nf %>% gsub(x=., "monitored_numeric_","") %>% 
    gsub(x=., "\\.parquet","") 
  ub <- as.numeric(unlist(lapply(strsplit(lf,"_"),"[[",1)))
  lb <- as.numeric(unlist(lapply(strsplit(lf,"_"),"[[",2)))
  ids <- pat$a_patientid
  
  is <- do.call(cbind, lapply(ids, function(id) i <- ub <= id & lb >= id))
  rownames(is) <- nf
  colnames(is) <- ids
  
  is <- is[rowSums(is) != 0, ]
  apply(is, 1, which)
})
  
dvarsres <- readbysplits(n=length(lis))
dvarsfi <- Reduce(intersect, lapply(dvarsres,colnames))
dvarsres <- lapply(dvarsres, function(patvals) patvals[,dvarsfi])

# write.csv(dvarsres[[1]],"database/Table2_monitored_vars_split1.csv")
# write.csv(dvarsres[[2]],"database/Table2_monitored_vars_split2.csv")
# write.csv(dvarsres[[3]],"database/Table2_monitored_vars_split3.csv")
# write.csv(dvarsres[[4]],"database/Table2_monitored_vars_split4.csv")
# write.csv(dvarsres[[5]],"database/Table2_monitored_vars_split5.csv")
# write.csv(dvarsres[[6]],"database/Table2_monitored_vars_split6.csv")
# write.csv(dvarsres[[7]],"database/Table2_monitored_vars_split7.csv")
# write.csv(dvarsres[[8]],"database/Table2_monitored_vars_split8.csv")
# write.csv(dvarsres[[9]],"database/Table2_monitored_vars_split9.csv")
# write.csv(dvarsres[[10]],"database/Table2_monitored_vars_split10.csv")

t1 <- read.csv("database/Table2_monitored_vars_split1.csv")
t2 <- read.csv("database/Table2_monitored_vars_split2.csv")
t <- rbind(t1,t2)
rm(t1,t2)
t3 <- read.csv("database/Table2_monitored_vars_split3.csv")
t <- rbind(t,t3)
rm(t3)
t4 <- read.csv("database/Table2_monitored_vars_split4.csv")
t <- rbind(t,t4)
rm(t4)
t5 <- read.csv("database/Table2_monitored_vars_split5.csv")
t <- rbind(t,t5)
rm(t5)
write.csv(t,"database/Table2_monitored_vars_half1.csv")
rm(t)

t6 <- read.csv("database/Table2_monitored_vars_split6.csv")
t7 <- read.csv("database/Table2_monitored_vars_split7.csv")
t <- rbind(t6,t7)
rm(t6,t7)
t8 <- read.csv("database/Table2_monitored_vars_split8.csv")
t <- rbind(t,t8)
rm(t8)
t9 <- read.csv("database/Table2_monitored_vars_split9.csv")
t <- rbind(t,t9)
rm(t9)
t10 <- read.csv("database/Table2_monitored_vars_split10.csv")
t <- rbind(t,t10)
rm(t10)
write.csv(t,"database/Table2_monitored_vars_half2.csv")
rm(t)

h1 <- read.csv("database/Table2_monitored_vars_half1.csv")
h2 <- read.csv("database/Table2_monitored_vars_half2.csv")
h <- rbind(h1,h2)
rm(h1,h2)
write.csv(h,"database/Table2_monitored_vars.csv")