library(arrow)
library(tidyverse)
library(purrr)
library(parallel)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dvar <- read_parquet("database/d_variables/d_variables.parquet")
dvar <- dvar[dvar$hospital_coded == 3,]
pat <- read.csv("Patients_hospital3.csv")
# sed <- read.csv("all_levels_sedation.csv")

# chunk <- read_parquet("database/derived_numeric/derived_numeric_3770056_3779595.parquet")
# chunk$var_name <- dvar$name[match(chunk$a_variableid,dvar$a_variableid)]

readbysplits <- function(n=100,b=10){
  sp <- split(seq_len(n),sort(rep(1:10,b)[1:n]))
  res <- lapply(sp, function(sl){
    res10 <- readparquet_byid(lis[sl])
    cnames <- Reduce(intersect, lapply(res10,colnames))
    res10 <- do.call(rbind, lapply(res10, function(patvals) 
      patvals[,cnames]))
  })
}

readparquet_byid <- function(l,var_id=3030000310){
  lapply(1:length(l), function(v){
    if(length(l[[v]]) > 0) {
      chunk <- read_parquet(paste0("database/derived_numeric/",names(l[v])))
      chunk <- chunk[chunk$a_patientid %in% as.numeric(names(l[[v]])),]
      chunk$var_name <- dvar$name[match(chunk$a_variableid,dvar$a_variableid)]
      chunk$a_variableid <- as.numeric(chunk$a_variableid) 
      if(!is.null(var_id)){
        chunk <- chunk[chunk$a_variableid == var_id,]
      }
      chunk <- spread(chunk[,c("var_name","time","value","a_patientid")],
                      key = c("var_name"),value="value")
    }
  })
}

lis <- local({
  nf <- list.files("database/derived_numeric/")
  lf <- nf %>% gsub(x=., "derived_numeric_","") %>% 
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
dvars_fi <- do.call(rbind,dvarsres)

write.csv(dvars_fi,"SOFA_all_patients.csv")
