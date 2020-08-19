library(data.table)
library(dplyr)
library(parallel)


age <- fread("data/age.txt")
names(age)[2] <- "age"
age$age <- 2020 - age$age
sex <- fread("data/sex.txt")
names(sex)[2] <- "sex"
bmi <- fread("data/bmi.txt")
names(bmi)[2] <- "bmi"

covid <- fread("data/covid19_result_2020_06_05.txt")

table(covid$eid %in% age$eid)

head(covid$eid)
dat <- tibble(eid=age$eid, age=age$age, sex=sex$sex, bmi=bmi$bmi, tested=eid %in% covid$eid %>% as.numeric)
table(dat$tested)

covp <- covid %>% dplyr::select(eid, positive=result)
dat <- merge(dat, covp, by="eid", all.x=TRUE)
dat <- subset(dat, select=-c(eid))


save(dat, file="data/dat.rdata")

