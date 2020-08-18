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

linker <- fread("data/linker_app15825.csv")
covid <- fread("data/covid19_result_2020_06_05.txt")

table(covid$eid %in% age$eid)

head(covid$eid)
dat <- tibble(eid=age$eid, age=age$age, sex=sex$sex, bmi=bmi$bmi, tested=eid %in% covid$eid %>% as.numeric)
table(dat$tested)

table(linker$app %in% dat$eid)
pc1 <- fread("data/pc1.txt")
pc1 <- merge(pc1, linker, by.x="FID", by.y="ieu") %>% dplyr::select(eid=app, pc1=V3, pc2=V4)
dat <- merge(dat, pc1, by="eid", all.x=TRUE)
dat <- subset(dat, select=-c(eid))

save(dat, file="data/dat.rdata")


summary(lm(age ~ pc1, dat))
summary(lm(age ~ pc1, dat, subset=dat$tested==1))

summary(lm(age ~ sex, dat))
summary(lm(age ~ sex, dat, subset=dat$tested==1))

summary(lm(pc1 ~ sex, dat))
summary(lm(pc1 ~ sex, dat, subset=dat$tested==1))

summary(lm(bmi ~ age, dat))
summary(lm(bmi ~ age, dat, subset=dat$tested==1))

summary(lm(pc1 ~ pc2, dat))
summary(lm(pc1 ~ pc2, dat, subset=dat$tested==1))

summary(lm(bmi ~ pc2, dat))
summary(lm(bmi ~ pc2, dat, subset=dat$tested==1))

summary(lm(age ~ pc2, dat))
summary(lm(age ~ pc2, dat, subset=dat$tested==1))

hist(dat$pc1, breaks=100)
