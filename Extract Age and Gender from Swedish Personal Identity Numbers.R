
library(lubridate)


# Personal Identity Number = Personnummer
# Swedish PIN structure: https://en.wikipedia.org/wiki/Personal_identity_number_(Sweden)


# 1. Convert PIN to date-of-birth and gender
# 2. Calculate age (current or at specific time)
# 3. Create age-categorizations


### 1. Convert Swedish Personal Identity Numbers to date-of-birth ang gender

# Note: Century is coerced to 1900
# pnummer <- Vector with swedish "personnummer"
pnummer<-c("111111-1234", "19111111-1234",
           "1111111234", "191111111234",
           "111111", "19111111",
           "11-11-11", "1911-11-11",
           "11-11-11-1234", "1911-11-11-1234")

pn<-data.frame(pnummer=pnummer, dob="", gender="1.5", stringsAsFactors=FALSE)
pn$pnummer<-gsub("\\D", "", pn$pnummer) # Remove any non-digit characters

for (i in 1:nrow(pn)) {
        if(nchar(pn$pnummer[i]) == 6) {
                pn$dob[i]<-paste("19", pn$pnummer[i], sep = "")
        }
        else if(nchar(pn$pnummer[i]) == 8) {
                pn$dob[i]<-substr(pn$pnummer[i],1,8)
        }
        else if(nchar(pn$pnummer[i]) == 10) {
                pn$dob[i]<-paste("19", substr(pn$pnummer[i],1,6), sep = "")
                pn$gender[i]<-substr(pn$pnummer[i],9,9)
        }
        else if(nchar(pn$pnummer[i]) == 12) {
                pn$dob[i]<-substr(pn$pnummer[i],1,8)
                pn$gender[i]<-substr(pn$pnummer[i],11,11)
        }
}


# Gender information

pn$gender<-as.numeric(pn$gender)

for (i in seq_along(pn$gender)) {
        if(pn$gender[i] == "1.5") {
                pn$gender[i]<-"NA"
        }
        else if(as.numeric(pn$gender[i]) %% 2 == 0) {
                pn$gender[i]<-"female"
        }
        else if(as.numeric(pn$gender[i]) %% 2 == 1) {
                pn$gender[i]<-"male"
        }
}


### 2. Calculate age (current or at specific time-point)

# calcDate <- Vector of length one: as.Date("2016-10-10") || Sys.Date()
calcDate <- as.Date("2016-10-10")
pn$dob<-ymd(pn$dob)
pn$age<-round(interval(pn$dob, calcDate) / duration(num = 1, units = "years"))



### 3. Age-categorization

pn$age[is.na(pn$age)]<-1
for (i in seq_along(pn$age)) {
        if(pn$age[i] >16 & pn$age[i]<25) {
                pn$ageCat[i]<-"16-24"
        }
        else if(pn$age[i] >24 & pn$age[i] <35) {
                pn$ageCat[i]<-"25-34"
        }
        else if(pn$age[i] >34 & pn$age[i] <45) {
                pn$ageCat[i]<-"35-44"
        }
        else if(pn$age[i] >44 & pn$age[i] <55) {
                pn$ageCat[i]<-"45-54"
        }
        else if(pn$age[i] >54 & pn$age[i] <65) {
                pn$ageCat[i]<-"55-54"
        }
        else if(pn$age[i] >64) {
                pn$ageCat[i]<-"65+"
        }
        else(pn$ageCat[i]<-"Missing")
}

pn$ageCat<-as.factor(pn$ageCat)


### Remove waste
rm(pnummer)
rm(calcDate)
rm(i)

### Print some output
table(pn$ageCat)
table(pn$gender)
tapply(pn$age, pn$gender, mean)
tapply(pn$age, pn$gender, sd)
