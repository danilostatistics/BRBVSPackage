## code to prepare `AREDS` dataset goes here

usethis::use_data(AREDS, overwrite = TRUE)

## wragling AREDS dataset

library(CopulaCenR)
library(GJRM)
library(plot3D)


dta1 <- AREDS[AREDS$ind==1, ]
dta2 <- AREDS[AREDS$ind==2, ]

names(dta1)[3] <- c('t11')
names(dta1)[4] <- c('t12')

names(dta2)[3] <- c('t21')
names(dta2)[4] <- c('t22')

dta2$cens2 <- as.factor(ifelse(dta2$status==1, 'I', 'R'))
dta1$cens1 <- as.factor(ifelse(dta1$status==1, 'I', 'R'))

dta1$t12 <- ifelse(dta1$t12 == Inf, NA, dta1$t12)
dta2$t22 <- ifelse(dta2$t22 == Inf, NA, dta2$t22)


# the severity score is eye related. Hence, there is one for the first eye and one for the second
names(dta2)[6] <- c('SevScale2E')
names(dta1)[6] <- c('SevScale1E')


AREDS_formatted<- cbind(dta1[,c(3,4,6,7,8,9)],
                        dta2[,c(3,4,6,9)])

tolz <- 1e-04

AREDS_formatted$t11<- ifelse(AREDS_formatted$t11 <= 0, tolz, AREDS_formatted$t11)
AREDS_formatted$t12<- ifelse(AREDS_formatted$t12 <= 0, tolz, AREDS_formatted$t12)
AREDS_formatted$t21<- ifelse(AREDS_formatted$t21 <= 0, tolz, AREDS_formatted$t21)
AREDS_formatted$t22<- ifelse(AREDS_formatted$t22 <= 0, tolz, AREDS_formatted$t22)

AREDS_formatted$rs2284665 <- as.factor(AREDS_formatted$rs2284665)

AREDS_formatted$cens <- as.factor(paste(AREDS_formatted$cens1, AREDS_formatted$cens2, sep=''))


AREDS_formatted <- AREDS_formatted[ AREDS_formatted$SevScale1E < 9,  ]


AREDS_formatted$SevScale1E <- as.factor(AREDS_formatted$SevScale1E)
AREDS_formatted$SevScale2E <- as.factor(AREDS_formatted$SevScale2E)
AREDS<- AREDS_formatted
