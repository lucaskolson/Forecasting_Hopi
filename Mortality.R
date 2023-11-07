NativeSurvivalTable <- read_xlsx("Aggregate Census Stats.xlsx", sheet = 1)
deaths <- readxl::read_xlsx("total deaths by sex AIAN.xlsx")

# separating out population values for males and females into separate dataframes
# generating lifetables for 1940 males
LifetableM1940 <- NativeSurvivalTable[1:18,c(1,3)]
colnames(LifetableM1940) <- c("AgeBracket", "lx")

# generating lifetables for 1940 females
LifetableF1940 <- NativeSurvivalTable[1:18,c(1,4)]
colnames(LifetableF1940) <- c("AgeBracket", "lx")

# generating lifetables for 1950 males
LifetableM1950 <- NativeSurvivalTable[1:18,c(1,6)]
colnames(LifetableM1950) <- c("AgeBracket", "lx")

# generating lifetables for 1950 females
LifetableF1950 <- NativeSurvivalTable[1:18,c(1,7)]
colnames(LifetableF1950) <- c("AgeBracket", "lx")

# generating lifetables for 1960 males
LifetableM1960 <- NativeSurvivalTable[1:18,c(1,9)]
colnames(LifetableM1960) <- c("AgeBracket", "lx")

# generating lifetables for 1960 females
LifetableF1960 <- NativeSurvivalTable[1:18,c(1,10)]
colnames(LifetableF1960) <- c("AgeBracket", "lx")

# generating lifetables for 1970 males
LifetableM1970 <- NativeSurvivalTable[1:18,c(1,12)]
colnames(LifetableM1970) <- c("AgeBracket", "lx")

# generating lifetables for 1970 females
LifetableF1970 <- NativeSurvivalTable[1:18,c(1,13)]
colnames(LifetableF1970) <- c("AgeBracket", "lx")

# generating lifetables for 1980 males
LifetableM1980 <- NativeSurvivalTable[1:18,c(1,15)]
colnames(LifetableM1980) <- c("AgeBracket", "lx")

# generating lifetables for 1980 females
LifetableF1980 <- NativeSurvivalTable[1:18,c(1,16)]
colnames(LifetableF1980) <- c("AgeBracket", "lx")

# generating lifetables for 1990 males
LifetableM1990 <- NativeSurvivalTable[1:18,c(1,18)]
colnames(LifetableM1990) <- c("AgeBracket", "lx")

# generating lifetables for 1990 females
LifetableF1990 <- NativeSurvivalTable[1:18,c(1,19)]
colnames(LifetableF1990) <- c("AgeBracket", "lx")

# generating lifetables for 2000 males
LifetableM2000 <- NativeSurvivalTable[1:18,c(1,21)]
colnames(LifetableM2000) <- c("AgeBracket", "lx")

# generating lifetables for 2000 females
LifetableF2000 <- NativeSurvivalTable[1:18,c(1,22)]
colnames(LifetableF2000) <- c("AgeBracket", "lx")

# generating lifetables for 2010 males
LifetableM2010 <- NativeSurvivalTable[1:18,c(1,24)]
colnames(LifetableM2010) <- c("AgeBracket", "lx")

# generating lifetables for 2010 females
LifetableF2010 <- NativeSurvivalTable[1:18,c(1,25)]
colnames(LifetableF2010) <- c("AgeBracket", "lx")

#imputation
#1940 male, 65-69 years and 70-74 years
#reported value for 60-64 in 1940: 4180
#reported sum for 65-74 in 1940: 6176
#survival rates based on UN model life table for males aged 60-64 with lexp of 39yrs: 0.8089
LifetableM1940[14,2] <- 3381.202 #4180*0.8089
LifetableM1940[15,2] <- 2794.798 #6176 - 3381.202
#reported value for 75+ in 1940: 2854
#survival rates based on UN model life table for males aged 70-74 with lexp of 39yrs: 0.6562
LifetableM1940[16,2] <- 1833.946 #2794.798*0.6562
#survival rates based on UN model life table for males aged 75-79 with lexp of 39yrs: 0.5587
LifetableM1940[17,2] <- 1020.054 #1833.946*0.5587, which is greater than surviving number (1020.054)
LifetableM1940[18,2] <- 0

#1940 female, 65-69 years and 70-74 years
#reported value for 60-64 in 1940: 3452.0
#reported sum for 65-74 in 1940: 5207
#survival rates based on UN model life table for females aged 60-64 with lexp of 35yrs: 0.8166
LifetableF1940[14,2] <- 2818.903 #3452.0*0.8166
LifetableF1940[15,2] <- 2388.097 #5207 - 2818.903
#reported value for 75+ in 1940: 2837.0
#survival rates based on UN model life table for females aged 70-74 with lexp of 35yrs: 0.6724
LifetableF1940[16,2] <- 1605.756 #2388.097*0.6724
#survival rates based on UN model life table for females aged 75-79 with lexp of 35yrs: 0.5633
LifetableF1940[17,2] <- 904.5224 #1605.756*0.5633
LifetableF1940[18,2] <- 326.7216 #2837.0-(1605.756+904.5224)

#1950 male, 80-84 years and 85+ years
#reported value for 70 to 74 years: 2431
#reported value for 75+: 3334
#survival rates based on UN model life table for males aged 70-74 with lexp of 39yrs: 0.6562
#survival rates based on UN model life table for males aged 75-79 with lexp of 39yrs: 0.5587
LifetableM1950[16,2] <- 1595.222 #2431*0.6562
LifetableM1950[17,2] <- 891.2505 #1595.222*0.5587
LifetableM1950[18,2] <- 847.5275 #3334 - (1595.222 + 891.2505)

#1950 female, 80-84 years and 85+ years
#reported value for 70 to 74 years: 2086
#reported value for 75+: 3018
#survival rates based on UN model life table for males aged 70-74 with lexp of 39yrs: 0.6724
#survival rates based on UN model life table for males aged 75-79 with lexp of 39yrs: 0.5633
LifetableF1950[16,2] <- 1402.626 #2086*0.6724
LifetableF1950[17,2] <- 790.0992 #1402.626*0.5633
LifetableF1950[18,2] <- 724.1235 #3018 - (1402.626 + 891.2505)

#1960 male, 80-84 years and 85+ years
#reported value for 70 to 74 years: 3524
#reported value for 75+: 4443
#survival rates based on UN model life table for males aged 70-74 with lexp of 39yrs: 0.6562
#survival rates based on UN model life table for males aged 75-79 with lexp of 39yrs: 0.5587
LifetableM1960[16,2] <- 2312.449 #3524*0.6562
LifetableM1960[17,2] <- 1291.965 #2312.449*0.5587
LifetableM1960[18,2] <- 838.586 #4443 - (2312.449 + 1291.965)

#1960 female, 80-84 years and 85+ years
#reported value for 70 to 74 years: 3333
#reported value for 75+: 4322
#survival rates based on UN model life table for males aged 70-74 with lexp of 39yrs: 0.6724
#survival rates based on UN model life table for males aged 75-79 with lexp of 39yrs: 0.5633
LifetableF1960[16,2] <- 2241.109 #3333*0.6724
LifetableF1960[17,2] <- 1262.417 #2241.109*0.5633
LifetableF1960[18,2] <- 818.474 #4322 - (2241.109 + 1262.417)

#defining empty dfs for mid-decennial years
col_names <- colnames(LifetableM1940)
age_brackets <- LifetableM1940[,1]

LifetableM1945 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1945) <- col_names
LifetableM1945[,1] <- age_brackets

LifetableF1945 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1945) <- col_names
LifetableF1945[,1] <- age_brackets

LifetableM1955 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1955) <- col_names
LifetableM1955[,1] <- age_brackets

LifetableF1955 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1955) <- col_names
LifetableF1955[,1] <- age_brackets

LifetableM1965 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1965) <- col_names
LifetableM1965[,1] <- age_brackets

LifetableF1965 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1965) <- col_names
LifetableF1965[,1] <- age_brackets

LifetableM1975 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1975) <- col_names
LifetableM1975[,1] <- age_brackets

LifetableF1975 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1975) <- col_names
LifetableF1975[,1] <- age_brackets

LifetableM1985 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1985) <- col_names
LifetableM1985[,1] <- age_brackets

LifetableF1985 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1985) <- col_names
LifetableF1985[,1] <- age_brackets

LifetableM1995 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM1995) <- col_names
LifetableM1995[,1] <- age_brackets

LifetableF1995 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF1995) <- col_names
LifetableF1995[,1] <- age_brackets

LifetableM2005 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableM2005) <- col_names
LifetableM2005[,1] <- age_brackets

LifetableF2005 <- data.frame(matrix(ncol=2, nrow = 18))
colnames(LifetableF2005) <- col_names
LifetableF2005[,1] <- age_brackets


#function to create new dataframe for mid-decennial calculations
genMY <- function(mydata1, mydata0, mydata2) {
  
  for (i in 1:nrow(mydata0)) {
    if (i==1) 
    {mydata1[i,2] <- (mydata0[i,2] + mydata2[i,2])/2} #for under five, lx equals average between under five pop. in previous and subsequent periods
    else if (i==nrow(mydata0)) 
    {mydata1[i,2] <- (mydata0[i,2] + mydata2[i,2])/2} #for 85+, lx equals average between under five pop. in previous and subsequent periods
    else {mydata1[i,2] <- (mydata0[i-1,2] + mydata2[i+1,2])/2} #all other age brackets, lx is the linear imputation of previous periods l(x-5) and subsequent periods l(x+5)
  }
  
  mydata1
}

#generate mid-decennial data frames with population numbers using linear assumptions
LifetableM1945 <- genMY(LifetableM1945, LifetableM1940, LifetableM1950)
LifetableF1945 <- genMY(LifetableF1945, LifetableF1940, LifetableF1950)

LifetableM1955 <- genMY(LifetableM1955, LifetableM1950, LifetableM1960)
LifetableF1955 <- genMY(LifetableF1955, LifetableF1950, LifetableF1960)

LifetableM1965 <- genMY(LifetableM1965, LifetableM1960, LifetableM1970)
LifetableF1965 <- genMY(LifetableF1965, LifetableF1960, LifetableF1970)

LifetableM1975 <- genMY(LifetableM1975, LifetableM1970, LifetableM1980)
LifetableF1975 <- genMY(LifetableF1975, LifetableF1970, LifetableF1980)

LifetableM1985 <- genMY(LifetableM1985, LifetableM1980, LifetableM1990)
LifetableF1985 <- genMY(LifetableF1985, LifetableF1980, LifetableF1990)

LifetableM1995 <- genMY(LifetableM1995, LifetableM1990, LifetableM2000)
LifetableF1995 <- genMY(LifetableF1995, LifetableF1990, LifetableF2000)

LifetableM2005 <- genMY(LifetableM2005, LifetableM2000, LifetableM2010)
LifetableF2005 <- genMY(LifetableF2005, LifetableF2000, LifetableF2010)

#death rates entered from US annual report on vital statistics
death_rates <- data.frame("age_bracket" = age_brackets, "1950M" = c(30.4, 1.9, 1.9, 4.9, 4.9, 8.3, 8.3, 8.3, 8.3, 18.3, 18.3, 18.3, 18.3, 70, 70, 70, 70, 70), "1950F" = c(25.3, 1.7, 1.7, 4.9, 4.9, 7.6, 7.6, 7.6, 7.6, 14.6, 14.6, 14.6, 14.6, 59, 59, 59, 59, 59), "2000M" = c(926.6, 26, 25.6, 148.5, 189.1, 193.7, 232.3, 305.4, 449.8, 542.2, 824.4, 1123.5, 1645.2, 2402.4, 3020.8, 3999.7, 5217.6, 7299), "2000F" = c(644.5, 21, 24, 61.5, 76.6, 85.1, 118.8, 204.9, 243.5, 291.4, 447.4, 778.4, 1071.4, 1747.1, 2357.5, 3047.2, 4327.2, 5949.5), "2010M" = c(576.8, 15.2, 21.1, 87.1, 147.5,139.6, 174.5, 216.1, 302.3, 431.2, 570, 878.5, 1047.5, 1591.5, 2555.9, 3840.9, 5489.7, 10268.1), "2010F" = c(390.8, NaN, 12,23.5, 53.7, 79.1, 92.6, 133.5, 160.4, 267.6, 390.5, 532.2, 745.9, 1185.6, 1906.4, 2761.3, 4357.2, 9277.9))

death_rates[,4:7] <- death_rates[,4:6]/100 # convert death rates for 2000-2010 from per 100,000 to per 1,000

#enter in number of deaths for 1960
death_1960 <-  data.frame("age_bracket" = c("under 5", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+", "not stated"), "1960M" = c(663, 73, 182, 185, 213, 245, 313, 324, 288, 164, 8), "1960F" = c(551, 43, 67, 99, 143, 170, 205, 224, 222, 141, 5)) 

#convert number of deaths in 1960 into rates, for men and women
#first, aggregate ppl counts from 1960 into 10 year age brackets
LifetableM1960.mod <- data.frame("ages" = death_1960[1:10,1])
LifetableM1960.mod$total <- NA
LifetableM1960.mod$total[1] <- as.numeric(LifetableM1960[1,2])
LifetableM1960.mod$total[2] <- as.numeric(LifetableM1960[2,2]) + as.numeric(LifetableM1960[3,2])
LifetableM1960.mod$total[3] <- as.numeric(LifetableM1960[4,2]) + as.numeric(LifetableM1960[5,2])
LifetableM1960.mod$total[4] <- as.numeric(LifetableM1960[6,2]) + as.numeric(LifetableM1960[7,2])
LifetableM1960.mod$total[5] <- as.numeric(LifetableM1960[8,2]) + as.numeric(LifetableM1960[9,2])
LifetableM1960.mod$total[6] <- as.numeric(LifetableM1960[10,2]) + as.numeric(LifetableM1960[11,2])
LifetableM1960.mod$total[7] <- as.numeric(LifetableM1960[12,2]) + as.numeric(LifetableM1960[13,2])
LifetableM1960.mod$total[8] <- as.numeric(LifetableM1960[14,2]) + as.numeric(LifetableM1960[15,2])
LifetableM1960.mod$total[9] <- as.numeric(LifetableM1960[16,2]) + as.numeric(LifetableM1960[17,2])
LifetableM1960.mod$total[10] <- as.numeric(LifetableM1960[18,2])

LifetableF1960.mod <- data.frame("ages" = death_1960[1:10,2])
LifetableF1960.mod$total <- NA
LifetableF1960.mod$total[1] <- as.numeric(LifetableF1960[1,2])
LifetableF1960.mod$total[2] <- as.numeric(LifetableF1960[2,2]) + as.numeric(LifetableF1960[3,2])
LifetableF1960.mod$total[3] <- as.numeric(LifetableF1960[4,2]) + as.numeric(LifetableF1960[5,2])
LifetableF1960.mod$total[4] <- as.numeric(LifetableF1960[6,2]) + as.numeric(LifetableF1960[7,2])
LifetableF1960.mod$total[5] <- as.numeric(LifetableF1960[8,2]) + as.numeric(LifetableF1960[9,2])
LifetableF1960.mod$total[6] <- as.numeric(LifetableF1960[10,2]) + as.numeric(LifetableF1960[11,2])
LifetableF1960.mod$total[7] <- as.numeric(LifetableF1960[12,2]) + as.numeric(LifetableF1960[13,2])
LifetableF1960.mod$total[8] <- as.numeric(LifetableF1960[14,2]) + as.numeric(LifetableF1960[15,2])
LifetableF1960.mod$total[9] <- as.numeric(LifetableF1960[16,2]) + as.numeric(LifetableF1960[17,2])
LifetableF1960.mod$total[10] <- as.numeric(LifetableF1960[18,2])

#second, use ppl counts to calculate age specific death rates
for (i in 1:(nrow(death_1960)-1)) 
{death_1960$`1960Mrates`[i] <- death_1960[i,2]/(LifetableM1960.mod$total[i]/1000)}

for (i in 1:(nrow(death_1960)-1)) 
{death_1960$`1960Frates`[i] <- death_1960[i,3]/(LifetableF1960.mod$total[i]/1000)}

#third, convert death rates back to five year age brackets, assuming equivalence across ten year periods
death_rates$`1960M`[1] <- death_1960$`1960Mrates`[1]
death_rates$`1960M`[2] <- death_1960$`1960Mrates`[2]
death_rates$`1960M`[3] <- death_1960$`1960Mrates`[2]
death_rates$`1960M`[4] <- death_1960$`1960Mrates`[3]
death_rates$`1960M`[5] <- death_1960$`1960Mrates`[3]
death_rates$`1960M`[6] <- death_1960$`1960Mrates`[4]
death_rates$`1960M`[7] <- death_1960$`1960Mrates`[4]
death_rates$`1960M`[8] <- death_1960$`1960Mrates`[5]
death_rates$`1960M`[9] <- death_1960$`1960Mrates`[5]
death_rates$`1960M`[10] <- death_1960$`1960Mrates`[6]
death_rates$`1960M`[11] <- death_1960$`1960Mrates`[6]
death_rates$`1960M`[12] <- death_1960$`1960Mrates`[7]
death_rates$`1960M`[13] <- death_1960$`1960Mrates`[7]
death_rates$`1960M`[14] <- death_1960$`1960Mrates`[8]
death_rates$`1960M`[15] <- death_1960$`1960Mrates`[8]
death_rates$`1960M`[16] <- death_1960$`1960Mrates`[9]
death_rates$`1960M`[17] <- death_1960$`1960Mrates`[9]
death_rates$`1960M`[18] <- death_1960$`1960Mrates`[10]

death_rates$`1960F`[1] <- death_1960$`1960Frates`[1]
death_rates$`1960F`[2] <- death_1960$`1960Frates`[2]
death_rates$`1960F`[3] <- death_1960$`1960Frates`[2]
death_rates$`1960F`[4] <- death_1960$`1960Frates`[3]
death_rates$`1960F`[5] <- death_1960$`1960Frates`[3]
death_rates$`1960F`[6] <- death_1960$`1960Frates`[4]
death_rates$`1960F`[7] <- death_1960$`1960Frates`[4]
death_rates$`1960F`[8] <- death_1960$`1960Frates`[5]
death_rates$`1960F`[9] <- death_1960$`1960Frates`[5]
death_rates$`1960F`[10] <- death_1960$`1960Frates`[6]
death_rates$`1960F`[11] <- death_1960$`1960Frates`[6]
death_rates$`1960F`[12] <- death_1960$`1960Frates`[7]
death_rates$`1960F`[13] <- death_1960$`1960Frates`[7]
death_rates$`1960F`[14] <- death_1960$`1960Frates`[8]
death_rates$`1960F`[15] <- death_1960$`1960Frates`[8]
death_rates$`1960F`[16] <- death_1960$`1960Frates`[9]
death_rates$`1960F`[17] <- death_1960$`1960Frates`[9]
death_rates$`1960F`[18] <- death_1960$`1960Frates`[10]


#reorder columns chronologically
col_order <- c("AgeBracket", "X1950M", "X1950F", "1960M", "1960F", "X2000M", "X2000F", "X2010M", "X2010F")
death_rates <- death_rates[,col_order]

#clean up column titles
colnames(death_rates) <- c("AgeBracket", "1950M", "1950F", "1960M", "1960F", "2000M", "2000F", "2010M", "2010F")

#split death rates into male and female data frame
death_rates_M <- death_rates[,c(1,2,4,6,8)]
death_rates_F <- death_rates[,c(1,3,5,7,9)]

#impute death rates based on linear assumption, for males
death_rates_M$`1955M` <- (death_rates_M$`1950M` + death_rates_M$`1960M`)/2
death_rates_M$`2005M` <- (death_rates_M$`2000M` + death_rates_M$`2010M`)/2

death_rates_M$`1980M` <- (death_rates_M$`1960M` + death_rates_M$`2000M`)/2

death_rates_M$`1970M` <- (death_rates_M$`1960M` + death_rates_M$`1980M`)/2
death_rates_M$`1965M` <- (death_rates_M$`1960M` + death_rates_M$`1970M`)/2
death_rates_M$`1975M` <- (death_rates_M$`1970M` + death_rates_M$`1980M`)/2

death_rates_M$`1990M` <- (death_rates_M$`1980M` + death_rates_M$`2000M`)/2
death_rates_M$`1985M` <- (death_rates_M$`1980M` + death_rates_M$`1990M`)/2
death_rates_M$`1995M` <- (death_rates_M$`1990M` + death_rates_M$`2000M`)/2

#reorder columns chronologically, for males
col_order1M <- c("AgeBracket", "1950M", "1955M", "1960M", "1965M", "1970M", "1975M", "1980M", "1985M", "1990M", "1995M", "2000M", "2005M", "2010M")
death_rates_M <- death_rates_M[,col_order1M]

#impute death rates based on linear assumption, for females
death_rates_F$`1955F` <- (death_rates_F$`1950F` + death_rates_F$`1960F`)/2
death_rates_F$`2005F` <- (death_rates_F$`2000F` + death_rates_F$`2010F`)/2

death_rates_F$`1980F` <- (death_rates_F$`1960F` + death_rates_F$`2000F`)/2

death_rates_F$`1970F` <- (death_rates_F$`1960F` + death_rates_F$`1980F`)/2
death_rates_F$`1965F` <- (death_rates_F$`1960F` + death_rates_F$`1970F`)/2
death_rates_F$`1975F` <- (death_rates_F$`1970F` + death_rates_F$`1980F`)/2

death_rates_F$`1990F` <- (death_rates_F$`1980F` + death_rates_F$`2000F`)/2
death_rates_F$`1985F` <- (death_rates_F$`1980F` + death_rates_F$`1990F`)/2
death_rates_F$`1995F` <- (death_rates_F$`1990F` + death_rates_F$`2000F`)/2

#reorder columns chronologically, for females
col_order1F <- c("AgeBracket", "1950F", "1955F", "1960F", "1965F", "1970F", "1975F", "1980F", "1985F", "1990F", "1995F", "2000F", "2005F", "2010F")
death_rates_F <- death_rates_F[,col_order1F]


#calculate deaths for 1940-45, females
for (i in 1:nrow(LifetableF1940)) {
  if (i==nrow(LifetableF1940)) {LifetableF1940$d[i] <- LifetableF1940$lx[i]} #for 85+, deaths equals # of ppl that period
  else {LifetableF1940$d[i] <- LifetableF1945$lx[i] - LifetableF1940$lx[i+1]} #for all other ages, deaths equals difference in subsequent age brackets of next period
}

for (i in 1:nrow(LifetableF1945)) {
  if (i==nrow(LifetableF1945)) {LifetableF1945$d[i] <- LifetableF1945$lx[i]} #for 85+, deaths equals # of ppl that period
  else {LifetableF1945$d[i] <- LifetableF1950$lx[i] - LifetableF1945$lx[i+1]} #for all other ages, deaths equals difference in subsequent age brackets of next period
}

#calculate deaths for 1940-45, males
for (i in 1:nrow(LifetableM1940)) {
  if (i==nrow(LifetableM1940)) {LifetableM1940$d[i] <- LifetableM1940$lx[i]} #for 85+, deaths equals # of ppl that period
  else {LifetableM1940$d[i] <- LifetableM1945$lx[i] - LifetableM1940$lx[i+1]} #for all other ages, deaths equals difference in subsequent age brackets of next period
}

for (i in 1:nrow(LifetableM1945)) {
  if (i==nrow(LifetableM1945)) {LifetableM1945$d[i] <- LifetableM1945$lx[i]} #for 85+, deaths equals # of ppl that period
  else {LifetableM1945$d[i] <- LifetableM1950$lx[i] - LifetableM1945$lx[i+1]} #for all other ages, deaths equals difference in subsequent age brackets of next period
}


#calculate deaths for 1950-2010, females
LifetableF1950$d <- (LifetableF1950$lx/1000)*death_rates_F$`1950F`
LifetableF1955$d <- (LifetableF1955$lx/1000)*death_rates_F$`1955F`
LifetableF1960$d <- (LifetableF1960$lx/1000)*death_rates_F$`1960F`
LifetableF1965$d <- (LifetableF1965$lx/1000)*death_rates_F$`1965F`
LifetableF1970$d <- (LifetableF1970$lx/1000)*death_rates_F$`1970F`
LifetableF1975$d <- (LifetableF1975$lx/1000)*death_rates_F$`1975F`
LifetableF1980$d <- (LifetableF1980$lx/1000)*death_rates_F$`1980F`
LifetableF1985$d <- (LifetableF1985$lx/1000)*death_rates_F$`1985F`
LifetableF1990$d <- (LifetableF1990$lx/1000)*death_rates_F$`1990F`
LifetableF1995$d <- (LifetableF1995$lx/1000)*death_rates_F$`1995F`
LifetableF2000$d <- (LifetableF2000$lx/1000)*death_rates_F$`2000F`
LifetableF2005$d <- (LifetableF2005$lx/1000)*death_rates_F$`2005F`
LifetableF2010$d <- (LifetableF2010$lx/1000)*death_rates_F$`2010F`

#calculate deaths for 1950-2010, males
LifetableM1950$d <- (LifetableM1950$lx/1000)*death_rates_M$`1950M`
LifetableM1955$d <- (LifetableM1955$lx/1000)*death_rates_M$`1955M`
LifetableM1960$d <- (LifetableM1960$lx/1000)*death_rates_M$`1960M`
LifetableM1965$d <- (LifetableM1965$lx/1000)*death_rates_M$`1965M`
LifetableM1970$d <- (LifetableM1970$lx/1000)*death_rates_M$`1970M`
LifetableM1975$d <- (LifetableF1975$lx/1000)*death_rates_M$`1975M`
LifetableM1980$d <- (LifetableM1980$lx/1000)*death_rates_M$`1980M`
LifetableM1985$d <- (LifetableM1985$lx/1000)*death_rates_M$`1985M`
LifetableM1990$d <- (LifetableM1990$lx/1000)*death_rates_M$`1990M`
LifetableM1995$d <- (LifetableM1995$lx/1000)*death_rates_M$`1995M`
LifetableM2000$d <- (LifetableM2000$lx/1000)*death_rates_M$`2000M`
LifetableM2005$d <- (LifetableM2005$lx/1000)*death_rates_M$`2005M`
LifetableM2010$d <- (LifetableM2010$lx/1000)*death_rates_M$`2010M`


# UN Model lifetables, general pattern for males, p202, https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwihtKzAlO_3AhWFk4kEHbWUBt4QFnoECA4QAQ&url=https%3A%2F%2Fwww.un.org%2Fdevelopment%2Fdesa%2Fpd%2Fsites%2Fwww.un.org.development.desa.pd%2Ffiles%2Ffiles%2Fdocuments%2F2020%2FJan%2Fun_1982_model_life_tables_for_developing_countries.pdf&usg=AOvVaw2_btxXmZ3brBmq82tOAcNb
#a for under five is guestimated for value bw 0 and 1
axM <- c(1, 2.5, 2.5, 2.626, 2.571, 2.534, 2.542, 2.540, 2.541, 2.540, 2.535, 2.532, 2.523, 2.483, 2.412, 2.322, 2.232, 3.921)

# UN Model lifetables, general pattern for females, p223, https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&ved=2ahUKEwihtKzAlO_3AhWFk4kEHbWUBt4QFnoECA4QAQ&url=https%3A%2F%2Fwww.un.org%2Fdevelopment%2Fdesa%2Fpd%2Fsites%2Fwww.un.org.development.desa.pd%2Ffiles%2Ffiles%2Fdocuments%2F2020%2FJan%2Fun_1982_model_life_tables_for_developing_countries.pdf&usg=AOvVaw2_btxXmZ3brBmq82tOAcNb
#a for under five is guestimated for value bw 0 and 1
axF <- c(1, 2.5, 2.5, 2.644, 2.558, 2.507, 2.493, 2.477, 2.494, 2.533, 2.557, 2.556, 2.537, 2.507, 2.455, 2.353, 2.255, 3.773)


#define function to modify population counts into complete life tables
genLT <- function(life_data) {
  life_data$a <- axM #adding in values for a
  
  life_data$L <- NaN #adding person years lived 
  
  for (i in 1:nrow(life_data)) {
    if (i==nrow(life_data)) {life_data$L[i] <- life_data$lx[i]*life_data$a[i]}
    else {life_data$L[i] <- life_data$lx[i+1]*5 + life_data$a[i]*life_data$d[i]}
  }
  
  life_data$m <- NaN #adding mortality rates
  
  for (i in 1:nrow(life_data)) 
  {life_data$m[i] <- life_data$d[i]/life_data$L[i]}
  
  life_data$q <- NaN #adding qx
  
  for (i in 1:nrow(life_data)) 
  {life_data$q[i] <- 5*life_data$m[i]/(1+2.5*life_data$m[i])}
  
  life_data$p <- NaN #adding px
  
  for (i in 1:nrow(life_data)) 
  {life_data$p[i] <- 1 - life_data$q[i]}
  
  life_data$Tx <- NaN #adding Tx
  
  for (i in 1:nrow(life_data)) 
  {life_data$Tx[i] <- sum(life_data$L[i:nrow(life_data)])}
  
  
  life_data$ex <- NaN #adding ex
  
  for (i in 1:nrow(life_data)) 
  {life_data$ex[i] <- life_data$Tx[i]/life_data$lx[i]}
  
  life_data
}


LT_all <- list(LifetableM1940=LifetableM1940, LifetableF1940=LifetableF1940, LifetableM1945=LifetableM1945, LifetableF1945=LifetableF1945, LifetableM1950=LifetableM1950, LifetableF1950=LifetableF1950, LifetableM1955=LifetableM1955, LifetableF1955=LifetableF1955, LifetableM1960=LifetableM1960, LifetableF1960=LifetableF1960, LifetableM1965=LifetableM1965, LifetableF1965=LifetableF1965, LifetableM1970=LifetableM1970, LifetableF1970=LifetableF1970, LifetableM1975=LifetableM1975, LifetableF1975=LifetableF1975, LifetableM1980=LifetableM1980, LifetableF1980=LifetableF1980, LifetableM1985=LifetableM1985, LifetableF1985=LifetableF1985, LifetableM1990=LifetableM1990, LifetableF1990=LifetableF1990, LifetableM1995=LifetableM1995, LifetableF1995=LifetableF1995, LifetableM2000=LifetableM2000, LifetableF2000=LifetableF2000, LifetableM2005=LifetableM2005, LifetableF2005=LifetableF2005, LifetableM2010=LifetableM2010, LifetableF2010=LifetableF2010)

LT_all <- lapply(LT_all, genLT)
list2env(LT_all, .GlobalEnv)


ages <- seq(0,85,5) 
exp_years_m <- seq(39,62,1)
exp_years_f <- seq(35,64,1)

#create df for male five year survival ratios based on UN model life tables, p. 270
exp_m <- data.frame(matrix(nrow = 18, ncol = 24))
colnames(exp_m) <- exp_years_m
rownames(exp_m) <- ages

exp_m[,1] <- c(.7854, .9117, .9710, .9746, .9631, .9543, .9480, .9381, .9254, .9082, .8848, .8536, .8089, .7427, .6562, .5587, .4595, .3251)
exp_m[,2] <- c(.7930, .9163, .9725, .9759, .9648, .9564, .9503, .9407, .9283, .9112, .8881, .8570, .8125, .7468, .6607, .5632, .4635, .3276)
exp_m[,3] <- c(.8006, .9207, .9740, .9770, .9664, .9583, .9525, .9432, .9310, .9143, .8913, .8604, .8161, .7508, .6651, .5678, .4676, .3301)
exp_m[,4] <- c(.8080, .9250, .9753, .9782, .9680, .9603, .9546, .9457, .9337, .9172, .8945, .8637, .8197, .7548, .6696, .5724, .4717, .3326)
exp_m[,5] <- c(.8152, .9290, .9767, .9792, .9695, .9621, .9567, .9481, .9364, .9201, .8976, .8671, .8233, .7589, .6741, .5770, .4759, .3352)
exp_m[,6] <- c(.8224, .9329, .9779, .9803, .9710, .9639, .9587, .9504, .9390, .9230, .9007, .8704, .8269, .7629, .6787, .5817, .4801, .3379)
exp_m[,7] <- c(.8294, .9367, .9792, .9813, .9724, .9656, .9607, .9526, .9415, .9258, .9038, .8737, .8305, .7670, .6833, .5864, .4844, .3405)
exp_m[,8] <- c(.8362, .9403, .9803, .9823, .9738, .9673, .9626, .9548, .9439, .9285, .9068, .8769, .8341, .7711, .6879, .5913, .4888, .3433)
exp_m[,9] <- c(.8430, .9438, .9815, .9832, .9751, .9689, .9644, .9570, .9463, .9312, .9098, .8802, .8377, .7752, .6926, .5961, .4932, .3460)  
exp_m[,10] <- c(.8496, .9471, .9826, .9841, .9764, .9705, .9662, .9590, .9487, .9338, .9127, .8834, .8412, .7794, .6973, .6011, .4978, .3489)
exp_m[,11] <- c(.8561, .9503, .9836, .9850, .9777, .9720, .9679, .9611, .9510, .9365, .9156, .8867, .8448, .7835, .7021, .6061, .5024, .3517)
exp_m[,12] <- c(.8625, .9534, .9846, .9858, .9789, .9735, .9696, .9630, .9532, .9390, .9185, .8899, .8484, .7877, .7069, .6112, .5071, .3547)
exp_m[,13] <- c(.8688, .9563, .9856, .9867, .9800, .9750, .9712, .9649, .9554, .9415, .9214, .8931, .8520, .7919, .7118, .6164, .5120, .3577)
exp_m[,14] <- c(.8809, .9619, .9874, .9882, .9823, .9777, .9743, .9686, .9596, .9464, .9270, .8994, .8593, .8005, .7217, .6271, .5220, .3639)
exp_m[,15] <- c(.8868, .9645, .9882, .9889, .9833, .9790, .9758, .9703, .9617, .9488, .9298, .9026, .8629, .8048, .7268, .6326, .5271, .3671)
exp_m[,16] <- c(.8924, .9671, .9890, .9896, .9843, .9803, .9773, .9720, .9637, .9512, .9326, .9058, .8666, .8092, .7320, .6382, .5325, .3704)
exp_m[,17] <- c(.8980, .9697, .9898, .9903, .9853, .9815, .9787, .9737, .9656, .9535, .9353, .9089, .8703, .8136, .7373, .6440, .5379, .3738)
exp_m[,18] <- c(.9034, .9721, .9906, .9910, .9863, .9827, .9800, .9753, .9675, .9558, .9380, .9121, .8740, .8181, .7426, .6499, .5436, .3774)
exp_m[,19] <- c(.9087, .9743, .9913, .9916, .9872, .9838, .9813, .9768, .9694, .9580, .9407, .9153, .8777, .8227, .7481, .6559, .5494, .3810)
exp_m[,20] <- c(.9139, .9765, .9920, .9922, .9881, .9849, .9826, .9783, .9712, .9602, .9433, .9184, .8815, .8273, .7537, .6621, .5553, .3847)
exp_m[,21] <- c(.9190, .9786, .9926, .9928, .9890, .9860, .9838, .9798, .9730, .9624, .9460, .9216, .8853, .8320, .7593, .6684, .5615, .3885)
exp_m[,22] <- c(.9240, .9805, .9932, .9933, .9898, .9870, .9850, .9812, .9747, .9645, .9486, .9247, .8891, .8367, .7651, .6749, .5678, .3925)
exp_m[,23] <- c(.9289, .9823, .9938, .9939, .9906, .9880, .9861, .9826, .9764, .9666, .9511, .9279, .8929, .8415, .7710, .6816, .5744, .3967)
exp_m[,24] <- c(.9336, .9841, .9944, .9944, .9913, .9889, .9872, .9839, .9780, .9686, .9537, .9310, .8968, .8463, .7770, .6884, .5812, .4009)
#exp_m[,25] <- c(.9382, .9857, .9949, .9949, .9921, .9899, .9882, .9851, .9796, .9706, .9562, .9341, .9007, .8513, .7832, .6955, .5883, .4054)
#exp_m[,26] <- c(.9427, .9872, .9954, .9953, .9927, .9907, .9892, .9864, .9811, .9726, .9587, .9373, .9046, .8563, .7895, .7028, .5957, .4100)
#exp_m[,27] <- c(.9471, .9886, .9959, .9958, .9934, .9916, .9902, .9875, .9826, .9745, .9612, .9404, .9086, .8614, .7959, .7103, .6033, .4148)
#exp_m[,28] <- c(.9513, .9900, .9963, .9962, .9940, .9923, .9911, .9886, .9840, .9763, .9636, .9435, .9126, .8666, .8025, .7181, .6112, .4198)
#exp_m[,29] <- c(.9554, .9912, .9967, .9966, .9946, .9931, .9920, .9897, .9854, .9781, .9660, .9466, .9166, .8719, .8093, .7261, .6195, .4251)
#exp_m[,30] <- c(.9593, .9923, .9971, .9970, .9952, .9938, .9928, .9907, .9867, .9799, .9684, .9497, .9207, .8772, .8162, .7343, .6281, .4306)

#create df for female five year survival ratios based on UN model life tables, p.271
exp_f <- data.frame(matrix(nrow = 18, ncol = 30))
colnames(exp_f) <- exp_years_f
rownames(exp_f) <- ages

exp_f[,1] <- c(.7771, .8810, .9580, .9614, .9414, .9291, .9228, .9176, .9156, .9098, .8926, .8618, .8166, .7555, .6724, .5633, .4501, .3061)
exp_f[,2] <- c(.7837, .8863, .9601, .9634, .9443, .9325, .9264, .9211, .9187, .9126, .8955, .8650, .8203, .7596, .6769, .5681, .4545, .3088)
exp_f[,3] <- c(0.7902	,0.8915	,0.9621	,0.9652	,0.9471	,0.9358	,0.9298	,0.9244	,0.9217	,0.9153	,0.8983	,0.8682	,0.824	,0.7637	,0.6813	,0.5729	,0.4589	,0.3116)
exp_f[,4] <- c(0.7966	,0.8964	,0.964	,0.967	,0.9499	,0.939	,0.933	,0.9276	,0.9246	,0.918	,0.9011	,0.8714	,0.8277	,0.7678	,0.6858	,0.5777	,0.4634	,0.3143)
exp_f[,5] <- c(0.8029	,0.9012	,0.9658	,0.9687	,0.9525	,0.942	,0.9362	,0.9307	,0.9274	,0.9206	,0.9039	,0.8745	,0.8313	,0.7719	,0.6902	,0.5825	,0.4679	,0.3171)
exp_f[,6] <- c(0.809	,0.9059	,0.9676	,0.9703	,0.9549	,0.945	,0.9392	,0.9337	,0.9302	,0.9232	,0.9066	,0.8776	,0.8349	,0.7759	,0.6946	,0.5873	,0.4725	,0.3199)
exp_f[,7] <- c(0.8151	,0.9103	,0.9692	,0.9718	,0.9573	,0.9478	,0.9421	,0.9367	,0.9329	,0.9257	,0.9092	,0.8806	,0.8384	,0.7799	,0.6991	,0.5922	,0.477	,0.3228)
exp_f[,8] <- c(0.821	,0.9146	,0.9709	,0.9733	,0.9596	,0.9505	,0.9449	,0.9395	,0.9355	,0.9282	,0.9118	,0.8836	,0.842	,0.784	,0.7036	,0.5971	,0.4817	,0.3256)
exp_f[,9] <- c(0.8269	,0.9188	,0.9724	,0.9748	,0.9618	,0.9531	,0.9476	,0.9422	,0.938	,0.9306	,0.9144	,0.8866	,0.8455	,0.788	,0.708	,0.602	,0.4864	,0.3285)
exp_f[,10] <- c(0.8326	,0.9228	,0.9739	,0.9761	,0.9639	,0.9556	,0.9503	,0.9449	,0.9405	,0.933	,0.9169	,0.8896	,0.849	,0.792	,0.7125	,0.607	,0.4911	,0.3315)
exp_f[,11] <- c(0.8383	,0.9267	,0.9753	,0.9774	,0.9659	,0.958	,0.9528	,0.9475	,0.9429	,0.9353	,0.9194	,0.8925	,0.8524	,0.796	,0.717	,0.612	,0.496	,0.3345)  
exp_f[,12] <- c(0.8439	,0.9304	,0.9767	,0.9787	,0.9679	,0.9603	,0.9553	,0.9499	,0.9453	,0.9376	,0.9219	,0.8954	,0.8559	,0.8	,0.7215	,0.6171	,0.5009	,0.3375)
exp_f[,13] <- c(0.8494	,0.934	,0.978	,0.9799	,0.9697	,0.9626	,0.9576	,0.9524	,0.9476	,0.9398	,0.9243	,0.8982	,0.8593	,0.804	,0.7261	,0.6223	,0.5058	,0.3406)
exp_f[,14] <- c(0.8548	,0.9375	,0.9793	,0.9811	,0.9715	,0.9647	,0.9599	,0.9547	,0.9498	,0.942	,0.9267	,0.9011	,0.8628	,0.808	,0.7307	,0.6275	,0.5109	,0.3437)
exp_f[,15] <- c(0.8601	,0.9409	,0.9805	,0.9822	,0.9732	,0.9668	,0.9621	,0.957	,0.952	,0.9442	,0.9291	,0.9039	,0.8662	,0.812	,0.7353	,0.6327	,0.516	,0.3469)
exp_f[,16] <- c(0.8653	,0.9441	,0.9817	,0.9833	,0.9749	,0.9687	,0.9642	,0.9592	,0.9542	,0.9463	,0.9314	,0.9067	,0.8696	,0.8161	,0.74	,0.6381	,0.5213	,0.3502)
exp_f[,17] <- c(0.8705	,0.9473	,0.9828	,0.9843	,0.9765	,0.9706	,0.9663	,0.9613	,0.9563	,0.9484	,0.9337	,0.9095	,.8730	,0.8201	,0.7447	,0.6435	,0.5266	,0.3536)
exp_f[,18] <- c(0.8756	,0.9503	,0.9839	,0.9853	,0.978	,0.9725	,0.9682	,0.9634	,0.9583	,0.9505	,0.936	,0.9123	,0.8764	,0.8242	,0.7494	,0.649	,0.5321	,0.3569)
exp_f[,19] <- c(0.8805	,0.9534	,0.9849	,0.9862	,0.9794	,0.9742	,0.9701	,0.9654	,0.9603	,0.9525	,0.9383	,0.915	,0.8798	,0.8283	,0.7542	,0.6546	,0.5376	,0.3604)
exp_f[,20] <- c(0.8853	,0.9563	,0.9859	,0.9871	,0.9808	,0.9759	,0.972	,0.9674	,0.9623	,0.9545	,0.9406	,0.9178	,0.8832	,0.8324	,0.7591	,0.6603	,0.5433	,0.364)
exp_f[,21] <- c(0.89	,0.9592	,0.9868	,0.988	,0.9821	,0.9775	,0.9738	,0.9693	,0.9642	,0.9564	,0.9428	,0.9205	,0.8866	,0.8365	,0.764	,0.666	,0.5491	,0.3677)
exp_f[,22] <- c(0.8947	,0.9619	,0.9877	,0.9888	,0.9834	,0.9791	,0.9755	,0.9711	,0.966	,0.9584	,0.945	,0.9232	,0.89	,0.8407	,0.7689	,0.6719	,0.5551	,0.3714)
exp_f[,23] <- c(0.8993	,0.9645	,0.9886	,0.9897	,0.9846	,0.9806	,0.9771	,0.9729	,0.9679	,0.9603	,0.9472	,0.9259	,0.8934	,0.8449	,0.774	,0.6779	,0.5612	,0.3753)
exp_f[,24] <- c(0.9038	,0.9671	,0.9894	,0.9904	,0.9858	,0.982	,0.9787	,0.9746	,0.9696	,0.9621	,0.9494	,0.9286	,0.8969	,0.8491	,0.7791	,0.684	,0.5675	,0.3792)
exp_f[,25] <- c(0.9083	,0.9695	,0.9902	,0.9911	,0.9869	,0.9834	,0.9802	,0.9763	,0.9714	,0.964	,0.9515	,0.9313	,0.9003	,0.8533	,0.7842	,0.6903	,0.5739	,0.3833)
exp_f[,26] <- c(0.9127	,0.9718	,0.991	,0.9918	,0.9879	,0.9846	,0.9816	,0.9779	,0.973	,0.9658	,0.9536	,0.934	,0.9037	,0.8576	,0.7895	,0.6967	,0.5806	,0.3875)
exp_f[,27] <- c(0.917	,0.974	,0.9917	,0.9925	,0.9889	,0.9859	,0.983	,0.9794	,0.9747	,0.9676	,0.9557	,0.9366	,0.9071	,0.8619	,0.7948	,0.7032	,0.5874	,0.3918)
exp_f[,28] <- c(0.9213	,0.9761	,0.9924	,0.9931	,0.9899	,0.9871	,0.9844	,0.9809	,0.9763	,0.9693	,0.9578	,0.9393	,0.9105	,0.8663	,0.8002	,0.7098	,0.5944	,0.3963)
exp_f[,29] <- c(0.9255	,0.9781	,0.9931	,0.9937	,0.9908	,0.9882	,0.9856	,0.9823	,0.9778	,0.971	,0.9599	,0.9419	,0.914	,0.8707	,0.8057	,0.7167	,0.6016	,0.4009)
exp_f[,30] <- c(0.9296	,0.98	,0.9937	,0.9943	,0.9916	,0.9892	,0.9869	,0.9837	,0.9793	,0.9727	,0.9619	,0.9445	,0.9174	,0.8751	,0.8113	,0.7236	,0.6091	,0.4057)


#brass relational model 1940f
p.logit.1940.f <- .5*log(LifetableF1940$p/(1-LifetableF1940$p))
p.logit.1940.f <- p.logit.1940.f

p.model.logit.1940.f <- .5*log(exp_f[,3]/(1-exp_f[,3])) #ex = 37
p.model.logit.1940.f <- p.model.logit.1940.f

logit.1940.f <- lm(p.logit.1940.f ~ p.model.logit.1940.f)
alpha <- logit.1940.f$coefficients[1]
beta <- logit.1940.f$coefficients[2]

brass.fit.1940.f <- alpha + beta*p.model.logit.1940.f
brass.fit.1940.f <- 1/(1+exp(-2*brass.fit.1940.f))


#brass relational model 1945f
p.logit.1945.f <- .5*log(LifetableF1945$p/(1-LifetableF1945$p))
p.logit.1945.f <- p.logit.1945.f

p.model.logit.1945.f <- .5*log(exp_f[,2]/(1-exp_f[,2])) #ex = 36
p.model.logit.1945.f <- p.model.logit.1945.f

logit.1945.f <- lm(p.logit.1945.f ~ p.model.logit.1945.f)
alpha <- logit.1945.f$coefficients[1]
beta <- logit.1945.f$coefficients[2]

brass.fit.1945.f <- alpha + beta*p.model.logit.1945.f
brass.fit.1945.f <- 1/(1+exp(-2*brass.fit.1945.f))


#brass relational model 1950f
p.logit.1950.f <- .5*log(LifetableF1950$p/(1-LifetableF1950$p)) 
p.logit.1950.f <- p.logit.1950.f

p.model.logit.1950.f <- .5*log(exp_f[,1]/(1-exp_f[,1])) #ex = 34, uses ex=35 instead bc of data availability
p.model.logit.1950.f <- p.model.logit.1950.f

logit.1950.f <- lm(p.logit.1950.f ~ p.model.logit.1950.f)
alpha <- logit.1950.f$coefficients[1]
beta <- logit.1950.f$coefficients[2]

brass.fit.1950.f <- alpha + beta*p.model.logit.1950.f
brass.fit.1950.f <- 1/(1+exp(-2*brass.fit.1950.f))


#brass relational model 1955f
p.logit.1955.f <- .5*log(LifetableF1955$p/(1-LifetableF1955$p)) 
p.logit.1955.f <- p.logit.1955.f

p.model.logit.1955.f <- .5*log(exp_f[,1]/(1-exp_f[,1])) #ex = 32, uses ex=35 instead bc of data availability
p.model.logit.1955.f <- p.model.logit.1955.f

logit.1955.f <- lm(p.logit.1955.f ~ p.model.logit.1955.f)
alpha <- logit.1955.f$coefficients[1]
beta <- logit.1955.f$coefficients[2]

brass.fit.1955.f <- alpha + beta*p.model.logit.1955.f
brass.fit.1955.f <- 1/(1+exp(-2*brass.fit.1955.f))


#brass relational model 1960f
p.logit.1960.f <- .5*log(LifetableF1960$p/(1-LifetableF1960$p))
p.logit.1960.f <- p.logit.1960.f

p.model.logit.1960.f <- .5*log(exp_f[,1]/(1-exp_f[,1])) #ex = 32, uses ex=35 instead bc of data availability
p.model.logit.1960.f <- p.model.logit.1960.f

logit.1960.f <- lm(p.logit.1960.f ~ p.model.logit.1960.f)
alpha <- logit.1960.f$coefficients[1]
beta <- logit.1960.f$coefficients[2]

brass.fit.1960.f <- alpha + beta*p.model.logit.1960.f
brass.fit.1960.f <- 1/(1+exp(-2*brass.fit.1960.f))


#brass relational model 1965f
p.logit.1965.f <- .5*log(LifetableF1965$p/(1-LifetableF1965$p))
p.logit.1965.f <- p.logit.1965.f

p.model.logit.1965.f <- .5*log(exp_f[,5]/(1-exp_f[,5])) #ex = 39
p.model.logit.1965.f <- p.model.logit.1965.f

logit.1965.f <- lm(p.logit.1965.f ~ p.model.logit.1965.f)
alpha <- logit.1965.f$coefficients[1]
beta <- logit.1965.f$coefficients[2]

brass.fit.1965.f <- alpha + beta*p.model.logit.1965.f
brass.fit.1965.f <- 1/(1+exp(-2*brass.fit.1965.f))


#brass relational model 1970f
p.logit.1970.f <- .5*log(LifetableF1970$p/(1-LifetableF1970$p))
p.logit.1970.f <- p.logit.1970.f

p.model.logit.1970.f <- .5*log(exp_f[,12]/(1-exp_f[,12])) #ex = 45
p.model.logit.1970.f <- p.model.logit.1970.f

logit.1970.f <- lm(p.logit.1970.f ~ p.model.logit.1970.f)
alpha <- logit.1970.f$coefficients[1]
beta <- logit.1970.f$coefficients[2]

brass.fit.1970.f <- alpha + beta*p.model.logit.1970.f
brass.fit.1970.f <- 1/(1+exp(-2*brass.fit.1970.f))


#brass relational model 1975f
p.logit.1975.f <- .5*log(LifetableF1975$p/(1-LifetableF1975$p))
p.logit.1975.f <- p.logit.1975.f

p.model.logit.1975.f <- .5*log(exp_f[,17]/(1-exp_f[,17])) #ex = 50
p.model.logit.1975.f <- p.model.logit.1975.f

logit.1975.f <- lm(p.logit.1975.f ~ p.model.logit.1975.f)
alpha <- logit.1975.f$coefficients[1]
beta <- logit.1975.f$coefficients[2]

brass.fit.1975.f <- alpha + beta*p.model.logit.1975.f
brass.fit.1975.f <- 1/(1+exp(-2*brass.fit.1975.f))


#brass relational model 1980f
p.logit.1980.f <- .5*log(LifetableF1980$p/(1-LifetableF1980$p))
p.logit.1980.f <- p.logit.1980.f

p.model.logit.1980.f <- .5*log(exp_f[,12]/(1-exp_f[,12])) #ex = 45
p.model.logit.1980.f <- p.model.logit.1980.f

logit.1980.f <- lm(p.logit.1980.f ~ p.model.logit.1980.f)
alpha <- logit.1980.f$coefficients[1]
beta <- logit.1980.f$coefficients[2]

brass.fit.1980.f <- alpha + beta*p.model.logit.1980.f
brass.fit.1980.f <- 1/(1+exp(-2*brass.fit.1980.f))


#brass relational model 1985f
p.logit.1985.f <- .5*log(LifetableF1985$p/(1-LifetableF1985$p))
p.logit.1985.f <- p.logit.1985.f

p.model.logit.1985.f <- .5*log(exp_f[,20]/(1-exp_f[,20])) #ex = 45
p.model.logit.1985.f <- p.model.logit.1985.f

logit.1985.f <- lm(p.logit.1985.f ~ p.model.logit.1985.f)
alpha <- logit.1985.f$coefficients[1]
beta <- logit.1985.f$coefficients[2]

brass.fit.1985.f <- alpha + beta*p.model.logit.1985.f
brass.fit.1985.f <- 1/(1+exp(-2*brass.fit.1985.f))


#brass relational model 1990f
p.logit.1990.f <- .5*log(LifetableF1990$p/(1-LifetableF1990$p))
p.logit.1990.f <- p.logit.1990.f
#p.logit.1990.f[6] <- NaN

p.model.logit.1990.f <- .5*log(exp_f[,20]/(1-exp_f[,20])) #ex = 53
p.model.logit.1990.f <- p.model.logit.1990.f

logit.1990.f <- lm(p.logit.1990.f ~ p.model.logit.1990.f)
alpha <- logit.1990.f$coefficients[1]
beta <- logit.1990.f$coefficients[2]

brass.fit.1990.f <- alpha + beta*p.model.logit.1990.f
brass.fit.1990.f <- 1/(1+exp(-2*brass.fit.1990.f))


#brass relational model 1995f
p.logit.1995.f <- .5*log(LifetableF1995$p/(1-LifetableF1995$p))
p.logit.1995.f <- p.logit.1995.f

p.model.logit.1995.f <- .5*log(exp_f[,25]/(1-exp_f[,25])) #ex = 58
p.model.logit.1995.f <- p.model.logit.1995.f

logit.1995.f <- lm(p.logit.1995.f ~ p.model.logit.1995.f)
alpha <- logit.1995.f$coefficients[1]
beta <- logit.1995.f$coefficients[2]

brass.fit.1995.f <- alpha + beta*p.model.logit.1995.f
brass.fit.1995.f <- 1/(1+exp(-2*brass.fit.1995.f))


#brass relational model 2000f
p.logit.2000.f <- .5*log(LifetableF2000$p/(1-LifetableF2000$p))
p.logit.2000.f <- p.logit.2000.f

p.model.logit.2000.f <- .5*log(exp_f[,29]/(1-exp_f[,29])) #ex = 62
p.model.logit.2000.f <- p.model.logit.2000.f

logit.2000.f <- lm(p.logit.2000.f ~ p.model.logit.2000.f)
alpha <- logit.2000.f$coefficients[1]
beta <- logit.2000.f$coefficients[2]

brass.fit.2000.f <- alpha + beta*p.model.logit.2000.f
brass.fit.2000.f <- 1/(1+exp(-2*brass.fit.2000.f))


#brass relational model 2005f
p.logit.2005.f <- .5*log(LifetableF2005$p/(1-LifetableF2005$p))
p.logit.2005.f <- p.logit.2005.f

p.model.logit.2005.f <- .5*log(exp_f[,30]/(1-exp_f[,30])) #ex = 62
p.model.logit.2005.f <- p.model.logit.2005.f

logit.2005.f <- lm(p.logit.2005.f ~ p.model.logit.2005.f)
alpha <- logit.2005.f$coefficients[1]
beta <- logit.2005.f$coefficients[2]

brass.fit.2005.f <- alpha + beta*p.model.logit.2005.f
brass.fit.2005.f <- 1/(1+exp(-2*brass.fit.2005.f))


#brass relational model 2000f
p.logit.2010.f <- .5*log(LifetableF2010$p/(1-LifetableF2010$p))
p.logit.2010.f <- p.logit.2010.f

p.model.logit.2010.f <- .5*log(exp_f[,30]/(1-exp_f[,30])) #ex = 63
p.model.logit.2010.f <- p.model.logit.2010.f

logit.2010.f <- lm(p.logit.2010.f ~ p.model.logit.2010.f)
alpha <- logit.2010.f$coefficients[1]
beta <- logit.2010.f$coefficients[2]

brass.fit.2010.f <- alpha + beta*p.model.logit.2010.f
brass.fit.2010.f <- 1/(1+exp(-2*brass.fit.2010.f))


#brass relational model 1940m
p.logit.1940.m <- .5*log(LifetableM1940$p/(1-LifetableM1940$p))
p.logit.1940.m <- p.logit.1940.m
p.logit.1940.m[17:18] <- 0 #change NaNs to 0 to create a lenght 18 output for fitted values, for Leslie function to wor in forecasting

p.model.logit.1940.m <- .5*log(exp_m[,3]/(1-exp_m[,3])) #ex = 37
p.model.logit.1940.m <- p.model.logit.1940.m

logit.1940.m <- lm(p.logit.1940.m ~ p.model.logit.1940.m)
alpha <- logit.1940.m$coefficients[1]
beta <- logit.1940.m$coefficients[2]

brass.fit.1940.m <- alpha + beta*p.model.logit.1940.m
brass.fit.1940.m <- 1/(1+exp(-2*brass.fit.1940.m))


#brass relational model 1945m
p.logit.1945.m <- .5*log(LifetableM1945$p/(1-LifetableM1945$p))
p.logit.1945.m <- p.logit.1945.m

p.model.logit.1945.m <- .5*log(exp_m[,2]/(1-exp_m[,2])) #ex = 36
p.model.logit.1945.m <- p.model.logit.1945.m

logit.1945.m <- lm(p.logit.1945.m ~ p.model.logit.1945.m)
alpha <- logit.1945.m$coefficients[1]
beta <- logit.1945.m$coefficients[2]

brass.fit.1945.m <- alpha + beta*p.model.logit.1945.m
brass.fit.1945.m <- 1/(1+exp(-2*brass.fit.1945.m))


#brass relational model 1950m
p.logit.1950.m <- .5*log(LifetableM1950$p/(1-LifetableM1950$p)) 
p.logit.1950.m <- p.logit.1950.m

p.model.logit.1950.m <- .5*log(exp_m[,1]/(1-exp_m[,1])) #ex = 34, uses ex=35 instead bc of data availability
p.model.logit.1950.m <- p.model.logit.1950.m

logit.1950.m <- lm(p.logit.1950.m ~ p.model.logit.1950.m)
alpha <- logit.1950.m$coefficients[1]
beta <- logit.1950.m$coefficients[2]

brass.fit.1950.m <- alpha + beta*p.model.logit.1950.m
brass.fit.1950.m <- 1/(1+exp(-2*brass.fit.1950.m))


#brass relational model 1955m
p.logit.1955.m <- .5*log(LifetableM1955$p/(1-LifetableM1955$p)) 
p.logit.1955.m <- p.logit.1955.m

p.model.logit.1955.m <- .5*log(exp_m[,1]/(1-exp_m[,1])) #ex = 32, uses ex=35 instead bc of data availability
p.model.logit.1955.m <- p.model.logit.1955.m

logit.1955.m <- lm(p.logit.1955.m ~ p.model.logit.1955.m)
alpha <- logit.1955.m$coefficients[1]
beta <- logit.1955.m$coefficients[2]

brass.fit.1955.m <- alpha + beta*p.model.logit.1955.m
brass.fit.1955.m <- 1/(1+exp(-2*brass.fit.1955.m))


#brass relational model 1960m
p.logit.1960.m <- .5*log(LifetableM1960$p/(1-LifetableM1960$p))
p.logit.1960.m <- p.logit.1960.m

p.model.logit.1960.m <- .5*log(exp_m[,1]/(1-exp_m[,1])) #ex = 32, uses ex=35 instead bc of data availability
p.model.logit.1960.m <- p.model.logit.1960.m

logit.1960.m <- lm(p.logit.1960.m ~ p.model.logit.1960.m)
alpha <- logit.1960.m$coefficients[1]
beta <- logit.1960.m$coefficients[2]

brass.fit.1960.m <- alpha + beta*p.model.logit.1960.m
brass.fit.1960.m <- 1/(1+exp(-2*brass.fit.1960.m))


#brass relational model 1965m
p.logit.1965.m <- .5*log(LifetableM1965$p/(1-LifetableM1965$p))
p.logit.1965.m <- p.logit.1965.m

p.model.logit.1965.m <- .5*log(exp_m[,5]/(1-exp_m[,5])) #ex = 39
p.model.logit.1965.m <- p.model.logit.1965.m

logit.1965.m <- lm(p.logit.1965.m ~ p.model.logit.1965.m)
alpha <- logit.1965.m$coefficients[1]
beta <- logit.1965.m$coefficients[2]

brass.fit.1965.m <- alpha + beta*p.model.logit.1965.m
brass.fit.1965.m <- 1/(1+exp(-2*brass.fit.1965.m))


#brass relational model 1970m
p.logit.1970.m <- .5*log(LifetableM1970$p/(1-LifetableM1970$p))
p.logit.1970.m <- p.logit.1970.m

p.model.logit.1970.m <- .5*log(exp_m[,12]/(1-exp_m[,12])) #ex = 45
p.model.logit.1970.m <- p.model.logit.1970.m

logit.1970.m <- lm(p.logit.1970.m ~ p.model.logit.1970.m)
alpha <- logit.1970.m$coefficients[1]
beta <- logit.1970.m$coefficients[2]

brass.fit.1970.m <- alpha + beta*p.model.logit.1970.m
brass.fit.1970.m <- 1/(1+exp(-2*brass.fit.1970.m))


#brass relational model 1975m
p.logit.1975.m <- .5*log(LifetableM1975$p/(1-LifetableM1975$p))
p.logit.1975.m <- p.logit.1975.m

p.model.logit.1975.m <- .5*log(exp_m[,17]/(1-exp_m[,17])) #ex = 50
p.model.logit.1975.m <- p.model.logit.1975.m

logit.1975.m <- lm(p.logit.1975.m ~ p.model.logit.1975.m)
alpha <- logit.1975.m$coefficients[1]
beta <- logit.1975.m$coefficients[2]

brass.fit.1975.m <- alpha + beta*p.model.logit.1975.m
brass.fit.1975.m <- 1/(1+exp(-2*brass.fit.1975.m))


#brass relational model 1980m
p.logit.1980.m <- .5*log(LifetableM1980$p/(1-LifetableM1980$p))
p.logit.1980.m <- p.logit.1980.m

p.model.logit.1980.m <- .5*log(exp_m[,12]/(1-exp_m[,12])) #ex = 45
p.model.logit.1980.m <- p.model.logit.1980.m

logit.1980.m <- lm(p.logit.1980.m ~ p.model.logit.1980.m)
alpha <- logit.1980.m$coefficients[1]
beta <- logit.1980.m$coefficients[2]

brass.fit.1980.m <- alpha + beta*p.model.logit.1980.m
brass.fit.1980.m <- 1/(1+exp(-2*brass.fit.1980.m))


#brass relational model 1985f
p.logit.1985.m <- .5*log(LifetableM1985$p/(1-LifetableM1985$p))
p.logit.1985.m <- p.logit.1985.m

p.model.logit.1985.m <- .5*log(exp_m[,20]/(1-exp_m[,20])) #ex = 45
p.model.logit.1985.m <- p.model.logit.1985.m

logit.1985.m <- lm(p.logit.1985.m ~ p.model.logit.1985.m)
alpha <- logit.1985.m$coefficients[1]
beta <- logit.1985.m$coefficients[2]

brass.fit.1985.m <- alpha + beta*p.model.logit.1985.m
brass.fit.1985.m <- 1/(1+exp(-2*brass.fit.1985.m))


#brass relational model 1990m
p.logit.1990.m <- .5*log(LifetableM1990$p/(1-LifetableM1990$p))
p.logit.1990.m <- p.logit.1990.m
#p.logit.1990.m[6] <- NaN

p.model.logit.1990.m <- .5*log(exp_m[,20]/(1-exp_m[,20])) #ex = 53
p.model.logit.1990.m <- p.model.logit.1990.m

logit.1990.m <- lm(p.logit.1990.m ~ p.model.logit.1990.m)
alpha <- logit.1990.m$coefficients[1]
beta <- logit.1990.m$coefficients[2]

brass.fit.1990.m <- alpha + beta*p.model.logit.1990.m
brass.fit.1990.m <- 1/(1+exp(-2*brass.fit.1990.m))


#brass relational model 1995f
p.logit.1995.m <- .5*log(LifetableM1995$p/(1-LifetableM1995$p))
p.logit.1995.m <- p.logit.1995.m

p.model.logit.1995.m <- .5*log(exp_m[,24]/(1-exp_m[,24])) #ex = 62
p.model.logit.1995.m <- p.model.logit.1995.m

logit.1995.m <- lm(p.logit.1995.m ~ p.model.logit.1995.m)
alpha <- logit.1995.m$coefficients[1]
beta <- logit.1995.m$coefficients[2]

brass.fit.1995.m <- alpha + beta*p.model.logit.1995.m
brass.fit.1995.m <- 1/(1+exp(-2*brass.fit.1995.m))


#brass relational model 2000m
p.logit.2000.m <- .5*log(LifetableM2000$p/(1-LifetableM2000$p))
p.logit.2000.m <- p.logit.2000.m

p.model.logit.2000.m <- .5*log(exp_m[,24]/(1-exp_m[,24])) #ex = 62
p.model.logit.2000.m <- p.model.logit.2000.m

logit.2000.m <- lm(p.logit.2000.m ~ p.model.logit.2000.m)
alpha <- logit.2000.m$coefficients[1]
beta <- logit.2000.m$coefficients[2]

brass.fit.2000.m <- alpha + beta*p.model.logit.2000.m
brass.fit.2000.m <- 1/(1+exp(-2*brass.fit.2000.m))


#brass relational model 2005m
p.logit.2005.m <- .5*log(LifetableM2005$p/(1-LifetableM2005$p))
p.logit.2005.m <- p.logit.2005.m

p.model.logit.2005.m <- .5*log(exp_m[,24]/(1-exp_m[,24])) #ex = 62
p.model.logit.2005.m <- p.model.logit.2005.m

logit.2005.m <- lm(p.logit.2005.m ~ p.model.logit.2005.m)
alpha <- logit.2005.m$coefficients[1]
beta <- logit.2005.m$coefficients[2]

brass.fit.2005.m <- alpha + beta*p.model.logit.2005.m
brass.fit.2005.m <- 1/(1+exp(-2*brass.fit.2005.m))


#brass relational model 2010m
p.logit.2010.m <- .5*log(LifetableM2010$p/(1-LifetableM2010$p))
p.logit.2010.m <- p.logit.2010.m

p.model.logit.2010.m <- .5*log(exp_m[,24]/(1-exp_m[,24])) #ex = 63
p.model.logit.2010.m <- p.model.logit.2010.m

logit.2010.m <- lm(p.logit.2010.m ~ p.model.logit.2010.m)
alpha <- logit.2010.m$coefficients[1]
beta <- logit.2010.m$coefficients[2]

brass.fit.2010.m <- alpha + beta*p.model.logit.2010.m
brass.fit.2010.m <- 1/(1+exp(-2*brass.fit.2010.m))