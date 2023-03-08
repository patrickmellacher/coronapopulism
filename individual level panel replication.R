library(haven)
library(ggplot2)
library(sqldf)
###download the SUF edition of the Austrian Corona Panel Project here: https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/28KQNS

library(fixest)
library(modelsummary)
library(gt)
library(here)
setwd(here())

austrian_coronapanel_full <- read_dta("10094_da_de_v5_0.dta")

#wave1
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$W1_Q12
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$W1_Q9A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$W1_Q9A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$W1_Q10A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$W1_Q10A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W1_Q12), ]$SD_GENDER
fpoe_votingintention <- NA
treatment <- 0
wave_1 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_1$wave <- "wave 1"
wave_1$date <- as.Date("2020-03-27")

#wave2
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$W2_Q14
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$W2_Q11A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$W2_Q11A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$W2_Q12A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$W2_Q12A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W2_Q14), ]$SD_GENDER
fpoe_votingintention <- NA
treatment <- 0
wave_2 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_2$wave <- "wave 2"
wave_2$date <- as.Date("2020-04-03")

#wave3
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$W3_Q14
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$W3_Q11A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$W3_Q11A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$W3_Q12A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$W3_Q12A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W3_Q14), ]$SD_GENDER
fpoe_votingintention <- NA
treatment <- 0
wave_3 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_3$wave <- "wave 3"
wave_3$date <- as.Date("2020-04-10")

#wave4
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W4_Q15), ]$W4_Q88A3
treatment <- 0
wave_4 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_4$wave <- "wave 4"
wave_4$date <- as.Date("2020-04-17")

#wave6
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W6_Q15), ]$W6_Q100A3
treatment <- 1
wave_6 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_6$wave <- "wave 6"
wave_6$date <- as.Date("2020-05-01")

#wave7
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q18
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q15A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q15A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q16A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q16A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$SD_GENDER
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W7_Q18), ]$W7_Q88A3
treatment <- 1
wave_7 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_7$wave <- "wave 7"
wave_7$date <- as.Date("2020-05-08")

#wave8
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$SD_GENDER
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W8_Q16), ]$W8_Q91A3
treatment <- 1
wave_8 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_8$wave <- "wave 8"
wave_8$date <- as.Date("2020-05-15")

#wave9
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W9_Q15), ]$W9_Q92A3
treatment <- 1
wave_9 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_9$wave <- "wave 9"
wave_9$date <- as.Date("2020-05-23")

#wave10
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$SD_GENDER
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W10_Q15), ]$W10_Q91A3
treatment <- 1
wave_10 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_10$wave <- "wave 10"
wave_10$date <- as.Date("2020-05-29")

#wave11
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q28
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q25A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q25A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q26A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q26A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$SD_GENDER
############
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W11_Q28), ]$W11_Q103A3
treatment <- 1
wave_11 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_11$wave <- "wave 11"
wave_11$date <- as.Date("2020-06-12")


#wave12
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W12_Q15), ]$W12_Q99A3
treatment <- 1
wave_12 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_12$wave <- "wave 12"
wave_12$date <- as.Date("2020-06-26")

#wave13
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$SD_GENDER
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W13_Q15), ]$W13_Q88A3
treatment <- 1
wave_13 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_13$wave <- "wave 13"
wave_13$date <- as.Date("2020-07-10")

#wave14
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$SD_GENDER
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W14_Q16), ]$W14_Q108A3
treatment <- 1
wave_14 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_14$wave <- "wave 14"
wave_14$date <- as.Date("2020-08-14")

#wave15
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$SD_GENDER
########
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W15_Q15), ]$W15_Q90A3
treatment <- 1
wave_15 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_15$wave <- "wave 15"
wave_15$date <- as.Date("2020-09-11")

#wave16
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W16_Q15), ]$W16_Q101A3
treatment <- 1
wave_16 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_16$wave <- "wave 16"
wave_16$date <- as.Date("2020-10-16")

#wave17
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$SD_GENDER
########
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W17_Q16), ]$W17_Q93A3
treatment <- 1
wave_17 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_17$wave <- "wave 17"
wave_17$date <- as.Date("2020-11-13")

#wave18
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$SD_BIRTHYEAR
#covid_diagnosed <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q27
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$SD_GENDER
#######
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W18_Q15), ]$W18_Q104A3
treatment <- 1
wave_18 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_18$wave <- "wave 18"
wave_18$date <- as.Date("2020-12-11")

#wave19
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W19_Q15), ]$W19_Q95A3
treatment <- 1
wave_19 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_19$wave <- "wave 19"
wave_19$date <- as.Date("2021-01-15")

#wave20
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$W20_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$W20_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$W20_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$W20_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$W20_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W20_Q16), ]$SD_GENDER
fpoe_votingintention <- NA
treatment <- 1
wave_20 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_20$wave <- "wave 20"
wave_20$date <- as.Date("2021-02-12")


#wave21
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q29
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q26A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q26A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q27A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q27A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W21_Q29), ]$W21_Q107A3
treatment <- 1
wave_21 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_21$wave <- "wave 21"
wave_21$date <- as.Date("2021-03-12")

#wave22
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$W22_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$W22_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$W22_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$W22_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$W22_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W22_Q15), ]$SD_GENDER
fpoe_votingintention <- NA
treatment <- 1
wave_22 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_22$wave <- "wave 22"
wave_22$date <- as.Date("2021-04-16")

#wave23
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W23_Q16), ]$W23_Q106A3
treatment <- 1
wave_23 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_23$wave <- "wave 23"
wave_23$date <- as.Date("2021-05-21")

#wave24
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$SD_GENDER
######
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W24_Q15), ]$W24_Q106A3
treatment <- 1
wave_24 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_24$wave <- "wave 24"
wave_24$date <- as.Date("2021-06-25")

#wave25
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$SD_GENDER
######
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W25_Q16), ]$W25_Q90A3
treatment <- 1
wave_25 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_25$wave <- "wave 25"
wave_25$date <- as.Date("2021-09-24")

#wave26
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q29
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q26A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q26A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q27A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q27A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W26_Q29), ]$W26_Q111A3
treatment <- 1
wave_26 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_26$wave <- "wave 26"
wave_26$date <- as.Date("2021-10-22")


#wave27
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q16
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q13A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q13A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q14A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q14A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$SD_GENDER
######
####unreliable data!
fpoe_votingintention <- NA#austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W27_Q16), ]$W27_Q103A3
treatment <- 1
wave_27 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_27$wave <- "wave 27"
wave_27$date <- as.Date("2021-11-26")

#wave28
respid <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$RESPID
answer <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q15
voted_for <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$SD_NRVOTE2019
education <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$SD_EDU
danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q12A1
danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q12A2
economic_danger <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q13A1
economic_danger_society <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q13A2
birthyear <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$SD_BIRTHYEAR
gender <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$SD_GENDER
fpoe_votingintention <- austrian_coronapanel_full[!is.na(austrian_coronapanel_full$W28_Q15), ]$W28_Q94A3
treatment <- 1
wave_28 <- data.frame(respid, answer, voted_for, treatment, danger, danger_society, education, economic_danger, economic_danger_society, birthyear, gender, fpoe_votingintention)
wave_28$wave <- "wave 28"
wave_28$date <- as.Date("2022-01-14")

total_waves <- rbind(wave_1, wave_2, wave_3, wave_4, wave_6, wave_7, wave_8, wave_9, wave_10, wave_11, wave_12, wave_13, wave_14, wave_15, wave_16, wave_17, wave_18, wave_19, wave_20, wave_21, wave_22, wave_23, wave_24, wave_25, wave_26, wave_27, wave_28)
total_waves <- total_waves[!is.na(total_waves$voted_for), ]

total_waves$fpoe_voter <- ifelse(total_waves$voted_for == 3, 1, 0)
total_waves$spoe_voter <- ifelse(total_waves$voted_for == 2, 1, 0)
total_waves$oevp_voter <- ifelse(total_waves$voted_for == 1, 1, 0)
total_waves$gruene_voter <- ifelse(total_waves$voted_for == 4, 1, 0)
total_waves$neos_voter <- ifelse(total_waves$voted_for == 5, 1, 0)
total_waves$voter <- ifelse(total_waves$voted_for == 1, "ÖVP", 0)
total_waves$voter <- ifelse(total_waves$voted_for == 2, "SPÖ", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 3, "FPÖ", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 4, "Grüne", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 5, "Neos", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 6, "Liste jetzt", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 9, "Non voter / Invalid", total_waves$voter)
total_waves$voter <- ifelse(total_waves$voted_for == 10, "Non voter / Invalid", total_waves$voter)

total_waves$non_or_invalid_voter <- ifelse(total_waves$voter == "Non voter / Invalid", 1, 0)
total_waves$not_declared <- ifelse(total_waves$voted_for == 99, 1, 0)

total_waves$not_eligible_to_vote <- ifelse(total_waves$voted_for == 11, 1, 0)

total_waves$covid_skeptic <- ifelse(total_waves$answer > 3 & total_waves$answer <= 5, 1, 0)

total_waves$covid_toolax <- ifelse(total_waves$answer < 3, 1, 0)

total_waves$covid_opposition <- total_waves$covid_skeptic + total_waves$covid_toolax

total_waves$covid_unsure <- ifelse(total_waves$answer == 88, 1, 0)

total_waves$covid_noanswer <- ifelse(total_waves$answer == 99, 1, 0)


total_waves$covid_lowdanger <- ifelse(total_waves$danger > 3 & total_waves$danger <= 5, 1, 0)

total_waves$covid_lowdanger_society <- ifelse(total_waves$danger_society > 3 & total_waves$danger_society <= 5, 1, 0)

total_waves$covid_highdanger <- ifelse(total_waves$danger < 3, 1, 0)

total_waves$covid_highdanger_society <- ifelse(total_waves$danger_society < 3, 1, 0)
total_waves$fpoe_voter_label <- ifelse(total_waves$fpoe_voter == 0, "Others", "Voted for FPÖ in 2019")
total_waves$fpoe_voter_label2 <- ifelse(total_waves$voter == "FPÖ", "FPÖ voter", ifelse(total_waves$voter == "Non voter / Invalid", "Non voter / Invalid", "Voter of another party"))
total_waves$skeptic_label <- ifelse(total_waves$covid_skeptic == 0, "Others", "Measures are exaggerated")
total_waves$skeptic_label2 <- ifelse(total_waves$covid_skeptic == 1, "Measures are exaggerated", ifelse(total_waves$covid_toolax == 1, "Measures are too lax", "Measures are appropriate"))


total_waves$only_compulsory <- ifelse(total_waves$education <= 2, 1, 0)
total_waves$lehre_bms <- ifelse(total_waves$education >= 3 & total_waves$education <= 4, 1, 0)
total_waves$matura <- ifelse(total_waves$education >= 5 & total_waves$education <= 6, 1, 0)
total_waves$university <- ifelse(total_waves$education >= 7 & total_waves$education <= 10, 1, 0)

total_waves$above64 <- ifelse(total_waves$birthyear <= 1955, 1, 0)
total_waves$below25 <- ifelse(total_waves$birthyear > 1995, 1, 0)

total_waves$male <- ifelse(total_waves$gender == 1, 1, 0)


total_waves$covid_loweconomicdanger <- ifelse(total_waves$economic_danger > 3 & total_waves$economic_danger <= 5, 1, 0)

total_waves$covid_loweconomicdanger_society <- ifelse(total_waves$economic_danger_society > 3 & total_waves$economic_danger_society <= 5, 1, 0)

total_waves$covid_higheconomicdanger <- ifelse(total_waves$economic_danger < 3, 1, 0)

total_waves$covid_higheconomicdanger_society <- ifelse(total_waves$economic_danger_society < 3, 1, 0)

total_waves$fpoe_votingintention <- ifelse(total_waves$fpoe_votingintention > 10, NA, total_waves$fpoe_votingintention)
total_waves$likely_fpoe_voter <- ifelse(total_waves$fpoe_votingintention > 5 & total_waves$fpoe_votingintention < 11, 1, 0)

only_compulsory_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_only_compulsory, AVG (covid_toolax) as covid_toolax_only_compulsory, AVG (covid_lowdanger) as covid_lowdanger_only_compulsory, AVG (covid_lowdanger_society) as covid_lowdanger_society_only_compulsory, AVG (covid_highdanger) as covid_highdanger_only_compulsory, AVG (covid_highdanger_society) as covid_highdanger_society_only_compulsory, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_only_compulsory, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_only_compulsory, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_only_compulsory, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_only_compulsory FROM total_waves WHERE above64 = 0 and below25 = 0 AND only_compulsory = 1 GROUP BY wave, date")
lehre_bms_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_lehre_bms, AVG (covid_toolax) as covid_toolax_lehre_bms, AVG (covid_lowdanger) as covid_lowdanger_lehre_bms, AVG (covid_lowdanger_society) as covid_lowdanger_society_lehre_bms, AVG (covid_highdanger) as covid_highdanger_lehre_bms, AVG (covid_highdanger_society) as covid_highdanger_society_lehre_bms, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_lehre_bms, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_lehre_bms, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_lehre_bms, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_lehre_bms FROM total_waves WHERE above64 = 0 and below25 = 0 AND lehre_bms = 1 GROUP BY wave, date")
matura_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_matura, AVG (covid_toolax) as covid_toolax_matura, AVG (covid_lowdanger) as covid_lowdanger_matura, AVG (covid_lowdanger_society) as covid_lowdanger_society_matura, AVG (covid_highdanger) as covid_highdanger_matura, AVG (covid_highdanger_society) as covid_highdanger_society_matura, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_matura, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_matura, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_matura, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_matura FROM total_waves WHERE above64 = 0 and below25 = 0 AND matura = 1 GROUP BY wave, date")
university_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_university, AVG (covid_toolax) as covid_toolax_university, AVG (covid_lowdanger) as covid_lowdanger_university, AVG (covid_lowdanger_society) as covid_lowdanger_society_university, AVG (covid_highdanger) as covid_highdanger_university, AVG (covid_highdanger_society) as covid_highdanger_society_university, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_university, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_university, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_university, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_university FROM total_waves WHERE above64 = 0 and below25 = 0 AND university = 1 GROUP BY wave, date")
above64_male_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_above64_male, AVG (covid_toolax) as covid_toolax_above64_male, AVG (covid_lowdanger) as covid_lowdanger_above64_male, AVG (covid_lowdanger_society) as covid_lowdanger_society_above64_male, AVG (covid_highdanger) as covid_highdanger_above64_male, AVG (covid_highdanger_society) as covid_highdanger_society_above64_male, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_above64_male, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_above64_male, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_above64_male, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_above64_male FROM total_waves WHERE above64 = 1 AND male = 1 GROUP BY wave, date")
above64_female_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_above64_female, AVG (covid_toolax) as covid_toolax_above64_female, AVG (covid_lowdanger) as covid_lowdanger_above64_female, AVG (covid_lowdanger_society) as covid_lowdanger_society_above64_female, AVG (covid_highdanger) as covid_highdanger_above64_female, AVG (covid_highdanger_society) as covid_highdanger_society_above64_female, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_above64_female, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_above64_female, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_above64_female, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_above64_female FROM total_waves WHERE above64 = 1 AND male = 0 AND gender < 3 GROUP BY wave, date")
under25_male_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_under25_male, AVG (covid_toolax) as covid_toolax_under25_male, AVG (covid_lowdanger) as covid_lowdanger_under25_male, AVG (covid_lowdanger_society) as covid_lowdanger_society_under25_male, AVG (covid_highdanger) as covid_highdanger_under25_male, AVG (covid_highdanger_society) as covid_highdanger_society_under25_male, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_under25_male, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_under25_male, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_under25_male, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_under25_male FROM total_waves WHERE below25 = 1 AND male = 1 GROUP BY wave, date")
under25_female_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_under25_female, AVG (covid_toolax) as covid_toolax_under25_female, AVG (covid_lowdanger) as covid_lowdanger_under25_female, AVG (covid_lowdanger_society) as covid_lowdanger_society_under25_female, AVG (covid_highdanger) as covid_highdanger_under25_female, AVG (covid_highdanger_society) as covid_highdanger_society_under25_female, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_under25_female, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_under25_female, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_under25_female, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_under25_female FROM total_waves WHERE below25 = 1  AND male = 0 AND gender < 3 GROUP BY wave, date")
#all_timeline <- sqldf("SELECT wave, date, AVG (covid_skeptic) as covid_skeptic_all, AVG (covid_toolax) as covid_toolax_all, AVG (covid_lowdanger) as covid_lowdanger_all, AVG (covid_lowdanger_society) as covid_lowdanger_society_all, AVG (covid_highdanger) as covid_highdanger_all, AVG (covid_highdanger_society) as covid_highdanger_society_all, AVG (covid_loweconomicdanger) as covid_loweconomicdanger_all, AVG (covid_loweconomicdanger_society) as covid_loweconomicdanger_society_all, AVG (covid_higheconomicdanger) as covid_higheconomicdanger_all, AVG (covid_higheconomicdanger_society) as covid_higheconomicdanger_society_all FROM total_waves WHERE below25 = 0 GROUP BY wave, date")

beliefs_and_skepticism <- cbind(only_compulsory_timeline, lehre_bms_timeline[3:12], matura_timeline[3:12], university_timeline[3:12], above64_male_timeline[3:12], above64_female_timeline[3:12], under25_male_timeline[3:12], under25_female_timeline[3:12])


options(scipen=999)
##testing for endogeneity
#probit panel with time FE
fpoe_intention_probit_panel_total1 <- feglm(likely_fpoe_voter ~ fpoe_voter + treatment:fpoe_voter | wave, data =  subset(total_waves), family = binomial('probit'))
summary(fpoe_intention_probit_panel_total1)

#probit panel pooled
fpoe_intention_probit_panel_woFE1 <- feglm(likely_fpoe_voter ~ fpoe_voter + treatment*fpoe_voter, data =  subset(total_waves), family = binomial('probit'))
summary(fpoe_intention_probit_panel_woFE1)

# #only non FPÖ voters
# fpoe_intention_probit_panel_non_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ covid_skeptic + covid_toolax + treatment:covid_toolax + treatment:covid_skeptic | wave, data =  subset(total_waves, fpoe_voter == 0), family = binomial('probit'))
# summary(fpoe_intention_probit_panel_non_FPÖ_voter1)
# 
# #only non FPÖ voters
# fpoe_intention_probit_panel_non_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ covid_skeptic + covid_toolax + treatment:covid_toolax + treatment:covid_skeptic + treatment , data =  subset(total_waves, fpoe_voter == 0), family = binomial('probit'))
# summary(fpoe_intention_probit_panel_non_FPÖ_voter1)
# 
# #only FPÖ voters
# fpoe_intention_probit_panel_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ covid_skeptic + covid_toolax + treatment:covid_toolax + treatment:covid_skeptic | wave, data =  subset(total_waves, fpoe_voter == 1), family = binomial('probit'))
# summary(fpoe_intention_probit_panel_FPÖ_voter1)
# 
# #only FPÖ voters
# fpoe_intention_probit_panel_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ covid_skeptic + treatment:covid_skeptic | wave, data =  subset(total_waves, fpoe_voter == 1), family = binomial('probit'))
# summary(fpoe_intention_probit_panel_FPÖ_voter1)

#taken
#only FPÖ voters
fpoe_intention_probit_panel_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ treatment*covid_toolax + treatment*covid_skeptic , data =  subset(total_waves, fpoe_voter == 1), family = binomial('probit'))
summary(fpoe_intention_probit_panel_FPÖ_voter1)

#only FPÖ voters with FE
fpoe_intention_probit_panel_FPÖ_voter2 <- feglm(likely_fpoe_voter ~ treatment*covid_toolax + treatment*covid_skeptic | wave , data =  subset(total_waves, fpoe_voter == 1), family = binomial('probit'))
summary(fpoe_intention_probit_panel_FPÖ_voter2)

#only non-FPÖ voters
fpoe_intention_probit_panel_non_FPÖ_voter1 <- feglm(likely_fpoe_voter ~ treatment*covid_toolax + treatment*covid_skeptic , data =  subset(total_waves, fpoe_voter == 0), family = binomial('probit'))
summary(fpoe_intention_probit_panel_non_FPÖ_voter1)

#only non-FPÖ voters with FE
fpoe_intention_probit_panel_non_FPÖ_voter2 <- feglm(likely_fpoe_voter ~ treatment*covid_toolax + treatment*covid_skeptic | wave , data =  subset(total_waves, fpoe_voter == 0), family = binomial('probit'))
summary(fpoe_intention_probit_panel_non_FPÖ_voter2)


#both voters
fpoe_intention_probit_panel_both_voters1 <- feglm(likely_fpoe_voter ~ treatment*fpoe_voter , data =  subset(total_waves), family = binomial('probit'))
summary(fpoe_intention_probit_panel_both_voters1)

#both voters with FE
fpoe_intention_probit_panel_both_voters2 <- feglm(likely_fpoe_voter ~ treatment*fpoe_voter | wave , data =  subset(total_waves), family = binomial('probit'))
summary(fpoe_intention_probit_panel_both_voters2)

library("gt")
library("modelsummary")

cm <- c('covid_skeptic' = 'Government response is exaggerated',
        'treatment:covid_skeptic' = 'Government response is exaggerated x Policy switch',
        'covid_toolax' = 'Government response is too lax',
        'treatment:covid_toolax' = 'Government response is too lax x Policy switch',
        'likely_fpoe_voter' = 'Would likely vote for the FPÖ',
        'fpoe_voter' = 'Voted for FPÖ in 2019',
        'treatment:fpoe_voter' = 'Voted for FPÖ in 2019 x Policy switch',
        'treatment' = 'Policy switch',
        'male' = "Male",
        'treatment:male' = 'Male x Policy switch',
        'below25' = "Below the age of 25",
        'below25:treatment' = 'Below the age of 25 x Policy switch',
        'above64' = "Above the age of 64",
        'treatment:above64' = 'Above the age of 64 x Policy switch',
        'lehre_bms' = 'Highest education: Apprenticeship',
        'treatment:lehre_bms' = 'Highest education: Apprenticeship x Policy switch',
        'matura' = 'Highest education: Secondary education',
        'treatment:matura' = 'Highest education: Secondary education x Policy switch',
        'university' = 'Highest education: University degree',
        'treatment:university' = 'Highest education: University degree x Policy switch',
        'covid_lowdanger' = 'Covid poses low danger to personal health',
        'covid_lowdanger_society' = 'Covid poses low danger to public health',
        'covid_loweconomicdanger' = 'Covid poses low danger to personal economic situation',
        'covid_loweconomicdanger_society' = 'Covid poses low danger to economy',
        'covid_highdanger' = 'Covid poses high danger to personal health',
        'covid_highdanger_society' = 'Covid poses high danger to public health',
        'covid_higheconomicdanger' = 'Covid poses high danger to personal economic situation',
        'covid_higheconomicdanger_society' = 'Covid poses high danger to economy',
        '(Intercept)' = 'Intercept')

tab1 <- modelsummary(list('(1)' =  fpoe_intention_probit_panel_non_FPÖ_voter1, '(2)' =  fpoe_intention_probit_panel_non_FPÖ_voter2, '(3)' = fpoe_intention_probit_panel_FPÖ_voter1, '(4)' = fpoe_intention_probit_panel_FPÖ_voter2, '(5)' = fpoe_intention_probit_panel_both_voters1, '(6)' = fpoe_intention_probit_panel_both_voters2), title = "Table 1: Voting intentions for the FPÖ", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, 
                     output = "gt", vcov=~wave+respid)


tab1 <- tab1 %>%
  # column labels
  tab_spanner(label = 'Group: Did not vote for FPÖ in 2019 only', columns = 2:3) %>%
  tab_spanner(label = 'Group: Voted for FPÖ in 2019 only', columns = 4:5) %>%
  tab_spanner(label = 'Group: All respondents', columns = 6:7)


gt::gtsave(tab1, filename = "table1.html")


#skepticism
#just time FE
skeptic_probit_panel1 <- feglm(covid_skeptic ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(skeptic_probit_panel1)

#with age
skeptic_probit_panel2 <- feglm(covid_skeptic ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(skeptic_probit_panel2)

#with age and education
skeptic_probit_panel3 <- feglm(covid_skeptic ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(skeptic_probit_panel3)

#with low danger to personal health & to economy
skeptic_probit_panel4 <- feglm(covid_skeptic ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter + covid_lowdanger + covid_lowdanger_society + covid_loweconomicdanger + covid_loweconomicdanger_society | wave, data =  total_waves, family = binomial('probit'))
summary(skeptic_probit_panel4)

#with high danger to personal health & economy
skeptic_probit_panel5 <- feglm(covid_skeptic ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter + covid_highdanger + covid_highdanger_society + covid_higheconomicdanger + covid_higheconomicdanger_society | wave, data =  total_waves, family = binomial('probit'))
summary(skeptic_probit_panel5)



#low health danger

lowdanger_probit_panel1 <- feglm(covid_lowdanger ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_probit_panel1)

#with age
lowdanger_probit_panel2 <- feglm(covid_lowdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_probit_panel2)

#with age and education
lowdanger_probit_panel3 <- feglm(covid_lowdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_probit_panel3)


#low health danger (society)

lowdanger_society_probit_panel1 <- feglm(covid_lowdanger_society ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_society_probit_panel1)

#with age
lowdanger_society_probit_panel2 <- feglm(covid_lowdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 +  treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_society_probit_panel2)

#with age and education
lowdanger_society_probit_panel3 <- feglm(covid_lowdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(lowdanger_society_probit_panel3)

#danger to economy
high_danger_economy_probit_panel1 <- feglm(covid_higheconomicdanger ~ fpoe_voter + treatment:fpoe_voter | wave, data =  subset(total_waves), family = binomial('probit'))
summary(high_danger_economy_probit_panel1)

#with age
high_danger_economy_probit_panel2 <- feglm(covid_higheconomicdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(high_danger_economy_probit_panel2)

#with age and edcuation
high_danger_economy_probit_panel3 <- feglm(covid_higheconomicdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(high_danger_economy_probit_panel3)

#low danger to economy
low_danger_economy_probit_panel1 <- feglm(covid_loweconomicdanger ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(low_danger_economy_probit_panel1, vcov = ~wave+respid)

#with age
low_danger_economy_probit_panel2 <- feglm(covid_loweconomicdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(low_danger_economy_probit_panel2)

#with age and edcuation
low_danger_economy_probit_panel3 <- feglm(covid_loweconomicdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(low_danger_economy_probit_panel3)

#danger to economy (society)
high_danger_economy_society_probit_panel1 <- feglm(covid_higheconomicdanger_society ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(high_danger_economy_society_probit_panel1)

#with age
high_danger_economy_society_probit_panel2 <- feglm(covid_higheconomicdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(high_danger_economy_society_probit_panel2)

#with age and education
high_danger_economy_society_probit_panel3 <- feglm(covid_higheconomicdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(high_danger_economy_society_probit_panel3)

#danger to economy (society)
Low_danger_economy_society_probit_panel1 <- feglm(covid_loweconomicdanger_society ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(Low_danger_economy_society_probit_panel1)

#with age
Low_danger_economy_society_probit_panel2 <- feglm(covid_loweconomicdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(Low_danger_economy_society_probit_panel2)

#with age and education
Low_danger_economy_society_probit_panel3 <- feglm(covid_loweconomicdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(Low_danger_economy_society_probit_panel3)



#too lax
#just time FE
too_lax_probit_panel1 <- feglm(covid_toolax ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(too_lax_probit_panel1)


#with age
too_lax_probit_panel2 <- feglm(covid_toolax ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(too_lax_probit_panel2)

#with age and education
too_lax_probit_panel3 <- feglm(covid_toolax ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(too_lax_probit_panel3)

#with low danger to personal health & economy
too_lax_probit_panel4 <- feglm(covid_toolax ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter + covid_lowdanger + covid_lowdanger_society + covid_loweconomicdanger + covid_loweconomicdanger_society | wave, data =  total_waves, family = binomial('probit'))
summary(too_lax_probit_panel4)

#with high danger to personal health & economy
too_lax_probit_panel5 <- feglm(covid_toolax ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:lehre_bms + treatment:above64 + treatment:matura + treatment:university + treatment:fpoe_voter  + covid_highdanger + covid_highdanger_society + covid_higheconomicdanger + covid_higheconomicdanger_society | wave, data =  total_waves, family = binomial('probit'))
summary(too_lax_probit_panel5)




#high health danger
highdanger_probit_panel1 <- feglm(covid_highdanger ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_probit_panel1, vcov=~wave+respid)

#with age
highdanger_probit_panel2 <- feglm(covid_highdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_probit_panel2)

#with age and education
highdanger_probit_panel3 <- feglm(covid_highdanger ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_probit_panel3)


#high health danger
highdanger_society_probit_panel1 <- feglm(covid_highdanger_society ~ fpoe_voter + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_society_probit_panel1)

#with age
highdanger_society_probit_panel2 <- feglm(covid_highdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + treatment:above64 + treatment:fpoe_voter | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_society_probit_panel2)

#with age and education
highdanger_society_probit_panel3 <- feglm(covid_highdanger_society ~ fpoe_voter + below25 + treatment:below25 + male + male:treatment + above64 + lehre_bms + matura + university + treatment:above64 + treatment:fpoe_voter + treatment:lehre_bms + treatment:matura + treatment:university | wave, data =  total_waves, family = binomial('probit'))
summary(highdanger_society_probit_panel3)


options(modelsummary_get="broom")

cm <- c('fpoe_voter' = 'Voted for FPÖ in 2019',
        'fpoe_voter:treatment' = 'Voted for FPÖ in 2019 x Policy switch',
        'treatment' = 'Policy switch',
        'male' = "Male",
        'treatment:male' = 'Male x Policy switch',
        'below25' = "Below the age of 25",
        'below25:treatment' = 'Below the age of 25 x Policy switch',
        'above64' = "Above the age of 64",
        'treatment:above64' = 'Above the age of 64 x Policy switch',
        'lehre_bms' = 'Highest education: Apprenticeship',
        'treatment:lehre_bms' = 'Highest education: Apprenticeship x Policy switch',
        'matura' = 'Highest education: Secondary education',
        'treatment:matura' = 'Highest education: Secondary education x Policy switch',
        'university' = 'Highest education: University degree',
        'treatment:university' = 'Highest education: University degree x Policy switch',
        'covid_lowdanger' = 'Covid poses low danger to personal health',
        'covid_lowdanger_society' = 'Covid poses low danger to public health',
        'covid_loweconomicdanger' = 'Covid poses low danger to personal economic situation',
        'covid_loweconomicdanger_society' = 'Covid poses low danger to economy',
        'covid_highdanger' = 'Covid poses high danger to personal health',
        'covid_highdanger_society' = 'Covid poses high danger to public health',
        'covid_higheconomicdanger' = 'Covid poses high danger to personal economic situation',
        'covid_higheconomicdanger_society' = 'Covid poses high danger to economy')

tab2 <- modelsummary(list('(1)' = skeptic_probit_panel1, '(2)' = skeptic_probit_panel3,'(3)' =  skeptic_probit_panel4, '(4)' =  skeptic_probit_panel5, '(5)' = too_lax_probit_panel1, '(6)' = too_lax_probit_panel3, '(7)' = too_lax_probit_panel4, '(8)' = too_lax_probit_panel5), title = "Table 2: Opinion on government policy", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, 
                    output = "gt", vcov=~wave+respid)

library(gt)
library(tidyverse)

tab2 <- tab2 %>%
  # column labels
  tab_spanner(label = 'DV: Government response is exaggerated', columns = 2:5) %>%
  tab_spanner(label = 'DV: Government response is too lax', columns = 6:9)

gt::gtsave(tab2, filename = "table2.html")

tab3 <- modelsummary(list('(1)' = lowdanger_probit_panel1, '(2)' = lowdanger_probit_panel3, '(3)' = lowdanger_society_probit_panel1,'(4)' =  lowdanger_society_probit_panel3, '(5)' =  low_danger_economy_probit_panel1, '(6)' = low_danger_economy_probit_panel3, '(7)' = Low_danger_economy_society_probit_panel1, '(8)' = Low_danger_economy_society_probit_panel3), title = "Table 3: Low danger", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, 
                     output = "gt", vcov=~wave+respid)


tab3 <- tab3 %>%
  # column labels
  tab_spanner(label = 'DV: Covid poses low danger to personal health ', columns = 2:3) %>%
  tab_spanner(label = 'DV: Covid poses low danger to public health', columns = 4:5) %>%
  tab_spanner(label = 'DV: Covid poses low danger to personal economic situation', columns = 6:7) %>%
  tab_spanner(label = 'DV: Covid poses low danger to economy', columns = 8:9)

gt::gtsave(tab3, filename = "table3.html")


tab4 <- modelsummary(list('(1)' = highdanger_probit_panel1, '(2)' = highdanger_probit_panel3, '(3)' = highdanger_society_probit_panel1,'(4)' =  highdanger_society_probit_panel3, '(5)' =  high_danger_economy_probit_panel1, '(6)' = high_danger_economy_probit_panel3, '(7)' = high_danger_economy_society_probit_panel1, '(8)' = high_danger_economy_society_probit_panel3), title = "Table 4: High danger", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm, 
                     output = "gt", vcov=~wave+respid)


tab4 <- tab4 %>%
  # column labels
  tab_spanner(label = 'DV: Covid poses high danger to personal health ', columns = 2:3) %>%
  tab_spanner(label = 'DV: Covid poses high danger to public health', columns = 4:5) %>%
  tab_spanner(label = 'DV: Covid poses high danger to personal economic situation', columns = 6:7) %>%
  tab_spanner(label = 'DV: Covid poses high danger to economy', columns = 8:9)

gt::gtsave(tab4, filename = "table4.html")








total_waves$wave <- factor(total_waves$wave, levels = c("wave 1", "wave 2", "wave 3", "wave 4", "wave 5", "wave 6", "wave 7", "wave 8", "wave 9", "wave 10", "wave 11", "wave 12", "wave 13", "wave 14", "wave 15", "wave 16", "wave 17", "wave 18", "wave 19", "wave 20", "wave 21", "wave 22", "wave 23", "wave 24"))
library(ggplot2)

Sys.setlocale("LC_TIME", "English") # Windows


##Fig. 2
#700x300
p <- ggplot(subset(total_waves, fpoe_voter == 0), aes(x=date, y=likely_fpoe_voter, color = skeptic_label2 , group = skeptic_label2 )) + 
  labs (title = "Did not vote for the FPÖ in 2019",x = "", y = "Would likely vote for FPÖ now", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange", position = position_dodge2(width = 8, preserve = "single")) + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("darkgreen", "blue", "red")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)


#Fig. 3
#515x300
p <- ggplot(subset(total_waves, fpoe_voter == 1), aes(x=date, y=likely_fpoe_voter, color = skeptic_label2 , group = skeptic_label2 )) + 
  labs (title = "Voted for FPÖ in 2019", x = "", y = "Would likely vote for FPÖ now", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange", position = position_dodge2(width = 8, preserve = "single")) + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("darkgreen", "blue", "red")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()    + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)


#Fig. 4
#515x300
p <- ggplot(subset(total_waves), aes(x=date, y=likely_fpoe_voter, color = fpoe_voter_label , group = fpoe_voter_label )) + 
  labs (title = "FPÖ voting intentions (total)", x = "", y = "Would likely vote for FPÖ now", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = "Policy turn")) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()    + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)


#Fig. 5 (left, row 1)
#360x230
p <- ggplot(subset(total_waves), aes(x=date, y=covid_lowdanger, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "low danger to personal health", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)

#Fig.  5 (right, row 1)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_highdanger, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "high danger to personal health", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() + theme(legend.position="none")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)


#Fig. 5 (left, row 2)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_lowdanger_society, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "low danger to public health", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()  + theme(legend.position="none")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)

#Fig.  5 (right, row 2)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_highdanger_society, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "high danger to public health", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)


#Fig. 5 (left, row 3)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_loweconomicdanger, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "low danger to own econ. situation", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()  + theme(legend.position="none")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=8))
print(p)


#Fig.  5 (right, row 3)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_higheconomicdanger, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "high danger to own econ. situation", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()  + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=8))
print(p)


#Fig. 5 (left, row 4)
p <- ggplot(subset(total_waves), aes(x=date, y=covid_loweconomicdanger_society, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "low danger to economy", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()  + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)

#Fig.  5 (right, row 4)
p <- ggplot(subset(total_waves, not_declared != 1  & not_eligible_to_vote != 1  & economic_danger_society != 99), aes(x=date, y=covid_higheconomicdanger_society, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "high danger to economy", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()  + theme(legend.position="none")  + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)



#Fig. 5 (left, row 5)
#360x280
p <- ggplot(subset(total_waves), aes(x=date, y=covid_skeptic, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "measures are exaggerated", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw()   + theme(legend.position="bottom") + guides(linetype = FALSE) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)

#Fig. 5 (right, row 5)
#360x280
p <- ggplot(subset(total_waves), aes(x=date, y=covid_toolax, color = fpoe_voter_label, group = fpoe_voter_label)) + 
  labs (x = "", y = "measures are too lax", color = "", linetype = "") +
  stat_summary(key_glyph = "pointrange") + 
  geom_vline(aes(xintercept = as.Date("2020-04-27"), linetype = 'Policy turn')) + 
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red","blue")) +
  scale_linetype_manual(values = "dashed") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_bw() + theme(legend.position="bottom", legend.box = "vertical") + guides(color = FALSE) + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + theme(axis.text=element_text(size=9), axis.title=element_text(size=9))
print(p)





