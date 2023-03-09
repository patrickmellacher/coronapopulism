library(ggplot2)
library(sqldf)
library(here)

setwd(here())

covid_faelle_timeline_regional <- read.csv("CovidFaelle_Timeline_GKZ.csv", sep = ';' )

#before and after policy change
covid_faelle_timeline_regional$date <- substr(covid_faelle_timeline_regional$Time, 0, 10)
covid_faelle_timeline_regional$dateAsDate <- as.Date(covid_faelle_timeline_regional$date, format = "%d.%m.%Y")
covid_faelle_timeline_regional$date <- as.character(covid_faelle_timeline_regional$dateAsDate)

####max date set in order to have full weeks!!
covid_faelle_weekly <- sqldf("SELECT GKZ, Bezirk, max(AnzEinwohner) AS population, SUM (AnzahlFaelle) AS tot_cases, SUM(AnzahlTotTaeglich) AS tot_deaths, MAX(AnzahlFaelleSum) AS cum_cases, MAX(AnzahlTotSum) as cum_deaths, strftime('%Y.%W',date) AS week, MAX(date) as max_date, strftime('%Y.%m.%d',date) from covid_faelle_timeline_regional WHERE date <= '2021-12-31' GROUP BY GKZ, Bezirk, strftime('%Y.%W',date)")

#cases and deaths
covid_faelle_weekly$cases_per_1000 <- covid_faelle_weekly$tot_cases / covid_faelle_weekly$population * 1000
covid_faelle_weekly$deaths_per_100000 <- covid_faelle_weekly$tot_deaths / covid_faelle_weekly$population * 100000

wahlergebnisse_nrw19 <- read.csv("wahlergebnisse_nrw19.csv", sep = ';', fileEncoding="latin1")
wahlergebnisse_nrw19$GKZ <- substr(wahlergebnisse_nrw19$X, 2, 4)
wahlergebnisse_nrw19$bezirkscode <- substr(wahlergebnisse_nrw19$X, 5, 6)

wahlergebnisse_nrw19$fpoe_percent19 <- wahlergebnisse_nrw19$FPÖ / wahlergebnisse_nrw19$Abgegebene * 100
wahlergebnisse_nrw19$gruene_percent19 <- wahlergebnisse_nrw19$GRÜNE / wahlergebnisse_nrw19$Abgegebene * 100
wahlergebnisse_nrw19$spoe_percent19 <- wahlergebnisse_nrw19$SPÖ / wahlergebnisse_nrw19$Abgegebene * 100
wahlergebnisse_nrw19$oevp_percent19 <- wahlergebnisse_nrw19$ÖVP / wahlergebnisse_nrw19$Abgegebene * 100
wahlergebnisse_nrw19$neos_percent19 <- wahlergebnisse_nrw19$NEOS/ wahlergebnisse_nrw19$Abgegebene * 100

wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$KPÖ), ]$KPÖ <- 0
wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$WANDL), ]$WANDL <- 0
wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$BZÖ), ]$BZÖ <- 0
wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$BIER), ]$BIER <- 0
wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$GILT), ]$GILT <- 0
wahlergebnisse_nrw19[is.na(wahlergebnisse_nrw19$SLP), ]$SLP <- 0
wahlergebnisse_nrw19$others_percent19 <- (wahlergebnisse_nrw19$KPÖ + wahlergebnisse_nrw19$WANDL + wahlergebnisse_nrw19$BZÖ + wahlergebnisse_nrw19$BIER + wahlergebnisse_nrw19$GILT + wahlergebnisse_nrw19$SLP )/ wahlergebnisse_nrw19$Abgegebene * 100


wahlergebnisse_nrw19_bezirke <- sqldf("select * FROM wahlergebnisse_nrw19 WHERE bezirkscode = '00'")

covid_faelle_weekly<- sqldf("select t1.*, t2.fpoe_percent19, t2.gruene_percent19, t2.spoe_percent19, t2.oevp_percent19, t2.neos_percent19, t2.others_percent19 from covid_faelle_weekly t1 INNER JOIN wahlergebnisse_nrw19_bezirke t2 ON t1.GKZ = t2.GKZ")


wahlergebnisse_bpw16 <- read.csv("wahlergebnisse_bpw16.csv", sep = ';')
wahlergebnisse_bpw16$GKZ_bezirk <- substr(wahlergebnisse_bpw16$GKZ, 2, 4)
wahlergebnisse_bpw16$bezirkscode <- substr(wahlergebnisse_bpw16$GKZ, 5, 6)

wahlergebnisse_bpw16$hofer_pct16 <- wahlergebnisse_bpw16$Hofer / wahlergebnisse_bpw16$Gültige * 100

wahlergebnisse_bpw16_bezirke <- sqldf("select * FROM wahlergebnisse_bpw16 WHERE bezirkscode = '00'")
wahlergebnisse_bpw16_bezirke$GKZ <- wahlergebnisse_bpw16_bezirke$GKZ_bezirk
covid_faelle_weekly <- sqldf("select t1.*, t2.hofer_pct16 FROM covid_faelle_weekly t1 INNER JOIN wahlergebnisse_bpw16_bezirke t2 ON t1.GKZ = t2.GKZ ")


covid_faelle_weekly$fpoe_intervention <- 0
covid_faelle_weekly[covid_faelle_weekly$week >= '2020.18', ]$fpoe_intervention <- 1


covid_faelle_total_before_intervention <- sqldf("select t1.GKZ, SUM(cases_per_1000) AS tot_cases_per_1000_before_intervention, SUM(deaths_per_100000) AS tot_deaths_per_100000_before_intervention from covid_faelle_weekly t1 WHERE fpoe_intervention = 0 GROUP BY t1.GKZ")

covid_faelle_weekly <- sqldf("select t1.*, t2.tot_cases_per_1000_before_intervention, t2.tot_deaths_per_100000_before_intervention from covid_faelle_weekly t1 INNER JOIN covid_faelle_total_before_intervention t2 ON t1.GKZ = t2.GKZ")

 #district demography
 bezirke_demographie <- read.csv("bezirke_demographie.csv", sep = ';')
 bezirke_demographie2 <- read.csv("bezirke_demographie2.csv", sep = ';')
 
 bezirke_demographie <- sqldf("select t1.*, t2.* FROM bezirke_demographie t1 INNER JOIN bezirke_demographie2 t2 ON t1.id = t2.id")
 bezirke_demographie[1] <- NULL
 bezirke_demographie[1] <- NULL

#age groups 5+6: "under 25" in ACPP

bezirke_demographie$m_10_pct <- bezirke_demographie$m_10 / bezirke_demographie$hws * 100
bezirke_demographie$m_11_pct <- bezirke_demographie$m_11 / bezirke_demographie$hws * 100
bezirke_demographie$w_10_pct <- bezirke_demographie$w_10 / bezirke_demographie$hws * 100
bezirke_demographie$w_11_pct <- bezirke_demographie$w_11 / bezirke_demographie$hws  * 100

bezirke_demographie$population_above_14 <- bezirke_demographie$m_05 + bezirke_demographie$m_06 + bezirke_demographie$m_07 + bezirke_demographie$m_08 + bezirke_demographie$m_09 + bezirke_demographie$m_10 + bezirke_demographie$m_11 + bezirke_demographie$w_05 + bezirke_demographie$w_06 + bezirke_demographie$w_07 + bezirke_demographie$w_08 + bezirke_demographie$w_09 + bezirke_demographie$w_10 + bezirke_demographie$w_11
bezirke_demographie$female_above_64_pct_linking <- (bezirke_demographie$w_10 + bezirke_demographie$w_11) / bezirke_demographie$population_above_14
bezirke_demographie$male_above_64_pct_linking <- (bezirke_demographie$m_10 + bezirke_demographie$m_11) / bezirke_demographie$population_above_14
bezirke_demographie$female_under_25_pct_linking <- (bezirke_demographie$w_05 + bezirke_demographie$w_06) / bezirke_demographie$population_above_14
bezirke_demographie$male_under_25_pct_linking <- (bezirke_demographie$m_05 + bezirke_demographie$m_06) / bezirke_demographie$population_above_14

bezirke_demographie$women_pct <- (bezirke_demographie$w_01 + bezirke_demographie$w_02 + bezirke_demographie$w_03 + bezirke_demographie$w_04 + bezirke_demographie$w_05 + bezirke_demographie$w_06 + bezirke_demographie$w_07 + bezirke_demographie$w_08 + bezirke_demographie$w_09 + bezirke_demographie$w_10 + bezirke_demographie$w_11 ) / bezirke_demographie$hws * 100
bezirke_demographie$under_25_pct <- (bezirke_demographie$m_01 + bezirke_demographie$m_02 + bezirke_demographie$m_03 + bezirke_demographie$m_04 +  bezirke_demographie$m_05 + bezirke_demographie$m_06) + (bezirke_demographie$w_01 + bezirke_demographie$w_02 + bezirke_demographie$w_03 + bezirke_demographie$w_04 +  bezirke_demographie$w_05 + bezirke_demographie$w_06) / bezirke_demographie$hws * 100

bezirke_demographie$geb_aut_in_pct <- bezirke_demographie$geb_aut / bezirke_demographie$hws * 100
bezirke_demographie$geb_tur_in_pct <- bezirke_demographie$geb_tur / bezirke_demographie$hws * 100
bezirke_demographie$geb_exj_in_pct <- bezirke_demographie$geb_exj / bezirke_demographie$hws * 100

covid_faelle_weekly <- sqldf("select t1.*, t2.under_25_pct, t2.women_pct, t2.population_above_14, t2.female_above_64_pct_linking, t2.male_above_64_pct_linking, t2.female_under_25_pct_linking, t2.male_under_25_pct_linking, t2.m_10_pct, t2.m_11_pct, t2.w_10_pct, t2.w_11_pct, t2.geb_aut_in_pct, t2.geb_tur_in_pct, t2.geb_exj_in_pct from covid_faelle_weekly t1 INNER JOIN bezirke_demographie t2 ON t1.GKZ = t2.id")

covid_faelle_weekly$tot_11_pct <- covid_faelle_weekly$m_11_pct + covid_faelle_weekly$w_11_pct 

covid_faelle_weekly$tot_10_pct <- covid_faelle_weekly$m_10_pct + covid_faelle_weekly$w_10_pct 

covid_faelle_weekly$tot_10_and_11_pct <- covid_faelle_weekly$m_11_pct + covid_faelle_weekly$w_11_pct + covid_faelle_weekly$m_10_pct + covid_faelle_weekly$w_10_pct


#district income and education


bruttoeinkommen <- read.csv("bruttoeinkommen.csv", sep = ';')

bruttoeinkommen$politischerbezirk <- bruttoeinkommen$Politische.Bezirke


covid_faelle_weekly<- sqldf("select t1.*, bruttoeinkommen_2018, pendler_2018, unselbststaendig_beschaeftigte_jan2020, arbeitslose_jan2020, pflichtschule, lehre, bms, ahs, bhs,  kolleg, akademie, hochschule from covid_faelle_weekly t1 INNER JOIN bruttoeinkommen t2 ON t1.GKZ = t2.id")


covid_faelle_weekly$pflichtschule_in_pct <- covid_faelle_weekly$Pflichtschule / covid_faelle_weekly$population_above_14 * 100 
covid_faelle_weekly$lehre_in_pct <- covid_faelle_weekly$Lehre / covid_faelle_weekly$population_above_14 * 100 
covid_faelle_weekly$bms_in_pct <- covid_faelle_weekly$BMS / covid_faelle_weekly$population_above_14 * 100
covid_faelle_weekly$ahs_in_pct <- covid_faelle_weekly$AHS / covid_faelle_weekly$population_above_14 * 100
covid_faelle_weekly$bhs_in_pct <- covid_faelle_weekly$BHS / covid_faelle_weekly$population_above_14 * 100
covid_faelle_weekly$kolleg_in_pct <- covid_faelle_weekly$Kolleg / covid_faelle_weekly$population_above_14 * 100
covid_faelle_weekly$akademie_in_pct <- covid_faelle_weekly$Akademie / covid_faelle_weekly$population_above_14 * 100
covid_faelle_weekly$hochschule_in_pct <- covid_faelle_weekly$Hochschule / covid_faelle_weekly$population_above_14 * 100 

#covid_faelle_weekly$max_pflichtschule_in_pct <- covid_faelle_weekly$pflichtschule_in_pct * (100 - covid_faelle_weekly$tot_10_and_11_pct) / 100
#covid_faelle_weekly$bms_und_lehre_in_pct <- (covid_faelle_weekly$bms_in_pct + covid_faelle_weekly$lehre_in_pct) * (100 - covid_faelle_weekly$tot_10_and_11_pct) / 100
#covid_faelle_weekly$matura_in_pct <- (covid_faelle_weekly$ahs_in_pct + covid_faelle_weekly$bhs_in_pct) * (100 - covid_faelle_weekly$tot_10_and_11_pct) / 100
#covid_faelle_weekly$university_in_pct <- (covid_faelle_weekly$kolleg_in_pct + covid_faelle_weekly$akademie_in_pct + covid_faelle_weekly$hochschule_in_pct) * (100 - covid_faelle_weekly$tot_10_and_11_pct) / 100

covid_faelle_weekly$max_pflichtschule_in_pct <- covid_faelle_weekly$pflichtschule_in_pct
covid_faelle_weekly$bms_und_lehre_in_pct <- (covid_faelle_weekly$bms_in_pct + covid_faelle_weekly$lehre_in_pct)
covid_faelle_weekly$matura_in_pct <- (covid_faelle_weekly$ahs_in_pct + covid_faelle_weekly$bhs_in_pct)
covid_faelle_weekly$university_in_pct <- (covid_faelle_weekly$kolleg_in_pct + covid_faelle_weekly$akademie_in_pct + covid_faelle_weekly$hochschule_in_pct)


covid_faelle_weekly$pendler_pct <- covid_faelle_weekly$pendler_2018 / covid_faelle_weekly$population * 100
covid_faelle_weekly$workers_pct <- covid_faelle_weekly$unselbststaendig_beschaeftigte_jan2020 / covid_faelle_weekly$population * 100
covid_faelle_weekly$unemployed_pct <- covid_faelle_weekly$arbeitslose_jan2020 / covid_faelle_weekly$population * 100

covid_faelle_weekly$ohne_matura_pct <- covid_faelle_weekly$pflichtschule_in_pct + covid_faelle_weekly$lehre_in_pct + covid_faelle_weekly$bms_in_pct

covid_faelle_weekly$fpoe_above_median <- 0
covid_faelle_weekly[covid_faelle_weekly$fpoe_percent19 > quantile(covid_faelle_weekly$fpoe_percent19, 0.5), ]$fpoe_above_median <- 1

library(data.table)

covid_faelle_weekly_2 <- data.table(covid_faelle_weekly)
covid_faelle_weekly_2 <- covid_faelle_weekly_2[order(max_date), .SD, by = GKZ]

## Rank the date within each GKZ 
covid_faelle_weekly_2 <- covid_faelle_weekly_2[, time:= 1:.N, .(GKZ)]



covid_faelle_weekly_2$group <- "<= median"
covid_faelle_weekly_2[covid_faelle_weekly_2$fpoe_above_median >= 1, ]$group <- "> median"

covid_faelle_weekly_2$dateAsDate <- as.Date(covid_faelle_weekly_2$max_date, format = "%Y-%m-%d")



vaccination_timeline <- read.csv("COVID19_vaccination_doses_timeline.csv", sep = ';')

vaccination_timeline$date <- substr(vaccination_timeline$date, 0, 10)
vaccination_timeline$dateAsDate <- as.Date(vaccination_timeline$date)

vaccination_per_state_and_day <- sqldf("SELECT state_id, date, sum(doses_administered_cumulative) AS cum_vaccinations FROM vaccination_timeline GROUP BY state_id, date")

second_dosevaccination_per_state_and_day <- sqldf("SELECT state_id, date, sum(doses_administered_cumulative) AS cum_second_dose_vaccinations FROM vaccination_timeline WHERE dose_number = 2 OR vaccine = 'Janssen' GROUP BY state_id, date ")


covid_faelle_weekly_2$state_id <- substr(covid_faelle_weekly_2$GKZ, 0, 1)


covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cum_vaccinations FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN vaccination_per_state_and_day t2 on t1.max_date = t2.date and t1.state_id = t2.state_id")
covid_faelle_weekly_2 <- sqldf("SELECT t1.*, cum_second_dose_vaccinations FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN second_dosevaccination_per_state_and_day t2 on t1.max_date = t2.date and t1.state_id = t2.state_id")

covid_faelle_weekly_2$inhabitants <- 296010 #Burgenland
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 2, ]$inhabitants <- 562089 #K?rnten
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 3, ]$inhabitants <- 1690879 #Nieder?sterreich
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 4, ]$inhabitants <- 1495608 #Ober?sterreich
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 5, ]$inhabitants <- 560710 #Salzburg
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 6, ]$inhabitants <- 1247077 #Steiermark
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 7, ]$inhabitants <- 760105 #Tirol
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 8, ]$inhabitants <- 399237 #Vorarlberg
covid_faelle_weekly_2[covid_faelle_weekly_2$state_id == 9, ]$inhabitants <- 1920949 #Wien


covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_vaccinations), ]$cum_vaccinations <- 0

covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_second_dose_vaccinations), ]$cum_second_dose_vaccinations <- 0


#impute regional vaccinations
vaccinations_local <- read.csv("COVID19_vaccination_municipalities.csv", sep = ';')
vaccinations_local$district_id <- substr(vaccinations_local$municipality_id, 0, 3)
vaccinations_local$state_id <- as.numeric(substr(vaccinations_local$municipality_id, 0, 1))
total_vaccinations_district <- sqldf("SELECT SUM(dose_1 + dose_2 + dose_3) AS cum_vaccinations, district_id, state_id FROM vaccinations_local GROUP BY district_id")
total_vaccinations_state <- sqldf("SELECT SUM(dose_1 + dose_2 + dose_3) AS cum_vaccinations, state_id FROM vaccinations_local GROUP BY state_id")
vaccinations_district <- sqldf("SELECT t1.district_id, t1.cum_vaccinations AS cum_vaccinations_district, t2.cum_vaccinations AS cum_vaccinations_state FROM total_vaccinations_district t1 INNER JOIN total_vaccinations_state t2 ON t1.state_id = t2.state_id")

vaccinations_district$cum_vaccination_share_district <- vaccinations_district$cum_vaccinations_district / vaccinations_district$cum_vaccinations_state

covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cum_vaccination_share_district FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN vaccinations_district t2 ON t1.GKZ = t2.district_id")

covid_faelle_weekly_2$cum_vaccinations_per_1k <- covid_faelle_weekly_2$cum_vaccinations / covid_faelle_weekly_2$inhabitants * 1000
covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k <- covid_faelle_weekly_2$cum_second_dose_vaccinations / covid_faelle_weekly_2$inhabitants * 1000
covid_faelle_weekly_2$cum_vaccinations_per_1k_imputed_regional <- ( covid_faelle_weekly_2$cum_vaccinations * covid_faelle_weekly_2$cum_vaccination_share_district) / covid_faelle_weekly_2$population * 1000
covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k_imputed_regional <- ( covid_faelle_weekly_2$cum_second_dose_vaccinations * covid_faelle_weekly_2$cum_vaccination_share_district ) / covid_faelle_weekly_2$population * 1000
covid_faelle_weekly_2$cum_cases_per_1k <- covid_faelle_weekly_2$cum_cases / covid_faelle_weekly_2$population * 1000
covid_faelle_weekly_2$cum_deaths_per_100k <- covid_faelle_weekly_2$cum_deaths / covid_faelle_weekly_2$population * 100000

#Vienna: data on cases and deaths only available for whole state
covid_faelle_weekly_2[covid_faelle_weekly_2$GKZ == 900, ]$cum_vaccinations_per_1k_imputed_regional <- covid_faelle_weekly_2[covid_faelle_weekly_2$GKZ == 900, ]$cum_vaccinations_per_1k
covid_faelle_weekly_2[covid_faelle_weekly_2$GKZ == 900, ]$cum_second_dose_vaccinations_per_1k_imputed_regional <- covid_faelle_weekly_2[covid_faelle_weekly_2$GKZ == 900, ]$cum_second_dose_vaccinations_per_1k

covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cases_per_1000 AS cases_per_1000_1_week_earlier, t2.deaths_per_100000 AS deaths_per_100000_1_week_earlier FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN covid_faelle_weekly_2 t2 ON t1.GKZ = t2.GKZ AND t1.time = (t2.time + 1)")
covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cases_per_1000 AS cases_per_1000_2_weeks_earlier FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN covid_faelle_weekly_2 t2 ON t1.GKZ = t2.GKZ AND t1.time = t2.time + 2")
covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cases_per_1000 AS cases_per_1000_3_weeks_earlier FROM covid_faelle_weekly_2 t1 LEFT OUTER JOIN covid_faelle_weekly_2 t2 ON t1.GKZ = t2.GKZ AND t1.time = t2.time + 3")
covid_faelle_weekly_2 <- sqldf("SELECT t1.*, t2.cum_cases_per_1k AS cum_cases_per_1k_4_weeks_earlier, t2.cum_vaccinations_per_1k  AS cum_vaccinations_per_1k_4_weeks_earlier, t2.cum_second_dose_vaccinations_per_1k AS cum_second_dose_vaccinations_per_1k_4_weeks_earlier, t2.cum_vaccinations_per_1k_imputed_regional AS cum_vaccinations_per_1k_imputed_regional_4_weeks_earlier, t2.cum_second_dose_vaccinations_per_1k_imputed_regional AS cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier FROM covid_faelle_weekly_2 t1 LEFT JOIN covid_faelle_weekly_2 t2 ON t1.GKZ = t2.GKZ AND t1.time = t2.time + 4")

covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cases_per_1000_1_week_earlier), ]$cases_per_1000_1_week_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$deaths_per_100000_1_week_earlier), ]$deaths_per_100000_1_week_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cases_per_1000_2_weeks_earlier), ]$cases_per_1000_2_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cases_per_1000_3_weeks_earlier), ]$cases_per_1000_3_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_cases_per_1k_4_weeks_earlier), ]$cum_cases_per_1k_4_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_vaccinations_per_1k_4_weeks_earlier), ]$cum_vaccinations_per_1k_4_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k_4_weeks_earlier), ]$cum_second_dose_vaccinations_per_1k_4_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_vaccinations_per_1k_imputed_regional_4_weeks_earlier), ]$cum_vaccinations_per_1k_imputed_regional_4_weeks_earlier <- 0
covid_faelle_weekly_2[is.na(covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier), ]$cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier <- 0


covid_faelle_weekly_2$cases_last_2_weeks <- covid_faelle_weekly_2$cases_per_1000_1_week_earlier + covid_faelle_weekly_2$cases_per_1000_2_weeks_earlier
covid_faelle_weekly_2$cases_last_3_weeks <- covid_faelle_weekly_2$cases_per_1000_1_week_earlier + covid_faelle_weekly_2$cases_per_1000_2_weeks_earlier + covid_faelle_weekly_2$cases_per_1000_3_weeks_earlier
covid_faelle_weekly_2$cases_last_3_weeks_incl <- covid_faelle_weekly_2$cases_per_1000_1_week_earlier + covid_faelle_weekly_2$cases_per_1000_2_weeks_earlier + covid_faelle_weekly_2$cases_per_1000_2_weeks_earlier + covid_faelle_weekly_2$cases_per_1000

####import data from ACPP first!
library(zoo)

covid_faelle_weekly_2$acpp_wave <- "wave 27"
max_date_wave26 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_26 t1 INNER JOIN wave_27 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave26$cutoff_date), ]$acpp_wave <- "wave 26"

max_date_wave25 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_25 t1 INNER JOIN wave_26 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave25$cutoff_date), ]$acpp_wave <- "wave 25"

max_date_wave24 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_24 t1 INNER JOIN wave_25 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave24$cutoff_date), ]$acpp_wave <- "wave 24"

max_date_wave23 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_23 t1 INNER JOIN wave_24 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave23$cutoff_date), ]$acpp_wave <- "wave 23"

max_date_wave22 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_22 t1 INNER JOIN wave_23 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave22$cutoff_date), ]$acpp_wave <- "wave 22"

max_date_wave21 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_21 t1 INNER JOIN wave_22 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave21$cutoff_date), ]$acpp_wave <- "wave 21"

max_date_wave20 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_20 t1 INNER JOIN wave_21 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave20$cutoff_date), ]$acpp_wave <- "wave 20"

max_date_wave19 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_19 t1 INNER JOIN wave_20 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave19$cutoff_date), ]$acpp_wave <- "wave 19"

max_date_wave18 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_18 t1 INNER JOIN wave_19 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave18$cutoff_date), ]$acpp_wave <- "wave 18"

max_date_wave17 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_17 t1 INNER JOIN wave_18 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave17$cutoff_date), ]$acpp_wave <- "wave 17"

max_date_wave16 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_16 t1 INNER JOIN wave_17 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave16$cutoff_date), ]$acpp_wave <- "wave 16"

max_date_wave15 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_15 t1 INNER JOIN wave_16 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave15$cutoff_date), ]$acpp_wave <- "wave 15"

max_date_wave14 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_14 t1 INNER JOIN wave_15 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave14$cutoff_date), ]$acpp_wave <- "wave 14"

max_date_wave13 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_13 t1 INNER JOIN wave_14 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave13$cutoff_date), ]$acpp_wave <- "wave 13"

max_date_wave12 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_12 t1 INNER JOIN wave_13 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave12$cutoff_date), ]$acpp_wave <- "wave 12"

max_date_wave11 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_11 t1 INNER JOIN wave_12 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave11$cutoff_date), ]$acpp_wave <- "wave 11"

max_date_wave10 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_10 t1 INNER JOIN wave_11 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave10$cutoff_date), ]$acpp_wave <- "wave 10"

max_date_wave9 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_9 t1 INNER JOIN wave_10 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave9$cutoff_date), ]$acpp_wave <- "wave 9"

max_date_wave8 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_8 t1 INNER JOIN wave_9 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave8$cutoff_date), ]$acpp_wave <- "wave 8"

max_date_wave7 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_7 t1 INNER JOIN wave_8 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave7$cutoff_date), ]$acpp_wave <- "wave 7"

max_date_wave6 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_6 t1 INNER JOIN wave_7 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave6$cutoff_date), ]$acpp_wave <- "wave 6"

max_date_wave4 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_4 t1 INNER JOIN wave_6 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave4$cutoff_date), ]$acpp_wave <- "wave 4"

max_date_wave3 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_3 t1 INNER JOIN wave_4 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave3$cutoff_date), ]$acpp_wave <- "wave 3"

max_date_wave2 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_2 t1 INNER JOIN wave_3 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave2$cutoff_date), ]$acpp_wave <- "wave 2"

max_date_wave1 <- sqldf("SELECT t1.date + ROUND ((t2.date - t1.date ) / 2) AS cutoff_date FROM wave_1 t1 INNER JOIN wave_2 t2 on t1.respid = t2.respid LIMIT 1")
covid_faelle_weekly_2[covid_faelle_weekly_2$max_date <= as.Date(max_date_wave1$cutoff_date), ]$acpp_wave <- "wave 1"

covid_faelle_weekly_3 <- sqldf("SELECT * FROM covid_faelle_weekly_2 t1 INNER JOIN beliefs_and_skepticism t2 ON t1.acpp_wave = t2.wave")


#danger to health
covid_faelle_weekly_3$covid_low_danger <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_lowdanger_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_lowdanger_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_lowdanger_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_lowdanger_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_university
covid_faelle_weekly_3$covid_low_danger_society <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_lowdanger_society_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_lowdanger_society_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_lowdanger_society_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_lowdanger_society_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_society_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_society_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_society_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_lowdanger_society_university

covid_faelle_weekly_3$covid_high_danger <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_highdanger_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_highdanger_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_highdanger_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_highdanger_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_university
covid_faelle_weekly_3$covid_high_danger_society <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_highdanger_society_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_highdanger_society_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_highdanger_society_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_highdanger_society_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_society_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_society_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_society_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_highdanger_society_university

#danger to economy

covid_faelle_weekly_3$covid_low_economicdanger <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_university
covid_faelle_weekly_3$covid_low_economicdanger_society <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_society_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_society_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_society_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_loweconomicdanger_society_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_society_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_society_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_society_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_loweconomicdanger_society_university

covid_faelle_weekly_3$covid_high_economicdanger <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_university
covid_faelle_weekly_3$covid_high_economicdanger_society <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_society_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_society_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_society_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_higheconomicdanger_society_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_society_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_society_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_society_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_higheconomicdanger_society_university


#skeptic (measures are exaggerated)
covid_faelle_weekly_3$covid_skeptic <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_skeptic_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_skeptic_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_skeptic_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_skeptic_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_skeptic_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_skeptic_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_skeptic_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_skeptic_university

#measures are too lax
covid_faelle_weekly_3$covid_toolax <- covid_faelle_weekly_3$male_under_25_pct_linking * covid_faelle_weekly_3$covid_toolax_under25_male + covid_faelle_weekly_3$female_under_25_pct_linking * covid_faelle_weekly_3$covid_toolax_under25_female + covid_faelle_weekly_3$male_above_64_pct_linking * covid_faelle_weekly_3$covid_toolax_above64_male + covid_faelle_weekly_3$female_above_64_pct_linking * covid_faelle_weekly_3$covid_toolax_above64_female + covid_faelle_weekly_3$max_pflichtschule_in_pct / 100 * covid_faelle_weekly_3$covid_toolax_only_compulsory  + covid_faelle_weekly_3$bms_und_lehre_in_pct / 100 * covid_faelle_weekly_3$covid_toolax_lehre_bms + covid_faelle_weekly_3$matura_in_pct / 100 * covid_faelle_weekly_3$covid_toolax_matura  + covid_faelle_weekly_3$university_in_pct / 100 * covid_faelle_weekly_3$covid_toolax_university

covid_faelle_weekly_2$cum_vaccinations_per_1k <- covid_faelle_weekly_2$cum_vaccinations / covid_faelle_weekly_2$inhabitants * 1000
covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k <- covid_faelle_weekly_2$cum_second_dose_vaccinations / covid_faelle_weekly_2$inhabitants * 1000
covid_faelle_weekly_2$cum_vaccinations_per_1k_imputed_regional <- ( covid_faelle_weekly_2$cum_vaccinations * covid_faelle_weekly_2$cum_vaccination_share_district) / covid_faelle_weekly_2$population * 1000
covid_faelle_weekly_2$cum_second_dose_vaccinations_per_1k_imputed_regional <- ( covid_faelle_weekly_2$cum_second_dose_vaccinations * covid_faelle_weekly_2$cum_vaccination_share_district ) / covid_faelle_weekly_2$population * 1000
covid_faelle_weekly_2$cum_cases_per_1k <- covid_faelle_weekly_2$cum_cases / covid_faelle_weekly_2$population * 1000

average_values_weekly <- sqldf("SELECT max_date, AVG(covid_low_danger) AS avg_covid_low_danger, AVG(covid_low_danger_society) AS avg_covid_low_danger_society, AVG(covid_high_danger) AS avg_covid_high_danger, AVG(covid_high_danger_society) AS avg_covid_high_danger_society, AVG(covid_low_economicdanger) AS avg_covid_low_economicdanger, AVG(covid_low_economicdanger_society) AS avg_covid_low_economicdanger_society, AVG(covid_high_economicdanger) AS avg_covid_high_economicdanger, AVG(covid_high_economicdanger_society) AS avg_covid_high_economicdanger_society, AVG(covid_skeptic) AS avg_covid_skeptic, AVG(covid_toolax) AS avg_covid_toolax, AVG(cum_vaccinations_per_1k) AS avg_cum_vaccinations_per_1k, AVG(cum_second_dose_vaccinations_per_1k) as avg_cum_second_dose_vaccinations_per_1k, AVG(cum_vaccinations_per_1k_imputed_regional) AS avg_cum_vaccinations_per_1k_imputed_regional, AVG(cum_cases_per_1k) AS avg_cum_cases_per_1k, AVG(cases_per_1000) AS avg_cases_per_1000, AVG(fpoe_percent19) AS avg_fpoe_percent19 FROM covid_faelle_weekly_3 GROUP BY max_date")

covid_faelle_weekly_4 <- sqldf("SELECT t1.*, t2.avg_covid_low_danger, avg_covid_low_danger_society, avg_covid_high_danger, avg_covid_high_danger_society, avg_covid_low_economicdanger, avg_covid_low_economicdanger_society, avg_covid_high_economicdanger, avg_covid_high_economicdanger_society, avg_covid_skeptic, avg_covid_toolax, avg_cum_vaccinations_per_1k, avg_cum_second_dose_vaccinations_per_1k,  avg_cum_vaccinations_per_1k_imputed_regional, avg_cum_cases_per_1k, avg_cases_per_1000, avg_fpoe_percent19 FROM covid_faelle_weekly_3 t1 INNER JOIN average_values_weekly t2 ON t1.max_date = t2.max_date")


covid_faelle_weekly_4$time <- as.factor(covid_faelle_weekly_4$time)
covid_faelle_weekly_4$GKZ <- as.factor(covid_faelle_weekly_4$GKZ)
covid_faelle_weekly_4$combined_opposition <- covid_faelle_weekly_4$spoe_percent19 +  covid_faelle_weekly_4$fpoe_percent19 + covid_faelle_weekly_4$neos_percent19 + covid_faelle_weekly_4$others_percent19

#centered
#danger to health
covid_faelle_weekly_4$covid_low_danger_centered <- covid_faelle_weekly_4$covid_low_danger - covid_faelle_weekly_4$avg_covid_low_danger
covid_faelle_weekly_4$covid_low_danger_society_centered <- covid_faelle_weekly_4$covid_low_danger_society - covid_faelle_weekly_4$avg_covid_low_danger_society

covid_faelle_weekly_4$covid_high_danger_centered <- covid_faelle_weekly_4$covid_high_danger - covid_faelle_weekly_4$avg_covid_high_danger
covid_faelle_weekly_4$covid_high_danger_society_centered <- covid_faelle_weekly_4$covid_high_danger_society - covid_faelle_weekly_4$avg_covid_high_danger_society

#danger to economy
covid_faelle_weekly_4$covid_low_economicdanger_centered <- covid_faelle_weekly_4$covid_low_economicdanger - covid_faelle_weekly_4$avg_covid_low_economicdanger
covid_faelle_weekly_4$covid_low_economicdanger_society_centered <- covid_faelle_weekly_4$covid_low_economicdanger_society - covid_faelle_weekly_4$avg_covid_low_economicdanger_society

covid_faelle_weekly_4$covid_high_economicdanger_centered <- covid_faelle_weekly_4$covid_high_economicdanger - covid_faelle_weekly_4$avg_covid_high_economicdanger
covid_faelle_weekly_4$covid_high_economicdanger_society_centered <- covid_faelle_weekly_4$covid_high_economicdanger_society - covid_faelle_weekly_4$avg_covid_high_economicdanger_society

#skeptic (measures are exaggerated)
covid_faelle_weekly_4$covid_skeptic_centered <- covid_faelle_weekly_4$covid_skeptic - covid_faelle_weekly_4$avg_covid_skeptic

#measures are too lax
covid_faelle_weekly_4$covid_toolax_centered <- covid_faelle_weekly_4$covid_toolax - covid_faelle_weekly_4$avg_covid_toolax

#vaccinations
covid_faelle_weekly_4$cum_vaccinations_per_1k_centered <- covid_faelle_weekly_4$cum_vaccinations_per_1k - covid_faelle_weekly_4$avg_cum_vaccinations_per_1k
covid_faelle_weekly_4$cum_second_dose_vaccinations_per_1k_centered <- covid_faelle_weekly_4$cum_second_dose_vaccinations_per_1k - covid_faelle_weekly_4$avg_cum_second_dose_vaccinations_per_1k
covid_faelle_weekly_4$cum_vaccinations_per_1k_imputed_regional_centered <- covid_faelle_weekly_4$cum_vaccinations_per_1k_imputed_regional - covid_faelle_weekly_4$avg_cum_vaccinations_per_1k_imputed_regional

#cases
covid_faelle_weekly_4$cum_cases_per_1k_centered <- covid_faelle_weekly_4$cum_cases_per_1k - covid_faelle_weekly_4$avg_cum_cases_per_1k
covid_faelle_weekly_4$cases_per_1000_centered <- covid_faelle_weekly_4$cases_per_1000 - covid_faelle_weekly_4$avg_cases_per_1000
#FPOE share
covid_faelle_weekly_4$fpoe_percent19_centered <- covid_faelle_weekly_4$fpoe_percent19 - covid_faelle_weekly_4$avg_fpoe_percent19



options("scipen"=100, "digits"=10)

Sys.setlocale("LC_TIME", "English") # Windows


##Figure 6 left 300x300

p <- ggplot(covid_faelle_weekly_2, aes(x=dateAsDate, y= (cum_cases / population * 1000), color=group)) + 
  labs (x = "", y = "Cumulative cases per 1k inhabitants", color = "FPOE vote share") +
  scale_color_manual(values = c("red", "blue")) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  stat_summary() + theme_bw()   + theme(legend.position = "none") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(p)

#Figure 6 right 400x300
p <- ggplot(covid_faelle_weekly_2, aes(x=dateAsDate, y= (cum_deaths / population * 100000), color=group)) + 
  labs (x = "", y = "Cum. deaths per 100k inhabitants", color = "FPOE vote share") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
  scale_color_manual(values = c("red", "blue")) +
  stat_summary() + theme_bw() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

print(p)


library(fixest)
#fixed effects model (district-level FE)
fe_deaths_fpoe1 <- feols(deaths_per_100000 ~ fpoe_intervention:fpoe_percent19 + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe1, vcov = "twoway")

fe_deaths_fpoe2 <- feols(deaths_per_100000 ~ fpoe_intervention:fpoe_percent19 + cases_last_2_weeks + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe2, vcov = "twoway")


fe_deaths_fpoe3 <- feols(deaths_per_100000 ~ covid_toolax + covid_high_danger + covid_high_danger_society + covid_high_economicdanger + covid_high_economicdanger_society + fpoe_intervention:fpoe_percent19 + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe3, vcov = "twoway")


fe_deaths_fpoe4 <- feols(deaths_per_100000 ~ covid_toolax +covid_high_danger + covid_high_danger_society + cases_last_2_weeks + covid_high_economicdanger + covid_high_economicdanger_society + fpoe_intervention:fpoe_percent19 + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe4, vcov = "twoway")

fe_deaths_fpoe5 <- feols(deaths_per_100000 ~ covid_skeptic +covid_low_danger + covid_low_danger_society + cases_last_2_weeks + covid_low_economicdanger + covid_low_economicdanger_society + fpoe_intervention:fpoe_percent19 + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe5, vcov = "twoway")

fe_deaths_fpoe7 <- feols(deaths_per_100000 ~ covid_skeptic +covid_low_danger + covid_low_danger_society + covid_low_economicdanger + covid_low_economicdanger_society + fpoe_intervention:fpoe_percent19 + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_deaths_fpoe7, vcov = "twoway")


#######cases
fe_cases_fpoe1 <- feols(cases_per_1000 ~ fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier + cum_second_dose_vaccinations_per_1k_4_weeks_earlier + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe1, vcov = "twoway")


fe_cases_fpoe2 <- feols(cases_per_1000 ~ fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier + cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe2, vcov = "twoway")

fe_cases_fpoe3 <- feols(cases_per_1000 ~ covid_toolax + covid_high_danger + covid_high_danger_society + covid_high_economicdanger + covid_high_economicdanger_society +fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier + cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe3, vcov = "twoway")

fe_cases_fpoe4 <- feols(cases_per_1000 ~ covid_skeptic +covid_low_danger + covid_low_danger_society + covid_low_economicdanger + covid_low_economicdanger_society + fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier + cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe4, vcov = "twoway")

##without vaccinations
fe_cases_fpoe5 <- feols(cases_per_1000 ~ fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe5, vcov = "twoway")

fe_cases_fpoe6 <- feols(cases_per_1000 ~ covid_toolax + covid_high_danger + covid_high_danger_society + covid_high_economicdanger + covid_high_economicdanger_society +fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier  + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe6, vcov = "twoway")

fe_cases_fpoe7 <- feols(cases_per_1000 ~ covid_skeptic + covid_low_danger + covid_low_danger_society + covid_low_economicdanger + covid_low_economicdanger_society + fpoe_intervention:fpoe_percent19 + cases_per_1000_1_week_earlier  + cum_cases_per_1k + deaths_per_100000_1_week_earlier | GKZ + time, data = covid_faelle_weekly_4, panel.id = ~GKZ+time)

summary(fe_cases_fpoe7, vcov = "twoway")




library("modelsummary")
library(gt)

modelsummary(list('(1)' = skeptic_probit_panel1, '(2)' = skeptic_probit_panel2, '(3)' = skeptic_probit_panel3,'(4)' =  skeptic_probit_panel4, '(5)' =  skeptic_probit_panel5, '(6)' = too_lax_probit_panel1, '(7)' = too_lax_probit_panel2, '(8)' = too_lax_probit_panel3, '(9)' = too_lax_probit_panel4, '(10)' = too_lax_probit_panel5), title = "Table 1: Opinion on government policy", stars =  c('*' = .05, '**' = .01, '***' = .001), 
             output = "gt")

cm2 <- c(
        'cases_per_1000_1_week_earlier' = 'lag(cases per 1k, 1)',
        'deaths_per_100000_1_week_earlier' = 'lag(deaths per 100k, 1)',
        'fpoe_intervention:fpoe_percent19' = 'FPOE vote share x Policy switch',
        'cum_cases_per_1k' = 'Cumulative number of cases per 1k inhabitants',
        'cum_second_dose_vaccinations_per_1k_4_weeks_earlier' = 'lag(State-level second-dose vaccinations per 1k, 4)',
        'cum_second_dose_vaccinations_per_1k_imputed_regional_4_weeks_earlier' = 'lag(Imputed district-level second-dose vaccinations per 1k, 4)',
        'cases_last_2_weeks' = 'Cases per 1k inhabitants in the weeks t-1 and t-2',
        'covid_toolax' = 'Index: government measures are too lax',
        'covid_high_danger' = 'Index: Covid-19 poses a high danger to personal health',
        'covid_high_danger_society' = 'Index: Covid-19 poses a high danger to public health',
        'covid_high_economicdanger' = 'Index: Covid-19 poses a high danger to personal economic situation',
        'covid_high_economicdanger_society' = 'Index: Covid-19 poses a high danger to the economy',
        'covid_skeptic' = 'Index: government measures are exaggerated',
        'covid_low_danger' = 'Index: Covid-19 poses a low danger to personal health',
        'covid_low_danger_society' = 'Index: Covid-19 poses a low danger to public health',
        'covid_low_economicdanger' = 'Index: Covid-19 poses a low danger to personal economic situation',
        'covid_low_economicdanger_society' = 'Index: Covid-19 poses a low danger to the economy'
        )

tab4 <- modelsummary(list('(1)' = fe_cases_fpoe5, '(2)' = fe_cases_fpoe6, '(3)' = fe_cases_fpoe7,'(4)' =  fe_cases_fpoe1, '(5)' =  fe_cases_fpoe2, '(6)' = fe_cases_fpoe3, '(7)' = fe_cases_fpoe4), title = "Table 4: Cases per 1k inhabitants", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm2, 
                     vcov = ~GKZ+time, output = "gt")


tab4 <- tab4 %>%
  # column labels
  tab_spanner(label = 'Without vaccinations', columns = 2:4) %>%
  tab_spanner(label = 'With vaccinations', columns = 5:8)

gt::gtsave(tab4, filename = "table4.html")


tab5 <- modelsummary(list('(1)' = fe_deaths_fpoe1, '(2)' = fe_deaths_fpoe3, '(3)' = fe_deaths_fpoe7,'(4)' =  fe_deaths_fpoe2, '(5)' =  fe_deaths_fpoe4, '(6)' = fe_deaths_fpoe5), title = "Table 5: Deaths per 100k inhabitants", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm2, 
                     vcov = ~GKZ+time, output = "gt")

tab5 <- tab5 %>%
  # column labels
  tab_spanner(label = 'Without controlling for cases', columns = 2:4) %>%
  tab_spanner(label = 'Controlling for cases', columns = 5:7)

gt::gtsave(tab5, filename = "table5.html")


