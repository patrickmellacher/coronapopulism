
library(ggplot2)
library(sqldf)
library(haven)
library(here)

setwd(here())

##download dataset here: https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/G3C2CS

X10736_da01_de_v1_0 <- read_stata("10736_da01_de_v1_0.dta")

X10736_da01_de_v1_0$PCR_TEST_NUMERIC <- as.numeric(X10736_da01_de_v1_0$PCR_Ergebnis)
X10736_da01_de_v1_0$AK_TEST_NUMERIC <- as.numeric(X10736_da01_de_v1_0$AK_Ergebnis)
X10736_da01_de_v1_0$politbeurt_numeric <- as.numeric(X10736_da01_de_v1_0$politbeurt)

X10736_da01_de_v1_0$corona_sceptical <- ifelse(X10736_da01_de_v1_0$politbeurt_numeric > 0 & X10736_da01_de_v1_0$politbeurt_numeric < 3, 1, 0)

X10736_da01_de_v1_0$corona_non_sceptical <- ifelse(X10736_da01_de_v1_0$politbeurt_numeric > 2 & X10736_da01_de_v1_0$politbeurt_numeric < 6, 1, 0)

X10736_da01_de_v1_0_with_test <- X10736_da01_de_v1_0[!is.na(X10736_da01_de_v1_0$PCR_TEST_NUMERIC), ]
X10736_da01_de_v1_0_with_politicalview <- X10736_da01_de_v1_0_with_test[X10736_da01_de_v1_0_with_test$politbeurt_numeric > 0, ]

X10736_da01_de_v1_0_with_antibody_test <- X10736_da01_de_v1_0_with_politicalview[!is.na(X10736_da01_de_v1_0_with_politicalview$AK_TEST_NUMERIC), ]

probit_all_tested_positively <- glm(PCR_TEST_NUMERIC ~ corona_sceptical, family = binomial(link = "probit"), 
                data = X10736_da01_de_v1_0_with_politicalview)

## model summary
summary(probit_all_tested_positively)


sqldf("select corona_sceptical, SUM(PCR_TEST_NUMERIC), COUNT(*) FROM X10736_da01_de_v1_0_with_politicalview GROUP BY corona_sceptical")

probit_all_tested_positively <- glm(PCR_TEST_NUMERIC ~ corona_sceptical, family = binomial(link = "probit"), 
                                    data = X10736_da01_de_v1_0_with_politicalview)

## model summary
summary(probit_all_tested_positively)


X10736_da01_de_v1_0_with_unknown_infection <- X10736_da01_de_v1_0_with_test[X10736_da01_de_v1_0_with_test$xposjebehEMS == 2, ]

X10736_da01_de_v1_0_with_test$known_infection <- ifelse (X10736_da01_de_v1_0_with_test$xposjebehEMS == 1, 1, 0)

X10736_da01_de_v1_0_with_politicalview$known_infection <- ifelse (X10736_da01_de_v1_0_with_politicalview$xposjebehEMS == 1, 1, 0)

X10736_da01_de_v1_0_with_antibody_test$known_infection <- ifelse (X10736_da01_de_v1_0_with_antibody_test$xposjebehEMS == 1, 1, 0)


probit_with_unknown_infection <- glm(PCR_TEST_NUMERIC ~ corona_sceptical, family = binomial(link = "probit"), 
                                    data = X10736_da01_de_v1_0_with_unknown_infection)

## model summary
summary(probit_with_unknown_infection)

probit_with_known_infection <- glm(known_infection ~ corona_sceptical, family = binomial(link = "probit"), 
                                    data = X10736_da01_de_v1_0_with_politicalview)

## model summary
summary(probit_with_known_infection)


probit_antibody <- glm(AK_TEST_NUMERIC ~ corona_sceptical, family = binomial(link = "probit"), 
                                   data = X10736_da01_de_v1_0_with_antibody_test)

## model summary
summary(probit_antibody)


cm3 <- c(
  'corona_sceptical' = 'corona skeptical',
  '(Intercept)' = '(Intercept)'
)

tab7 <- modelsummary(list('(1) All individuals' = probit_all_tested_positively, '(2) Only unreported' = probit_with_unknown_infection, '(3) All individuals' = probit_with_known_infection,'(4) All individuals' =  probit_antibody), title = "Table 6: Individual-level evidence from the Covid prevalence study in November (Probit)", stars =  c('*' = .05, '**' = .01, '***' = .001), coef_map = cm3, output = "gt")


tab7 <- tab7 %>%
  # column labels
  tab_spanner(label = 'PCR test positive', columns = 2:3) %>%
  tab_spanner(label = 'Reported infection', columns = 4:4) %>%
  tab_spanner(label = 'Antibody test positive', columns = 5:5)

gt::gtsave(tab7, filename = "table7.html")



# library
library(ggplot2)

# create a dataset
specie <- c(rep("PCR test positive" , 2) , rep("reported infections" , 2) , rep("unreported current infections" , 2) , rep("antibody test positive" , 2))
condition <- rep(c("non-skeptic", "corona skeptic") , 4)
levels(condition) <- c("non-skeptic", "corona skeptic")
levels(specie) <- c("PCR test positive", "reported infections", "unreported current infections", "antibody test positive")
value <- c(0.02595338983, 0.05822784810, 0.02744063325, 0.03291139241, 0.01085187195, 0.04450261780, 0.03594771242, 0.06527415144  )
dataset <- data.frame(specie,condition,value)


# Grouped
ggplot(dataset, aes(fill=factor(condition, levels = c("non-skeptic", "corona skeptic")), y=value, x=factor(specie, levels = c("reported infections", "PCR test positive", "unreported current infections", "antibody test positive")))) + 
  geom_bar(position="dodge", stat="identity", width=0.5) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("")+
  xlab("") + 
  theme_bw() + theme(legend.title = element_blank()) + theme(legend.position = "bottom") + scale_fill_manual(values=c("#c3563f", "lightblue3"))
