
library(here)
library(deSolve)
library(ggplot2)
library(sqldf)
setwd(here())

#Assumption: type 1 has a lower R0 than type 2!
#omega: share of contacts which are homophilic
sir <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    p1 = (1 - omega) * a1 * N1 / ((1 - omega) * a1 * N1 + (1 - omega) * a2 * N2)
    p2 = (1 - omega) * a2 * N2 / ((1 - omega) * a1 * N1 + (1 - omega) * a2 * N2)
    p11 <- omega + (1 - omega) * p1
    p12 <- (1 - omega) * p2
    p21 <- (1 - omega) * p1
    p22 <- omega + (1 - omega) * p2
    mu1 <- pi_x / alpha1
    mu2 <- pi_x / alpha2
    dS1 <- - (p11 * a1 * S1 * I1 / N1 + p12 * a1 * S1 * I2 / N2) #- S1 * (beta1 / N1 * (omega + (1 - omega) * beta1 / (beta1 + beta2) ) * I1 + beta2 / N2 * (1 - omega) * beta2 / (beta1 + beta2) * I2 )
    dS2 <- - (p21 * a2 * S2 * I1 / N1 + p22 * a2 * S2 * I2 / N2) #- S2 * (beta2 / N2 * (omega + (1 - omega) * beta2 / (beta1 + beta2) ) * I2 + beta1 / N1 * (1 - omega) * beta1 / (beta1 + beta2) * I1 )
    dI1 <-  (p11 * a1 * S1 * I1 / N1 + p12 * a1 * S1 * I2 / N2) * ( 1 - alpha1) - gamma * I1
    dI2 <- (p21 * a2 * S2 * I1 / N1 + p22 * a2 * S2 * I2 / N2) * ( 1 - alpha2) - gamma * I2
    dQ1 <-  (p11 * a1 * S1 * I1 / N1 + p12 * a1 * S1 * I2 / N2) * alpha1 - gamma * Q1
    dQ2 <-  (p21 * a2 * S2 * I1 / N1 + p22 * a2 * S2 * I2 / N2)  * alpha2 - gamma * Q2
    dR <-                 gamma * (I1 + I2) + gamma * (1 - mu1) * Q1 + gamma * (1 - mu2) * Q2
    dD <-                 gamma * mu1 * Q1 + gamma * mu2 * Q2
    drep <- (p11 * a1 * S1 * I1 / N1 + p12 * a1 * S1 * I2 / N2) * alpha1 + (p21 * a2 * S2 * I1 / N1 + p22 * a2 * S2 * I2 / N2)  * alpha2
    dN1 <- 0#-(beta1 / N1 * S1 * I1 * omega  + beta2  / N2 * S1 * I2 * ( 1 - omega) ) * alpha1 + gamma * mu1 * Q1#- (beta1 / N1 * S1 * I1 * omega  + beta2  / N1 * S1 * I2 * ( 1 - omega) ) * alpha1  + gamma * (I1 + I2) + gamma * (1 - mu1) * Q1 
    dN2 <- 0#- (beta1 / N1 * S2 * I1 * ( 1 - omega )  + beta2  / N2 * S2 * I2 * omega  ) * alpha2 + gamma * mu2 * Q2#- (beta1 / N2 * S2 * I1 * ( 1 - omega )  + beta2  / N2 * S2 * I2 * omega  )  * alpha2  + gamma * (I1 + I2) + gamma * (1 - mu2) * Q2
    return(list(c(dS1, dS2, dI1, dI2, dQ1, dQ2, dR, dD, drep, dN1, dN2)))
  })
}

i <- 0.00
R0TypeI <- 2.5
HomophilicMixing_current <- data.frame()
  deltaR0 <- 0
  #while (deltaR0 < 3)
  while (deltaR0 < 2)
  {
    h <- 0.0
    while (h < 1)
      {
        i <- 0
        while (i < 1)
        {
          times      <- seq(0, 500, by = 1)
          init       <- c(S1 = (1-i)-0.001*(1-i), S2 =(i)-0.001*i , I1 = 0.001*(1-i), I2 = 0.001*i, Q1 = 0.0, Q2 = 0.0, R = 0.0, D = 0.0, rep = 0.0, N1 = (1-i), N2 = i)
          parameters <- c(a1 = R0TypeI / 7, a2 = (R0TypeI + deltaR0) / 7, omega = h, gamma = 1 / 7, alpha1 = 0.45, alpha2 = 0.27, pi_x = 0.0079)
          ## Solve using ode (General Solver for Ordinary Differential Equations)
          out <- ode(y = init, times = times, func = sir, parms = parameters)
          ## change to data frame
          out <- as.data.frame(out)
          out$group <- "R02 = 1.5"
          
          HomophilicMixing_fly <- data.frame(homophily = c(h), group2 = c(i), r01= c(R0TypeI), r02= c(R0TypeI + deltaR0), rep = c(out[out$time == 500, ]$rep), D = c(out[out$time == 500, ]$D), S1 = c(out[out$time == 500, ]$S1), S2 = c(out[out$time == 500, ]$S2), N1 = c(out[out$time == 500, ]$N1), N2 = c(out[out$time == 500, ]$N2)) 
          
          HomophilicMixing_current <- rbind(HomophilicMixing_current, HomophilicMixing_fly)
          
          i <- i + 0.01
        }
        h <- h + 1#0.01
      }
    deltaR0 <- deltaR0 + 0.5
  }
HomophilicMixing_current$groupsize <- paste("corona skeptics: ", round(HomophilicMixing_current$group2, digits=3))
HomophilicMixing_current$r02_group <- paste("R02: ", round(HomophilicMixing_current$r02, digits=3))

#Fig. 11 (left)
#300x300
p <- ggplot(subset(HomophilicMixing_current)) + 
  labs (x = expression("Share of skeptics N"[2]), y = expression(S[1]/N[1]~after~500~periods)) +
  geom_line(aes(x = group2, y = S1/N1, group = r02_group, col = r02_group)) +
  theme_bw() + 
  ylim(0, 0.5)+
  theme(legend.title=element_blank(), legend.box="vertical", legend.margin=margin(), legend.position="none")


print(p)

#Fig. 11 (right)
#400x300
p <- ggplot(subset(HomophilicMixing_current)) + 
  labs (x = "corona skeptics", y = expression(S[2]/N[2]~after~500~periods)) +
  geom_line(aes(x = group2, y = S2/N2, group = r02_group, col = r02_group)) +
  theme_bw() + 
  ylim(0, 0.5) +
  theme(legend.title=element_blank(), legend.box="vertical", legend.margin=margin(), legend.position="right")

print(p)