## ---- message=FALSE, warning=FALSE--------------------------------------------
library(mvtnorm)
arm1 <- rmvnorm(n = 30, mean  = rep(10, 5), sigma = diag(5) + 0.5)
test_data <- as.data.frame(cbind(1:30, rep(1, 30), arm1))
colnames(test_data) <- c("id","arm",paste0("time_", 1:5))
knitr::kable((test_data), "simple")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(copulaSim)

## ---- message=FALSE, warning=FALSE--------------------------------------------
## Generate 1 simulated dataset
simu_S1 <- copula.sim(data.input = test_data[,-c(1,2)], 
                      id.vec = test_data$id, 
                      arm.vec = test_data$arm,
                      n.patient = 50 , 
                      n.simulation = 1, 
                      seed = 2022, 
                      validation.type = "energy",
                      verbose = TRUE)
## Obtain the simulated long-form dataset
simu_S1$data.simul

## ---- message=FALSE, warning=FALSE, fig.dim = c(8, 3)-------------------------
library(dplyr)
## Obtain the empirical long-form dataset
empir <- simu_S1$data.input.long %>% mutate(cate = "empirical_n30") %>% rename(data = data.input)

## Produce the marginal density plot
simul <- simu_S1$data.simul %>%  mutate(cate = "copulaSim_n50") %>% 
         rename(data = data.sim) %>% select(-sim.id)  
library(ggplot2)
rbind(empir, simul) %>% filter(grepl('time', col.name)) %>%
  ggplot(aes(x = data, color = cate, fill = cate)) +
  facet_wrap(.~col.name, ncol = 5) +
  geom_density(alpha = 0.001, size = 1)

## ---- message=FALSE, warning=FALSE--------------------------------------------
## Converting the long-form simulated dataset to wide-form 
simu.wide <- extract.data.sim(simu_S1)
simu.wide

## ---- message=FALSE, warning=FALSE--------------------------------------------
## Generate 100 simulated datasets
simu_S100 <- copula.sim(data.input = test_data[,-c(1,2)], 
                         id.vec = test_data$id, 
                         arm.vec = test_data$arm,
                         n.patient = 50 , 
                         n.simulation = 100, 
                         seed = 2022, 
                         validation.type = "none",
                         verbose = FALSE)

## Compare the marginal mean via the function compare.copula.sim 
compare <- compare.copula.sim(simu_S100)
knitr::kable(compare$mean.comparison, "simple")

## ---- message=FALSE, warning=FALSE--------------------------------------------
## Generate Empirical Data 
##  Assume that the single-arm, 3-dimensional empirical data follows multivariate normal data
arm1 <- rmvnorm(n = 80, mean  = c(10,10.5,11), sigma = diag(3) + 0.5)
test_data2 <- as.data.frame(cbind(1:80, rep(1,80), arm1))
colnames(test_data2) <- c("id", "arm", paste0("time_", 1:3))

## Generate 1 simulated datasets with one empirical arm and two new-arms
## The mean difference between empirical arm and 
## (i) the 1st new arm is assumed to be 2.5, 2.55, and 2.6 at each time point
## (ii) the 2nd new arm is assumed to be 4.5, 4.55, and 4.6 at each time point

newARM <- new.arm.copula.sim(data.input = test_data2[,-c(1,2)], 
                             id.vec = test_data2$id,
                             arm.vec = test_data2$arm, 
                             n.patient = 100 , 
                             n.simulation = 1, 
                             seed = 2022,
                             shift.vec.list = list(c(2.5,2.55,2.6), c(4.5,4.55,4.6)),
                             verbose = FALSE)

## Obtain the simulated long-form dataset
newARM$data.simul

## Verify the mean difference
newARM$data.simul %>%  
  group_by(.data$arm, .data$col.num) %>%
  summarise(N = n(), Mean = mean(.data$data.sim), SD = sd(.data$data.sim))

