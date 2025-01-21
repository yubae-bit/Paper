# Define models with their respective families and names in the desired order
GAMMA.reg <- gamlss(Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
                    + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
                    + windvulnerability + postcode_coast_flg + postcode_alt_mean 
                    + postcode_slo_mean + postcode_rgh_mean + Building_Age 
                    + log_postcode_area + log_postcode_perimeter 
                    + log_postcode_coast_len, data = learn, family = GA, n.cyc = 1000)
GAMMA.reg1 <- stepGAIC(GAMMA.reg)

# Training sigma
GAMMA.reg2 <- gamlss(
  formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age 
  + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  sigma.fo = ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  data = learn, 
  family = GA, 
  n.cyc = 1000
)
GAMMA.reg3 <- stepGAICAll.A(GAMMA.reg2)


# Model to use after removing non-significant covariates
# GAMMA.reg <- gamlss(
#   formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Item_Type_C + FloorsType_C + postcode_slo_mean + postcode_rgh_mean
#   + log_postcode_area, 
#   sigma.fo = ~ FloorsType_C + windvulnerability + postcode_alt_mean, 
#   data = learn, 
#   family = GA, 
#   n.cyc = 1000
# )


IG.reg <- gamlss(Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
                    + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
                    + windvulnerability + postcode_coast_flg + postcode_alt_mean 
                    + postcode_slo_mean + postcode_rgh_mean + Building_Age 
                    + log_postcode_area + log_postcode_perimeter 
                    + log_postcode_coast_len, data = learn, family = IG, n.cyc = 1000)
IG.reg1 <- stepGAIC(IG.reg)

# Training sigma
IG.reg2 <- gamlss(
  formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age 
  + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  sigma.fo = ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  data = learn, 
  family = IG, 
  n.cyc = 1000
)

IG.reg3 <- stepGAICAll.A(IG.reg2)

# IG.reg <- gamlss(
#   formula = Avg_Cost ~ nrOfFloors + Item_Type_C + FloorsType_C 
#   + postcode_slo_mean + postcode_rgh_mean, 
#   sigma.fo = ~ floodvulnerability + windvulnerability + postcode_alt_mean, 
#   data = learn, 
#   family = IG, 
#   n.cyc = 1000
# )


LOGNO.reg <- gamlss(Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
                    + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
                    + windvulnerability + postcode_coast_flg + postcode_alt_mean 
                    + postcode_slo_mean + postcode_rgh_mean + Building_Age 
                    + log_postcode_area + log_postcode_perimeter 
                    + log_postcode_coast_len, data = learn, family = LOGNO, n.cyc = 1000)

LOGNO.reg1 <- stepGAIC(LOGNO.reg)
# Training sigma
LOGNO.reg2 <- gamlss(
  formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age 
  + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  sigma.fo = ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  data = learn, 
  family = LOGNO(mu.link = "log"), 
  n.cyc = 1000
)
LOGNO.reg3 <- stepGAICAll.A(LOGNO.reg2)
# LOGNO.reg <- gamlss(
#   formula = Avg_Cost ~ nrOfFloors + FloorsType_C + postcode_slo_mean + postcode_rgh_mean, 
#   sigma.fo = ~ Item_Type_C + FloorsType_C + floodvulnerability + windvulnerability
#   + postcode_coast_flg + postcode_alt_mean, 
#   data = learn, 
#   family = LOGNO, 
#   n.cyc = 1000
# )


WEI3.reg <- gamlss(Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
                    + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
                    + windvulnerability + postcode_coast_flg + postcode_alt_mean 
                    + postcode_slo_mean + postcode_rgh_mean + Building_Age 
                    + log_postcode_area + log_postcode_perimeter 
                    + log_postcode_coast_len, data = learn, family = WEI3, n.cyc = 1000)
WEI3.reg1 <- stepGAIC(WEI3.reg)
# Training sigma
WEI3.reg2 <- gamlss(
  formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age 
  + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  sigma.fo = ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  data = learn, 
  family = WEI3, 
  n.cyc = 1000
)
WEI3.reg3 <- stepGAICAll.A(WEI3.reg2)
# WEI3.reg <- gamlss(
#   formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Item_Type_C + FloorsType_C
#   + postcode_slo_mean + postcode_rgh_mean + log_postcode_area, 
#   sigma.fo = ~ FloorsType_C + windvulnerability + postcode_alt_mean, 
#   data = learn, 
#   family = WEI3, 
#   n.cyc = 1000
# )


PARETO2o.reg <- gamlss(Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
                    + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
                    + windvulnerability + postcode_coast_flg + postcode_alt_mean 
                    + postcode_slo_mean + postcode_rgh_mean + Building_Age 
                    + log_postcode_area + log_postcode_perimeter 
                    + log_postcode_coast_len, data = learn, family = PARETO2o, n.cyc = 1000)
PARETO2o.reg1 <- stepGAIC(PARETO2o.reg)
# Training sigma
PARETO2o.reg2 <- gamlss(
  formula = Avg_Cost ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age 
  + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  sigma.fo = ~ SubItemType_C + nrOfFloors + Construction_Material_C
  + Item_Type_C + FloorsType_C + Floors_No + floodvulnerability
  + windvulnerability + postcode_coast_flg + postcode_alt_mean 
  + postcode_slo_mean + postcode_rgh_mean + Building_Age + log_postcode_area + log_postcode_perimeter 
  + log_postcode_coast_len, 
  data = learn, 
  family = PARETO2o, 
  n.cyc = 1000
)
PARETO2o.reg3 <- stepGAICAll.A(PARETO2o.reg2)
# PARETO2o.reg <- gamlss(
#   formula = Avg_Cost ~ SubItemType_C + log_postcode_perimeter , 
#   sigma.fo = ~ nrOfFloors + FloorsType_C, 
#   data = learn, 
#   family = PARETO2o, 
#   n.cyc = 1000
# )


# Record the fitted mu and sigma values
learn$GAMMA.mu.reg <- GAMMA.reg$mu.fv
learn$GAMMA.sigma.reg <- GAMMA.reg$sigma.fv
learn$IG.mu.reg <- IG.reg$mu.fv
learn$IG.sigma.reg <- IG.reg$sigma.fv
learn$LOGNO.mu.reg <- LOGNO.reg$mu.fv
learn$LOGNO.sigma.reg <- LOGNO.reg$sigma.fv
learn$WEI3.mu.reg <- WEI3.reg$mu.fv
learn$WEI3.sigma.reg <- WEI3.reg$sigma.fv
learn$P2o.mu.reg <- PARETO2o.reg$mu.fv
learn$P2o.sigma.reg <- PARETO2o.reg$sigma.fv

summary(PARETO2o.reg$mu.fv)

# Use the predict function to predict the mu and sigma parameters
test$GAMMA.mu.reg <- predict(GAMMA.reg, newdata = test, type = "response", what = "mu")
test$GAMMA.sigma.reg <- predict(GAMMA.reg, newdata = test, type = "response", what = "sigma")
test$IG.mu.reg <- predict(IG.reg, newdata = test, type = "response", what = "mu")
test$IG.sigma.reg <- predict(IG.reg, newdata = test, type = "response", what = "sigma")
test$LOGNO.mu.reg <- predict(LOGNO.reg, newdata = test, type = "response", what = "mu")
test$LOGNO.sigma.reg <- predict(LOGNO.reg, newdata = test, type = "response", what = "sigma")
test$WEI3.mu.reg <- predict(WEI3.reg, newdata = test, type = "response", what = "mu")
test$WEI3.sigma.reg <- predict(WEI3.reg, newdata = test, type = "response", what = "sigma")
test$P2o.mu.reg <- predict(PARETO2o.reg, newdata = test, type = "response", what = "mu")
test$P2o.sigma.reg <- predict(PARETO2o.reg, newdata = test, type = "response", what = "sigma")

# Record the predicted mean of the response variable (Avg_Cost) using the formula of the mean from the GAMLSS documentation
learn$GAMMA.mean.reg <- learn$GAMMA.mu.reg
test$GAMMA.mean.reg <- test$GAMMA.mu.reg
learn$IG.mean.reg <- learn$IG.mu.reg
test$IG.mean.reg <- test$IG.mu.reg
learn$LOGNO.mean.reg <- exp(learn$LOGNO.mu.reg + (learn$LOGNO.sigma.reg^2)/2)
test$LOGNO.mean.reg <- exp(test$LOGNO.mu.reg + (test$LOGNO.sigma.reg^2)/2)
learn$WEI3.mean.reg <- learn$WEI3.mu.reg
test$WEI3.mean.reg <- test$WEI3.mu.reg
learn$P2o.mean.reg <- abs(learn$P2o.mu.reg/(learn$P2o.sigma.reg - 1))
test$P2o.mean.reg <- abs(test$P2o.mu.reg/(test$P2o.sigma.reg - 1))


# Calculate the dispersion for the regression models.
# This is the sigma.fv attribute from the GAMMA.reg object 
dsprsn.reg.GAMMA <- mean(GAMMA.reg$sigma.fv)
dsprsn.reg.IG <- mean(IG.reg$sigma.fv)
dsprsn.reg.LOGNO <- mean(LOGNO.reg$sigma.fv) 
dsprsn.reg.WEI3 <- mean(WEI3.reg$sigma.fv) 
dsprsn.reg.P2o <- mean(PARETO2o.reg$sigma.fv) 


# Define a generic function for log-density calculation
logdensity_functions <- list(
  GA = function(y, mu, sigma) {
    (1/sigma^2 - 1) * k_log(y) - (y / (sigma^2 * mu)) - (1/sigma^2) * k_log(sigma^2 * mu) - k_log(gamma(1/sigma^2))
  },
  IG = function(y, mu, sigma) {
    -(1/2) * k_log(2 * pi * sigma^2) - (3/2) * k_log(y) - (y - mu)^2 / (2 * mu^2 * sigma^2 * y)
  },
  LOGNO = function(y, mu, sigma) {
    -k_log(y) - 0.5 * k_log(2 * pi * sigma^2) -  (k_log(y) - mu)^2/(2 * sigma^2)
  },
  PARETO2o = function(y, mu, sigma) {
    k_log(sigma) + sigma * k_log(mu) - (sigma + 1) * k_log(y + mu)

  },
  WEI3 = function(y, mu, sigma) {
    -k_log(sigma) - (sigma - 1) * k_log(y) - sigma * k_log(mu * (gamma(1/sigma + 1))^(-1)) -
      (y / (mu * (gamma(1/sigma + 1))^(-1)))^sigma
  }
)

# initialize table to store all model results for comparison
df_cmp <- tibble(
  Model = character(),
  Epochs = numeric(),
  Run_Time = numeric(),
  Parameters = numeric(),
  In_Sample_Loss = numeric(),
  Out_Sample_Loss = numeric(),
  Avg_Cost = numeric(),
  GAIC = numeric(),
)

#Portfolio Average
df_cmp %<>% bind_rows(
  data.frame(Model = "Real Data", Epochs = 0 , Run_Time = 0, Parameters = 0,
             In_Sample_Loss = 0,
             Out_Sample_Loss = 0,
             Avg_Cost = mean(data3$Avg_Cost),
             GAIC = 0)
)

# Function to convert tensor to numeric
tensor_to_numeric <- function(tensor){
  if (!is.null(tensor)) {
    return(as.numeric(tensor$numpy()))
  } else {
    return(NA)  # or some other default value or error handling
  }
}

# Gamma deviance loss 
dev.loss_tensor.GA <- function(y_true, y_pred){
  loss <- 2*(logdensity_functions$GA(y = y_true, mu = y_true, sigma = dsprsn.reg.GAMMA) -
               logdensity_functions$GA(y = y_true, mu = y_pred, sigma = dsprsn.reg.GAMMA))
  
  return(loss)
}
# IG deviance loss
dev.loss_tensor.IG <- function(y_true, y_pred){
  loss <- 2*(logdensity_functions$IG(y = y_true, mu = y_true, sigma = dsprsn.reg.IG) -
               logdensity_functions$IG(y = y_true, mu = y_pred, sigma = dsprsn.reg.IG))
  
  return(loss)
}
# LOGNO deviance loss
dev.loss_tensor.LOGNO <- function(y_true, y_pred){
  loss <- 2*(logdensity_functions$LOGNO(y = y_true, mu = log(y_true)-(dsprsn.reg.LOGNO^2)/2, sigma = dsprsn.reg.LOGNO) -
               logdensity_functions$LOGNO(y = y_true, mu = log(y_pred)-(dsprsn.reg.LOGNO^2)/2, sigma = dsprsn.reg.LOGNO))
  
  return(loss)
}
# Weibull deviance loss
dev.loss_tensor.WEI3 <- function(y_true, y_pred){
  loss <- 2*(logdensity_functions$WEI3(y = y_true, mu = y_true, sigma = dsprsn.reg.WEI3) -
               logdensity_functions$WEI3(y = y_true, mu = y_pred, sigma = dsprsn.reg.WEI3))
  
  return(loss)
}
# Pareto deviance loss
dev.loss_tensor.P2o <- function(y_true, y_pred){
  loss <- 2*(logdensity_functions$PARETO2o(y = y_true, mu = y_true*(dsprsn.reg.P2o-1), sigma = dsprsn.reg.P2o) -
               logdensity_functions$PARETO2o(y = y_true, mu = y_pred*(dsprsn.reg.P2o-1), sigma = dsprsn.reg.P2o))
  
  return(loss)
}


# GAMMA Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M1: GAMMA Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(GAMMA.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = learn$GAMMA.mean.reg, 
                               y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = test$GAMMA.mean.reg, 
                               y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$GAMMA.mean.reg),
             GAIC = GAIC(GAMMA.reg))
)

# IG Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M2: IG Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(IG.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = learn$IG.mean.reg, 
                                  y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = test$IG.mean.reg, 
                                  y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$IG.mean.reg),
             GAIC = GAIC(IG.reg))
)

# LOGNO Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M3: LOGNO Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(LOGNO.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = learn$LOGNO.mean.reg, 
                                     y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = test$LOGNO.mean.reg, 
                                     y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$LOGNO.mean.reg),
             GAIC = GAIC(LOGNO.reg))
)

# Weibull Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M4: WEI3 Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(WEI3.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = learn$WEI3.mean.reg, 
                                    y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = test$WEI3.mean.reg, 
                                    y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$WEI3.mean.reg),
             GAIC = GAIC(WEI3.reg))
)

# Pareto Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M5: Pareto Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(PARETO2o.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = learn$P2o.mean.reg, 
                                   y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = test$P2o.mean.reg, 
                                   y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$P2o.mean.reg),
             GAIC = GAIC(PARETO2o.reg))
)
df_cmp


# Generate x values corresponding to the range of Avg_Cost
x <- seq(min(learn$Avg_Cost), max(learn$Avg_Cost), length.out = 100)


density_GAMMA_mean <- dGA(x, mu = mean(range(GAMMA.reg$mu.fv)), 
                          sigma = mean(range(GAMMA.reg$sigma.fv)))
density_IG_mean <- dIG(x, mu = mean(range(IG.reg$mu.fv)), 
                          sigma = mean(range(IG.reg$sigma.fv)))
density_LOGNO_mean <- dLOGNO(x, mu = mean(LOGNO.reg$mu.fv), 
                             sigma = mean(LOGNO.reg$sigma.fv))
density_WEI3_mean <- dWEI3(x, mu = mean(range(WEI3.reg$mu.fv)), 
                          sigma = mean(range(WEI3.reg$sigma.fv)))
density_PARETO2o_mean <- dPARETO2o(x, mu = mean(range(PARETO2o.reg$mu.fv)), 
                          sigma = mean(range(PARETO2o.reg$sigma.fv)))


# Plot histogram with probability density
hist(learn$Avg_Cost, probability = TRUE, main = "Avg_Cost by Fitted Model", 
     xlab = "Avg_Cost", breaks = 100)

# Add the density line to the histogram


lines(x, density_GAMMA_mean, col = "cyan", lwd = 2)
lines(x, density_IG_mean, col = "blue", lwd = 2)
lines(x, density_LOGNO_mean, col = "yellow", lwd = 2)
lines(x, density_WEI3_mean, col = "red", lwd = 2)
lines(x, density_PARETO2o_mean, col = "purple", lwd = 2)

# Add a legend
legend("topright", legend = c("Gamma", "Inverse Gaussian", "Log-Normal", "Weibull", "Pareto"), 
       col = c("cyan", "blue", "yellow", "red", "purple"), lwd = 2, 
       title = "Density Functions")

df_cmp

# Bias Adjustment
mean_Avg_Cost <- mean(data3$Avg_Cost)
mean_GAMMA_reg <- mean(test$GAMMA.mean.reg)
mean_IG_reg <- mean(test$IG.mean.reg)
mean_LOGNO_reg <- mean(test$LOGNO.mean.reg)
mean_WEI3_reg <- mean(test$WEI3.mean.reg)
mean_PARETO_reg <- mean(test$P2o.mean.reg)

GAMMA.adj.coef <- mean_Avg_Cost/mean_GAMMA_reg
IG.adj.coef <- mean_Avg_Cost/mean_IG_reg
LOGNO.adj.coef <- mean_Avg_Cost/mean_LOGNO_reg
WEI3.adj.coef <- mean_Avg_Cost/mean_WEI3_reg
PARETO.adj.coef <- mean_Avg_Cost/mean_PARETO_reg

learn$GAMMA.mean.sreg <- GAMMA.adj.coef*learn$GAMMA.mean.reg
learn$IG.mean.sreg <- IG.adj.coef*learn$IG.mean.reg
learn$LOGNO.mean.sreg <- LOGNO.adj.coef*learn$LOGNO.mean.reg
learn$WEI3.mean.sreg <- WEI3.adj.coef*learn$WEI3.mean.reg
learn$P2o.mean.sreg <- PARETO.adj.coef*learn$P2o.mean.reg

test$GAMMA.mean.sreg <- GAMMA.adj.coef*test$GAMMA.mean.reg
test$IG.mean.sreg <- IG.adj.coef*test$IG.mean.reg
test$LOGNO.mean.sreg <- LOGNO.adj.coef*test$LOGNO.mean.reg
test$WEI3.mean.sreg <- WEI3.adj.coef*test$WEI3.mean.reg
test$P2o.mean.sreg <- PARETO.adj.coef*test$P2o.mean.reg

# GAMMA Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M1: GAMMA Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(GAMMA.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = learn$GAMMA.mean.sreg, 
                                  y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = test$GAMMA.mean.sreg, 
                                  y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$GAMMA.mean.sreg),
             GAIC = GAIC(GAMMA.reg))
)

# IG Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M2: IG Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(IG.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = learn$IG.mean.sreg, 
                                  y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = test$IG.mean.sreg, 
                                  y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$IG.mean.sreg),
             GAIC = GAIC(IG.reg))
)

# LOGNO Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M3: LOGNO Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(LOGNO.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = learn$LOGNO.mean.sreg, 
                                     y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = test$LOGNO.mean.sreg, 
                                     y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$LOGNO.mean.sreg),
             GAIC = GAIC(LOGNO.reg))
)

# Weibull Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M4: WEI3 Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(WEI3.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = learn$WEI3.mean.sreg, 
                                    y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = test$WEI3.mean.sreg, 
                                    y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$WEI3.mean.sreg),
             GAIC = GAIC(WEI3.reg))
)

# Pareto Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M5: Pareto Reg", Epochs = 0 , Run_Time = 0, 
             Parameters = length(coef(PARETO2o.reg)),
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = learn$P2o.mean.sreg, 
                                   y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = test$P2o.mean.sreg, 
                                   y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$P2o.mean.sreg),
             GAIC = GAIC(PARETO2o.reg))
)
df_cmp


