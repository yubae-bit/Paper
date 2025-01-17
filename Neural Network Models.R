# For Pareto Regression and Neural Networks
# learning sample
learn2 <- data3[ll,]

# testing sample
test2 <- data3[-ll,]


# Select the feature space
features <- c("Floors_No", "Building_Age", "Construction_Material_C", "Item_Type_C", 
              "FloorsType_C", "SubItemType_C", "nrOfFloors", "floodvulnerability",
              "windvulnerability", "postcode_coast_flg", "postcode_alt_mean",
              "postcode_slo_mean", "postcode_rgh_mean", "log_postcode_area",
              "log_postcode_perimeter", "log_postcode_coast_len")

# Feature matrices for training and test samples
Xlearn <- as.matrix(learn2[, features])
Xtest <- as.matrix(test2[, features])

# Rename categorical features by transforming and removing original columns
learn2 <- learn2 %>%
  dplyr::mutate(
    Construction_MaterialX = as.integer(factor(Construction_Material_C)) - 1,
    Item_TypeX = as.integer(factor(Item_Type_C)) - 1,
    FloorsTypeX = as.integer(factor(FloorsType_C)) - 1,
    SubItemTypeX = as.integer(factor(SubItemType_C)) - 1,
    postcode_coast_flgX = as.integer(factor(postcode_coast_flg)) - 1
  ) %>%
  dplyr::select(-Construction_Material_C, -Item_Type_C, -FloorsType_C, -SubItemType_C, -postcode_coast_flg)

# Rename numerical features and directly assign them to new variables
Floors_Nolearn <- learn2$Floors_No
Building_Agelearn <- learn2$Building_Age
Construction_Materiallearn <- learn2$Construction_MaterialX
Item_Typelearn <- learn2$Item_TypeX
FloorsTypelearn <- learn2$FloorsTypeX
SubItemTypelearn <- learn2$SubItemTypeX
postcode_coast_flglearn <- learn2$postcode_coast_flgX
nrOfFloorslearn <- learn2$nrOfFloors
floodvulnerabilitylearn <- learn2$floodvulnerability
windvulnerabilitylearn <- learn2$windvulnerability
postcode_alt_meanlearn <- learn2$postcode_alt_mean
postcode_slo_meanlearn <- learn2$postcode_slo_mean
postcode_rgh_meanlearn <- learn2$postcode_rgh_mean
log_postcode_arealearn <- learn2$log_postcode_area
log_postcode_perimeterlearn <- learn2$log_postcode_perimeter
log_postcode_coast_lenlearn <- learn2$log_postcode_coast_len

# Combine renamed variables into a feature matrix
Xlearn2 <- as.matrix(cbind(Floors_Nolearn, Building_Agelearn, Construction_Materiallearn, 
                           Item_Typelearn, FloorsTypelearn, SubItemTypelearn, postcode_coast_flglearn, 
                           nrOfFloorslearn, floodvulnerabilitylearn, windvulnerabilitylearn,
                           postcode_alt_meanlearn, postcode_slo_meanlearn, postcode_rgh_meanlearn, 
                           log_postcode_arealearn, log_postcode_perimeterlearn, log_postcode_coast_lenlearn))




# Rename categorical features by transforming and removing original columns in test2
test2 <- test2 %>%
  dplyr::mutate(
    Construction_MaterialX = as.integer(factor(Construction_Material_C)) - 1,
    Item_TypeX = as.integer(factor(Item_Type_C)) - 1,
    FloorsTypeX = as.integer(factor(FloorsType_C)) - 1,
    SubItemTypeX = as.integer(factor(SubItemType_C)) - 1,
    postcode_coast_flgX = as.integer(factor(postcode_coast_flg)) - 1
  ) %>%
  dplyr::select(-Construction_Material_C, -Item_Type_C, -FloorsType_C, -SubItemType_C, -postcode_coast_flg)

# Rename numerical features and directly assign them to new variables for test set
Floors_Notest <- test2$Floors_No
Building_Agetest <- test2$Building_Age
Construction_Materialtest <- test2$Construction_MaterialX
Item_Typetest <- test2$Item_TypeX
FloorsTypetest <- test2$FloorsTypeX
SubItemTypetest <- test2$SubItemTypeX
postcode_coast_flgtest <- test2$postcode_coast_flgX
nrOfFloorstest <- test2$nrOfFloors
floodvulnerabilitytest <- test2$floodvulnerability
windvulnerabilitytest <- test2$windvulnerability
postcode_alt_meantest <- test2$postcode_alt_mean
postcode_slo_meantest <- test2$postcode_slo_mean
postcode_rgh_meantest <- test2$postcode_rgh_mean
log_postcode_areatest <- test2$log_postcode_area
log_postcode_perimetertest <- test2$log_postcode_perimeter
log_postcode_coast_lentest <- test2$log_postcode_coast_len

# Combine renamed variables into a feature matrix for the test set
Xtest2 <- as.matrix(cbind(Floors_Notest, Building_Agetest, Construction_Materialtest, 
                          Item_Typetest, FloorsTypetest, SubItemTypetest, postcode_coast_flgtest, 
                          nrOfFloorstest, floodvulnerabilitytest, windvulnerabilitytest,
                          postcode_alt_meantest, postcode_slo_meantest, postcode_rgh_meantest, 
                          log_postcode_areatest, log_postcode_perimetertest, log_postcode_coast_lentest))


# dimension embedding layers for categorical features
d <- 2 

#SubItemTypeLabel <- length(unique(learn3$SubItemTypeX))
Construction_MaterialLabel <- length(unique(learn2$Construction_MaterialX))
Item_TypeLabel <- length(unique(learn2$Item_TypeX))
FloorsTypeLabel <- length(unique(learn2$FloorsTypeX))
SubItemTypeLabel <- length(unique(learn2$SubItemTypeX))
postcode_coast_flgLabel <- length(unique(learn2$postcode_coast_flgX))

#-------------------------------------------------------------------------------
#------------M4: Deep neural network with dropout layers (drNN) ----------------
#-------------------------------------------------------------------------------
# define network
q0 <- length(features)  # dimension of features
q1 <- 15                 # number of neurons in first hidden layer
q2 <- 10                 # number of neurons in second hidden layer
q3 <- 5                 # number of neurons in second hidden layer
p0 <- 0.05               # dropout rate

# dimension embedding layers for categorical features
d <- 2 

# Function to dynamically compute lambda_hom and pass it to the model
create_model_dr <- function(loss_function, lambda_hom) {
  Sys.setenv(PYTHONHASHSEED = seed)
  set.seed(seed)
  reticulate::py_set_seed(seed)
  tensorflow::tf$random$set_seed(seed)
  set_random_seed(seed)
  
  # The model definition code goes here
  Design <- layer_input(shape = c(q0), dtype = 'float32', name = 'Design') 
  Construction_Material <- layer_input(shape = c(1), dtype = 'int32', name = 'Construction_Material')
  Item_Type <- layer_input(shape = c(1), dtype = 'int32', name = 'Item_Type')
  FloorsType <- layer_input(shape = c(1), dtype = 'int32', name = 'FloorsType')
  SubItemType <- layer_input(shape = c(1), dtype = 'int32', name = 'SubItemType')
  postcode_coast_flg <- layer_input(shape = c(1), dtype = 'int32', name = 'postcode_coast_flg')
  
  Construction_MaterialEmb <- Construction_Material %>%
    layer_embedding(input_dim = Construction_MaterialLabel, output_dim = d, input_length = 1, name = 'Construction_MaterialEmb') %>%
    layer_flatten(name='Construction_Material_flat')
  
  Item_TypeEmb <- Item_Type %>%
    layer_embedding(input_dim = Item_TypeLabel, output_dim = d, input_length = 1, name = 'Item_TypeEmb') %>%
    layer_flatten(name='Item_Type_flat')
  
  FloorsTypeEmb <- FloorsType %>%
    layer_embedding(input_dim = FloorsTypeLabel, output_dim = d, input_length = 1, name = 'FloorsTypeEmb') %>%
    layer_flatten(name='FloorsType_flat')
  
  SubItemTypeEmb <- SubItemType %>%
    layer_embedding(input_dim = SubItemTypeLabel, output_dim = d, input_length = 1, name = 'SubItemTypeEmb') %>%
    layer_flatten(name = 'SubItemType_flat')
  
  postcode_coast_flgEmb <- postcode_coast_flg %>%
    layer_embedding(input_dim = postcode_coast_flgLabel, output_dim = d, input_length = 1, name = 'postcode_coast_flgEmb') %>%
    layer_flatten(name = 'postcode_coast_flg_flat')
  
  
  Network <- list(Design, Construction_MaterialEmb, Item_TypeEmb, FloorsTypeEmb, SubItemTypeEmb, postcode_coast_flgEmb) %>%
    layer_concatenate(name = 'concate') %>%
    layer_dense(units = q1, activation = 'tanh', name = 'layer1') %>%
    layer_dropout(rate = p0) %>%
    layer_dense(units = q2, activation = 'tanh', name = 'layer2') %>%
    layer_dropout(rate = p0) %>%
    layer_dense(units = q3, activation = 'tanh', name = 'layer3') %>%
    layer_dropout(rate = p0) %>%
    layer_dense(units = 1, activation = 'linear', name = 'Network',
                weights = list(array(0, dim = c(q3, 1)), array(log(lambda_hom), dim = c(1))))  # Change dim to c(q1, 2) and c(2)
  
  Response <- Network %>%
    # layer_add(name = 'Add')%>%
    layer_dense(units = 1, activation = k_exp, name = 'Response', trainable = FALSE,
                weights = list(array(1, dim = c(1, 1)), array(0, dim = c(1))))  # Make sure this aligns with the output of the 'Network' layer
  
  model_dr <- keras_model(inputs = c(Design, Construction_Material, Item_Type, FloorsType, SubItemType, postcode_coast_flg), outputs = c(Response))
  
  model_dr %>% compile(
    loss = loss_function, # Use the input loss function
    optimizer = 'nadam'
  )
  
  return(model_dr)
}

# Set hyperparameters
epochs <- 300
batch_size <- 16
validation_split <- 0.25 # Set to >0 to see train/validation loss in plot(fit)
verbose <- 1

# Expanded list of loss functions
loss_functions <- c("dev.loss_tensor.GA", "dev.loss_tensor.IG", "dev.loss_tensor.LOGNO", 
                    "dev.loss_tensor.WEI3", "dev.loss_tensor.P2o")

# Map loss functions to corresponding regression models for lambda_hom calculation
model_mapping <- list(
  "GA" = GAMMA.reg,
  "IG" = IG.reg,
  "LOGNO" = LOGNO.reg,
  "WEI3" = WEI3.reg,
  "P2o" = PARETO2o.reg
)

# Loop over loss functions
for (loss_func in loss_functions) {
  # Extract the loss function name
  loss_name <- gsub("dev.loss_tensor.", "", loss_func)
  
  # Compute lambda_hom dynamically for the corresponding regression model
  reg_model <- model_mapping[[loss_name]] # Map loss function to its model
  lambda_hom <- mean(range(reg_model$mu.fv)) # Compute lambda_hom for the specific model
  
  
  # Train the model
  exec_time <- system.time({
    model_dr <- create_model_dr(get(loss_func), lambda_hom) # Pass lambda_hom dynamically
    
    # Callback
    CBs <- callback_model_checkpoint(local_path, monitor =  "val_loss", save_best_only = TRUE, verbose = 1, save_weights_only = TRUE)
    
    fit <- model_dr %>% fit(
      x = list(Xlearn2, Construction_Materiallearn, Item_Typelearn, FloorsTypelearn, SubItemTypelearn, postcode_coast_flglearn),
      y = as.matrix(learn$Avg_Cost),
      epochs = epochs,
      batch_size = batch_size,
      validation_split = validation_split,
      verbose = verbose
    )
  })
  
  # Save predictions to dynamically named columns
  learn2[[paste0(loss_name, "_NN")]] <- as.vector(model_dr %>% predict(list(Xlearn2, Construction_Materiallearn, Item_Typelearn, FloorsTypelearn, SubItemTypelearn, postcode_coast_flglearn)))
  test2[[paste0(loss_name, "_NN")]] <- as.vector(model_dr %>% predict(list(Xtest2, Construction_Materialtest, Item_Typetest, FloorsTypetest, SubItemTypetest, postcode_coast_flgtest)))
  
  trainable_params <- sum(unlist(lapply(model_dr$trainable_weights, k_count_params)))
  
  # Append NN results to df_cmp
  df_cmp <- df_cmp %>% bind_rows(
    data.frame(
      Model = paste("M:", loss_name, "NN"), 
      Epochs = epochs, 
      Run_Time = round(e
      Parameters = trainable_params,
      In_Sample_Loss = mean(tensor_to_numeric(
        get(loss_func)(y_pred = learn2[[paste0(loss_name, "_NN")]], 
                       y_true = as.vector(unlist(learn2$Avg_Cost))))),
      Out_Sample_Loss = mean(tensor_to_numeric(
        get(loss_func)(y_pred = test2[[paste0(loss_name, "_NN")]], 
                       y_true = as.vector(unlist(test2$Avg_Cost))))),
      Avg_Cost = mean(test2[[paste0(loss_name, "_NN")]]),
      GAIC = 0
    )
  )
}
df_cmp

# Bias Adjustment
mean_Avg_Cost <- mean(data3$Avg_Cost)
mean_GAMMA_NN <- mean(test2$GA_NN)
mean_IG_NN <- mean(test2$IG_NN)
mean_LOGNO_NN <- mean(test2$LOGNO_NN)
mean_WEI3_NN <- mean(test2$WEI3_NN)
mean_PARETO_NN <- mean(test2$P2o_NN)

GAMMA.adj.coef.NN <- mean_Avg_Cost/mean_GAMMA_NN
IG.adj.coef.NN <- mean_Avg_Cost/mean_IG_NN
LOGNO.adj.coef.NN <- mean_Avg_Cost/mean_LOGNO_NN
WEI3.adj.coef.NN <- mean_Avg_Cost/mean_WEI3_NN
PARETO.adj.coef.NN <- mean_Avg_Cost/mean_PARETO_NN

learn$GAMMA.mean.sNN <- GAMMA.adj.coef.NN*learn2$GA_NN
learn$IG.mean.sNN <- IG.adj.coef.NN*learn2$IG_NN
learn$LOGNO.mean.sNN <- LOGNO.adj.coef.NN*learn2$LOGNO_NN
learn$WEI3.mean.sNN <- WEI3.adj.coef.NN*learn2$WEI3_NN
learn$P2o.mean.sNN <- PARETO.adj.coef.NN*learn2$P2o_NN

test$GAMMA.mean.sNN <- GAMMA.adj.coef.NN*test2$GA_NN
test$IG.mean.sNN <- IG.adj.coef.NN*test2$IG_NN
test$LOGNO.mean.sNN <- LOGNO.adj.coef.NN*test2$LOGNO_NN
test$WEI3.mean.sNN <- WEI3.adj.coef.NN*test2$WEI3_NN
test$P2o.mean.sNN <- PARETO.adj.coef.NN*test2$P2o_NN

# GAMMA Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M1: GAMMA NN", Epochs = 0 , Run_Time = 0, 
             Parameters = trainable_params,
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = learn$GAMMA.mean.sNN, 
                                  y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.GA(y_pred = test$GAMMA.mean.sNN, 
                                  y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$GAMMA.mean.sNN),
             GAIC = 0)
)

# IG Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M2: IG NN", Epochs = 0 , Run_Time = 0, 
             Parameters = trainable_params,
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = learn$IG.mean.sNN, 
                                  y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.IG(y_pred = test$IG.mean.sNN, 
                                  y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$IG.mean.sNN),
             GAIC = 0)
)

# LOGNO Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M3: LOGNO NN", Epochs = 0 , Run_Time = 0, 
             Parameters = trainable_params,
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = learn$LOGNO.mean.sNN, 
                                     y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.LOGNO(y_pred = test$LOGNO.mean.sNN, 
                                     y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$LOGNO.mean.sNN),
             GAIC = 0)
)

# Weibull Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M4: WEI3 NN", Epochs = 0 , Run_Time = 0, 
             Parameters = trainable_params,
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = learn$WEI3.mean.sNN, 
                                    y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.WEI3(y_pred = test$WEI3.mean.sNN, 
                                    y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$WEI3.mean.sNN),
             GAIC = 0)
)

# Pareto Results
df_cmp %<>% bind_rows(
  data.frame(Model = "M5: Pareto NN", Epochs = 0 , Run_Time = 0, 
             Parameters = trainable_params,
             In_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = learn$P2o.mean.sNN, 
                                   y_true = as.vector(unlist(learn$Avg_Cost))))),
             Out_Sample_Loss = mean(tensor_to_numeric(
               dev.loss_tensor.P2o(y_pred = test$P2o.mean.sNN, 
                                   y_true = as.vector(unlist(test$Avg_Cost))))),
             Avg_Cost = mean(test$P2o.mean.sNN),
             GAIC = 0)
)
df_cmp




