##########################
### Funções auxiliares ###
##########################

getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
  
  hypothesis_string <- "hypothesis <- formula(target ~ "
  for(d in 1:degree){
    for(i in 1:length(real_feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 "I(", real_feature_names[i], "^", d, ") + ",
                                 sep = "")
    }
  }
  
  if(typeof(categorical_feature_names) != "logical"){
    for(i in 1:length(categorical_feature_names)){
      hypothesis_string <- paste(hypothesis_string, 
                                 categorical_feature_names[i], " + ",
                                 sep = "")
    } 
  }
  
  
  hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
  hypothesis_string <- paste(hypothesis_string, ")")
  hypothesis <- eval(parse(text=hypothesis_string))
  return(hypothesis)
}

# Mean Absolute Error
MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

############################
### Análise exploratória ###
############################
setwd("~/Desktop/MDC/INF-0619 - Projeto Final")
train_val_set <- read.csv("bigquery-geotab-intersection-congestion/train.csv", stringsAsFactors = T)
any(is.na(train_val_set)) # false

random_indexes <- sample(1:nrow(train_val_set), size=0.8*nrow(train_val_set))
train_set <- train_val_set[random_indexes, ]
val_set  <- train_val_set[-random_indexes, ]

head(train_set)

# one hot encoding - train set
train_set$City_Philadelphia <- ifelse(train_set$City == 'Philadelphia', 1, 0)
train_set$City_Boston <- ifelse(train_set$City == 'Boston', 1, 0)
train_set$City_Atlanta <- ifelse(train_set$City == 'Atlanta', 1, 0)
train_set$City_Chicago <- ifelse(train_set$City == 'Chicago', 1, 0)
train_set$City <- NULL

train_set$EntryHeading_N <- ifelse(train_set$EntryHeading == 'N', 1, 0)
train_set$EntryHeading_W <- ifelse(train_set$EntryHeading == 'W', 1, 0)
train_set$EntryHeading_S <- ifelse(train_set$EntryHeading == 'S', 1, 0)
train_set$EntryHeading_SW <- ifelse(train_set$EntryHeading == 'SW', 1, 0)
train_set$EntryHeading_E <- ifelse(train_set$EntryHeading == 'E', 1, 0)
train_set$EntryHeading_NW <- ifelse(train_set$EntryHeading == 'NW', 1, 0)
train_set$EntryHeading_NE <- ifelse(train_set$EntryHeading == 'NE', 1, 0)
train_set$EntryHeading_SE <- ifelse(train_set$EntryHeading == 'SE', 1, 0)
train_set$EntryHeading <- NULL

train_set$ExitHeading_N <- ifelse(train_set$ExitHeading == 'N', 1, 0)
train_set$ExitHeading_W <- ifelse(train_set$ExitHeading == 'W', 1, 0)
train_set$ExitHeading_S <- ifelse(train_set$ExitHeading == 'S', 1, 0)
train_set$ExitHeading_SW <- ifelse(train_set$ExitHeading == 'SW', 1, 0)
train_set$ExitHeading_E <- ifelse(train_set$ExitHeading == 'E', 1, 0)
train_set$ExitHeading_NW <- ifelse(train_set$ExitHeading == 'NW', 1, 0)
train_set$ExitHeading_NE <- ifelse(train_set$ExitHeading == 'NE', 1, 0)
train_set$ExitHeading_SE <- ifelse(train_set$ExitHeading == 'SE', 1, 0)
train_set$ExitHeading <- NULL

train_set_RowId <- train_set$RowId
train_set$RowId <- NULL
train_set$EntryStreetName <- NULL
train_set$ExitStreetName <- NULL
train_set$Path <- NULL
train_set_Weekend <- train_set$Weekend
train_set$Weekend <- NULL
train_set$Weekend <- train_set_Weekend

train_set_TotalTimeStopped_p20 <-train_set$TotalTimeStopped_p20
train_set_TotalTimeStopped_p40 <-train_set$TotalTimeStopped_p40
train_set_TotalTimeStopped_p50 <-train_set$TotalTimeStopped_p50
train_set_TotalTimeStopped_p60 <-train_set$TotalTimeStopped_p60
train_set_TotalTimeStopped_p80 <-train_set$TotalTimeStopped_p80
train_set$TotalTimeStopped_p20 <- NULL
train_set$TotalTimeStopped_p40 <- NULL
train_set$TotalTimeStopped_p50 <- NULL
train_set$TotalTimeStopped_p60 <- NULL
train_set$TotalTimeStopped_p80 <- NULL

train_set_TimeFromFirstStop_p20 <- train_set$TimeFromFirstStop_p20
train_set_TimeFromFirstStop_p40 <- train_set$TimeFromFirstStop_p40
train_set_TimeFromFirstStop_p50 <- train_set$TimeFromFirstStop_p50
train_set_TimeFromFirstStop_p60 <- train_set$TimeFromFirstStop_p60
train_set_TimeFromFirstStop_p80 <- train_set$TimeFromFirstStop_p80
train_set$TimeFromFirstStop_p20 <- NULL
train_set$TimeFromFirstStop_p40 <- NULL
train_set$TimeFromFirstStop_p50 <- NULL
train_set$TimeFromFirstStop_p60 <- NULL
train_set$TimeFromFirstStop_p80 <- NULL

train_set_DistanceToFirstStop_p20 <- train_set$DistanceToFirstStop_p20
train_set_DistanceToFirstStop_p40 <- train_set$DistanceToFirstStop_p40
train_set_DistanceToFirstStop_p50 <- train_set$DistanceToFirstStop_p50
train_set_DistanceToFirstStop_p60 <- train_set$DistanceToFirstStop_p60
train_set_DistanceToFirstStop_p80 <- train_set$DistanceToFirstStop_p80
train_set$DistanceToFirstStop_p20 <- NULL
train_set$DistanceToFirstStop_p40 <- NULL
train_set$DistanceToFirstStop_p50 <- NULL
train_set$DistanceToFirstStop_p60 <- NULL
train_set$DistanceToFirstStop_p80 <- NULL


# one hot encoding - validation set
val_set$City_Philadelphia <- ifelse(val_set$City == 'Philadelphia', 1, 0)
val_set$City_Boston <- ifelse(val_set$City == 'Boston', 1, 0)
val_set$City_Atlanta <- ifelse(val_set$City == 'Atlanta', 1, 0)
val_set$City_Chicago <- ifelse(val_set$City == 'Chicago', 1, 0)
val_set$City <- NULL

val_set$EntryHeading_N <- ifelse(val_set$EntryHeading == 'N', 1, 0)
val_set$EntryHeading_W <- ifelse(val_set$EntryHeading == 'W', 1, 0)
val_set$EntryHeading_S <- ifelse(val_set$EntryHeading == 'S', 1, 0)
val_set$EntryHeading_SW <- ifelse(val_set$EntryHeading == 'SW', 1, 0)
val_set$EntryHeading_E <- ifelse(val_set$EntryHeading == 'E', 1, 0)
val_set$EntryHeading_NW <- ifelse(val_set$EntryHeading == 'NW', 1, 0)
val_set$EntryHeading_NE <- ifelse(val_set$EntryHeading == 'NE', 1, 0)
val_set$EntryHeading_SE <- ifelse(val_set$EntryHeading == 'SE', 1, 0)
val_set$EntryHeading <- NULL

val_set$ExitHeading_N <- ifelse(val_set$ExitHeading == 'N', 1, 0)
val_set$ExitHeading_W <- ifelse(val_set$ExitHeading == 'W', 1, 0)
val_set$ExitHeading_S <- ifelse(val_set$ExitHeading == 'S', 1, 0)
val_set$ExitHeading_SW <- ifelse(val_set$ExitHeading == 'SW', 1, 0)
val_set$ExitHeading_E <- ifelse(val_set$ExitHeading == 'E', 1, 0)
val_set$ExitHeading_NW <- ifelse(val_set$ExitHeading == 'NW', 1, 0)
val_set$ExitHeading_NE <- ifelse(val_set$ExitHeading == 'NE', 1, 0)
val_set$ExitHeading_SE <- ifelse(val_set$ExitHeading == 'SE', 1, 0)
val_set$ExitHeading <- NULL

val_set_RowId <- val_set$RowId
val_set$RowId <- NULL
val_set$EntryStreetName <- NULL
val_set$ExitStreetName <- NULL
val_set$Path <- NULL
val_set_Weekend <- val_set$Weekend
val_set$Weekend <- NULL
val_set$Weekend <- val_set_Weekend

val_set_TotalTimeStopped_p20 <-val_set$TotalTimeStopped_p20
val_set_TotalTimeStopped_p40 <-val_set$TotalTimeStopped_p40
val_set_TotalTimeStopped_p50 <-val_set$TotalTimeStopped_p50
val_set_TotalTimeStopped_p60 <-val_set$TotalTimeStopped_p60
val_set_TotalTimeStopped_p80 <-val_set$TotalTimeStopped_p80
val_set$TotalTimeStopped_p20 <- NULL
val_set$TotalTimeStopped_p40 <- NULL
val_set$TotalTimeStopped_p50 <- NULL
val_set$TotalTimeStopped_p60 <- NULL
val_set$TotalTimeStopped_p80 <- NULL

val_set_TimeFromFirstStop_p20 <- val_set$TimeFromFirstStop_p20
val_sett_TimeFromFirstStop_p40 <- val_set$TimeFromFirstStop_p40
val_set_TimeFromFirstStop_p50 <- val_set$TimeFromFirstStop_p50
val_set_TimeFromFirstStop_p60 <- val_set$TimeFromFirstStop_p60
val_set_TimeFromFirstStop_p80 <- val_set$TimeFromFirstStop_p80
val_set$TimeFromFirstStop_p20 <- NULL
val_set$TimeFromFirstStop_p40 <- NULL
val_set$TimeFromFirstStop_p50 <- NULL
val_set$TimeFromFirstStop_p60 <- NULL
val_set$TimeFromFirstStop_p80 <- NULL

val_set_DistanceToFirstStop_p20 <- val_set$DistanceToFirstStop_p20
val_set_DistanceToFirstStop_p40 <- val_set$DistanceToFirstStop_p40
val_set_DistanceToFirstStop_p50 <- val_set$DistanceToFirstStop_p50
val_set_DistanceToFirstStop_p60 <- val_set$DistanceToFirstStop_p60
val_set_DistanceToFirstStop_p80 <- val_set$DistanceToFirstStop_p80
val_set$DistanceToFirstStop_p20 <- NULL
val_set$DistanceToFirstStop_p40 <- NULL
val_set$DistanceToFirstStop_p50 <- NULL
val_set$DistanceToFirstStop_p60 <- NULL
val_set$DistanceToFirstStop_p80 <- NULL



# Normalização train set
train_set$target <- train_set_TotalTimeStopped_p20
min_features <- apply(train_set, 2, min)
min_features

max_features <- apply(train_set, 2, max)
max_features

diff <- max_features - min_features
diff

train_set <- sweep(train_set, 2, min_features, "-")
train_set <- sweep(train_set, 2, diff, "/")
summary(train_set)

# Normalização val set
val_set$target <- val_set_TotalTimeStopped_p20
min_features <- apply(val_set, 2, min)
min_features

max_features <- apply(val_set, 2, max)
max_features

diff <- max_features - min_features
diff
val_set <- sweep(val_set, 2, min_features, "-")
val_set <- sweep(val_set, 2, diff, "/")
summary(val_set)

# dataTest[,2:30] <- sweep(dataTest[,2:30], 2, min_features, "-")
# dataTest[,2:30] <- sweep(dataTest[,2:30], 2, diff, "/")
# summary(dataTest)


#############################
### Definição do baseline ###
#############################







# Definindo o baseline
no_categorical_features <- names(train_set[, 1:5])
categorical_features <- names(train_set[, 6:26])
hypothesis <- getHypothesis(no_categorical_features, categorical_features, 1)
baseline <- lm(formula = hypothesis, data = train_set)
summary(baseline)


trainPred <- predict(baseline, dataTrain)
valPred <- predict(baseline, dataVal)
testPred <- predict(baseline, dataTest)


# Cálculo do erro MAE para o baseline
mae_val_baseline <- MAE(valPred, dataVal$target); mae_val_baseline


# Função para plotar os gráficos de erro
plot_complexity <- function(
  baselineError,
  trainErrors,
  validationErrors,
  testErrors,
  title = "Erro Médio Absoluto (MAE)",
  xlabel = "Complexidade"
){
  
  plot(validationErrors, xlab=xlabel, ylab="Erro", ylim=c(260, 400),
       pch="+", col="blue",  xaxt="n")
  axis(1, at=1:length(trainErrors), labels=seq(from = 1, to = length(trainErrors), by = 1), las=1)
  points(trainErrors, pch="*", col="red")
  points(rep(baselineError, length(validationErrors)), pch="o", col="green")
  
  lines(trainErrors, col="red", lty=2)
  lines(validationErrors, col="blue", lty=2)
  lines(testErrors, col="black", lty=2)
  lines(rep(baselineError, length(validationErrors)), col="green", lty=2)
  title(main = "Erro Médio Absoluto (MAE)")
  legend(1, 290, legend=c("Treino", "Validação", "Baseline", "Teste"), 
         col=c("red","blue", "green", "black"), lty=2, cex=0.8)
}

# regressão polinomial
hypothesis_f01 <- getHypothesis(no_categorical_features, categorical_features, 1)
hypothesis_f02 <- getHypothesis(no_categorical_features, categorical_features, 2)
hypothesis_f03 <- getHypothesis(no_categorical_features, categorical_features, 3)
hypothesis_f04 <- getHypothesis(no_categorical_features, categorical_features, 4)
hypothesis_f05 <- getHypothesis(no_categorical_features, categorical_features, 5)
hypothesis_f06 <- getHypothesis(no_categorical_features, categorical_features, 6)

models <- c(hypothesis_f01, hypothesis_f02, hypothesis_f03, hypothesis_f04, hypothesis_f05, hypothesis_f06)
total_mae_train <- c(length(models))
total_mae_val <- c(length(models))
total_mae_test <- c(length(models))

i <- 1
for(f in models){
  
  model <- lm(formula=f, data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  testPred <- predict(model, dataTest)
  
  mae_train <- MAE(trainPred, dataTrain$target)
  total_mae_train[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$target)
  total_mae_val[i] <- mae_val
  
  mae_val <- MAE(testPred, dataTest$target)
  total_mae_test[i] <- mae_val
  
  i <- i + 1
  
}

total_mae_train
# [1] 370.0937 352.1933 349.7766 345.2659 341.0588 339.8772
total_mae_val
# [1] 371.0654 352.9237 351.0492 346.4808 343.0935 343.4974
