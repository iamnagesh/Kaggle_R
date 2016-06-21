# TITLE  : Santander Data Preparation
# Author : Nagesh Kommuri - https://in.linkedin.com/in/iamknagesh

train <- read.csv("../input/train.csv")
test <- read.csv("../input/test.csv")

##### Removing IDs
train$ID <- NULL
test.id <- test$ID
test$ID <- NULL

##### Extracting TARGET
train.y <- train$TARGET
train$TARGET <- NULL

### removing duplicate rows from train
names(train)
train <- train[!duplicated(train),]
train.y <- train.y[as.integer(row.names(train))]
row.names(train) <- NULL

##### Removing constant features
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

##### Removing identical features
features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]


### names of different types of variables

ind_var_names = names(train)[grep('ind_', names(train))]
num_var_names = names(train)[grep('num_', names(train))]
saldo_names = names(train)[grep('saldo', names(train))]  ## contains both saldo_varX and saldo_medio_varX
saldo_var_names = saldo_names[grep('saldo_var', saldo_names)]
saldo_medio_var_names = saldo_names[grep('saldo_medio', saldo_names)]


### finding variables in which all the 1s are in one value(like 0)
kind_of_useless_vars <- NULL
for(i in 1:length(train)){
  if(sum(train.y[train[,i] > 0]) <= 50){
    kind_of_useless_vars <- c(kind_of_useless_vars, names(train)[i])
  }
}

### train
useless_df <- train[kind_of_useless_vars]
useless_df$sum <- apply(useless_df, 1, sum)
useless_df$sum[useless_df$sum != 0] <- 1
table(useless_df$sum, train.y)
length(useless_df)-1

train_useless_removed <- as.integer(useless_df$sum)
### test
useless_df <- test[kind_of_useless_vars]
useless_df$sum <- apply(useless_df, 1, sum)
useless_df$sum[useless_df$sum != 0] <- 1
test_useless_removed <- as.integer(useless_df$sum)
rm(useless_df)

feature.names <- feature.names[!feature.names %in% kind_of_useless_vars]
### names of different types of variables

ind_var_names = feature.names[grep('ind_', feature.names)]
num_var_names = feature.names[grep('num_', feature.names)]
saldo_names = feature.names[grep('saldo', feature.names)]  ## contains both saldo_varX and saldo_medio_varX
saldo_var_names = saldo_names[grep('saldo_var', saldo_names)]
saldo_medio_var_names = saldo_names[grep('saldo_medio', saldo_names)]

train <- train[, feature.names]
test <- test[, feature.names]

train_x <- cbind(train, "TARGET" = train.y)
test_x <- test
test_x$TARGET <- -1
x_data <- rbind(train_x, test_x)


x_data$var5_cat <- x_data$ind_var5 + x_data$ind_var5_0
x_data$var8_cat <- x_data$ind_var8 + x_data$ind_var8_0
x_data$var25_cat <- x_data$ind_var25_cte + x_data$ind_var25_0
x_data$var26_cat <- x_data$ind_var26_cte + x_data$ind_var26_0
x_data$var30_cat <- x_data$ind_var30 + x_data$ind_var30_0
x_data$var37_cat <- x_data$ind_var37_cte + x_data$ind_var37_0

x_data$var9_cat <- x_data$ind_var9_ult1 + x_data$ind_var9_cte_ult1
x_data$var43_cat <- x_data$ind_var43_emit_ult1 + x_data$ind_var43_recib_ult1
x_data$var10_cat <- x_data$ind_var10_ult1 + x_data$ind_var10cte_ult1

summary(as.factor(x_data$var10))
names(x_data)
## removing variables
remove_vars <- c("ind_var10_ult1", "ind_var10cte_ult1","ind_var9_ult1", "ind_var9_cte_ult1",
                 "ind_var43_emit_ult1", "ind_var43_recib_ult1","ind_var5", "ind_var5_0",
                 "ind_var8", "ind_var8_0","ind_var30", "ind_var30_0",
                 "ind_var25_cte", "ind_var25_0","ind_var26_cte", "ind_var26_0",
                 "ind_var37_cte", "ind_var37_0")

final.features <- names(x_data)[!names(x_data) %in% remove_vars]

### creating dummies for the newly created _cat variables
to_dummies <- final.features[83:91]
dummy_df <- NULL
for(i in 1:length(to_dummies)){
  dummy_df <- cbind(dummy_df, dummy(to_dummies[i], data = x_data, sep = "_"))
}
dummy_df <- as.data.frame(dummy_df)

final.features <- final.features[!final.features %in% to_dummies]

x_data.y <- x_data$TARGET
x_data$TARGET <- NULL
names(x_data)
x_data <- x_data[,final.features[-length(final.features)]]
x_data <- cBind(x_data, dummy_df,
                "IMP_VAR" = c(train_useless_removed, test_useless_removed),
                "TARGET" = x_data.y)
names(x_data)

sum(x_data$IMP_VAR)
x_data$IMP_VAR[x_data$var15 < 23 | x_data$var15 > 94] <- 1

train <- x_data[x_data$TARGET > -1,]
test <- x_data[x_data$TARGET == -1,]

### Binning

vars_to_bin <- names(train)[c(1:13,40:49,74:81)]
for(j in 1: length(vars_to_bin)){
  a <- smbinning(df = train, y = "TARGET", x = vars_to_bin[j])
  # a
  if(a != "No Bins" & a != "Characteristic (x) has less than 10 uniques values"){
    if(a$iv >= 0.1){
      print(a$iv)
      train$b <- train[, a$col_id]
      test$b <- test[, a$col_id]
      for(i in 1:length(a$bands)){
        train$b[train$b > a$bands[i] & train$b <= a$bands[i+1]] <- a$bands[i+1]
        test$b[test$b > a$bands[i] & test$b <= a$bands[i+1]] <- a$bands[i+1]
      }
      test$b[test$b == a$bands[1]] <- a$bands[2]
      train$b[train$b == a$bands[1]] <- a$bands[2]
      names(train)[length(train)] <- paste("bin", vars_to_bin[j], sep = "_")
      names(test)[length(test)] <- paste("bin", vars_to_bin[j], sep = "_")
    }
  }
}

### creating dummies from the newly created "bin" features
x_data <- rbind(train, test)
x_data$TARGET <- NULL
to_dummies <- names(train)[111:119]
dummy_df <- NULL
for(i in 1:length(to_dummies)){
  dummy_df <- cbind(dummy_df, dummy(to_dummies[i], data = x_data, sep = "_"))
}
dummy_df <- as.data.frame(dummy_df)

final.features <- names(x_data)[!names(x_data) %in% to_dummies]
x_data <- x_data[,final.features]
x_data <- cBind(x_data, dummy_df, "TARGET" = x_data.y)

names(x_data) <- gsub("[.]", "_", names(x_data))
names(x_data) <- names(x_data) <- gsub("-", "_", names(x_data))

train <- x_data[x_data$TARGET > -1,]
test <- x_data[x_data$TARGET == -1,]
