library(mice)
library(docstring)
library(paramtest)
# 
# test_data <-  dataset[1:50,]
# 
# ini <- mice(test_data, maxit=0, print=F)
# pred <- ini$pred
# pred
# ini <- mice(data,method = 'pmm',pred=quickpred(data, mincor=0.56,method = 'spearman'))
# 
# fit1 <- with(ini,lm((as.formula(as.character(test[1])))))
# mean(tidy(pool(fit1))$fmi)
# 







#### FROMULA CREATING #### 

formula_creating <- function(df,coll_miss,coll_no_miss,coll_type,percent_of_missing){
  #' Creating formula
  #' 
  #' @description This function create formula to use in with function form mice packag.
  #' Alsow inform if its nesessery to use gml insted of lm (no numeric values in dataset).
  #' 
  #' @param df data.frame. Df to impute with colnames and without target column.
  #' @param coll_miss character vector. Names of columns with NA.
  #' @param coll_no_miss character vector. Names of columns without NA.
  #' @param coll_type character vector. Vector containing column type names.
  #' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
  #' @usage formula_creating(df,coll_miss,coll_no_miss,coll_type,percent_of_missing)
  #' @return List with formula object[1] and information if its no numeric value in dataset[2].
  
  # Flags if no numeric value in df
  no_numeric <-  T
  
  #If df contains numeric values 
  if  ('numeric' %in% coll_type | 'intiger' %in% coll_type){
    no_numeric <- F
    numeric_columns <- colnames(df)[ifelse('numeric' == coll_type | 'intiger' == coll_type,T,F)]
    
    #If some numeric columns dont contain missing data 
    numeric_no_missing <- intersect(numeric_columns,colnames(df)[percent_of_missing==0])
    if (length(numeric_no_missing)>0){
      predicted_value <- numeric_no_missing[1]
      if (sum(percent_of_missing>0)>=3){
        columns_missing  <-  as.data.frame(cbind(percent_of_missing,colnames(df)))
        columns_missing <- columns_missing[order(as.numeric(as.character(columns_missing$percent_of_missing)),decreasing = TRUE),]
        predicting_values <- columns_missing$V2[1:3]
      }
      else{ predicting_values <- coll_miss}
      
    }
    
    else{
      columns_missing_type <- cbind(percent_of_missing,colnames(df),coll_type,col=c('x1','x2','x3'))
      columns_missing_type_n_i <- columns_missing_type[columns_missing_type$coll_type=='numeric' | columns_missing_type$coll_type == 'initger',]
      if (length(row.names(columns_missing_type_n_i))==1) {no_numeric <-  T }
      predicted_value <- columns_missing_type_n_i[order(columns_missing$percent_of_missing),][1]
      if (length(row.names(columns_missing_type[-1,]))>=3){
        predicting_values <-  columns_missing_type[order(as.numeric(as.character(columns_missing_type$percent_of_missing)),decreasing = T),V2][1:3]
      }
      else{predicting_value <-setdiff(predicted_value,coll_miss)}
    }
    
    
  }
  # If df dont contains numeric value 
  if (no_numeric){
    predicted_value <- coll_no_miss[1]
    if (sum(percent_of_missing>0)>=3){
      columns_missing  <-  cbind(percent_of_missing,colnames(df),col=c('x1','x2'))
      columns_missing <- columns_missing[order(columns_missing$percent_of_missing,decreasing = TRUE),]
      predicting_values <- columns_missing$V2[1:3]
    }
    else{ predicting_values <- coll_miss}
  }
  
  

  return(list(as.formula(paste(as.character(predicted_value),paste(as.character(predicting_values),collapse = '+'),sep='~')),no_numeric))
  
}


##Testing 
# percent_of_missing <- 1:34
# for (i in 1:34){
#   percent_of_missing[i]  <- (sum(is.na(dataset[,i]))/540)*100 
# }
# 
# coll_type <- 1:34 
# for (i in 1:34){
#   coll_type[i] <- (class(dataset[,i]))
# }
# 
# test <- formula_creating(data,coll_miss = coll_miss,coll_no_miss,coll_type,percent_of_missing)
# Looks like it is working 

#### RANDOM SEARCH ####
random_param_mice_search <- function(low_corr=0,up_corr=1,methods_random = c('pmm'),df,formula,no_numeric,iter,random.seed=123,correlation=T){
  #' Performing randomSearch for method and corr or fraction of featuers use to create prediction matrix. 
  #' 
  #' @description This function perform random search and retrun values coresponding to best mean imf (missing information fraction).
  #' 
  #' @param low_corr double betwen 0,1 defoult 0 lower boundry of correlation set.
  #' @param up_corr double betwen 0,1 defoult 1 upper boundry of correlation set. Both of this parametrs work the same for fraction of features.
  #' @param methods_random set of methods to chose. Defoult 'pmm'.
  #' @param df data frame to inpute.
  #' @param formule first product of formula_creating() funtion. For example formula_creating(...)[1]
  #' @param no_numeric second product of formula_creating() function.
  #' @param iter number of iteration for randomSearch.
  #' @param random.seed radnom seed.
  #' @param correlation If True correlation is using if Fales fraction of featuers. Defoult True.
  #' 
  #' @return List with best corr (or fraction ) at first place, best method at second and results of every iteration at 3.
  set.seed(random.seed)
  corr <- runif(iter,0,1)
  met <- sample(methods_random,iter,replace = T)
  
  
  # Performing random search and saving result 
  result <- rep(1,iter)
  
  for (i in 1:iter){
    skip_to_next <- F
    
    tryCatch(
      {
        if (correlation){
        inputation <- mice(df,method = met[i],pred=quickpred(df, mincor=corr[i],method = 'spearman'),seed = random.seed)}
        if (!correlation){
          inputation <- mice(df,method = met[i],pred=quickpred(df, minpuc=corr[i],method = 'spearman'),seed = random.seed)
        }
        
        if (as.logical(no_numeric[1])){
          fit <- with(inputation,glm(as.formula(as.character(formula)),family = binomial))
        }
        if (!as.logical(no_numeric[1])){
          
        fit <- with(inputation,expr = lm((as.formula(as.character(formula)))))
        }
        result[i] <- mean(tidy(pool(fit))$fmi)
        
      }, error = function(e) { skip_to_next <<- TRUE})
    if(skip_to_next) { next }
    
  }
  
  # Returning result 
  return(list(corr[which.min(result)],met[which.min(result)],result))
  
}
  
# Test Random Search 

# test2 <- random_param_mice_search(df=data,formula =test[1],no_numeric = test[2],iter=1)
# test2[3]  
# Looks like it is working 



##### AUTOTUNE_MICE #### 

autotune_mice <- function(df,m=5,maxit=5,coll_miss,coll_no_miss,coll_type,percent_of_missing,low_corr=0,up_corr=1,methods_random=c('pmm'),iter,random.seed=123,optimize_no_numeric = F,correlation=T ){
  #' Automatical tuning of parametrs and inputation using mice package.
  #' 
  #' @description Function inpute missing data using mice functions. First perform random search using linear models (generalaise linear models if only 
  #' categorical values are avalibe). Using glm its problematic function allowe user to skip optimalization in that case but its can lead to errors.
  #' Function optimize prediction matrix and method. Ather mice parametrs like number of sets(m) or max number of iterations(maxit) shoud be set 
  #' as hight as posible for best results(higher values are requaiers more time to perform inputation).
  #'
  #'
  #' 
  #' @param df data frame for imputation.
  #' @param m number of sets produce by mice.
  #' @param maxit maximum number of iteration for mice.
  #' @param coll_miss name of columns with missong values.
  #' @param coll_no_miss character vector. Names of columns without NA.
  #' @param coll_type character vector. Vector containing column type names.
  #' @param percent_of_missing numeric vector. Vector contatining percent of missing data in columns for example  c(0,1,0,0,11.3,..)
  #' @param low_corr double betwen 0,1 defoult 0 lower boundry of correlation set.
  #' @param up_corr double betwen 0,1 defoult 1 upper boundry of correlation set. Both of this parametrs work the same for fraction of features.
  #' @param methods_random set of methods to chose. Defoult 'pmm'.
  #' @param iter number of iteration for randomSearch.
  #' @param random.seed radnom seed.
  #' @param optimize_no_numeric if use wont to optimize when no numeric values exsist in df. Defoult False
  #' @param correlation If True correlation is using if Fales fraction of featuers. Defoult True.
  #' @return Return mids object for future using in ml algoritm u can use complete() function form mice package to extract imputed datesets.
 
  
  formula_cre <- formula_creating(df,coll_miss,coll_no_miss,coll_type,percent_of_missing)
  formula <- formula_cre[1]
  no_numeric <- as.logical(formula_cre[2])
  if (!optimize_no_numeric){
  params <- random_param_mice_search(df=df,low_corr = low_corr,up_corr = up_corr,methods_random = methods_random,formula = formula,no_numeric = no_numeric,random.seed = random.seed,iter=iter,correlation = correlation)
  if (correlation){
  imp_final <- mice(df,m=m,maxit = maxit,method = as.character(params[2]),pred=quickpred(df, mincor=as.numeric(params[1]),method = 'spearman'),seed = random.seed)
  }
  if (!correlation){
    imp_final <- mice(df,m=m,maxit = maxit,method = as.character(params[2]),pred=quickpred(df, minpuc = as.numeric(params[1]),method = 'spearman'),seed = random.seed)
    }
  }
   if (optimize_no_numeric){
     if (correlation){
       imp_final <- mice(df,m=m,maxit = maxit,method = 'pmm',pred=quickpred(df, mincor=0.5,method = 'spearman'),seed = random.seed)
     }
     if (!correlation){
       imp_final <- mice(df,m=m,maxit = maxit,method = 'pmm',pred=quickpred(df, minpuc = 0.5,method = 'spearman'),seed = random.seed)
     }
  }
  return(list(imp_final))
  
} 
#### TESTS ##### 
# final_imp <- autotune_mice(data,m=5,maxit = 5,coll_miss = coll_miss,coll_no_miss = coll_no_miss,coll_type = coll_type,percent_of_missing = percent_of_missing,
#               iter=1)
# out_of_the_box <-  mice(data,pred=quickpred(data, mincor=0.28,method = 'spearman'),method = 'pmm',seed=123)
# class(final_imp)
# fit1 <- with(final_imp,lm(as.formula(as.character(test[1]))))
# fit2 <- with(out_of_the_box,lm(as.formula(as.character(test[1]))))
# 
# mean(tidy(pool(fit2))$fmi)
# 
# 
