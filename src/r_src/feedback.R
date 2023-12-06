library(tidyverse)
library(glmnet)

predictors = read_csv('D:/F2023/AOU/aou_processed_data/trial.csv') %>% 
  select(-c(pid, hasLC)) %>% 
  as.matrix()

outcome = read_csv('D:/f2023/AOU/aou_processed_data/trial.csv')$hasLC


#cv_model3 = cv.glmnet(predictors, outcome, alpha=1, family='binomial')
#plot(cv_model3)
#saveRDS(cv_model3, file = 'D:/F2023/AOU/aou_processed_data/cv_model3.rds')

cv_model3 = readRDS('D:/F2023/AOU/aou_processed_data/cv_model3.rds')

best_lambda3 = cv_model3$lambda.min

lasso3 = glmnet(predictors, outcome, family = 'binomial', alpha=1, 
                best_lambda = best_lambda3)

#saveRDS(lasso3, file = 'D:/F2023/AOU/aou_processed_data/lasso3.rds')
#lasso3 = readRDS('D:/F2023/AOU/aou_processed_data/lasso3.rds')

coef_lasso = predict(lasso3, type = 'coefficients', s = best_lambda3)

significant_predictors = row.names(coef_lasso)[coef_lasso[-1,1]!=0]

names(coef_lasso[significant_predictors, ])

non_zero_table = coef_lasso %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  filter(s1 != 0) %>% 
  rownames_to_column(var = 'Variable')

#non_zero_table[-1,] %>% 
#  write.csv('D:/F2023/AOU/aou_processed_data/lasso3_importFeatures.csv')

trial2 = read_csv('D:/F2023/AOU/aou_processed_data/trial.csv') %>%  
  select(all_of(non_zero_table$Variable[-1]), hasLC)

best_model = glm(hasLC ~ ., data = trial2, family = 'binomial')
#saveRDS(best_model, file = 'D:/F2023/AOU/aou_processed_data/best_model3.rds')
#best_model3 = readRDS('D:/F2023/AOU/aou_processed_data/best_model3.rds')


summary(best_model)$coefficients %>% 
  as.data.frame() %>% 
  mutate(oddsRatio = exp(Estimate)) %>%
  slice(-1) %>% 
  arrange(desc(oddsRatio)) %>% 
  rownames_to_column(var = 'conceptCombinations') #%>% 
  #write.csv('D:/F2023/AOU/aou_processed_data/oddsRatio3.csv', row.names = FALSE)

summary(best_model)

# McFadden's Pseudo-R-squared
null_model = glm(hasLC ~ 1, data = trial2, family = 'binomial')
ll_full = logLik(best_model)
ll_null = logLik(null_model)
mcfadden_r2 = 1 - (as.numeric(ll_full) / as.numeric(ll_null))


greaterThanOne_OR = summary(best_model)$coefficients %>% 
  as.data.frame() %>% 
  mutate(oddsRatio = exp(Estimate)) %>%
  slice(-1) %>% 
  arrange(desc(oddsRatio)) %>% 
  rownames_to_column(var = 'conceptCombinations') %>% 
  filter(oddsRatio > 0.1)

trial3 = trial2 %>% 
  select(all_of(greaterThanOne_OR$conceptCombinations), hasLC)

best_model_extended = glm(hasLC~., data = trial3, family = 'binomial')

summary(best_model_extended)$coefficients %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  slice(-1) %>% 
  mutate(oddsRatio = exp(Estimate)) %>% 
  arrange(desc(oddsRatio)) %>% 
  rownames_to_column(var='diseaseCombinations') #%>% 
  #write.csv('D:/F2023/AOU/aou_processed_data/best_model_extended.csv', row.names = FALSE)
