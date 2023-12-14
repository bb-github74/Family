library(tidyverse)
library(glmnet)


non_select = c('pid','csdT','csn','csdLC','isBefore')

trial = read_csv('D:/F2023/AOU/aou_processed_data/trialTwo.csv') %>% 
  select(-any_of(non_select), -scn)

trial = trial %>% 
  mutate_all(~ ifelse(is.na(.), 0,.))

sum(is.na(trial))


predictors = trial %>% 
  select(-hasLC) %>% 
  as.matrix()

outcome = trial$hasLC


# cross validate to find best lambda 
#cv_model4 = cv.glmnet(predictors, outcome, alpha=1, family='binomial')
#saveRDS(cv_model4, file='D:/F2023/AOU/aou_processed_data/cv_model4.rds')

cv_model4 = readRDS('D:/F2023/AOU/aou_processed_data/cv_model4.rds')
# visualize
plot(cv_model4)

best_lambda4 = cv_model4$lambda.min

#lasso4 = glmnet(predictors, outcome, family = 'binomial', alpha=1, best_lambda = best_lambda4)
#saveRDS(lasso4, file='D:/F2023/AOU/aou_processed_data/lasso4.rds')
lasso4 = readRDS('D:/F2023/AOU/aou_processed_data/lasso4.rds')

coef_lasso4 = predict(lasso4, type = 'coefficients', s = best_lambda4)

none_zero_predictors4 = coef_lasso4 %>% 
  as.matrix() %>% 
  as.data.frame() %>% 
  slice(-1) %>% 
  filter(s1 != 0) %>% 
  rownames_to_column(var = 'Variable') %>% 
  rename(coeffs = s1) %>% 
  as.data.frame()

none_zero_predictors4

#trial41 = read_csv('D:/F2023/AOU/aou_processed_data/trialTwo.csv') %>%  

trial41 = trial %>% 
  select(all_of(none_zero_predictors4$Variable))

trial41 = trial41 %>% 
  cbind(outcome)

sum(is.na(trial41))

best_model_extended = glm(outcome ~ ., data = trial41, family = 'binomial')

summary(best_model_extended)$coefficients %>% 
  as.data.frame() %>% 
  mutate(oddsRatio = exp(Estimate)) %>%
  slice(-1) %>% 
  rownames_to_column(var = 'Variable') %>%
  arrange(desc(oddsRatio)) #%>% 
  #write.csv('D:/F2023/AOU/aou_processed_data/oddsRatio4.csv', row.names = FALSE)

summary(best_model_extended)
writeLines(capture.output(summary(best_model_extended)), "D:/F2023/AOU/aou_processed_data/model4_summary.txt")


# McFadden's Pseudo-R-squared
null_model4 = glm(outcome ~ 1, data = trial41, family = 'binomial')
ll_full4 = logLik(best_model_extended)
ll_null4 = logLik(null_model4)
mcfadden4_r2 = 1 - (as.numeric(ll_full4) / as.numeric(ll_null4)) #
mcfadden4_r2 #0.18