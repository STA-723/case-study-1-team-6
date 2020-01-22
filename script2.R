library(readr)
library(tidyverse)
library(corrr)
library(e1071)
library(caret)

data = readRDS("Longnecker.rds") %>% 
  dplyr::select(-albumin) %>% 
  filter(gestational_age < 55) %>% 
  mutate(smoking_status = factor(ifelse(smoking_status, "smoking", "non-smoking")),
         preterm = gestational_age < 37,
         race = factor(ifelse(race=="white", "white", "other")),
         center = factor(center),
         n_col_NA = rowSums(cbind(is.na(score_education), 
                                  is.na(score_income), 
                                  is.na(score_occupation)))
  )

# Removing observation with NA PCBs
data = data[-1857, ]

PCB_vars = c("pcb_028", "pcb_052", "pcb_074", "pcb_105", "pcb_118", "pcb_153", "pcb_170", "pcb_138", "pcb_180", "pcb_194", "pcb_203")

PCBs = scale(as.matrix(data[, PCB_vars]), scale=T)

PCB_summary = PCBs %*% princomp(PCBs)$loadings[,1, drop=F]
data = data %>% mutate(PCB_summary = PCB_summary)

# Proportion of NA in the albumin variable.
mean(is.na(readRDS("Longnecker.rds")$albumin)) %>% round(2)

# Proportion of NA values among score_income, score_education and score_occupation.
mean(is.na(
  data %>%
    select(score_income, score_education, score_occupation)
)) %>% round(2)

# Proportion of NA values elsewhere
mean(is.na(
  data %>%
    select(-score_income, -score_education, -score_occupation)
))


my_function <- function(values) {
  case_when(
    is.na(values) ~ "M",
    !is.na(values) ~ "O"
  )
}





#####################################################################################

data %>% select(starts_with("score")) %>% gather("score","value") %>% ggplot(aes(x=value)) + geom_histogram() + facet_wrap(~score)

# Select score variables for imputation
Y <- data %>% select(score_education,score_income,score_occupation) 

# Select other predictor variables. Also turn factor variables into numeric
X <- data %>% select(-c(starts_with("pcb_0"),starts_with("pcb_1"),pcb_203,gestational_age,starts_with("score"),preterm,n_col_NA))
levels(X$center) <- c(0,1,2,3,4,5,6,7,8,9,10,11)
X$center <- as.numeric(X$center)-1
X$smoking_status <- as.numeric(X$smoking_status)-1
X$race <- as.numeric(X$race)-1

# All complete cases in Y
mis_ind <- !complete.cases(Y)

# Varaibles to run Stan file
N_mis <- sum(mis_ind == TRUE)
N_obs <- nrow(Y) - N_mis
y_obs <- Y[!mis_ind,]
x_obs <- X[!mis_ind,]
x_mis <- X[mis_ind,]

# Data list for Stan file
data_list_score_education <- list(K=ncol(x_mis),N_obs=N_obs,N_mis=N_mis,y_obs=y_obs[,1],x_obs=x_obs,x_mis=x_mis)
data_list_score_income <- list(K=ncol(x_mis),N_obs=N_obs,N_mis=N_mis,y_obs=y_obs[,2],x_obs=x_obs,x_mis=x_mis)
data_list_score_occupation <- list(K=ncol(x_mis),N_obs=N_obs,N_mis=N_mis,y_obs=y_obs[,3],x_obs=x_obs,x_mis=x_mis)

# Imputed values are simulated one at a time.
# That is, the imputed score variables are treated as independent normals
model_score_education <- stan(file="Imputation.stan",data=data_list_score_education,chains=1,iter=2000) %>% 
  summary(pars="y_mis")
model_score_income <- stan(file="Imputation.stan",data=data_list_score_income,chains=1,iter=2000) %>% 
  summary(pars="y_mis")
model_score_occupation <- stan(file="Imputation.stan",data=data_list_score_occupation,chains=1,iter=2000) %>% 
  summary(pars="y_mis")

# Extracts Stan output
imputed_score_education <- model_score_education$summary[,1]
imputed_score_income <- model_score_income$summary[,1]
imputed_score_occupation <- model_score_occupation$summary[,1]

# The three chunks below replace NA values with imputed values
score_education_imputations <- Y[,1]
score_education_imputations[mis_ind] <- imputed_score_education
score_education_imputations[!is.na(Y[,1])] <- Y[!is.na(Y[,1]),1]

score_income_imputations <- Y[,2]
score_income_imputations[mis_ind] <- imputed_score_income
score_income_imputations[!is.na(Y[,2])] <- Y[!is.na(Y[,2]),2]

score_occupation_imputations <- Y[,3]
score_occupation_imputations[mis_ind] <- imputed_score_occupation
score_occupation_imputations[!is.na(Y[,3])] <- Y[!is.na(Y[,3]),3]

# Creates new variables in the original dataset
data_i <- data %>% mutate(score_education_i=score_education_imputations,
                                         score_income_i=score_income_imputations,
                                         score_occupation_i=score_occupation_imputations)
# Writes the new dataset to csv with imputed score variables
write.csv(data_i, file = "data_i.csv")


# Compare the two distributions
data_i <- read.csv("data_i.csv")

data_i <- data_i %>% mutate(score_income_encoder = my_function(score_income),
                            score_occupation_encoder = my_function(score_occupation),
                            score_education_encoder = my_function(score_education))

data_i %>% select(score_education_encoder,score_education_i) %>% 
  ggplot(aes(x=score_education_i,color=score_education_encoder)) + geom_histogram() 

data_i %>% select(starts_with("score")) %>% gather("score","value") %>% ggplot(aes(x=value)) + geom_histogram() + facet_wrap(~score)


data <- data_i %>% select(-c("score_education","score_income","score_occupation"))



# Correlation between dde and pcb summary holding other variables constant
df <- data %>% 
  select(-c(gestational_age,preterm,n_col_NA,starts_with("pcb_1"),starts_with("pcb_0"),starts_with("pcb_2")))  

# Treat gestational_age as continuous for now
response <- data %>% select(gestational_age)

# Colinearity after extracting linear dependence of other variables
lm(cbind(dde,PCB_summary) ~. ,data=df)  %>% residuals() %>% cor()

# Correlation with response and the residuals
residuals_response <- lm(cbind(dde,PCB_summary) ~. ,data=df)  %>% residuals() %>% cbind(.,response) 

cor.test(residuals_response[,1],residuals_response[,3])
cor.test(residuals_response[,2],residuals_response[,3])

# Same analysis except extracting non-linear dependence using SVM
df_1 <- df %>% select(-PCB_summary)
df_2 <- df %>% select(-dde)

res1 <- svm(dde ~., data=df_1) %>% residuals()
res2 <- svm(PCB_summary~.,data=df_2) %>% residuals()

cor(cbind(response,res1,res2))
















