
# ## Load libraries
# Instructions: 
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(purrr)
library(httpgd)
library(cowplot)
library(ISLR2)
library(stringr)
library(corrplot)

## ## You can use this function to generate a formula from a vector of 
# Instructions: variables
make_formula <- function(x){
  as.formula(
    paste("quality ~ ", paste(x, collapse = " + "))
  )
}
make_formula(c("a", "b", "c"))

## ## You can use this function to generate a model matrix for glmnet()
# Instructions: 
make_model_matrix <- function(formula){
  X <- model.matrix(formula, df)[, -1]
  cnames <- colnames(X)
  for(i in 1:ncol(X)){
    if(!cnames[i] == "typewhite"){
      X[, i] <- scale(X[, i])
    } else {
      colnames(X)[i] <- "type"
    }
  }
  return(X)
}




#! Question 1

# ## Question 1.1
# Instructions: 
url1 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
url2 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"

df1 <- read_delim(url1, delim=";")
df2 <- read_delim(url2, delim=";")

# ## Question 1.2
# Instructions: 
df <- bind_rows(
  df1 %>% mutate(type="white"), 
  df2 %>% mutate(type="red")
) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  select(-c(fixed_acidity, free_sulfur_dioxide)) %>% 
  mutate(type = as.factor(type)) %>% 
  drop_na()

# ## Question 1.3
# Instructions: 
df_summary <- df %>% 
  group_by(type) %>% 
  summarize(mean = mean(quality), sd = sd(quality), n=length(quality))

diff_mean <- df_summary$mean %>% diff()
sp <- sum(df_summary$sd^2 * (df_summary$n-1))  / sum(df_summary$n - 2)

t1 <- diff_mean / (sqrt(sp) * sqrt(1/(nrow(df1)) + 1/(nrow(df2))))

# ## Question 1.4
# Instructions: 
t_test <- t.test(
  df %>% filter(type == "white") %>% select(quality),
  df %>% filter(type == "red")   %>% select(quality),
  var.equal=T
)

t2 <- t_test$statistic


# ## Question 1.5
# Instructions: 
cat_model <- lm(quality ~ type, df)
t3 <- coef(summary(cat_model))[, "t value"][2]

# ## Question 1.6
# Instructions: 
c(t1, t2, t3)


#! Question 2

# ## Question 2.1
# Instructions: 
full_model <- lm(quality ~ ., df)
summary(full_model)


# ## Question 2.2
# Instructions: 
model_citric <- lm(quality ~ citric_acid, df)
summary(model_citric)

model_sulfur <- lm(quality ~ total_sulfur_dioxide, df)
summary(model_sulfur)

# ## Question 2.3
# Instructions: 
df %>% 
  select(where(is.numeric)) %>% 
  cor() %>% 
  round(digits=2) %>% 
  corrplot(diag=F)

# ## Question 2.4
# Instructions: 
library(car)
vif(full_model)



#! Question 3

null_model <- lm(quality ~ ., df)
full_model <- lm(quality ~ ., df)


# ## Question 3.1
# Instructions: 
backward_model <- step(full_model, direction="backward")
backward_formula <- formula(backward_model)

# ## Question 3.2
# Instructions: 
forward_model <- step(null_model, direction="forward")
forward_formula <- formula(forward_model)

# ## Question 3.3
# Instructions: 
library(glmnet)
# X <- model.matrix(full_model, df)[, -1]

# ## Question 3.4
# Instructions: 
y <- df$quality
X <- make_model_matrix(full_model)
lasso <- cv.glmnet(X, y, alpha=1)
ridge <- cv.glmnet(X, y, alpha=0)

# ## Question 3.5
# Instructions: 
par(mfrow=c(1, 2))
plot(ridge, main="Ridge")
plot(lasso, main="LASSO")

# ## Question 3.6
# Instructions: 
lasso_coef <- coef(lasso, s="lambda.1se")
lasso_vars <- rownames(lasso_coef)[which(abs(lasso_coef) > 0)][-1]
lasso_formula <- make_formula(lasso_vars)

# ## Question 3.7
# Instructions: 
ridge_coef <- coef(ridge, s="lambda.1se")
ridge_vars <- rownames(ridge_coef)[which(abs(ridge_coef) > 0)][-1]
ridge_formula <- make_formula(ridge_vars)


#! Question 4

# ## Question 4.1
# Instructions: 
library(caret)
x_vars <- colnames(df %>% select(-quality))

# ## Question 4.2
# Instructions: 
formulas <- map(
  1:length(x_vars),
  \(x){
    vars <- combn(x_vars, x, simplify=F)
    map(vars, make_formula)
  }
) %>% unlist()


sample(formulas, 4) %>% unlist() %>% as.character()

# ## Question 4.3
# Instructions: 
models <- map(formulas, \(x) lm(x, df))
summaries <- map(models, \(x) broom::glance(x)) %>% bind_rows()

# ## Question 4.4
# Instructions: 
Rsq <- summaries$adj.r.squared
rsq_formula <- formulas[[which(Rsq == max(Rsq))]]

# ## Question 4.5
# Instructions: 
aic <- summaries$AIC
aic_formula <- formulas[[which(aic == min(aic))]]


# ## Question 4.6
# Instructions: 

null_formula <- formula(null_model)
full_formula <- formula(full_model)

final_formulas <- c(
  null_formula,
  full_formula,
  rsq_formula,
  aic_formula,
  backward_formula,
  forward_formula,
  lasso_formula, 
  ridge_formula
)

# ## Question 4.7
# Instructions: 
summary_table <- map(
  final_formulas, 
  \(x) lm(x, df) %>%
    broom::glance() %>% 
    select(c(sigma, adj.r.squared, AIC, df, p.value))
) %>% bind_rows()

summary_table