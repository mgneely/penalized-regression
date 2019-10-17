library(glmnet)
library(foreach)
library(pROC)
library(ggplot2)
library(ggfortify)
library(ggpubr)
library(gridExtra)
library(tidyverse)

# read in the state medicaid data
dataset <- read_csv("...\medicaid_data.csv")

# store some things
d <- dataset
f <- oppose_expansion ~ gop_governor + percent_favorable_aca + gop_leg + bal2012 + multiplier + 
                        percent_nonwhite + percent_uninsured + percent_metro + percent_poverty 
y <- d$...
X <- model.matrix(f, d)
n <- ...(y)

## LASSO WITH ALPHA = 1
# CV to find optimal lambda
... <- ...(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 1)
plot(cv1) # viz cv

# Fit the model
lassomod <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv1$..., 
                   alpha = 1)

# Store fitted values across multiple values of lambda
cv1.glmnet.fit <- (cv1$glmnet.fit)
plot(cv1.glmnet.fit)

## RIDGE WITH ALPHA = 0
# CV to find optimal lambda
cv2 <- ...(X, y, 
                 family = "binomial", 
                 nfold = 10, 
                 alpha = 0)
plot(cv2)

# Fit the model
ridgemod <- glmnet(X, y, 
                   family = "binomial", 
                   lambda = cv2$..., 
                   alpha = ...)

# Store the fitted values for plotting
cv2.glmnet.fit <- (cv2$glmnet.fit)
plot(cv2.glmnet.fit)

## ELASTIC NET WITH 0 < ALPHA < 1
# CV for search for alpha values (ranging between 0 and 1, for mixing of L1 and L2)
doParallel::registerDoParallel(cores=2)
a <- seq(0.1, 0.9, 0.05) # create a basic sequence to constrain the search
search <- foreach(i = a, .combine = rbind) %dopar% {
  # calcluate multiple lambda values at all alpha values to find the optimal mix between L1 and L2
  cv <- cv.glmnet(X, y, 
                  family = "binomial",
                  nfold = 10, 
                  alpha = i)
  # Store it in a data frame to search next
  data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
}

# Grab the alpha value for the optimal lambda value that minimizes MSE 
cv3 <- search[search$cvm == ...(search$cvm), ]

# Fit the model
elasticnetmod <- glmnet(X, y, 
                        family = "binomial", 
                        lambda = cv3$lambda.min, 
                        alpha = cv3$alpha)

# store values across all models
cv3.1 <- cv.glmnet(X, y, 
                   family = "binomial", 
                   nfold = 10,
                   alpha = 0.85)
...(cv3.1)# quick visual

cv3.glmnet.fit <- (cv3.1$glmnet.fit)
plot(cv3.glmnet.fit)

# (Nicer) plot of the output
lassoplot <- autoplot(cv1$glmnet.fit, "lambda", label = TRUE, main = "LASSO (alpha = 1)") + 
  ylim(-5,5) + 
  theme(legend.position="right") + 
  scale_colour_discrete(name = "Variables", 
                        labels = c("Intercept", "GOP Governor", "% Favorable ACA", "GOP Legislature", "2012 Ballot", "Multiplier", "% Nonwhite", "% Uninsured", "% Metropolitan", "% Poverty")) + 
  theme(legend.title = element_text(size=20)) + 
  theme(legend.text = element_text(size = 18)) + 
  geom_vline(data = NULL, 
             xintercept = log(cv1$lambda.min), 
             na.rm = FALSE, show.legend = TRUE)

ridgeplot <- autoplot(cv2$glmnet.fit, "lambda", label = TRUE, main = "Ridge (alpha = 0)") + 
  ylim(-2,2) + 
  xlim(-4,4) + 
  geom_vline(data = NULL, 
             xintercept = log(cv2$lambda.min), 
             na.rm = FALSE, 
             show.legend = TRUE)

elasticnetplot <- autoplot(cv3.1$glmnet.fit, "lambda", label = TRUE, main = "Elastic Net (alpha = 0.85)") + 
  ylim(-5,5) + 
  geom_vline(data = NULL, 
             xintercept = log(cv3$lambda.min), 
             na.rm = FALSE, 
             show.legend = TRUE)

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

mylegend <- g_legend(lassoplot)


figure <- ...(arrangeGrob(ridgeplot + 
                                     theme_bw() +
                                     theme(legend.position="none"),
                                   lassoplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   elasticnetplot + 
                                     theme_bw() +
                                     theme(legend.position="none"), 
                                   mylegend, nrow = 1))
