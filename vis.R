# Descriptive
tabIncome <- summary(data$Income)

tabHSE <- table(data$Health_HSE)
tabHRS <- table(data$Health_HRS)  


# Model summaries
sumM1 <- summary(fit_1)

sumM2 <- summary(fit_2)
anvM12 <- anova(fit_1, fit_2)

sumM5 <- summary(fit_5)
anvM25 <- anova(fit_2, fit_5)

sumM6 <- summary(fit_6)
anvM56 <- anova(fit_5, fit_6)

sumM7 <- summary(fit_7)
anvM67 <- anova(fit_6, fit_7)

sumM8 <- summary(fit_8)
anvM78 <- anova(fit_7, fit_8)


# Assumption checking
graphHealth <- hist(data$Health_HRS)

graphIncome1 <- plot(density(data$Income, na.rm=TRUE), 
     main="Density Plot of Income", 
     xlab="Income", 
     ylab="Density", 
     col="royalblue", 
     lwd=2, 
     xlim=c(min(data$Income), max(data$Income)))

graphIncome2 <- plot(density(data$Income, na.rm=TRUE), 
                     main="Density Plot of Income (Zoomed-In)", 
                     xlab="Income", 
                     ylab="Density", 
                     col="salmon", 
                     lwd=2, 
                     xlim=c(min(data$Income), 1500))





residuals <- lavResiduals(fit_8, type = "raw")
density(residuals)

residuals <- resid(fit_8, type='cor')$cov
plot(residuals)


# stem(residuals[lower.tri(residuals, diag=FALSE)])












