# Descriptive
tabAll <- as.data.frame(apply(data, 2, summary))[,c(4,7)]
tabHRS <- as.data.frame(table(data$Health_HRS))  
colnames(tabHRS) <- c("Response", "Freq")

# Model summaries
sumM1 <- summary(fit_1)

sumM2 <- summary(fit_2)
anvM12 <- anova(fit_1, fit_2)
anvM23 <- anova(fit_2, fit_3)
anvM24 <- anova(fit_2, fit_4)

sumM5 <- summary(fit_5)
anvM25 <- anova(fit_2, fit_5)

sumM6 <- summary(fit_6)
anvM56 <- anova(fit_5, fit_6)

sumM7 <- summary(fit_7)
anvM67 <- anova(fit_6, fit_7)

sumM8 <- summary(fit_8)
anvM78 <- anova(fit_7, fit_8)







