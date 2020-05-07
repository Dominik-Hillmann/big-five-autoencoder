# Factor Analysis Big Five Personality Factors
personality.survey <- read.csv(
    'C:\\Users\\Dominik USER\\repositories\\big-five-autoencoder\\data\\data-final.csv',
    sep = '\t'
)
# Get only the question values. no meta information needed here.
# Also, make it numeric.
personality.survey <- personality.survey[, 1:50]
personality.survey <- sapply(personality.survey, as.numeric)
personality.survey <- na.omit(personality.survey)

question.cors <- cor(personality.survey)
eigen.personality <- eigen(question.cors)
personality.E <- eigen.personality$vectors
personality.Lambda <- eigen.personality$values

print(personality.Lambda) # Already ordered by loading strength.

loadings = cbind(
    sqrt(personality.Lambda[1]) * personality.E[, 1],
    sqrt(personality.Lambda[2]) * personality.E[, 2], 
    sqrt(personality.Lambda[3]) * personality.E[, 3], 
    sqrt(personality.Lambda[4]) * personality.E[, 4], 
    sqrt(personality.Lambda[5]) * personality.E[, 5]
)

# Original dimensions most important for factor 1 -> Extroversion
rev(rownames(loadings[order(abs(loadings[, 1])), ]))

rev(rownames(loadings[order(abs(loadings[, 2])), ]))
rev(rownames(loadings[order(abs(loadings[, 3])), ]))
rev(rownames(loadings[order(abs(loadings[, 4])), ]))
rev(rownames(loadings[order(abs(loadings[, 5])), ]))

#####

loadings = data.frame(loadings)
colnames(loadings) = c('Factor 1', 'Factor 2', 'Factor 3', 'Factor 4', 'Factor 5')
rownames(loadings) = colnames(personality.survey)

prop.lambda.1 = Lambda[1] / sum(Lambda)
prop.lambda.2 = prop.lambda.1 + (Lambda[2] / sum(Lambda))
communalities = (loadings[,1] ** 2) + (loadings[,2] ** 2)
spec.var = diag(R) - communalities

# Faktorladungen f?r m == 2
round(loadings, 4)
# Anteil an erkl?rter Varianz f?r Faktor 1,2 von links nach rechts
round(c(prop.lambda.1, prop.lambda.2), 4)
# die Kommunalit?ten
round(communalities, 4)
# und die spezifischen Varianzen
round(spec.var, 4)