#Assignment 5
#Is there evidence for a genotype-environment interaction for ovariole number?
#What is the correct distribution to use (hint, you mind need to use `glmer`)?
#In your submission, include write out your final model and report the statistical tests and statistics (e.g., Chisq, p-value, degrees of freedom) that you used to determine if there is a genotype-environment interaction. Make a figure showing the reaction norms for ovariole number for each genotype.
#Submit your analysis as a single PDF file.

library(data.table)
library(lme4)
library(ggplot2)
clinal <- fread(
  "https://raw.githubusercontent.com/BIOL8082/Stats_Spring2026/refs/heads/main/Class05_modelSelection_mixedEffects/clinal_flies.csv"
)

m_NoGE <- glmer(ovn~food + (1|geno) + (1|vial), data = clinal, family = poisson(link = "log"))

m_GE <- glmer(ovn~food + (food|geno)+ (1|vial), data = clinal, family = poisson(link = "log"))
#executing m_GE gave me "boundary (singular) fit: see help ("isSingular)
#isSingular(m_GE) gave me True 
#VarCorr(m_GE) here geno food SD ~ 0, there is no meaningful genotype-environment interaction.
anova(m_NoGE, m_GE, test = "Chisq")

clinal[, pred := predict(m_GE, type = "response", re.form = NULL)]
pred_grid <- clinal[
  ,
  .(pred = mean(pred)),
  by = .(geno, food)
]

ggplot(pred_grid, aes(x = food, y = pred, group = geno)) +
  geom_line(alpha = 0.7) +
  labs(
    x = "Food type",
    y = "Predicted ovariole number",
    title = "Reaction norms for ovariole number by genotype"
  ) +
  theme_minimal()
