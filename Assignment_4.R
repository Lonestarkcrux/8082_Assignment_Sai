### libraries
library(data.table)
library(ggplot2)
library(AER)
library(gglm)
library(car)
library(patchwork)
library(MASS)

### load data
data("EquationCitations", package = "AER")
EquationCitations <- as.data.table(EquationCitations)


#for self citations
#additive model
m_self_a <-glm.nb(selfcites~journal+log(mainequations+1) + log(pages),data= EquationCitations)
#two way interaction
m_self_b <-glm.nb(selfcites~journal*log(pages)+journal*log(mainequations+1),data=EquationCitations)
#3 way interaction
m_self_c<-glm.nb(selfcites~journal*log(pages)*log(mainequations+1),data=EquationCitations)
anova(m_self_a,m_self_b,m_self_c,test="Chisq")
summary(m_self_b)
summary(m_self_c)

#similarly for other citations
m_other_a <- glm.nb(
  othercites ~ journal + log(pages) + log(mainequations + 1),
  data = EquationCitations
)

m_other_b <- glm.nb(
  othercites ~ journal * log(pages) +
    journal * log(mainequations + 1),
  data = EquationCitations
)

m_other_c <- glm.nb(
  othercites ~ journal * log(pages) * log(mainequations + 1),
  data = EquationCitations
)

anova(m_other_a, m_other_b, m_other_c, test = "Chisq")

#figures
pA <- ggplot(EquationCitations, aes(x = log(pages), y = selfcites, color = journal)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "glm.nb", se = FALSE) +
  theme_bw() +
  labs(
    title = "Self-citations vs paper length",
    x = "log(pages)",
    y = "selfcites"
  )

# Self-cites vs equations
pB <- ggplot(EquationCitations, aes(x = log(mainequations + 1), y = selfcites, color = journal)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "glm.nb", se = FALSE) +
  theme_bw() +
  labs(
    title = "Self-citations vs equation use",
    x = "log(mainequations + 1)",
    y = "selfcites"
  )

# Other-cites vs pages
pC <- ggplot(EquationCitations, aes(x = log(pages), y = othercites, color = journal)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "glm.nb", se = FALSE) +
  theme_bw() +
  labs(
    title = "Other-citations vs paper length",
    x = "log(pages)",
    y = "othercites"
  )

# Other-cites vs equations
pD <- ggplot(EquationCitations, aes(x = log(mainequations + 1), y = othercites, color = journal)) +
  geom_point(alpha = 0.25, size = 1) +
  geom_smooth(method = "glm.nb", se = FALSE) +
  theme_bw() +
  labs(
    title = "Other-citations vs equation use",
    x = "log(mainequations + 1)",
    y = "othercites"
  )

# Combine
(pA | pB) / (pC | pD) + plot_annotation(tag_levels = "A")

