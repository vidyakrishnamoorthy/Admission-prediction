Grad_Admission <- read.csv("datasets/grad admission/Admission_Predict.csv")
cor(Grad_Admission, use = "complete.obs")
summary(Grad_Admission)
sapply(Grad_Admission, class)
names(Grad_Admission)
nrow(Grad_Admission)
sapply(Grad_Admission, is.na)

sapply(Grad_Admission, summary)

Grad_Admission$University.Rating[Grad_Admission$GRE.Score < 305 & Grad_Admission$University.Rating %in% c(4,5)]
Grad_Admission[Grad_Admission$GRE.Score < 305 & Grad_Admission$University.Rating %in% c(4,5),]
options(digits = 2)

boxplot(Grad_Admission)

summary(is.na(Grad_Admission))

sort(Grad_Admission$GRE.Score[Grad_Admission$University.Rating == 5])

library(psych)
pairs.panels(data.frame(Grad_Admission$GRE.Score, Grad_Admission$TOEFL.Score,
             Grad_Admission$SOP, Grad_Admission$LOR,
             Grad_Admission$CGPA, Grad_Admission$Chance.of.Admit))

Grad_Admission[Grad_Admission$Chance.of.Admit == 0.34,]

boxplot(Grad_Admission$CGPA)
boxplot(Grad_Admission$SOP)
boxplot(Grad_Admission$LOR)
boxplot(Grad_Admission$GRE.Score)
boxplot(Grad_Admission$Chance.of.Admit)
boxplot(Grad_Admission$SOP)
Grad_Admission[(Grad_Admission$CGPA < 7 
                | Grad_Admission$LOR < 1.5
                | Grad_Admission$Chance.of.Admit<0.4),]
outlier(Grad_Admission, plot=TRUE)

Grad.model <- lm(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                   University.Rating + SOP + LOR + CGPA + Research, 
                 data = Grad_Admission)
summary(Grad.model)

summary(Grad_Admission$GRE.Score)
sd(Grad_Admission$GRE.Score)
sd(Grad_Admission$GRE.Score) * 1.80e-03

summary(Grad_Admission$TOEFL.Score)
sd(Grad_Admission$TOEFL.Score)
sd(Grad_Admission$TOEFL.Score) * 3.68e-03

summary(Grad_Admission$University.Rating)
sd(Grad_Admission$University.Rating)
sd(Grad_Admission$University.Rating) * 8.78e-03

summary(Grad_Admission$SOP)
sd(Grad_Admission$SOP)
sd(Grad_Admission$SOP) * 9.94e-05

summary(Grad_Admission$LOR)
sd(Grad_Admission$LOR)
sd(Grad_Admission$LOR) * 2.15e-02

summary(Grad_Admission$CGPA)
sd(Grad_Admission$CGPA)
sd(Grad_Admission$CGPA) * 1.05e-01

summary(Grad_Admission$Research)
sd(Grad_Admission$Research)
sd(Grad_Admission$Research) * 2.44e-02


# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       -1.259432   0.124731  -10.10  < 2e-16 ***
#   GRE.Score          0.001737   0.000598    2.91   0.0039 ** 
#   TOEFL.Score        0.002920   0.001090    2.68   0.0077 ** 
#   University.Rating  0.005717   0.004770    1.20   0.2315    
#   SOP               -0.003305   0.005562   -0.59   0.5527    
#   LOR                0.022353   0.005541    4.03  6.6e-05 ***
#   CGPA               0.118939   0.012219    9.73  < 2e-16 ***
#   Research           0.024525   0.007960    3.08   0.0022 **

# SD * coeff:
#     Column name: Standard deviation -> Standard deviation * Coefficient
#     GRE.Score: 11.5 -> 0.0207
#     TOEFL.Score: 6.07 -> 0.0223
#     University.Rating: 1.14 -> 0.01
#     SOP: 1.01 -> 1e-04
#     LOR: 0.898 -> 0.0193
#     CGPA: 0.596 -> 0.0626
#     Research: 0.498 -> 0.0122


Grad_Admission.uni.rank.high <- Grad_Admission[Grad_Admission$University.Rating > 3, ]
nrow(Grad_Admission.uni.rank.high)
Grad.model.rank.high <- lm(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                             SOP + LOR + CGPA + Research, 
                           data = Grad_Admission.uni.rank.high)
summary(Grad.model.rank.high)
step.model.grad <- step(Grad.model.rank.high)
summary(step.model.grad)

summary(Grad_Admission.uni.rank.high$GRE.Score)
sd(Grad_Admission.uni.rank.high$GRE.Score)
sd(Grad_Admission.uni.rank.high$GRE.Score) * 0.001164

summary(Grad_Admission.uni.rank.high$TOEFL.Score)
sd(Grad_Admission.uni.rank.high$TOEFL.Score)
sd(Grad_Admission.uni.rank.high$TOEFL.Score) * 0.005052

summary(Grad_Admission.uni.rank.high$SOP)
sd(Grad_Admission.uni.rank.high$SOP)
sd(Grad_Admission.uni.rank.high$SOP) * 0.029530

summary(Grad_Admission.uni.rank.high$LOR)
sd(Grad_Admission.uni.rank.high$LOR)
sd(Grad_Admission.uni.rank.high$LOR) * 0.013458

summary(Grad_Admission.uni.rank.high$CGPA)
sd(Grad_Admission.uni.rank.high$CGPA)
sd(Grad_Admission.uni.rank.high$CGPA) * 0.091757

summary(Grad_Admission.uni.rank.high$Research)
sd(Grad_Admission.uni.rank.high$Research)
sd(Grad_Admission.uni.rank.high$Research) * 0.047482

# SD * coeff:
#     Column name: Standard deviation -> Standard deviation * Coefficient
#     GRE.Score: 9.45 -> 0.011
#     TOEFL.Score: 4.44 -> 0.0224
#     SOP: 0.689 -> 0.0204
#     LOR: 0.688 -> 0.00925
#     CGPA: 0.436 -> 0.04
#     Research: 0.378 -> 0.018

###################################################################

library(dplyr)
Grad_Admission.avg.cgpa.un4 <- filter(Grad_Admission[Grad_Admission$University.Rating == 4,],between(CGPA, 7, 9))
nrow(Grad_Admission.avg.cgpa.un4)
Grad_Admission.avg.cgpa.un4
Grad.model.avg.cgpa.uni4 <- lm(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                             SOP + Research, data = Grad_Admission.avg.cgpa.un4)
summary(Grad.model.avg.cgpa.uni4)
sd(Grad_Admission.avg.cgpa.un4$GRE.Score) * 0.000549
sd(Grad_Admission.avg.cgpa.un4$TOEFL.Score) * 0.008010
sd(Grad_Admission.avg.cgpa.un4$SOP) * 0.062097
sd(Grad_Admission.avg.cgpa.un4$Research) * 0.071003

Grad_Admission[Grad_Admission$University.Rating == 5 &
                 Grad_Admission$CGPA %in% seq(from = 7, to = 9),]
# SD * coeff:
#     Column name: Standard deviation -> Standard deviation * Coefficient
#     GRE.Score: 9.45 -> 0.011
#     TOEFL.Score: 4.44 -> 0.0224
#     SOP: 0.689 -> 0.0204
#     LOR: 0.688 -> 0.00925
#     CGPA: 0.436 -> 0.04
#     Research: 0.378 -> 0.018

###################################################################

library(DAAG)
Grad_Admission.cvlm  <- CVlm(Grad_Admission, 
                             formula(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                                       University.Rating + SOP + LOR + CGPA + Research, 
                                     data = Grad_Admission), 25)
Grad_Admission.cvlm
sqrt(mean((Grad_Admission.cvlm$Chance.of.Admit - Grad_Admission.cvlm$cvpred)^2))
Grad_Admission.lm  <- lm(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                           University.Rating + SOP + LOR + CGPA + Research, 
                         data = Grad_Admission)
summary(Grad_Admission.lm)

summary(Grad_Admission[Grad_Admission$University.Rating == 5, ])


####################################################################

grad.full.model <- lm(Chance.of.Admit ~  GRE.Score + TOEFL.Score + 
                   University.Rating + SOP + LOR + CGPA + Research, 
                 data = Grad_Admission)
grad.step.model <- step(grad.full.model)
summary(grad.step.model)

