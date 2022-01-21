# grad LM
# full model lm
# step model lm
# lasso
# training, testing
Grad_Admission <- read.csv("datasets/grad admission/Admission_Predict.csv")
cor(Grad_Admission[-1])

Grad_Admission.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission)
full.step.model <- step(Grad_Admission.model)
summary(full.step.model)

Grad_Admission.uni1 <- Grad_Admission[Grad_Admission$University.Rating == 1, ]
Grad_Admission.uni2 <- Grad_Admission[Grad_Admission$University.Rating == 2, ]
Grad_Admission.uni3 <- Grad_Admission[Grad_Admission$University.Rating == 3, ]
Grad_Admission.uni4 <- Grad_Admission[Grad_Admission$University.Rating == 4, ]
Grad_Admission.uni5 <- Grad_Admission[Grad_Admission$University.Rating == 5, ]

#uni rating 1
Grad_Admission.uni1.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.uni1)
nrow(Grad_Admission.uni1) # 26
summary(Grad_Admission.uni1.model) # 0.8617
uni1.training.rows <- sample(1:nrow(Grad_Admission.uni1), size = 21)
uni1.training <- Grad_Admission.uni1[uni1.training.rows,]
uni1.testing <- Grad_Admission.uni1[-uni1.training.rows,]
uni1.full.predictions <- predict(Grad_Admission.uni1.model, newdata = uni1.testing)
sqrt(mean((uni1.full.predictions - uni1.testing$Chance.of.Admit)^2))
uni1.step.model <- step(Grad_Admission.uni1.model)
summary(uni1.step.model)
uni1.step.predictions <- predict(uni1.step.model, newdata = uni1.testing)
sqrt(mean((uni1.step.predictions - uni1.testing$Chance.of.Admit)^2))
uni1.lasso.model <- l1ce(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                           LOR + CGPA + Research, data = Grad_Admission.uni1, 
                         absolute.t = TRUE, bound = 0.1)
uni1.lasso.predictions <- predict(uni1.lasso.model, newdata = Grad_Admission.uni1)
sqrt(mean((uni1.lasso.predictions - Grad_Admission.uni1$Chance.of.Admit)^2))

library(dplyr)
Grad_Admission.avg.cgpa.un1 <- filter(Grad_Admission[Grad_Admission$University.Rating == 1,],
                                      between(CGPA, 7, 9))
Grad_Admission.avg.cgpa.un1.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.avg.cgpa.un1)
nrow(Grad_Admission.avg.cgpa.un1) # 24
summary(Grad_Admission.avg.cgpa.un1.model) # 0.8617
sd(Grad_Admission.avg.cgpa.un1$GRE.Score) * 0.0007226
sd(Grad_Admission.avg.cgpa.un1$TOEFL.Score) * 0.0046793
sd(Grad_Admission.avg.cgpa.un1$SOP) * 0.0029669
sd(Grad_Admission.avg.cgpa.un1$LOR) * 0.0667733
sd(Grad_Admission.avg.cgpa.un1$CGPA) * 0.0596190
sd(Grad_Admission.avg.cgpa.un1$Research) * 0.0324216

#uni rating 2
Grad_Admission.uni2.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.uni2)
nrow(Grad_Admission.uni2) # 107
summary(Grad_Admission.uni2.model) # 0.08007
uni2.training.rows <- sample(1:nrow(Grad_Admission.uni2), size = 97)
uni2.training <- Grad_Admission.uni2[uni2.training.rows,]
uni2.testing <- Grad_Admission.uni2[-uni2.training.rows,]
uni2.full.predictions <- predict(Grad_Admission.uni2.model, newdata = uni2.testing)
sqrt(mean((uni2.full.predictions - uni2.testing$Chance.of.Admit)^2))
uni2.step.model <- step(Grad_Admission.uni2.model)
summary(uni2.step.model)
uni2.step.predictions <- predict(uni2.step.model, newdata = uni2.testing)
sqrt(mean((uni2.step.predictions - uni2.testing$Chance.of.Admit)^2))
uni2.lasso.model <- l1ce(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                           LOR + CGPA + Research, data = Grad_Admission.uni2, 
                         absolute.t = TRUE, bound = 2)
summary(uni2.lasso.model)
uni2.lasso.predictions <- predict(uni2.lasso.model, newdata = Grad_Admission.uni2)
sqrt(mean((uni2.lasso.predictions - Grad_Admission.uni2$Chance.of.Admit)^2))

Grad_Admission.avg.cgpa.un2 <- filter(Grad_Admission[Grad_Admission$University.Rating == 2,],
                                      between(CGPA, 7, 9))
Grad_Admission.avg.cgpa.un2.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                          LOR + CGPA + Research, data = Grad_Admission.avg.cgpa.un2)
nrow(Grad_Admission.avg.cgpa.un2) # 104
summary(Grad_Admission.avg.cgpa.un2.model) # 0.8617
sd(Grad_Admission.avg.cgpa.un2$GRE.Score) * 0.0007877
sd(Grad_Admission.avg.cgpa.un2$TOEFL.Score) * 0.0055233
sd(Grad_Admission.avg.cgpa.un2$SOP) * 0.0198032
sd(Grad_Admission.avg.cgpa.un2$LOR) * 0.0187195
sd(Grad_Admission.avg.cgpa.un2$CGPA) * 0.1343241
sd(Grad_Admission.avg.cgpa.un2$Research) * 0.0083224

#uni rating 3
Grad_Admission.uni3.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.uni3)
nrow(Grad_Admission.uni3) # 133
summary(Grad_Admission.uni3.model) # 0.489
uni3.training.rows <- sample(1:nrow(Grad_Admission.uni3), size = 103)
uni3.training <- Grad_Admission.uni3[uni3.training.rows,]
uni3.testing <- Grad_Admission.uni3[-uni3.training.rows,]
uni3.full.predictions <- predict(Grad_Admission.uni3.model, newdata = uni3.testing)
sqrt(mean((uni3.full.predictions - uni3.testing$Chance.of.Admit)^2))
uni3.step.model <- step(Grad_Admission.uni3.model)
summary(uni3.step.model)
uni3.step.predictions <- predict(uni3.step.model, newdata = uni3.testing)
sqrt(mean((uni3.step.predictions - uni3.testing$Chance.of.Admit)^2))
uni3.lasso.model <- l1ce(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                           LOR + CGPA + Research, data = Grad_Admission.uni3, 
                         absolute.t = TRUE, bound = 0.1)
uni3.lasso.predictions <- predict(uni3.lasso.model, newdata = Grad_Admission.uni3)
sqrt(mean((uni3.lasso.predictions - Grad_Admission.uni3$Chance.of.Admit)^2))

Grad_Admission.avg.cgpa.un3 <- filter(Grad_Admission[Grad_Admission$University.Rating == 3,],
                                      between(CGPA, 7, 9))
Grad_Admission.avg.cgpa.un3.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                          LOR + CGPA + Research, data = Grad_Admission.avg.cgpa.un3)
nrow(Grad_Admission.avg.cgpa.un3) # 117
summary(Grad_Admission.avg.cgpa.un3.model)
sd(Grad_Admission.avg.cgpa.un3$GRE.Score) * 0.002006
sd(Grad_Admission.avg.cgpa.un3$TOEFL.Score) * 0.001168
sd(Grad_Admission.avg.cgpa.un3$SOP) * 0.016053
sd(Grad_Admission.avg.cgpa.un3$LOR) * 0.025265
sd(Grad_Admission.avg.cgpa.un3$CGPA) * 0.125693
sd(Grad_Admission.avg.cgpa.un3$Research) * 0.027525

#uni rating 4
Grad_Admission.uni4.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.uni4)
nrow(Grad_Admission.uni4) # 74
summary(Grad_Admission.uni4.model) # 0.8296
uni4.training.rows <- sample(1:nrow(Grad_Admission.uni4), size = 64)
uni4.training <- Grad_Admission.uni4[uni4.training.rows,]
uni4.testing <- Grad_Admission.uni4[-uni4.training.rows,]
uni4.full.predictions <- predict(Grad_Admission.uni4.model, newdata = uni4.testing)
sqrt(mean((uni4.full.predictions - uni4.testing$Chance.of.Admit)^2))
uni4.step.model <- step(Grad_Admission.uni4.model)
summary(uni4.step.model)
uni4.step.predictions <- predict(uni4.step.model, newdata = uni4.testing)
sqrt(mean((uni4.step.predictions - uni4.testing$Chance.of.Admit)^2))
uni4.lasso.model <- l1ce(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                           LOR + CGPA + Research, data = Grad_Admission.uni4, 
                         absolute.t = TRUE, bound = 0.1)
uni4.lasso.predictions <- predict(uni4.lasso.model, newdata = Grad_Admission.uni4)
sqrt(mean((uni4.lasso.predictions - Grad_Admission.uni4$Chance.of.Admit)^2))

sd(Grad_Admission.uni4$TOEFL.Score) * 0.006132
sd(Grad_Admission.uni4$SOP) * 0.026110
sd(Grad_Admission.uni4$LOR) * 0.018456
sd(Grad_Admission.uni4$CGPA) * 0.085813
sd(Grad_Admission.uni4$Research) * 0.040222



Grad_Admission.avg.cgpa.un4 <- filter(Grad_Admission[Grad_Admission$University.Rating == 4,],
                                      between(CGPA, 7, 9))
Grad_Admission.avg.cgpa.un4.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                          LOR + CGPA + Research, data = Grad_Admission.avg.cgpa.un4)
nrow(Grad_Admission.avg.cgpa.un4) # 29
summary(Grad_Admission.avg.cgpa.un4.model)
sd(Grad_Admission.avg.cgpa.un4$GRE.Score) * 1.801e-05
sd(Grad_Admission.avg.cgpa.un4$TOEFL.Score) * 9.959e-03
sd(Grad_Admission.avg.cgpa.un4$SOP) * 3.621e-02
sd(Grad_Admission.avg.cgpa.un4$LOR) * 2.829e-02
sd(Grad_Admission.avg.cgpa.un4$CGPA) * 5.661e-02
sd(Grad_Admission.avg.cgpa.un4$Research) * 5.847e-02

#uni rating 5
Grad_Admission.uni5.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                  LOR + CGPA + Research, data = Grad_Admission.uni5)
nrow(Grad_Admission.uni5) # 60
summary(Grad_Admission.uni5.model) # 0.9153
uni5.training.rows <- sample(1:nrow(Grad_Admission.uni5), size = 55)
uni5.training <- Grad_Admission.uni5[uni5.training.rows,]
uni5.testing <- Grad_Admission.uni5[-uni5.training.rows,]
uni5.full.predictions <- predict(Grad_Admission.uni5.model, newdata = uni5.testing)
sqrt(mean((uni5.full.predictions - uni5.testing$Chance.of.Admit)^2))
uni5.step.model <- step(Grad_Admission.uni5.model)
summary(uni5.step.model)
uni5.step.predictions <- predict(uni5.step.model, newdata = uni5.testing)
sqrt(mean((uni5.step.predictions - uni5.testing$Chance.of.Admit)^2))
uni5.lasso.model <- l1ce(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                           LOR + CGPA + Research, data = Grad_Admission.uni5, 
                         absolute.t = TRUE, bound = 0.1)
uni5.lasso.predictions <- predict(uni5.lasso.model, newdata = Grad_Admission.uni5)
sqrt(mean((uni5.lasso.predictions - Grad_Admission.uni5$Chance.of.Admit)^2))

sd(Grad_Admission.uni5$TOEFL.Score) * 0.0039194
sd(Grad_Admission.uni5$SOP) * 0.0265184
sd(Grad_Admission.uni5$CGPA) * 0.0874598
sd(Grad_Admission.uni5$Research) * 0.0728405

Grad_Admission.avg.cgpa.un5 <- filter(Grad_Admission[Grad_Admission$University.Rating == 5,],
                                      between(CGPA, 7, 9))
Grad_Admission.avg.cgpa.un5.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                          LOR + CGPA + Research, data = Grad_Admission.avg.cgpa.un5)
nrow(Grad_Admission.avg.cgpa.un5) # 12
summary(Grad_Admission.avg.cgpa.un5.model)
sd(Grad_Admission.avg.cgpa.un5$GRE.Score) * 0.003081
sd(Grad_Admission.avg.cgpa.un5$TOEFL.Score) * 0.006793
sd(Grad_Admission.avg.cgpa.un5$SOP) * 0.038043
sd(Grad_Admission.avg.cgpa.un5$LOR) * 0.057887
sd(Grad_Admission.avg.cgpa.un5$CGPA) * 0.081062
sd(Grad_Admission.avg.cgpa.un5$Research) * 0.053514



# cook's distance
par(mfcol = c(3,2))
full.uni.cook <- cooks.distance(Grad_Admission.model)
plot(full.uni.cook, ylab = "Cook's distance", main = "All universities", 
     col = "lightsteelblue", type = "b")

uni1.cook <- cooks.distance(Grad_Admission.uni1.model)
plot(uni1.cook, ylab = "Cook's distance", main = "University Rating 1",
     col = "aquamarine1", type = "b")
uni2.cook <- cooks.distance(Grad_Admission.uni2.model)
plot(uni2.cook, ylab = "Cook's distance", main = "University Rating 2",
     col = "blueviolet", type = "b")
uni3.cook <- cooks.distance(Grad_Admission.uni3.model)
plot(uni3.cook, ylab = "Cook's distance", main = "University Rating 3",
     col = "turquoise", type = "b")
uni4.cook <- cooks.distance(Grad_Admission.uni4.model)
plot(uni4.cook, ylab = "Cook's distance", main = "University Rating 4",
     col = "darkslateblue", type = "b")
uni5.cook <- cooks.distance(Grad_Admission.uni5.model)
plot(uni5.cook, ylab = "Cook's distance", main = "University Rating 5",
     col = "deepskyblue2", type = "b")

colors()


# All universities
Grad_Admission.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                                   LOR + CGPA + Research, data = Grad_Admission)
nrow(Grad_Admission) # 400
summary(Grad_Admission.model) # 0.803
full.training.rows <- sample(1:nrow(Grad_Admission), size = 350)
full.training <- Grad_Admission[full.training.rows,]
full.testing <- Grad_Admission[-full.training.rows,]
full.full.predictions <- predict(Grad_Admission.model, newdata = full.testing)
sqrt(mean((full.full.predictions - full.testing$Chance.of.Admit)^2))
full.step.model <- step(Grad_Admission.model)
summary(full.step.model)
full.step.predictions <- predict(full.step.model, newdata = full.testing)
sqrt(mean((full.step.predictions - full.testing$Chance.of.Admit)^2))

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission)

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission.uni1)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission.uni1)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission.uni1)

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission.uni2)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission.uni2)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission.uni2)

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission.uni3)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission.uni3)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission.uni3)

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission.uni4)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission.uni4)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission.uni4)

lm(Chance.of.Admit ~ SOP + Research, data = Grad_Admission.uni5)
lm(Chance.of.Admit ~ LOR, data = Grad_Admission.uni5)
lm(Chance.of.Admit ~ SOP + Research + LOR, data = Grad_Admission.uni5)


coef1 <- Grad_Admission.avg.cgpa.un1.model$coefficients[-1]
coef2 <- Grad_Admission.avg.cgpa.un2.model$coefficients[-1]
coef3 <- Grad_Admission.avg.cgpa.un3.model$coefficients[-1]
coef4 <- Grad_Admission.avg.cgpa.un4.model$coefficients[-1]
coef5 <- Grad_Admission.avg.cgpa.un5.model$coefficients[-1]

coef1
Grad_Admission.avg.cgpa.un1.model

library(ggplot2)

University.Rating <- c(rep("1", 6) , rep("2", 6) , rep("3", 6) , rep("4", 6), rep("5", 6) )
Requirements <- rep(c("GRE.Score" , "TOEFL.Score" , "SOP", "LOR", "CGPA", "Research") , 5)
Coefficients <- c(coef1, coef2, coef3, coef4, coef5)
data <- data.frame(University.Rating,Requirements,Coefficients)

ggplot(data, aes(fill=Requirements, y=Coefficients, x=University.Rating, xlab(1,2,3,4,5))) + 
        scale_fill_manual(values=c("forestgreen", "steelblue2", "navyblue" ,"gold", "tomato", "darkgrey")) + 
        geom_bar(position="dodge", stat="identity")

Univ1 <- filter(Grad_Admission[University.Rating==1, c(-1)], between(CGPA,7,9))
nrow(Univ1)
Univ1
nrow(Grad_Admission.avg.cgpa.un1)
ModelUniv1 <- lm(Chance.of.Admit~ ., data = Univ1)
ModelUniv1

par(mfrow=c(1,2))
boxplot(Grad_Admission$Chance.of.Admit~Grad_Admission$University.Rating,
               	subset = Grad_Admission$Research=="1", col="Maroon",
               	main= "Candidates with Research Experience",
        ylim = c(0.3,1))
boxplot(Grad_Admission$Chance.of.Admit~Grad_Admission$University.Rating,
               	subset = Grad_Admission$Research=="0", col="Grey",
               	main= "Candidates with NO Research Experience",
        ylim = c(0.3,1))

filter(Grad_Admission[Grad_Admission$University.Rating == 1,],
       between(Chance.of.Admit, 0.75, 0.8))

abc.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                        LOR + CGPA + Research, data = Grad_Admission.uni1)

abc1.model <- lm(Chance.of.Admit ~ GRE.Score + TOEFL.Score + SOP +
                        LOR + CGPA + Research, data = filter(Grad_Admission.uni1, Serial.No. != 140))

Grad_Admission.uni1[Grad_Admission.uni1$Research == 0 &
                            Grad_Admission.uni1$CGPA >= 8.0,]
filter(Grad_Admission.uni1, Serial.No. == 140)
summary(abc.model)
summary(abc1.model)

par(mfrow = c(1,2), cex = 0.7)
plot(Grad_Admission.uni1$Chance.of.Admit, abc.model$fitted.values,
     xlim = c(0.4, 0.8), ylim = c(0.4, 0.8),
     xlab = "Chance of admit", ylab = "Fitted values", main = "Including outlier")
abline(0,1)
sqrt(mean((Grad_Admission.uni1$Chance.of.Admit - abc.model$fitted.values)^2))
plot(filter(Grad_Admission.uni1, Serial.No. != 140)$Chance.of.Admit, abc1.model$fitted.values,
     xlim = c(0.4, 0.8),
     ylim = c(0.4, 0.8),
     xlab = "Chance of admit", ylab = "Fitted values", main = "Without outlier")
abline(0,1)
sqrt(mean((filter(Grad_Admission.uni1, Serial.No. != 140)$Chance.of.Admit - abc1.model$fitted.values)^2))

summary(Grad_Admission.uni1)



#SOP LOR in Univ 1, 2, 3
par(mfrow= c(1,2))
Univ123 <-filter(Grad_Admission, (University.Rating==1|University.Rating==2|University.Rating==3) )
Univ123.NoResearch <- filter(Univ123, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ123.NoResearch, main= "Universities Ranking 1, 2 and 3",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ123.NoResearch$SOP)
Smean<- mean(Univ123.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

#SOP LOR in Univ 4 and 5
Univ45 <-filter(Grad_Admission, (University.Rating==4|University.Rating==5) )
Univ45.NoResearch <- filter(Univ45, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ45.NoResearch, main= "Universities Ranking 4 and 5",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ45.NoResearch$SOP)
Smean<- mean(Univ45.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

# For universities

#SOP LOR in Univ 1, 2, 3
par(mfrow= c(2,3))
Univ123 <-filter(Grad_Admission, (University.Rating==1) )
Univ123.NoResearch <- filter(Univ123, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ123.NoResearch, main= "Universities Ranking 1",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ123.NoResearch$SOP)
Smean<- mean(Univ123.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

#SOP LOR in Univ 4 and 5
Univ45 <-filter(Grad_Admission, (University.Rating==2) )
Univ45.NoResearch <- filter(Univ45, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ45.NoResearch, main= "Universities Ranking 2",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ45.NoResearch$SOP)
Smean<- mean(Univ45.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

#SOP LOR in Univ 1, 2, 3
Univ123 <-filter(Grad_Admission, (University.Rating==3) )
Univ123.NoResearch <- filter(Univ123, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ123.NoResearch, main= "Universities Ranking 3",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ123.NoResearch$SOP)
Smean<- mean(Univ123.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

#SOP LOR in Univ 4 and 5
Univ45 <-filter(Grad_Admission, (University.Rating==4) )
Univ45.NoResearch <- filter(Univ45, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ45.NoResearch, main= "Universities Ranking 4",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ45.NoResearch$SOP)
Smean<- mean(Univ45.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

#SOP LOR in Univ 1, 2, 3
Univ123 <-filter(Grad_Admission, (University.Rating==5) )
Univ123.NoResearch <- filter(Univ123, Research==0)
plot(jitter(SOP)~ jitter(LOR), data= Univ123.NoResearch, main= "Universities Ranking 5",
     xlab="SOP", ylab="LOR")
Lmean<- mean(Univ123.NoResearch$SOP)
Smean<- mean(Univ123.NoResearch$LOR)
abline(v= Lmean)
abline(h= Smean)

# 