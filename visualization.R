grad_admission <- read.csv("datasets/grad admission/Admission_Predict.csv")
names(grad_admission)

options(digits = 2)
cor(grad_admission[-1])

attach(grad_admission)
par(mfcol = c(2,4))
par(mfcol = c(1,1))

boxplot(grad_admission$GRE.Score, main = "GRE.Score")
boxplot(grad_admission$TOEFL.Score, main = "TOEFL.Score")
boxplot(grad_admission$University.Rating, main = "University.Rating")
boxplot(grad_admission$SOP, main = "SOP")
boxplot(grad_admission$LOR, main = "LOR")
boxplot(grad_admission$CGPA, main = "CGPA")
boxplot(grad_admission$Research, main = "Research")
boxplot(grad_admission$Chance.of.Admit, main = "Chance.of.Admit")

hist(grad_admission$GRE.Score, main = "GRE.Score", xlab = "GRE Score")
hist(grad_admission$TOEFL.Score, main = "TOEFL.Score", xlab = "TOEFL Score")
hist(grad_admission$SOP)
hist(grad_admission$LOR)
hist(grad_admission$CGPA, main = "CGPA")
hist(grad_admission$Chance.of.Admit, main = "Chance of Admit", xlab = "Chance.of.Admit")


plot(table(grad_admission$University.Rating), ylim = c(0,150), 
     ylab = "frequency", 
     xlab = 'University Rating',
     main = "University.Rating")
plot(table(grad_admission$Research), ylim = c(0,300), 
     ylab = "frequency", 
     xlab = 'Research',
     main = "Research")
pie(table(grad_admission$Research), main = "Research", col = c("seagreen3", "light blue"))
plot(table(grad_admission$SOP), ylim = c(0,100), 
     ylab = "frequency", 
     xlab = 'SOP',
     main = "SOP")
plot(table(grad_admission$LOR), ylim = c(0,100), 
     ylab = "frequency", 
     xlab = 'LOR',
     main = "LOR")


plot(grad_admission$Chance.of.Admit)

library(ggplot2)
plot(TOEFL.Score ~ GRE.Score)
abline(GRE.Score ~ TOEFL.Score)

# GRE.Score vs Chance.of.admit 0.80
data <- data.frame(x = grad_admission$GRE.Score, y = grad_admission$Chance.of.Admit)
ggplot(data, aes(x=x,y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=1, color="red", fill=alpha("orange", 0.1), alpha=0.7, shape=1, stroke=2)

# TOEFL.Score vs Chance.of.admit 0.79
data <- data.frame(x = grad_admission$TOEFL.Score, y = grad_admission$Chance.of.Admit)
ggplot(data, aes(x=x,y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=1, color="red", fill=alpha("orange", 0.1), alpha=0.7, shape=1, stroke=2)

# University.Rating vs Chance.of.admit 0.71
data <- data.frame(x = grad_admission$University.Rating, y = grad_admission$Chance.of.Admit)
ggplot(data, aes(x=x,y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=1, color="red", fill=alpha("orange", 0.1), alpha=0.7, shape=1, stroke=2)

# CGPA vs Chance.of.admit 0.87
data <- data.frame(x = grad_admission$CGPA, y = grad_admission$Chance.of.Admit)
ggplot(data, aes(x=x,y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y)) +
  geom_point( size=1, color="red", fill=alpha("orange", 0.1), alpha=0.7, shape=1, stroke=2)

##########
par(mfcol= c(1,1))
hist(grad_admission$GRE.Score, main = "GRE.Score", xlab = "GRE Score")

hist(grad_admission$TOEFL.Score, main = "TOEFL.Score", xlab = "TOEFL Score")

plot(table(grad_admission$University.Rating), ylim = c(0,150), 
     ylab = "frequency", 
     xlab = 'University Rating',
     main = "University.Rating")

plot(table(grad_admission$SOP), ylim = c(0,100), 
     ylab = "frequency", 
     xlab = 'SOP',
     main = "SOP")

plot(table(grad_admission$LOR), ylim = c(0,100), 
     ylab = "frequency", 
     xlab = 'LOR',
     main = "LOR")

boxplot(grad_admission$CGPA, main = "CGPA")

pie(table(grad_admission$Research), main = "Research", col = c("seagreen3", "light blue"))

hist(grad_admission$Chance.of.Admit, main = "Chance of Admit", xlab = "Chance.of.Admit")

par(mfcol = c(3,4))
plot(GRE.Score ~ TOEFL.Score)
panel.smooth(TOEFL.Score, GRE.Score)

plot(GRE.Score ~ CGPA)
panel.smooth(CGPA, GRE.Score)

plot(GRE.Score ~ Chance.of.Admit)
panel.smooth(Chance.of.Admit, GRE.Score)

plot(TOEFL.Score ~ University.Rating)
panel.smooth(University.Rating, TOEFL.Score)

plot(TOEFL.Score ~ CGPA)
panel.smooth(CGPA, TOEFL.Score)

plot(Chance.of.Admit ~ TOEFL.Score)
panel.smooth(TOEFL.Score, Chance.of.Admit)

plot(University.Rating ~ SOP)
panel.smooth(SOP, University.Rating)

plot(University.Rating ~ CGPA)
panel.smooth(CGPA, University.Rating)

plot(University.Rating ~ Chance.of.Admit)
panel.smooth(Chance.of.Admit, University.Rating)

plot(SOP ~ LOR)
panel.smooth(LOR, SOP)

plot(SOP ~ CGPA)
panel.smooth(CGPA, SOP)

plot(CGPA ~ Chance.of.Admit)
panel.smooth(Chance.of.Admit, CGPA)

