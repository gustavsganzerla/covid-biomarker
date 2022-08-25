###scripts to perform feature selection in biomarkers data of COVID, sepsis, and septic shock patients

#I am isolating the dependent variables (X) from the other clinical information present in the data.
#then, I append the Y (label) to the new dataset.
data <- read.csv("~/Documents/covid/benchmark_paper/datasets/all_patients.csv")
data_x <- data[,7:36]
data_x$mof <- data$MOF

#####Boruta

install.packages('Boruta')
library(Boruta)

colnames(data_x[,1:30]) <- c("ICAM-1",	"Lipo",	"MPO",
                             "VCAM",	"PDL1",	"GCSF",	"IL1b",	"VEGFC",	"DDimer",	"ESelectin",	
                             "Ferritin",	"SPD",	"IL10",	"IL17A",	"GMCSF",	"IL7",	"CXCL10",	"ANG2",	
                             "IL1ra",	"IL6", "CCL2",	"IL12", "IL2",	"IL4",	"IL15",	"GranzymeB",	
                             "IFNg", "TNFa", "sIL6R",	"sGP130")

boruta_output <- Boruta(data_x$mof ~ ., data=data_x[,1:30], doTrace=0)  

names(boruta_output)

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)  

roughFixMod <- TentativeRoughFix(boruta_output)
boruta_signif <- getSelectedAttributes(roughFixMod)
print(boruta_signif)

imps <- attStats(roughFixMod)
imps2 = imps[imps$decision != 'Rejected', c('meanImp', 'decision')]
head(imps2[order(-imps2$meanImp), ])  # descending sort

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")  

#####Lasso regression
install.packages("glmnet")
library(glmnet)

x <- as.matrix(data_x[,1:30])
y <- as.double(as.matrix(ifelse(data_x$mof==0, 0, 1))) # Only Class

set.seed(100)
cv.lasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

plot(cv.lasso)


cat('Min Lambda: ', cv.lasso$lambda.min, '\n 1Sd Lambda: ', cv.lasso$lambda.1se)
df_coef <- round(as.matrix(coef(cv.lasso, s=cv.lasso$lambda.min)), 12)

df_coef[df_coef[, 1] != 0, ]

####information gain
#these values were obtained in Python with the script namely information_gain.py
information_gain <- data.frame(c(0.10020154, 0.        , 0.07415846, 0.05956189, 0.11999551,
                      0.07305173, 0.05569018, 0.09154714, 0.        , 0.00695067,
                      0.        , 0.        , 0.01781756, 0.01804687, 0.09119376,
                      0.06876391, 0.01225613, 0.        , 0.1560334 , 0.05213682,
                      0.02987779, 0.04186097, 0.0242144 , 0.03960486, 0.15434779,
                      0.09888347, 0.        , 0.04390859, 0.02396775, 0.        ))

colnames(information_gain) <- "information_gain"

information_gain$variable <- c("ICAM-1",	"Lipo",	"MPO",
                               "VCAM",	"PDL1",	"GCSF",	"IL1b",	"VEGFC",	"DDimer",	"ESelectin",	
                               "Ferritin",	"SPD",	"IL10",	"IL17A",	"GMCSF",	"IL7",	"CXCL10",	"ANG2",	
                               "IL1ra",	"IL6", "CCL2",	"IL12", "IL2",	"IL4",	"IL15",	"GranzymeB",	
                               "IFNg", "TNFa", "sIL6R",	"sGP130")


information_gain <- sort(information_gain$information_gain)

ggplot(data = information_gain, aes(x = variable, y = information_gain))+
  geom_bar(stat = 'identity', color = "blue", fill = "blue", position = position_dodge(width=0.1), width=0.8)+
  coord_flip()+
  geom_hline(yintercept = 0.09888347, linetype = 2)+
  xlab("")+
  ylab("Information gain")
