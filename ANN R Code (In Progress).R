library(neuralnet)
library(VIM)
library(caret)
library(gamlss.add)
#Create a vector of the feature names
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
bc <- read.csv(url, header = F, na.strings="?", col.names = c("ID","Thickness","USize","UShape", "Adhesion","ESize", "Nuclei", "Chromatin", "Nucleoli", "Mitoses", "Class"))
bc <- bc[,-1]
str(bc)
table(is.na(bc))
aggr(bc, cex.lab=1, cex.axis = .4, numbers = T, gap = 0)
bc <- na.omit(bc)
prop.table(table(bc$Class))
sampleSize <- floor(.7 * nrow(bc))
set.seed(10)
Ind <- sample(seq_len(nrow(bc)), size = sampleSize)

Train <- bc[Ind, ]
Test <- bc[-Ind, ]
n <- paste(names(bc[1:9]), collapse = ' + ')
f <- as.formula(c("Class ~" , n))
nn <- neuralnet(f, Train, hidden = c(10), linear.output = FALSE)
nn2 <- neuralnet(f, Train, hidden = c(10,10), linear.output = FALSE)
plot(nn)
plot(nn2)
