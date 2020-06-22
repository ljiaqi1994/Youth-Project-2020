library(vars)
library(forecast)
library(tseries)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.8.0_251/jre')
library(xlsx)
###############################读取数据############################
par(mfrow = c(1,1))

# 设置路径
setwd('C:/Users/lvjiaqi/Desktop/青年课题')

# 读取数据
data <- read.xlsx('data/所有变量数据.xlsx',sheetIndex = 1,stringsAsFactors = FALSE,encoding="UTF-8")
data[,1] <- as.Date(data[,1])
r_dr <- data[,5]
cd <- data[,6]
mlf <- data[,2]
drr <- data[,3]
repo <- data[,4]

###############################数据平稳性检验############################
# 原序列平稳性检验
for(j in 2 : ncol(data)){
  cat(colnames(data)[j])
  print(adf.test(data[,j]))
}

# 对非平稳序列进行处理
tsList <- 2:4
for (j in tsList){data[2:nrow(data),j] <- diff(data[,j])}
data <- data[2:nrow(data),]

par(mfrow = c(3,1))
for(j in 2 : ncol(data)){
  cat(colnames(data)[j])
  print(adf.test(data[,j]))
  plot(data[,1],data[,j],type = 'l',xlab = colnames(data)[j],ylab = NA)
}

###############################将数据进行归一化处理############################
for( j in 2 : ncol(dataSet)){
  dataSet[,j] <- (dataSet[,j] - min(dataSet[,j])) / (max(dataSet[,j])- min(dataSet[,j]))
}
###############################VAR模型滞后阶数确定############################
# criteira-based selection
VARselect(data[,2:ncol(data)], lag.max = 10, type = "const")

###############################VAR模型拟合############################
lag0 <- 3
model <- VAR(data[,2:ncol(data)],p = lag0,type = 'const')

model_stable <- stability(model, h = 0.15, dynamic = FALSE, rescale = TRUE)
par(mfrow = c(1,1))
plot(model_stable,xlab = NA,ylab = NA)
# 单位根检验
roots(model)
# 残差正态检验
normality.test(model, multivariate.only = TRUE)
# 残差序列相关性检验
serial.test(model, lags.pt = 16, lags.bg = 5, type = c("PT.asymptotic"))

###############################格兰杰因果检验############################
lag0 <- 3
for(j in 2 : 4){
  modelTmp1 <- VAR(data[,c(j,ncol(data)-1)],p = lag0,type = 'const')
  print(causality(modelTmp1,cause = colnames(data)[j])$Granger)
  modelTmp2 <- VAR(data[,c(j,ncol(data))],p = lag0,type = 'const')
  print(causality(modelTmp2,cause = colnames(data)[j])$Granger)
}

###############################后续分析############################
impul<-irf(model)#脉冲响应
plot(impul)

depo<-fevd(model)#方差分解
depo
plot(depo)

var.predict <- predict(model,n.ahead = 3, ci = 0.95)#预测
fanchart(var.predict,names = "a",main = NA)



#Credit to Wang Xiangyu, Risk Management Dept., Shanghai Clearing House