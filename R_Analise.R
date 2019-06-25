
types = unique(iosFlutterAndNativePerformanceCSV$PLATFORM_ID)

types_qtd = length(types)

means = rep(NA, types_qtd)

ics = matrix(nrow = types_qtd, ncol = 2)

print(types)

for(i in 1:length(types)) {
  
  rows = which(iosFlutterAndNativePerformanceCSV$PLATFORM_ID == types[i])
  
  remoteData = iosFlutterAndNativePerformanceCSV$LOCAL[rows]
  
  means[i] = mean(remoteData)  
  
  stdev = sd(remoteData)
  n = length(remoteData)
  
  error = qnorm(0.975)*stdev/sqrt(n)
  
  ics[i,1] = means[i] + error
  ics[i,2] = means[i] - error
  
#print(t.test(remoteData))  
}

#hist(means)

plot(iosFlutterAndNativePerformanceCSV$PLATFORM_ID,
     iosFlutterAndNativePerformanceCSV$LOCAL, pch=1, col="darkgrey")

points(x=types, y=means, col="red")
segments(x0=types, x1=types,
         y0=ics[,2], y1=ics[,1],
         col = "red", lwd = 3)

#LIBS 

library(sciplot)
library(MASS)
library(TeachingDemos)
library(rcompanion)

lineplot.CI(androidFlutterAndNativePerformanceCSV$PLATFORM,
            androidFlutterAndNativePerformanceCSV$LIKE_API,
            data = Performances, xlab = "PLATAFORMAS",
            ylab = "TEMPO DE PROCESSAMENTO")



