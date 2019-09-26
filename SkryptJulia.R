rm(list=ls())
setwd("C:/Users/Default/Desktop/Dokumenty_J/sgh/mgr")
getwd()
install.packages("neuralnet")
library("neuralnet")
library("NeuralNetTools")
library("ggplot2")
####library("dplyr")

#mydata=read.csv('Zapotrzebowanie_pon.csv',sep=",",header=TRUE)
#mydata

#min_data <- apply(mydata, 2, min)
#data_scaled <- scale(mydata, center=min_data, scale=max_data - min_data)
#data <-mydata[,c(3:102)]
#max_data <- apply(data, 2, max)
#min_data <- apply(data, 2, min)
#data_scaled <- scale(data, center=min_data, scale=max_data - min_data)
#index=sample(1:nrow(data), round(0.70*nrow(data)))
#x=mydata[,c(1,2)]
#newdata <- cbind(x,data_scaled)

df <- as.data.frame(read.csv("Zapotrzebowanie.csv"))
df_pon=data.frame(df[df$PON==1 & df$NT!=1,])
df_dr=data.frame(df[df$DR==1 & df$NT!=1,])
df_sob=data.frame(df[df$SOB==1 & df$NT!=1,])
df_ndz=data.frame(df[df$NDZ==1 & df$NT!=1,])
df_nt=data.frame(df[df$NT==1,])



#str(df)

#normalizujemy dane przed uczeniem sieci neuronowej. Dziêki normalizacji jednostki danych
#s¹ eliminowane, co pozwala ³atwo porównywaæ dane bo s¹ w zakresie [0, 1]
#Przed zastosowaniem metody wybranej do normalizacji nale¿y obliczyæ minimalne
#i maksymalne wartoœci ka¿dej kolumny bazy danych.Ostatnia linijka skaluje dane, ze skalowania wy³¹czone s¹ dwie pierwsze kolumny - da
max_data <- apply(df[,8:103], 2, max)
min_data <- apply(df[,8:103], 2, min)
data_scaled <- as.data.frame(scale(df[,8:103], center=min_data, scale=max_data - min_data))
data_scaled <- cbind(df[,1:7], data_scaled)

#index=sample(1:nrow(df[3:98]), round(0.70*nrow(df[3:98])))
index=sample(1:nrow(df), round(0.70*nrow(df)))
train_data <-as.data.frame(data_scaled[index,])
test_data <-as.data.frame(data_scaled[-index,])
#test_dataj

#budujemy sieæ
install.packages("neuralnet")
library("neuralnet")
#net_data1 <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data, hidden=c(6), linear.output=TRUE, threshold=0.01)
net_data1 <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data2 <-neuralnet(ED2~TEMP2+ZACHM2+OPAD2, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data3 <-neuralnet(ED3~TEMP3+ZACHM3+OPAD3, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data4 <-neuralnet(ED4~TEMP4+ZACHM4+OPAD4, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data5 <-neuralnet(ED5~TEMP5+ZACHM5+OPAD5, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data6 <-neuralnet(ED6~TEMP6+ZACHM6+OPAD6, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data7 <-neuralnet(ED7~TEMP7+ZACHM7+OPAD7, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data8 <-neuralnet(ED8~TEMP8+ZACHM8+OPAD8, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data9 <-neuralnet(ED9~TEMP9+ZACHM9+OPAD9, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data10 <-neuralnet(ED10~TEMP10+ZACHM10+OPAD10, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data11 <-neuralnet(ED11~TEMP11+ZACHM11+OPAD11, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data12 <-neuralnet(ED12~TEMP12+ZACHM12+OPAD12, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data13 <-neuralnet(ED13~TEMP13+ZACHM13+OPAD13, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data14 <-neuralnet(ED14~TEMP14+ZACHM14+OPAD14, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data15 <-neuralnet(ED15~TEMP15+ZACHM15+OPAD15, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data16 <-neuralnet(ED16~TEMP16+ZACHM16+OPAD16, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data17 <-neuralnet(ED17~TEMP17+ZACHM17+OPAD17, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data18 <-neuralnet(ED18~TEMP18+ZACHM18+OPAD18, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data19 <-neuralnet(ED19~TEMP19+ZACHM19+OPAD19, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data20 <-neuralnet(ED20~TEMP20+ZACHM20+OPAD20, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data21 <-neuralnet(ED21~TEMP21+ZACHM21+OPAD21, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data22 <-neuralnet(ED22~TEMP22+ZACHM22+OPAD22, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data23 <-neuralnet(ED23~TEMP23+ZACHM23+OPAD23, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)
net_data24 <-neuralnet(ED24~TEMP24+ZACHM24+OPAD24, data=train_data, hidden=10, linear.output=TRUE, threshold=0.01)

#net_data1 <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data, hidden=c(5,2), linear.output=TRUE, threshold=0.01)
net_data1$result.matrix
net_data2$result.matrix
net_data3$result.matrix
net_data4$result.matrix
net_data5$result.matrix
net_data6$result.matrix
net_data7$result.matrix
net_data8$result.matrix
net_data9$result.matrix
net_data10$result.matrix
net_data11$result.matrix
net_data12$result.matrix
net_data13$result.matrix
net_data14$result.matrix
net_data15$result.matrix
net_data16$result.matrix
net_data17$result.matrix
net_data18$result.matrix
net_data19$result.matrix
net_data20$result.matrix
net_data21$result.matrix
net_data22$result.matrix
net_data23$result.matrix
net_data24$result.matrix

plot(net_data1)
plot(net_data2)
plot(net_data3)
plot(net_data4)
plot(net_data5)
plot(net_data6)
plot(net_data7)
plot(net_data8)
plot(net_data9)
plot(net_data10)
plot(net_data11)
plot(net_data12)
plot(net_data13)
plot(net_data14)
plot(net_data15)
plot(net_data16)
plot(net_data17)
plot(net_data18)
plot(net_data19)
plot(net_data20)
plot(net_data21)
plot(net_data22)
plot(net_data23)
plot(net_data24)

library("NeuralNetTools")

garson(net_data1)
garson(net_data2)
garson(net_data3)
garson(net_data4)
garson(net_data5)
garson(net_data6)
garson(net_data7)
garson(net_data8)
garson(net_data9)
garson(net_data10)
garson(net_data11)
garson(net_data12)
garson(net_data13)
garson(net_data14)
garson(net_data15)
garson(net_data16)
garson(net_data17)
garson(net_data18)
garson(net_data19)
garson(net_data20)
garson(net_data21)
garson(net_data22)
garson(net_data23)
garson(net_data24)


#weryfikujemy zdolnoœæ do przewidywania
predict_net_test1 <- compute(net_data1, test_data[,7:102])
predict_net_test2 <- compute(net_data2, test_data[,7:102])
predict_net_test3 <- compute(net_data3, test_data[,7:102])
predict_net_test4 <- compute(net_data4, test_data[,7:102])
predict_net_test5 <- compute(net_data5, test_data[,7:102])
predict_net_test6 <- compute(net_data6, test_data[,7:102])
predict_net_test7 <- compute(net_data7, test_data[,7:102])
predict_net_test8 <- compute(net_data8, test_data[,7:102])
predict_net_test9 <- compute(net_data9, test_data[,7:102])
predict_net_test10 <- compute(net_data10, test_data[,7:102])
predict_net_test11 <- compute(net_data11, test_data[,7:102])
predict_net_test12 <- compute(net_data12, test_data[,7:102])
predict_net_test13 <- compute(net_data13, test_data[,7:102])
predict_net_test14 <- compute(net_data14, test_data[,7:102])
predict_net_test15 <- compute(net_data15, test_data[,7:102])
predict_net_test16 <- compute(net_data16, test_data[,7:102])
predict_net_test17 <- compute(net_data17, test_data[,7:102])
predict_net_test18 <- compute(net_data18, test_data[,7:102])
predict_net_test19 <- compute(net_data19, test_data[,7:102])
predict_net_test20 <- compute(net_data20, test_data[,7:102])
predict_net_test21 <- compute(net_data21, test_data[,7:102])
predict_net_test22 <- compute(net_data22, test_data[,7:102])
predict_net_test23 <- compute(net_data23, test_data[,7:102])
predict_net_test24 <- compute(net_data24, test_data[,7:102])




#results <- data.frame(actual=test_data$ED1, prediction=predict_net_test1$net.result, DATAD=test_data$DATA)
resultsn1 <- data.frame(DATAD=test_data$DATA, actual1=(test_data$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1), prediction1=(predict_net_test1$net.result)*(max(df$ED1)-min(df$ED1))+min(df$ED1), TEMPERATURA1=test_data$TEMP1*(max(df$TEMP1)-min(df$TEMP1))+min(df$TEMP1))
resultsn2 <- data.frame(DATAD=test_data$DATA, actual2=(test_data$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2), prediction2=(predict_net_test2$net.result)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
resultsn3 <- data.frame(DATAD=test_data$DATA, actual3=(test_data$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3), prediction3=(predict_net_test3$net.result)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
resultsn4 <- data.frame(DATAD=test_data$DATA, actual4=(test_data$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4), prediction4=(predict_net_test4$net.result)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
resultsn5 <- data.frame(DATAD=test_data$DATA, actual5=(test_data$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5), prediction5=(predict_net_test5$net.result)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
resultsn6 <- data.frame(DATAD=test_data$DATA, actual6=(test_data$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6), prediction6=(predict_net_test6$net.result)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
resultsn7 <- data.frame(DATAD=test_data$DATA, actual7=(test_data$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7), prediction7=(predict_net_test7$net.result)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
resultsn8 <- data.frame(DATAD=test_data$DATA, actual8=(test_data$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8), prediction8=(predict_net_test8$net.result)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
resultsn9 <- data.frame(DATAD=test_data$DATA, actual9=(test_data$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9), prediction9=(predict_net_test9$net.result)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
resultsn10 <- data.frame(DATAD=test_data$DATA, actual10=(test_data$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10), prediction10=(predict_net_test10$net.result)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
resultsn11 <- data.frame(DATAD=test_data$DATA, actual11=(test_data$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11), prediction11=(predict_net_test11$net.result)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
resultsn12 <- data.frame(DATAD=test_data$DATA, actual12=(test_data$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12), prediction12=(predict_net_test12$net.result)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
resultsn13 <- data.frame(DATAD=test_data$DATA, actual13=(test_data$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13), prediction13=(predict_net_test13$net.result)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
resultsn14 <- data.frame(DATAD=test_data$DATA, actual14=(test_data$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14), prediction14=(predict_net_test14$net.result)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
resultsn15 <- data.frame(DATAD=test_data$DATA, actual15=(test_data$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15), prediction15=(predict_net_test15$net.result)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
resultsn16 <- data.frame(DATAD=test_data$DATA, actual16=(test_data$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16), prediction16=(predict_net_test16$net.result)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
resultsn17 <- data.frame(DATAD=test_data$DATA, actual17=(test_data$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17), prediction17=(predict_net_test17$net.result)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
resultsn18 <- data.frame(DATAD=test_data$DATA, actual18=(test_data$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18), prediction18=(predict_net_test18$net.result)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
resultsn19 <- data.frame(DATAD=test_data$DATA, actual19=(test_data$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19), prediction19=(predict_net_test19$net.result)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
resultsn20 <- data.frame(DATAD=test_data$DATA, actual20=(test_data$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20), prediction20=(predict_net_test20$net.result)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
resultsn21 <- data.frame(DATAD=test_data$DATA, actual21=(test_data$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21), prediction21=(predict_net_test21$net.result)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
resultsn22 <- data.frame(DATAD=test_data$DATA, actual22=(test_data$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22), prediction22=(predict_net_test22$net.result)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
resultsn23 <- data.frame(DATAD=test_data$DATA, actual23=(test_data$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23), prediction23=(predict_net_test23$net.result)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
resultsn24 <- data.frame(DATAD=test_data$DATA, actual24=(test_data$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24), prediction24=(predict_net_test24$net.result)*(max(df$ED24)-min(df$ED24))+min(df$ED24))

julcia=(resultsn1[resultsn1$DATAD='2010/01/02',])

T1 <- data.frame(Rzeczywista_wartosc_zapotrzebowania=rbind(resultsn1[1,2], resultsn2[1,2], resultsn3[1,2], resultsn4[1,2], resultsn5[1,2], resultsn6[1,2], resultsn7[1,2], resultsn8[1,2], resultsn9[1,2], resultsn10[1,2], resultsn11[1,2], resultsn12[1,2], resultsn13[1,2], resultsn14[1,2], resultsn15[1,2], resultsn16[1,2], resultsn17[1,2], resultsn18[1,2], resultsn19[1,2], resultsn20[1,2], resultsn21[1,2], resultsn22[1,2], resultsn23[1,2], resultsn24[1,2]))
T2 <- data.frame(Prognozowana_wartosc_zapotrzebowania=rbind(resultsn1[1,3], resultsn2[1,3], resultsn3[1,3], resultsn4[1,3], resultsn5[1,3], resultsn6[1,3], resultsn7[1,3], resultsn8[1,3], resultsn9[1,3], resultsn10[1,3], resultsn11[1,3], resultsn12[1,3], resultsn13[1,3], resultsn14[1,3], resultsn15[1,3], resultsn16[1,3], resultsn17[1,3], resultsn18[1,3], resultsn19[1,3], resultsn20[1,3], resultsn21[1,3], resultsn23[1,3], resultsn23[1,3], resultsn24[1,3]))
T8 <- data.frame(Rzeczywista_wartosc_zapotrzebowania=rbind(resultsn1[8,2], resultsn2[8,2], resultsn3[8,2], resultsn4[8,2], resultsn5[8,2], resultsn6[8,2], resultsn7[8,2], resultsn8[8,2], resultsn9[8,2], resultsn10[8,2], resultsn11[8,2], resultsn12[8,2], resultsn13[8,2], resultsn14[8,2], resultsn15[8,2], resultsn16[8,2], resultsn17[8,2], resultsn18[8,2], resultsn19[8,2], resultsn20[8,2], resultsn21[8,2], resultsn22[8,2], resultsn23[8,2], resultsn24[8,2]))
T9 <- data.frame(Prognozowana_wartosc_zapotrzebowania=rbind(resultsn1[8,3], resultsn2[8,3], resultsn3[8,3], resultsn4[8,3], resultsn5[8,3], resultsn6[8,3], resultsn7[8,3], resultsn8[8,3], resultsn9[8,3], resultsn10[8,3], resultsn11[8,3], resultsn12[8,3], resultsn13[8,3], resultsn14[8,3], resultsn15[8,3], resultsn16[8,3], resultsn17[8,3], resultsn18[8,3], resultsn19[8,3], resultsn20[8,3], resultsn21[8,3], resultsn22[8,3], resultsn23[8,3], resultsn24[8,3]))



resultsn1 %>% filter(DATAD='2010/01/01')

#T11 <- data.frame(t(rbind(dataj=resultsn1[1,1], Zap_rzecz1=resultsn1[1,2], Zap_rzecz2=resultsn2[1,2], Zap_rzecz3=resultsn3[1,2], Zap_rzecz4=resultsn4[1,2], Zap_rzecz5=resultsn5[1,2])))
#h <- data.frame(c(1:5))
#names(h) <- c("dataj","Zap_rzecz1","Zap_rzecz2","Zap_rzecz3","Zap_rzecz4","Zap_rzecz5")
TableT <- cbind(Godzina=c(1:24), T8, T9)
TableT2 <- cbind(Godzina=c(1:24), T8, T9)
T8 <- cbind(T8, Godzina=c(1:24))
T9 <- cbind(T9, Godzina=c(1:24))


tableresults_all <- cbind(resultsn1, resultsn2[2:3], resultsn3[2:3])
#qplot(x=, y=c(3,5), data=tableresults_all)
#ggplot2(data=tableresults_all, aes(x=))
ggplot(data=TableT, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania))+ geom_point()+ geom_point(data=TableT, aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), color='red')
ggplot(data=T1, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania))+ geom_point()

ggplot(data=T8, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania))+ geom_line()+ geom_line(data=T9, aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), color='red')
plot(T8[,1], lwd=2, type='l')
lines(T9[,1], col=2, lty=2)
plot(TableT$Rzeczywista_wartosc_zapotrzebowania)
matplot(TableT$Prognozowana_wartosc_zapotrzebowania, type="l")



#Oblicza wyniki wszystkich neuronów dla okreœlonych dowolnych wektorów wspó³zmiennych 
#otrzymuj¹cych wyszkolon¹ sieæ neuronow¹. Wa¿ne jest, aby kolejnoœæ zmiennych towarzysz¹cych 
#by³a taka sama w nowej macierzy lub ramce danych, jak w oryginalnej sieci neuronowej. Nastêpnie, 
#w celu wizualizacji, w pierwszych wierszach wyniku przewidywania u¿ywana jest funkcja drukowania, 
#pokazana w nastêpuj¹cy sposób:
print(head(predict_net_test1$net.result)*(max(df$ED1)-min(df$ED1))+min(df$ED1))

predict_net_test_start1 <- predict_net_test1$net.result*(max(df$ED1)-min(df$ED1))+min(df$ED1)
predict_net_test_start2 <- predict_net_test2$net.result*(max(df$ED2)-min(df$ED2))+min(df$ED2)
predict_net_test_start3 <- predict_net_test3$net.result*(max(df$ED3)-min(df$ED3))+min(df$ED3)
predict_net_test_start4 <- predict_net_test4$net.result*(max(df$ED4)-min(df$ED4))+min(df$ED4)
predict_net_test_start5 <- predict_net_test5$net.result*(max(df$ED5)-min(df$ED5))+min(df$ED5)
predict_net_test_start6 <- predict_net_test6$net.result*(max(df$ED6)-min(df$ED6))+min(df$ED6)
predict_net_test_start7 <- predict_net_test7$net.result*(max(df$ED7)-min(df$ED7))+min(df$ED7)
predict_net_test_start8 <- predict_net_test8$net.result*(max(df$ED8)-min(df$ED8))+min(df$ED8)
predict_net_test_start9 <- predict_net_test9$net.result*(max(df$ED9)-min(df$ED9))+min(df$ED9)
predict_net_test_start10 <- predict_net_test10$net.result*(max(df$ED10)-min(df$ED10))+min(df$ED10)
predict_net_test_start11 <- predict_net_test11$net.result*(max(df$ED11)-min(df$ED11))+min(df$ED11)
predict_net_test_start12 <- predict_net_test12$net.result*(max(df$ED12)-min(df$ED12))+min(df$ED12)
predict_net_test_start13 <- predict_net_test13$net.result*(max(df$ED13)-min(df$ED13))+min(df$ED13)
predict_net_test_start14 <- predict_net_test14$net.result*(max(df$ED14)-min(df$ED14))+min(df$ED14)
predict_net_test_start15 <- predict_net_test15$net.result*(max(df$ED15)-min(df$ED15))+min(df$ED15)
predict_net_test_start16 <- predict_net_test16$net.result*(max(df$ED16)-min(df$ED16))+min(df$ED16)
predict_net_test_start17 <- predict_net_test17$net.result*(max(df$ED17)-min(df$ED17))+min(df$ED17)
predict_net_test_start18 <- predict_net_test18$net.result*(max(df$ED18)-min(df$ED18))+min(df$ED18)
predict_net_test_start19 <- predict_net_test19$net.result*(max(df$ED19)-min(df$ED19))+min(df$ED19)
predict_net_test_start20 <- predict_net_test20$net.result*(max(df$ED20)-min(df$ED20))+min(df$ED20)
predict_net_test_start21 <- predict_net_test21$net.result*(max(df$ED21)-min(df$ED21))+min(df$ED21)
predict_net_test_start22 <- predict_net_test22$net.result*(max(df$ED22)-min(df$ED22))+min(df$ED22)
predict_net_test_start23 <- predict_net_test23$net.result*(max(df$ED23)-min(df$ED23))+min(df$ED23)
predict_net_test_start24 <- predict_net_test24$net.result*(max(df$ED24)-min(df$ED24))+min(df$ED24)


test_start1 <- as.data.frame((test_data$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1))
test_start2 <- as.data.frame((test_data$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
test_start3 <- as.data.frame((test_data$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
test_start4 <- as.data.frame((test_data$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
test_start5 <- as.data.frame((test_data$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
test_start6 <- as.data.frame((test_data$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
test_start7 <- as.data.frame((test_data$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
test_start8 <- as.data.frame((test_data$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
test_start9 <- as.data.frame((test_data$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
test_start10 <- as.data.frame((test_data$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
test_start11 <- as.data.frame((test_data$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
test_start12 <- as.data.frame((test_data$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
test_start13 <- as.data.frame((test_data$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
test_start14 <- as.data.frame((test_data$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
test_start15 <- as.data.frame((test_data$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
test_start16 <- as.data.frame((test_data$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
test_start17 <- as.data.frame((test_data$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
test_start18 <- as.data.frame((test_data$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
test_start19 <- as.data.frame((test_data$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
test_start20 <- as.data.frame((test_data$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
test_start21 <- as.data.frame((test_data$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
test_start22 <- as.data.frame((test_data$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
test_start23 <- as.data.frame((test_data$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
test_start24 <- as.data.frame((test_data$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24))


test_start1


MSE.net_data1 <- sum((test_start1-predict_net_test_start1)^2/nrow(test_start1))
MSE.net_data2 <- sum((test_start2-predict_net_test_start2)^2/nrow(test_start2))
MSE.net_data3 <- sum((test_start3-predict_net_test_start3)^2/nrow(test_start3))
MSE.net_data4 <- sum((test_start4-predict_net_test_start4)^2/nrow(test_start4))
MSE.net_data5 <- sum((test_start5-predict_net_test_start5)^2/nrow(test_start5))
MSE.net_data6 <- sum((test_start6-predict_net_test_start6)^2/nrow(test_start6))
MSE.net_data7 <- sum((test_start7-predict_net_test_start7)^2/nrow(test_start7))
MSE.net_data8 <- sum((test_start8-predict_net_test_start8)^2/nrow(test_start8))
MSE.net_data9 <- sum((test_start9-predict_net_test_start9)^2/nrow(test_start9))
MSE.net_data10 <- sum((test_start10-predict_net_test_start10)^2/nrow(test_start10))
MSE.net_data11 <- sum((test_start11-predict_net_test_start11)^2/nrow(test_start11))
MSE.net_data12 <- sum((test_start12-predict_net_test_start12)^2/nrow(test_start12))
MSE.net_data13 <- sum((test_start13-predict_net_test_start13)^2/nrow(test_start13))
MSE.net_data14 <- sum((test_start14-predict_net_test_start14)^2/nrow(test_start14))
MSE.net_data15 <- sum((test_start15-predict_net_test_start15)^2/nrow(test_start15))
MSE.net_data16 <- sum((test_start16-predict_net_test_start16)^2/nrow(test_start16))
MSE.net_data17 <- sum((test_start17-predict_net_test_start17)^2/nrow(test_start17))
MSE.net_data18 <- sum((test_start18-predict_net_test_start18)^2/nrow(test_start18))
MSE.net_data19 <- sum((test_start19-predict_net_test_start19)^2/nrow(test_start19))
MSE.net_data20 <- sum((test_start20-predict_net_test_start20)^2/nrow(test_start20))
MSE.net_data21 <- sum((test_start21-predict_net_test_start21)^2/nrow(test_start21))
MSE.net_data22 <- sum((test_start22-predict_net_test_start22)^2/nrow(test_start22))
MSE.net_data23 <- sum((test_start23-predict_net_test_start23)^2/nrow(test_start23))
MSE.net_data24 <- sum((test_start24-predict_net_test_start24)^2/nrow(test_start24))


MSE.net_data1
MSE_all <- data.frame(cbind(MSE.net_data1, MSE.net_data2, MSE.net_data3, MSE.net_data4, MSE.net_data5))

Regression_Model1 <- lm(ED1~TEMP1+ZACHM1+OPAD1, data=df)
Regression_Model2 <- lm(ED2~TEMP2+ZACHM2+OPAD2, data=df)
Regression_Model3 <- lm(ED3~TEMP3+ZACHM2+OPAD3, data=df)
Regression_Model4 <- lm(ED4~TEMP4+ZACHM2+OPAD4, data=df)
Regression_Model5 <- lm(ED5~TEMP5+ZACHM2+OPAD5, data=df)


summary(Regression_Model1)
summary(Regression_Model2)
summary(Regression_Model3)
summary(Regression_Model4)
summary(Regression_Model5)

test <- df[-index]
predict_lm <- predict(Regression_Model1, test) #tu jakis blad wyrzuca
MSE.lm1 <- sum((predict_lm-test$ED1)^2/nrow(test))

MSE.net_data
MSE.lm1

#predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)
predict_net_test1$net.result <- sapply(predict_net_test1$net.result,round,digits=0)
test_start1$net.result <- sapply(predict_net_test1$net.result,round,digits=0)
#table(test_data$Private,predicted_data$net.result)
table(test_data$ED1, predict_net_test1$net.result)
table(test_data$ED1, test_start1$net.result)
#table(test_data$Private,predicted_data$net.result)
table(test_data$ED1)
#predicted_data$net.result <- sapply(predicted_data$net.result,round,digits=0)
predict_net_test1$net.result <- sapply(predict_net_test1$net.result,round,digits=0)

################################Dzien roboczy
df_dr=data.frame(df[df$DR==1 & df$NT!=1,])

max_data_dr <- apply(df_dr[,8:103], 2, max)
min_data_dr <- apply(df_dr[,8:103], 2, min)
data_scaled_dr <- as.data.frame(scale(df_dr[,8:103], center=min_data_dr, scale=max_data_dr - min_data_dr))
data_scaled_dr <- cbind(df_dr[,1:7], data_scaled_dr)

#index_dr=sample(1:nrow(df_dr), round(0.70*nrow(df_dr)))
#train_data_dr <-as.data.frame(data_scaled_dr[index_dr,])
#test_data_dr <-as.data.frame(data_scaled_dr[-index_dr,])
train_data_dr=as.data.frame(data_scaled_dr[1:1215,])
test_data_dr=as.data.frame(data_scaled_dr[1216:nrow(data_scaled_dr),])

install.packages("neuralnet")
library("neuralnet")

net_data1_dr <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data2_dr <-neuralnet(ED2~TEMP2+ZACHM2+OPAD2, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data3_dr <-neuralnet(ED3~TEMP3+ZACHM3+OPAD3, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data4_dr <-neuralnet(ED4~TEMP4+ZACHM4+OPAD4, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data5_dr <-neuralnet(ED5~TEMP5+ZACHM5+OPAD5, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data6_dr <-neuralnet(ED6~TEMP6+ZACHM6+OPAD6, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data7_dr <-neuralnet(ED7~TEMP7+ZACHM7+OPAD7, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data8_dr <-neuralnet(ED8~TEMP8+ZACHM8+OPAD8, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data9_dr <-neuralnet(ED9~TEMP9+ZACHM9+OPAD9, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data10_dr <-neuralnet(ED10~TEMP10+ZACHM10+OPAD10, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data11_dr <-neuralnet(ED11~TEMP11+ZACHM11+OPAD11, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data12_dr <-neuralnet(ED12~TEMP12+ZACHM12+OPAD12, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data13_dr <-neuralnet(ED13~TEMP13+ZACHM13+OPAD13, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data14_dr <-neuralnet(ED14~TEMP14+ZACHM14+OPAD14, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data15_dr <-neuralnet(ED15~TEMP15+ZACHM15+OPAD15, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data16_dr <-neuralnet(ED16~TEMP16+ZACHM16+OPAD16, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data17_dr <-neuralnet(ED17~TEMP17+ZACHM17+OPAD17, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data18_dr <-neuralnet(ED18~TEMP18+ZACHM18+OPAD18, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data19_dr <-neuralnet(ED19~TEMP19+ZACHM19+OPAD19, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data20_dr <-neuralnet(ED20~TEMP20+ZACHM20+OPAD20, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data21_dr <-neuralnet(ED21~TEMP21+ZACHM21+OPAD21, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data22_dr <-neuralnet(ED22~TEMP22+ZACHM22+OPAD22, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data23_dr <-neuralnet(ED23~TEMP23+ZACHM23+OPAD23, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)
net_data24_dr <-neuralnet(ED24~TEMP24+ZACHM24+OPAD24, data=train_data_dr, hidden=10, linear.output=TRUE, threshold=0.01)

net_data1_dr$result.matrix
net_data2_dr$result.matrix
net_data3_dr$result.matrix
net_data4_dr$result.matrix
net_data5_dr$result.matrix
net_data6_dr$result.matrix
net_data7_dr$result.matrix
net_data8_dr$result.matrix
net_data9_dr$result.matrix
net_data10_dr$result.matrix
net_data11_dr$result.matrix
net_data12_dr$result.matrix
net_data13_dr$result.matrix
net_data14_dr$result.matrix
net_data15_dr$result.matrix
net_data16_dr$result.matrix
net_data17_dr$result.matrix
net_data18_dr$result.matrix
net_data19_dr$result.matrix
net_data20_dr$result.matrix
net_data21_dr$result.matrix
net_data22_dr$result.matrix
net_data23_dr$result.matrix
net_data24_dr$result.matrix

plot(net_data1_dr)
plot(net_data2_dr)
plot(net_data3_dr)
plot(net_data4_dr)
plot(net_data5_dr)
plot(net_data6_dr)
plot(net_data7_dr)
plot(net_data8_dr)
plot(net_data9_dr)
plot(net_data10_dr)
plot(net_data11_dr)
plot(net_data12_dr)
plot(net_data13_dr)
plot(net_data14_dr)
plot(net_data15_dr)
plot(net_data16_dr)
plot(net_data17_dr)
plot(net_data18_dr)
plot(net_data19_dr)
plot(net_data20_dr)
plot(net_data21_dr)
plot(net_data22_dr)
plot(net_data23_dr)
plot(net_data24_dr)

par(mfrow=c(2,2))
gwplot(net_data1_dr,selected.covariate="TEMP1",min=-2.5, max=5, col="darkorchid4")
gwplot(net_data1_dr,selected.covariate="ZACHM1", min=-2.5, max=5, col="cornflowerblue")
gwplot(net_data1_dr,selected.covariate="OPAD1", min=-2.5, max=5, col="darkolivegreen4")

library("NeuralNetTools")

garson(net_data1_dr)
garson(net_data2_dr)
garson(net_data3_dr)
garson(net_data4_dr)
garson(net_data5_dr)
garson(net_data6_dr)
garson(net_data7_dr)
garson(net_data8_dr)
garson(net_data9_dr)
garson(net_data10_dr)
garson(net_data11_dr)
garson(net_data12_dr)
garson(net_data13_dr)
garson(net_data14_dr)
garson(net_data15_dr)
garson(net_data16_dr)
garson(net_data17_dr)
garson(net_data18_dr)
garson(net_data19_dr)
garson(net_data20_dr)
garson(net_data21_dr)
garson(net_data22_dr)
garson(net_data23_dr)
garson(net_data24_dr)

predict_net_test1_dr <- (compute(net_data1_dr, test_data_dr[,8:103]))
predict_net_test2_dr <- compute(net_data2_dr, test_data_dr[,8:103])
predict_net_test3_dr <- compute(net_data3_dr, test_data_dr[,8:103])
predict_net_test4_dr <- compute(net_data4_dr, test_data_dr[,8:103])
predict_net_test5_dr <- compute(net_data5_dr, test_data_dr[,8:103])
predict_net_test6_dr <- compute(net_data6_dr, test_data_dr[,8:103])
predict_net_test7_dr <- compute(net_data7_dr, test_data_dr[,8:103])
predict_net_test8_dr <- compute(net_data8_dr, test_data_dr[,8:103])
predict_net_test9_dr <- compute(net_data9_dr, test_data_dr[,8:103])
predict_net_test10_dr <- compute(net_data10_dr, test_data_dr[,8:103])
predict_net_test11_dr <- compute(net_data11_dr, test_data_dr[,8:103])
predict_net_test12_dr <- compute(net_data12_dr, test_data_dr[,8:103])
predict_net_test13_dr <- compute(net_data13_dr, test_data_dr[,8:103])
predict_net_test14_dr <- compute(net_data14_dr, test_data_dr[,8:103])
predict_net_test15_dr <- compute(net_data15_dr, test_data_dr[,8:103])
predict_net_test16_dr <- compute(net_data16_dr, test_data_dr[,8:103])
predict_net_test17_dr <- compute(net_data17_dr, test_data_dr[,8:103])
predict_net_test18_dr <- compute(net_data18_dr, test_data_dr[,8:103])
predict_net_test19_dr <- compute(net_data19_dr, test_data_dr[,8:103])
predict_net_test20_dr <- compute(net_data20_dr, test_data_dr[,8:103])
predict_net_test21_dr <- compute(net_data21_dr, test_data_dr[,8:103])
predict_net_test22_dr <- compute(net_data22_dr, test_data_dr[,8:103])
predict_net_test23_dr <- compute(net_data23_dr, test_data_dr[,8:103])
predict_net_test24_dr <- compute(net_data24_dr, test_data_dr[,8:103])

resultsn1_dr <- data.frame(DATAD=test_data_dr$DATA, actual1=(test_data_dr$ED1)*(max(df_dr$ED1)-min(df_dr$ED1))+min(df_dr$ED1), prediction1=(predict_net_test1_dr$net.result)*(max(df_dr$ED1)-min(df_dr$ED1))+min(df_dr$ED1), TEMPERATURA1=test_data_dr$TEMP1*(max(df_dr$TEMP1)-min(df_dr$TEMP1))+min(df_dr$TEMP1))
resultsn2_dr <- data.frame(DATAD=test_data_dr$DATA, actual2=(test_data_dr$ED2)*(max(df_dr$ED2)-min(df_dr$ED2))+min(df_dr$ED2), prediction2=(predict_net_test2_dr$net.result)*(max(df_dr$ED2)-min(df_dr$ED2))+min(df_dr$ED2))
resultsn3_dr <- data.frame(DATAD=test_data_dr$DATA, actual3=(test_data_dr$ED3)*(max(df_dr$ED3)-min(df_dr$ED3))+min(df_dr$ED3), prediction3=(predict_net_test3_dr$net.result)*(max(df_dr$ED3)-min(df_dr$ED3))+min(df_dr$ED3))
resultsn4_dr <- data.frame(DATAD=test_data_dr$DATA, actual4=(test_data_dr$ED4)*(max(df_dr$ED4)-min(df_dr$ED4))+min(df_dr$ED4), prediction4=(predict_net_test4_dr$net.result)*(max(df_dr$ED4)-min(df_dr$ED4))+min(df_dr$ED4))
resultsn5_dr <- data.frame(DATAD=test_data_dr$DATA, actual5=(test_data_dr$ED5)*(max(df_dr$ED5)-min(df_dr$ED5))+min(df_dr$ED5), prediction5=(predict_net_test5_dr$net.result)*(max(df_dr$ED5)-min(df_dr$ED5))+min(df_dr$ED5))
resultsn6_dr <- data.frame(DATAD=test_data_dr$DATA, actual6=(test_data_dr$ED6)*(max(df_dr$ED6)-min(df_dr$ED6))+min(df_dr$ED6), prediction6=(predict_net_test6_dr$net.result)*(max(df_dr$ED6)-min(df_dr$ED6))+min(df_dr$ED6))
resultsn7_dr <- data.frame(DATAD=test_data_dr$DATA, actual7=(test_data_dr$ED7)*(max(df_dr$ED7)-min(df_dr$ED7))+min(df_dr$ED7), prediction7=(predict_net_test7_dr$net.result)*(max(df_dr$ED7)-min(df_dr$ED7))+min(df_dr$ED7))
resultsn8_dr <- data.frame(DATAD=test_data_dr$DATA, actual8=(test_data_dr$ED8)*(max(df_dr$ED8)-min(df_dr$ED8))+min(df_dr$ED8), prediction8=(predict_net_test8_dr$net.result)*(max(df_dr$ED8)-min(df_dr$ED8))+min(df_dr$ED8))
resultsn9_dr <- data.frame(DATAD=test_data_dr$DATA, actual9=(test_data_dr$ED9)*(max(df_dr$ED9)-min(df_dr$ED9))+min(df_dr$ED9), prediction9=(predict_net_test9_dr$net.result)*(max(df_dr$ED9)-min(df_dr$ED9))+min(df_dr$ED9))
resultsn10_dr <- data.frame(DATAD=test_data_dr$DATA, actual10=(test_data_dr$ED10)*(max(df_dr$ED10)-min(df_dr$ED10))+min(df_dr$ED10), prediction10=(predict_net_test10_dr$net.result)*(max(df_dr$ED10)-min(df_dr$ED10))+min(df_dr$ED10))
resultsn11_dr <- data.frame(DATAD=test_data_dr$DATA, actual11=(test_data_dr$ED11)*(max(df_dr$ED11)-min(df_dr$ED11))+min(df_dr$ED11), prediction11=(predict_net_test11_dr$net.result)*(max(df_dr$ED11)-min(df_dr$ED11))+min(df_dr$ED11))
resultsn12_dr <- data.frame(DATAD=test_data_dr$DATA, actual12=(test_data_dr$ED12)*(max(df_dr$ED12)-min(df_dr$ED12))+min(df_dr$ED12), prediction12=(predict_net_test12_dr$net.result)*(max(df_dr$ED12)-min(df_dr$ED12))+min(df_dr$ED12))
resultsn13_dr <- data.frame(DATAD=test_data_dr$DATA, actual13=(test_data_dr$ED13)*(max(df_dr$ED13)-min(df_dr$ED13))+min(df_dr$ED13), prediction13=(predict_net_test13_dr$net.result)*(max(df_dr$ED13)-min(df_dr$ED13))+min(df_dr$ED13))
resultsn14_dr <- data.frame(DATAD=test_data_dr$DATA, actual14=(test_data_dr$ED14)*(max(df_dr$ED14)-min(df_dr$ED14))+min(df_dr$ED14), prediction14=(predict_net_test14_dr$net.result)*(max(df_dr$ED14)-min(df_dr$ED14))+min(df_dr$ED14))
resultsn15_dr <- data.frame(DATAD=test_data_dr$DATA, actual15=(test_data_dr$ED15)*(max(df_dr$ED15)-min(df_dr$ED15))+min(df_dr$ED15), prediction15=(predict_net_test15_dr$net.result)*(max(df_dr$ED15)-min(df_dr$ED15))+min(df_dr$ED15))
resultsn16_dr <- data.frame(DATAD=test_data_dr$DATA, actual16=(test_data_dr$ED16)*(max(df_dr$ED16)-min(df_dr$ED16))+min(df_dr$ED16), prediction16=(predict_net_test16_dr$net.result)*(max(df_dr$ED16)-min(df_dr$ED16))+min(df_dr$ED16))
resultsn17_dr <- data.frame(DATAD=test_data_dr$DATA, actual17=(test_data_dr$ED17)*(max(df_dr$ED17)-min(df_dr$ED17))+min(df_dr$ED17), prediction17=(predict_net_test17_dr$net.result)*(max(df_dr$ED17)-min(df_dr$ED17))+min(df_dr$ED17))
resultsn18_dr <- data.frame(DATAD=test_data_dr$DATA, actual18=(test_data_dr$ED18)*(max(df_dr$ED18)-min(df_dr$ED18))+min(df_dr$ED18), prediction18=(predict_net_test18_dr$net.result)*(max(df_dr$ED18)-min(df_dr$ED18))+min(df_dr$ED18))
resultsn19_dr <- data.frame(DATAD=test_data_dr$DATA, actual19=(test_data_dr$ED19)*(max(df_dr$ED19)-min(df_dr$ED19))+min(df_dr$ED19), prediction19=(predict_net_test19_dr$net.result)*(max(df_dr$ED19)-min(df_dr$ED19))+min(df_dr$ED19))
resultsn20_dr <- data.frame(DATAD=test_data_dr$DATA, actual20=(test_data_dr$ED20)*(max(df_dr$ED20)-min(df_dr$ED20))+min(df_dr$ED20), prediction20=(predict_net_test20_dr$net.result)*(max(df_dr$ED20)-min(df_dr$ED20))+min(df_dr$ED20))
resultsn21_dr <- data.frame(DATAD=test_data_dr$DATA, actual21=(test_data_dr$ED21)*(max(df_dr$ED21)-min(df_dr$ED21))+min(df_dr$ED21), prediction21=(predict_net_test21_dr$net.result)*(max(df_dr$ED21)-min(df_dr$ED21))+min(df_dr$ED21))
resultsn22_dr <- data.frame(DATAD=test_data_dr$DATA, actual22=(test_data_dr$ED22)*(max(df_dr$ED22)-min(df_dr$ED22))+min(df_dr$ED22), prediction22=(predict_net_test22_dr$net.result)*(max(df_dr$ED22)-min(df_dr$ED22))+min(df_dr$ED22))
resultsn23_dr <- data.frame(DATAD=test_data_dr$DATA, actual23=(test_data_dr$ED23)*(max(df_dr$ED23)-min(df_dr$ED23))+min(df_dr$ED23), prediction23=(predict_net_test23_dr$net.result)*(max(df_dr$ED23)-min(df_dr$ED23))+min(df_dr$ED23))
resultsn24_dr <- data.frame(DATAD=test_data_dr$DATA, actual24=(test_data_dr$ED24)*(max(df_dr$ED24)-min(df_dr$ED24))+min(df_dr$ED24), prediction24=(predict_net_test24_dr$net.result)*(max(df_dr$ED24)-min(df_dr$ED24))+min(df_dr$ED24))


#remove.packages("neuralnet")
install.packages("dplyr")
library("dplyr")
DATAJ='2018/12/28'
#j=filter(resultsn1_dr, DATAD=='2010/01/13')
r_dr=data.frame(t(data.frame(cbind(filter(resultsn1_dr, DATAD==DATAJ)[1,2]), (filter(resultsn2_dr, DATAD==DATAJ)[1,2]), (filter(resultsn3_dr, DATAD==DATAJ)[1,2]), (filter(resultsn4_dr, DATAD==DATAJ)[1,2]), 
             (filter(resultsn5_dr, DATAD==DATAJ)[1,2]), (filter(resultsn6_dr, DATAD==DATAJ)[1,2]), (filter(resultsn7_dr, DATAD==DATAJ)[1,2]), (filter(resultsn8_dr, DATAD==DATAJ)[1,2]), (filter(resultsn9_dr, DATAD==DATAJ)[1,2]), 
             (filter(resultsn10_dr, DATAD==DATAJ)[1,2]), (filter(resultsn11_dr, DATAD==DATAJ)[1,2]),  (filter(resultsn12_dr, DATAD==DATAJ)[1,2]), (filter(resultsn13_dr, DATAD==DATAJ)[1,2]), 
             (filter(resultsn14_dr, DATAD==DATAJ)[1,2]), (filter(resultsn15_dr, DATAD==DATAJ)[1,2]), (filter(resultsn16_dr, DATAD==DATAJ)[1,2]), (filter(resultsn17_dr, DATAD==DATAJ)[1,2]), (filter(resultsn18_dr, DATAD==DATAJ)[1,2]), 
             (filter(resultsn19_dr, DATAD==DATAJ)[1,2]), (filter(resultsn20_dr, DATAD==DATAJ)[1,2]), (filter(resultsn21_dr, DATAD==DATAJ)[1,2]), (filter(resultsn22_dr, DATAD==DATAJ)[1,2]), (filter(resultsn23_dr, DATAD==DATAJ)[1,2]), 
             (filter(resultsn24_dr, DATAD==DATAJ)[1,2]))))

names(r_dr)[1] <- "Rzeczywista_wartosc_zapotrzebowania"
#row.names(j)[1] <- "Zapotrzebowanie1"
#row.names(j)[2] <- "Zapotrzebowanie2"
#row.names(j)[3] <- "Zapotrzebowanie3"
#row.names(j)[4] <- "Zapotrzebowanie4"
#row.names(j)[5] <- "Zapotrzebowanie5"
#row.names(j)[6] <- "Zapotrzebowanie6"
#row.names(j)[7] <- "Zapotrzebowanie7"
#row.names(j)[8] <- "Zapotrzebowanie8"
#row.names(j)[9] <- "Zapotrzebowanie9"
#row.names(j)[10] <- "Zapotrzebowanie10"
#row.names(j)[11] <- "Zapotrzebowanie11"
#row.names(j)[12] <- "Zapotrzebowanie12"
#row.names(j)[13] <- "Zapotrzebowanie13"
#row.names(j)[14] <- "Zapotrzebowanie14"
#row.names(j)[15] <- "Zapotrzebowanie15"
#row.names(j)[16] <- "Zapotrzebowanie16"
#row.names(j)[17] <- "Zapotrzebowanie17"
#row.names(j)[18] <- "Zapotrzebowanie18"
#row.names(j)[19] <- "Zapotrzebowanie19"
#row.names(j)[20] <- "Zapotrzebowanie20"
#row.names(j)[21] <- "Zapotrzebowanie21"
#row.names(j)[22] <- "Zapotrzebowanie22"
#row.names(j)[23] <- "Zapotrzebowanie23"
#row.names(j)[24] <- "Zapotrzebowanie24"


r_dr <- cbind(Godzina=c(1:24), r_dr)

p_dr=data.frame(t(data.frame(cbind(filter(resultsn1_dr, DATAD==DATAJ)[1,3]), (filter(resultsn2_dr, DATAD==DATAJ)[1,3]), (filter(resultsn3_dr, DATAD==DATAJ)[1,3]), (filter(resultsn4_dr, DATAD==DATAJ)[1,3]), 
             (filter(resultsn5_dr, DATAD==DATAJ)[1,3]), (filter(resultsn6_dr, DATAD==DATAJ)[1,3]), (filter(resultsn7_dr, DATAD==DATAJ)[1,3]), (filter(resultsn8_dr, DATAD==DATAJ)[1,3]), (filter(resultsn9_dr, DATAD==DATAJ)[1,3]), 
             (filter(resultsn10_dr, DATAD==DATAJ)[1,3]), (filter(resultsn11_dr, DATAD==DATAJ)[1,3]),  (filter(resultsn12_dr, DATAD==DATAJ)[1,3]), (filter(resultsn13_dr, DATAD==DATAJ)[1,3]), 
             (filter(resultsn14_dr, DATAD==DATAJ)[1,3]), (filter(resultsn15_dr, DATAD==DATAJ)[1,3]), (filter(resultsn16_dr, DATAD==DATAJ)[1,3]), (filter(resultsn17_dr, DATAD==DATAJ)[1,3]), (filter(resultsn18_dr, DATAD==DATAJ)[1,3]), 
             (filter(resultsn19_dr, DATAD==DATAJ)[1,3]), (filter(resultsn20_dr, DATAD==DATAJ)[1,3]), (filter(resultsn21_dr, DATAD==DATAJ)[1,3]), (filter(resultsn22_dr, DATAD==DATAJ)[1,3]), (filter(resultsn23_dr, DATAD==DATAJ)[1,3]), 
             (filter(resultsn24_dr, DATAD==DATAJ)[1,3]))))

r_dr <- cbind(r_dr, p_dr[,1])
names(r_dr)[3] <- "Prognozowana_wartosc_zapotrzebowania"

library("ggplot2")
library("ggalt")
ggplot(data=r_dr, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania)) + geom_xspline(color='steelblue', size=1.05, lineend = "round") + geom_line(data=r_dr, 
aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), linetype=2, color='violetred', size=1.05) +
  xlab("Godzina doby") + ylab("Zapotrzebowanie [MW]") + 
  ggtitle("Porównanie rzeczywistego i prognozowanego zu¿ycia energii") +
  theme(plot.title = element_text(hjust = 0.5, face="italic")) + scale_x_discrete(limit = c(1:24)) +
  theme(panel.background = element_rect(fill = 'azure2', colour = "#6D9EC1",
                                  size = 2, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white"))  #+ annotate("text", x = 14, y = 1000, label = "Rzeczywista wartoœæ zapotrzebowania", color="pink")

predict_net_test_start1_dr <- predict_net_test1_dr$net.result*(max(df$ED1)-min(df$ED1))+min(df$ED1)
predict_net_test_start2_dr <- predict_net_test2_dr$net.result*(max(df$ED2)-min(df$ED2))+min(df$ED2)
predict_net_test_start3_dr <- predict_net_test3_dr$net.result*(max(df$ED3)-min(df$ED3))+min(df$ED3)
predict_net_test_start4_dr <- predict_net_test4_dr$net.result*(max(df$ED4)-min(df$ED4))+min(df$ED4)
predict_net_test_start5_dr <- predict_net_test5_dr$net.result*(max(df$ED5)-min(df$ED5))+min(df$ED5)
predict_net_test_start6_dr <- predict_net_test6_dr$net.result*(max(df$ED6)-min(df$ED6))+min(df$ED6)
predict_net_test_start7_dr <- predict_net_test7_dr$net.result*(max(df$ED7)-min(df$ED7))+min(df$ED7)
predict_net_test_start8_dr <- predict_net_test8_dr$net.result*(max(df$ED8)-min(df$ED8))+min(df$ED8)
predict_net_test_start9_dr <- predict_net_test9_dr$net.result*(max(df$ED9)-min(df$ED9))+min(df$ED9)
predict_net_test_start10_dr <- predict_net_test10_dr$net.result*(max(df$ED10)-min(df$ED10))+min(df$ED10)
predict_net_test_start11_dr <- predict_net_test11_dr$net.result*(max(df$ED11)-min(df$ED11))+min(df$ED11)
predict_net_test_start12_dr <- predict_net_test12_dr$net.result*(max(df$ED12)-min(df$ED12))+min(df$ED12)
predict_net_test_start13_dr <- predict_net_test13_dr$net.result*(max(df$ED13)-min(df$ED13))+min(df$ED13)
predict_net_test_start14_dr <- predict_net_test14_dr$net.result*(max(df$ED14)-min(df$ED14))+min(df$ED14)
predict_net_test_start15_dr <- predict_net_test15_dr$net.result*(max(df$ED15)-min(df$ED15))+min(df$ED15)
predict_net_test_start16_dr <- predict_net_test16_dr$net.result*(max(df$ED16)-min(df$ED16))+min(df$ED16)
predict_net_test_start17_dr <- predict_net_test17_dr$net.result*(max(df$ED17)-min(df$ED17))+min(df$ED17)
predict_net_test_start18_dr <- predict_net_test18_dr$net.result*(max(df$ED18)-min(df$ED18))+min(df$ED18)
predict_net_test_start19_dr <- predict_net_test19_dr$net.result*(max(df$ED19)-min(df$ED19))+min(df$ED19)
predict_net_test_start20_dr <- predict_net_test20_dr$net.result*(max(df$ED20)-min(df$ED20))+min(df$ED20)
predict_net_test_start21_dr <- predict_net_test21_dr$net.result*(max(df$ED21)-min(df$ED21))+min(df$ED21)
predict_net_test_start22_dr <- predict_net_test22_dr$net.result*(max(df$ED22)-min(df$ED22))+min(df$ED22)
predict_net_test_start23_dr <- predict_net_test23_dr$net.result*(max(df$ED23)-min(df$ED23))+min(df$ED23)
predict_net_test_start24_dr <- predict_net_test24_dr$net.result*(max(df$ED24)-min(df$ED24))+min(df$ED24)

test_start1_dr <- as.data.frame((test_data_dr$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1))
test_start2_dr <- as.data.frame((test_data_dr$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
test_start3_dr <- as.data.frame((test_data_dr$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
test_start4_dr <- as.data.frame((test_data_dr$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
test_start5_dr <- as.data.frame((test_data_dr$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
test_start6_dr <- as.data.frame((test_data_dr$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
test_start7_dr <- as.data.frame((test_data_dr$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
test_start8_dr <- as.data.frame((test_data_dr$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
test_start9_dr <- as.data.frame((test_data_dr$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
test_start10_dr <- as.data.frame((test_data_dr$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
test_start11_dr <- as.data.frame((test_data_dr$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
test_start12_dr <- as.data.frame((test_data_dr$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
test_start13_dr <- as.data.frame((test_data_dr$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
test_start14_dr <- as.data.frame((test_data_dr$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
test_start15_dr <- as.data.frame((test_data_dr$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
test_start16_dr <- as.data.frame((test_data_dr$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
test_start17_dr <- as.data.frame((test_data_dr$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
test_start18_dr <- as.data.frame((test_data_dr$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
test_start19_dr <- as.data.frame((test_data_dr$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
test_start20_dr <- as.data.frame((test_data_dr$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
test_start21_dr <- as.data.frame((test_data_dr$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
test_start22_dr <- as.data.frame((test_data_dr$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
test_start23_dr <- as.data.frame((test_data_dr$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
test_start24_dr <- as.data.frame((test_data_dr$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24))

MSE.net_data1_dr <- sum((test_start1_dr-predict_net_test_start1_dr)^2/nrow(test_start1_dr))
MSE.net_data2_dr <- sum((test_start2_dr-predict_net_test_start2_dr)^2/nrow(test_start2_dr))
MSE.net_data3_dr <- sum((test_start3_dr-predict_net_test_start3_dr)^2/nrow(test_start3_dr))
MSE.net_data4_dr <- sum((test_start4_dr-predict_net_test_start4_dr)^2/nrow(test_start4_dr))
MSE.net_data5_dr <- sum((test_start5_dr-predict_net_test_start5_dr)^2/nrow(test_start5_dr))
MSE.net_data6_dr <- sum((test_start6_dr-predict_net_test_start6_dr)^2/nrow(test_start6_dr))
MSE.net_data7_dr <- sum((test_start7_dr-predict_net_test_start7_dr)^2/nrow(test_start7_dr))
MSE.net_data8_dr <- sum((test_start8_dr-predict_net_test_start8_dr)^2/nrow(test_start8_dr))
MSE.net_data9_dr <- sum((test_start9_dr-predict_net_test_start9_dr)^2/nrow(test_start9_dr))
MSE.net_data10_dr <- sum((test_start10_dr-predict_net_test_start10_dr)^2/nrow(test_start10_dr))
MSE.net_data11_dr <- sum((test_start11_dr-predict_net_test_start11_dr)^2/nrow(test_start11_dr))
MSE.net_data12_dr <- sum((test_start12_dr-predict_net_test_start12_dr)^2/nrow(test_start12_dr))
MSE.net_data13_dr <- sum((test_start13_dr-predict_net_test_start13_dr)^2/nrow(test_start13_dr))
MSE.net_data14_dr <- sum((test_start14_dr-predict_net_test_start14_dr)^2/nrow(test_start14_dr))
MSE.net_data15_dr <- sum((test_start15_dr-predict_net_test_start15_dr)^2/nrow(test_start15_dr))
MSE.net_data16_dr <- sum((test_start16_dr-predict_net_test_start16_dr)^2/nrow(test_start16_dr))
MSE.net_data17_dr <- sum((test_start17_dr-predict_net_test_start17_dr)^2/nrow(test_start17_dr))
MSE.net_data18_dr <- sum((test_start18_dr-predict_net_test_start18_dr)^2/nrow(test_start18_dr))
MSE.net_data19_dr <- sum((test_start19_dr-predict_net_test_start19_dr)^2/nrow(test_start19_dr))
MSE.net_data20_dr <- sum((test_start20_dr-predict_net_test_start20_dr)^2/nrow(test_start20_dr))
MSE.net_data21_dr <- sum((test_start21_dr-predict_net_test_start21_dr)^2/nrow(test_start21_dr))
MSE.net_data22_dr <- sum((test_start22_dr-predict_net_test_start22_dr)^2/nrow(test_start22_dr))
MSE.net_data23_dr <- sum((test_start23_dr-predict_net_test_start23_dr)^2/nrow(test_start23_dr))
MSE.net_data24_dr <- sum((test_start24_dr-predict_net_test_start24_dr)^2/nrow(test_start24_dr))

#ponizej suma mse dla poszzcegolnych godzin dla tych wszystkich dat
MSE=data.frame(t(data.frame(cbind(MSE.net_data1_dr, MSE.net_data2_dr, MSE.net_data3_dr, 
                     MSE.net_data4_dr, MSE.net_data5_dr, MSE.net_data6_dr, 
                     MSE.net_data7_dr, MSE.net_data8_dr, MSE.net_data9_dr, 
                     MSE.net_data10_dr, MSE.net_data11_dr, MSE.net_data12_dr, 
                     MSE.net_data13_dr, MSE.net_data14_dr, MSE.net_data15_dr, 
                     MSE.net_data16_dr, MSE.net_data17_dr, MSE.net_data18_dr, 
                     MSE.net_data19_dr, MSE.net_data20_dr, MSE.net_data21_dr, 
                     MSE.net_data22_dr, MSE.net_data23_dr, MSE.net_data24_dr))))

names(MSE)[1] <- "MSE"


ME.net_data <- cbind(test_data_dr[,1], 
                     (test_start1_dr-predict_net_test_start1_dr), 
                     (test_start2_dr-predict_net_test_start2_dr), 
                     (test_start3_dr-predict_net_test_start3_dr), 
                     (test_start4_dr-predict_net_test_start4_dr), 
                     (test_start5_dr-predict_net_test_start5_dr), 
                     (test_start6_dr-predict_net_test_start6_dr), 
                     (test_start7_dr-predict_net_test_start7_dr), 
                     (test_start8_dr-predict_net_test_start8_dr), 
                     (test_start9_dr-predict_net_test_start9_dr), 
                     (test_start10_dr-predict_net_test_start10_dr), 
                     (test_start11_dr-predict_net_test_start11_dr), 
                     (test_start12_dr-predict_net_test_start12_dr), 
                     (test_start13_dr-predict_net_test_start13_dr), 
                     (test_start14_dr-predict_net_test_start14_dr), 
                     (test_start15_dr-predict_net_test_start15_dr), 
                     (test_start16_dr-predict_net_test_start16_dr), 
                     (test_start17_dr-predict_net_test_start17_dr), 
                     (test_start18_dr-predict_net_test_start18_dr), 
                     (test_start19_dr-predict_net_test_start19_dr), 
                     (test_start20_dr-predict_net_test_start20_dr), 
                     (test_start21_dr-predict_net_test_start21_dr), 
                     (test_start22_dr-predict_net_test_start22_dr), 
                     (test_start23_dr-predict_net_test_start23_dr), 
                     (test_start24_dr-predict_net_test_start24_dr))
ME.net_data$"ME [MW]" <-(rowSums(ME.net_data[,2:25]))/24

names(ME.net_data)[1] <- "ME"



RMSE.net_data <- cbind(test_data_dr[,1], 
                       (test_start1_dr-predict_net_test_start1_dr)^2, 
                       (test_start2_dr-predict_net_test_start2_dr)^2, 
                       (test_start3_dr-predict_net_test_start3_dr)^2, 
                       (test_start4_dr-predict_net_test_start4_dr)^2, 
                       (test_start5_dr-predict_net_test_start5_dr)^2, 
                       (test_start6_dr-predict_net_test_start6_dr)^2, 
                       (test_start7_dr-predict_net_test_start7_dr)^2, 
                       (test_start8_dr-predict_net_test_start8_dr)^2, 
                       (test_start9_dr-predict_net_test_start9_dr)^2, 
                       (test_start10_dr-predict_net_test_start10_dr)^2, 
                       (test_start11_dr-predict_net_test_start11_dr)^2, 
                       (test_start12_dr-predict_net_test_start12_dr)^2, 
                       (test_start13_dr-predict_net_test_start13_dr)^2, 
                       (test_start14_dr-predict_net_test_start14_dr)^2, 
                       (test_start15_dr-predict_net_test_start15_dr)^2, 
                       (test_start16_dr-predict_net_test_start16_dr)^2, 
                       (test_start17_dr-predict_net_test_start17_dr)^2, 
                       (test_start18_dr-predict_net_test_start18_dr)^2, 
                       (test_start19_dr-predict_net_test_start19_dr)^2, 
                       (test_start20_dr-predict_net_test_start20_dr)^2, 
                       (test_start21_dr-predict_net_test_start21_dr)^2, 
                       (test_start22_dr-predict_net_test_start22_dr)^2, 
                       (test_start23_dr-predict_net_test_start23_dr)^2, 
                       (test_start24_dr-predict_net_test_start24_dr)^2)
RMSE.net_data$"RMSE [MW]" <-sqrt(rowSums(RMSE.net_data[,2:25])/24)

names(RMSE.net_data)[1] <- "RMSE"



MPE.net_data <- cbind(test_data_dr[,1], 
                      (test_start1_dr-predict_net_test_start1_dr)/(test_start1_dr), 
                      (test_start2_dr-predict_net_test_start2_dr)/(test_start2_dr), 
                      (test_start3_dr-predict_net_test_start3_dr)/(test_start3_dr), 
                      (test_start4_dr-predict_net_test_start4_dr)/(test_start4_dr), 
                      (test_start5_dr-predict_net_test_start5_dr)/(test_start5_dr), 
                      (test_start6_dr-predict_net_test_start6_dr)/(test_start6_dr), 
                      (test_start7_dr-predict_net_test_start7_dr)/(test_start7_dr), 
                      (test_start8_dr-predict_net_test_start8_dr)/(test_start8_dr), 
                      (test_start9_dr-predict_net_test_start9_dr)/(test_start9_dr), 
                      (test_start10_dr-predict_net_test_start10_dr)/(test_start10_dr), 
                      (test_start11_dr-predict_net_test_start11_dr)/(test_start11_dr), 
                      (test_start12_dr-predict_net_test_start12_dr)/(test_start12_dr), 
                      (test_start13_dr-predict_net_test_start13_dr)/(test_start13_dr), 
                      (test_start14_dr-predict_net_test_start14_dr)/(test_start14_dr), 
                      (test_start15_dr-predict_net_test_start15_dr)/(test_start15_dr), 
                      (test_start16_dr-predict_net_test_start16_dr)/(test_start16_dr), 
                      (test_start17_dr-predict_net_test_start17_dr)/(test_start17_dr), 
                      (test_start18_dr-predict_net_test_start18_dr)/(test_start18_dr), 
                      (test_start19_dr-predict_net_test_start19_dr)/(test_start19_dr), 
                      (test_start20_dr-predict_net_test_start20_dr)/(test_start20_dr), 
                      (test_start21_dr-predict_net_test_start21_dr)/(test_start21_dr), 
                      (test_start22_dr-predict_net_test_start22_dr)/(test_start22_dr), 
                      (test_start23_dr-predict_net_test_start23_dr)/(test_start23_dr), 
                      (test_start24_dr-predict_net_test_start24_dr)/(test_start24_dr))

MPE.net_data$"MPE [%]" <-(rowSums(MPE.net_data[,2:25]))/24*100

names(MPE.net_data)[1] <- "MPE"

MAE.net_data <- cbind(test_data_dr[,1], 
                      abs(test_start1_dr-predict_net_test_start1_dr), 
                      abs(test_start2_dr-predict_net_test_start2_dr), 
                      abs(test_start3_dr-predict_net_test_start3_dr), 
                      abs(test_start4_dr-predict_net_test_start4_dr), 
                      abs(test_start5_dr-predict_net_test_start5_dr), 
                      abs(test_start6_dr-predict_net_test_start6_dr), 
                      abs(test_start7_dr-predict_net_test_start7_dr), 
                      abs(test_start8_dr-predict_net_test_start8_dr), 
                      abs(test_start9_dr-predict_net_test_start9_dr), 
                      abs(test_start10_dr-predict_net_test_start10_dr), 
                      abs(test_start11_dr-predict_net_test_start11_dr), 
                      abs(test_start12_dr-predict_net_test_start12_dr), 
                      abs(test_start13_dr-predict_net_test_start13_dr), 
                      abs(test_start14_dr-predict_net_test_start14_dr), 
                      abs(test_start15_dr-predict_net_test_start15_dr), 
                      abs(test_start16_dr-predict_net_test_start16_dr), 
                      abs(test_start17_dr-predict_net_test_start17_dr), 
                      abs(test_start18_dr-predict_net_test_start18_dr), 
                      abs(test_start19_dr-predict_net_test_start19_dr), 
                      abs(test_start20_dr-predict_net_test_start20_dr), 
                      abs(test_start21_dr-predict_net_test_start21_dr), 
                      abs(test_start22_dr-predict_net_test_start22_dr), 
                      abs(test_start23_dr-predict_net_test_start23_dr), 
                      abs(test_start24_dr-predict_net_test_start24_dr))

MAE.net_data$"MAE [MW]" <-(rowSums(MAE.net_data[,2:25]))/24

names(MAE.net_data)[1] <- "MAE"


MAPE.net_data <- cbind(test_data_dr[,1], 
                       abs((test_start1_dr-predict_net_test_start1_dr)/(test_start1_dr)), 
                       abs((test_start2_dr-predict_net_test_start2_dr)/(test_start2_dr)), 
                       abs((test_start3_dr-predict_net_test_start3_dr)/(test_start3_dr)), 
                       abs((test_start4_dr-predict_net_test_start4_dr)/(test_start4_dr)), 
                       abs((test_start5_dr-predict_net_test_start5_dr)/(test_start5_dr)), 
                       abs((test_start6_dr-predict_net_test_start6_dr)/(test_start6_dr)), 
                       abs((test_start7_dr-predict_net_test_start7_dr)/(test_start7_dr)), 
                       abs((test_start8_dr-predict_net_test_start8_dr)/(test_start8_dr)), 
                       abs((test_start9_dr-predict_net_test_start9_dr)/(test_start9_dr)), 
                       abs((test_start10_dr-predict_net_test_start10_dr)/(test_start10_dr)), 
                       abs((test_start11_dr-predict_net_test_start11_dr)/(test_start11_dr)), 
                       abs((test_start12_dr-predict_net_test_start12_dr)/(test_start12_dr)), 
                       abs((test_start13_dr-predict_net_test_start13_dr)/(test_start13_dr)), 
                       abs((test_start14_dr-predict_net_test_start14_dr)/(test_start14_dr)), 
                       abs((test_start15_dr-predict_net_test_start15_dr)/(test_start15_dr)), 
                       abs((test_start16_dr-predict_net_test_start16_dr)/(test_start16_dr)), 
                       abs((test_start17_dr-predict_net_test_start17_dr)/(test_start17_dr)), 
                       abs((test_start18_dr-predict_net_test_start18_dr)/(test_start18_dr)), 
                       abs((test_start19_dr-predict_net_test_start19_dr)/(test_start19_dr)), 
                       abs((test_start20_dr-predict_net_test_start20_dr)/(test_start20_dr)), 
                       abs((test_start21_dr-predict_net_test_start21_dr)/(test_start21_dr)), 
                       abs((test_start22_dr-predict_net_test_start22_dr)/(test_start22_dr)), 
                       abs((test_start23_dr-predict_net_test_start23_dr)/(test_start23_dr)), 
                       abs((test_start24_dr-predict_net_test_start24_dr)/(test_start24_dr)))
                       
                       MAPE.net_data$"MAPE [%]" <-(rowSums(MAPE.net_data[,2:25]))/24*100
                       
                       names(MAPE.net_data)[1] <- "MAPE"
                       
errors.net_data <- cbind(test_data_dr[,1], ME.net_data[,26], RMSE.net_data[,26], MPE.net_data[,26], MAE.net_data[,26], MAPE.net_data[,26], abs((test_start1_dr-predict_net_test_start1_dr)/(test_start1_dr)))                    
colnames(errors.net_data) <- c("Data","ME [MW]", "RMSE [MW]", "MPE [%]", "MAE [MW]", "MAPE [%]")
errors.net_data <- errors.net_data[,1:6]

##########################Poniedzia³ek
df_pon=data.frame(df[df$PON==1 & df$NT!=1,])

max_data_pon <- apply(df_pon[,8:103], 2, max)
min_data_pon <- apply(df_pon[,8:103], 2, min)
data_scaled_pon <- as.data.frame(scale(df_pon[,8:103], center=min_data_pon, scale=max_data_pon - min_data_pon))
data_scaled_pon <- cbind(df_pon[,1:7], data_scaled_pon)
train_data_pon=as.data.frame(data_scaled_pon[1:298,])
test_data_pon=as.data.frame(data_scaled_pon[299:nrow(data_scaled_pon),])


index_pon=sample(1:nrow(df_pon), round(0.70*nrow(df_pon)))
train_data_pon <-as.data.frame(data_scaled_pon[index_pon,])
test_data_pon <-as.data.frame(data_scaled_pon[-index_pon,])

install.packages("neuralnet")
library("neuralnet")

net_data1_pon <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data2_pon <-neuralnet(ED2~TEMP2+ZACHM2+OPAD2, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data3_pon <-neuralnet(ED3~TEMP3+ZACHM3+OPAD3, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data4_pon <-neuralnet(ED4~TEMP4+ZACHM4+OPAD4, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data5_pon <-neuralnet(ED5~TEMP5+ZACHM5+OPAD5, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data6_pon <-neuralnet(ED6~TEMP6+ZACHM6+OPAD6, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data7_pon <-neuralnet(ED7~TEMP7+ZACHM7+OPAD7, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data8_pon <-neuralnet(ED8~TEMP8+ZACHM8+OPAD8, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data9_pon <-neuralnet(ED9~TEMP9+ZACHM9+OPAD9, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data10_pon <-neuralnet(ED10~TEMP10+ZACHM10+OPAD10, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data11_pon <-neuralnet(ED11~TEMP11+ZACHM11+OPAD11, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data12_pon <-neuralnet(ED12~TEMP12+ZACHM12+OPAD12, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data13_pon <-neuralnet(ED13~TEMP13+ZACHM13+OPAD13, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data14_pon <-neuralnet(ED14~TEMP14+ZACHM14+OPAD14, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data15_pon <-neuralnet(ED15~TEMP15+ZACHM15+OPAD15, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data16_pon <-neuralnet(ED16~TEMP16+ZACHM16+OPAD16, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data17_pon <-neuralnet(ED17~TEMP17+ZACHM17+OPAD17, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data18_pon <-neuralnet(ED18~TEMP18+ZACHM18+OPAD18, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data19_pon <-neuralnet(ED19~TEMP19+ZACHM19+OPAD19, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data20_pon <-neuralnet(ED20~TEMP20+ZACHM20+OPAD20, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data21_pon <-neuralnet(ED21~TEMP21+ZACHM21+OPAD21, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data22_pon <-neuralnet(ED22~TEMP22+ZACHM22+OPAD22, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data23_pon <-neuralnet(ED23~TEMP23+ZACHM23+OPAD23, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)
net_data24_pon <-neuralnet(ED24~TEMP24+ZACHM24+OPAD24, data=train_data_pon, hidden=10, linear.output=TRUE, threshold=0.01)

net_data1_pon$result.matrix
net_data2_pon$result.matrix
net_data3_pon$result.matrix
net_data4_pon$result.matrix
net_data5_pon$result.matrix
net_data6_pon$result.matrix
net_data7_pon$result.matrix
net_data8_pon$result.matrix
net_data9_pon$result.matrix
net_data10_pon$result.matrix
net_data11_pon$result.matrix
net_data12_pon$result.matrix
net_data13_pon$result.matrix
net_data14_pon$result.matrix
net_data15_pon$result.matrix
net_data16_pon$result.matrix
net_data17_pon$result.matrix
net_data18_pon$result.matrix
net_data19_pon$result.matrix
net_data20_pon$result.matrix
net_data21_pon$result.matrix
net_data22_pon$result.matrix
net_data23_pon$result.matrix
net_data24_pon$result.matrix

plot(net_data1_pon)
plot(net_data2_pon)
plot(net_data3_pon)
plot(net_data4_pon)
plot(net_data5_pon)
plot(net_data6_pon)
plot(net_data7_pon)
plot(net_data8_pon)
plot(net_data9_pon)
plot(net_data10_pon)
plot(net_data11_pon)
plot(net_data12_pon)
plot(net_data13_pon)
plot(net_data14_pon)
plot(net_data15_pon)
plot(net_data16_pon)
plot(net_data17_pon)
plot(net_data18_pon)
plot(net_data19_pon)
plot(net_data20_pon)
plot(net_data21_pon)
plot(net_data22_pon)
plot(net_data23_pon)
plot(net_data24_pon)

library("NeuralNetTools")

garson(net_data1_pon)
garson(net_data2_pon)
garson(net_data3_pon)
garson(net_data4_pon)
garson(net_data5_pon)
garson(net_data6_pon)
garson(net_data7_pon)
garson(net_data8_pon)
garson(net_data9_pon)
garson(net_data10_pon)
garson(net_data11_pon)
garson(net_data12_pon)
garson(net_data13_pon)
garson(net_data14_pon)
garson(net_data15_pon)
garson(net_data16_pon)
garson(net_data17_pon)
garson(net_data18_pon)
garson(net_data19_pon)
garson(net_data20_pon)
garson(net_data21_pon)
garson(net_data22_pon)
garson(net_data23_pon)
garson(net_data24_pon)

predict_net_test1_pon <- (compute(net_data1_pon, test_data_pon[,8:103]))
predict_net_test2_pon <- compute(net_data2_pon, test_data_pon[,8:103])
predict_net_test3_pon <- compute(net_data3_pon, test_data_pon[,8:103])
predict_net_test4_pon <- compute(net_data4_pon, test_data_pon[,8:103])
predict_net_test5_pon <- compute(net_data5_pon, test_data_pon[,8:103])
predict_net_test6_pon <- compute(net_data6_pon, test_data_pon[,8:103])
predict_net_test7_pon <- compute(net_data7_pon, test_data_pon[,8:103])
predict_net_test8_pon <- compute(net_data8_pon, test_data_pon[,8:103])
predict_net_test9_pon <- compute(net_data9_pon, test_data_pon[,8:103])
predict_net_test10_pon <- compute(net_data10_pon, test_data_pon[,8:103])
predict_net_test11_pon <- compute(net_data11_pon, test_data_pon[,8:103])
predict_net_test12_pon <- compute(net_data12_pon, test_data_pon[,8:103])
predict_net_test13_pon <- compute(net_data13_pon, test_data_pon[,8:103])
predict_net_test14_pon <- compute(net_data14_pon, test_data_pon[,8:103])
predict_net_test15_pon <- compute(net_data15_pon, test_data_pon[,8:103])
predict_net_test16_pon <- compute(net_data16_pon, test_data_pon[,8:103])
predict_net_test17_pon <- compute(net_data17_pon, test_data_pon[,8:103])
predict_net_test18_pon <- compute(net_data18_pon, test_data_pon[,8:103])
predict_net_test19_pon <- compute(net_data19_pon, test_data_pon[,8:103])
predict_net_test20_pon <- compute(net_data20_pon, test_data_pon[,8:103])
predict_net_test21_pon <- compute(net_data21_pon, test_data_pon[,8:103])
predict_net_test22_pon <- compute(net_data22_pon, test_data_pon[,8:103])
predict_net_test23_pon <- compute(net_data23_pon, test_data_pon[,8:103])
predict_net_test24_pon <- compute(net_data24_pon, test_data_pon[,8:103])

resultsn1_pon <- data.frame(DATAD=test_data_pon$DATA, actual1=(test_data_pon$ED1)*(max(df_pon$ED1)-min(df_pon$ED1))+min(df_pon$ED1), prediction1=(predict_net_test1_pon$net.result)*(max(df_pon$ED1)-min(df_pon$ED1))+min(df_pon$ED1), TEMPERATURA1=test_data_pon$TEMP1*(max(df_pon$TEMP1)-min(df_pon$TEMP1))+min(df_pon$TEMP1))
resultsn2_pon <- data.frame(DATAD=test_data_pon$DATA, actual2=(test_data_pon$ED2)*(max(df_pon$ED2)-min(df_pon$ED2))+min(df_pon$ED2), prediction2=(predict_net_test2_pon$net.result)*(max(df_pon$ED2)-min(df_pon$ED2))+min(df_pon$ED2))
resultsn3_pon <- data.frame(DATAD=test_data_pon$DATA, actual3=(test_data_pon$ED3)*(max(df_pon$ED3)-min(df_pon$ED3))+min(df_pon$ED3), prediction3=(predict_net_test3_pon$net.result)*(max(df_pon$ED3)-min(df_pon$ED3))+min(df_pon$ED3))
resultsn4_pon <- data.frame(DATAD=test_data_pon$DATA, actual4=(test_data_pon$ED4)*(max(df_pon$ED4)-min(df_pon$ED4))+min(df_pon$ED4), prediction4=(predict_net_test4_pon$net.result)*(max(df_pon$ED4)-min(df_pon$ED4))+min(df_pon$ED4))
resultsn5_pon <- data.frame(DATAD=test_data_pon$DATA, actual5=(test_data_pon$ED5)*(max(df_pon$ED5)-min(df_pon$ED5))+min(df_pon$ED5), prediction5=(predict_net_test5_pon$net.result)*(max(df_pon$ED5)-min(df_pon$ED5))+min(df_pon$ED5))
resultsn6_pon <- data.frame(DATAD=test_data_pon$DATA, actual6=(test_data_pon$ED6)*(max(df_pon$ED6)-min(df_pon$ED6))+min(df_pon$ED6), prediction6=(predict_net_test6_pon$net.result)*(max(df_pon$ED6)-min(df_pon$ED6))+min(df_pon$ED6))
resultsn7_pon <- data.frame(DATAD=test_data_pon$DATA, actual7=(test_data_pon$ED7)*(max(df_pon$ED7)-min(df_pon$ED7))+min(df_pon$ED7), prediction7=(predict_net_test7_pon$net.result)*(max(df_pon$ED7)-min(df_pon$ED7))+min(df_pon$ED7))
resultsn8_pon <- data.frame(DATAD=test_data_pon$DATA, actual8=(test_data_pon$ED8)*(max(df_pon$ED8)-min(df_pon$ED8))+min(df_pon$ED8), prediction8=(predict_net_test8_pon$net.result)*(max(df_pon$ED8)-min(df_pon$ED8))+min(df_pon$ED8))
resultsn9_pon <- data.frame(DATAD=test_data_pon$DATA, actual9=(test_data_pon$ED9)*(max(df_pon$ED9)-min(df_pon$ED9))+min(df_pon$ED9), prediction9=(predict_net_test9_pon$net.result)*(max(df_pon$ED9)-min(df_pon$ED9))+min(df_pon$ED9))
resultsn10_pon <- data.frame(DATAD=test_data_pon$DATA, actual10=(test_data_pon$ED10)*(max(df_pon$ED10)-min(df_pon$ED10))+min(df_pon$ED10), prediction10=(predict_net_test10_pon$net.result)*(max(df_pon$ED10)-min(df_pon$ED10))+min(df_pon$ED10))
resultsn11_pon <- data.frame(DATAD=test_data_pon$DATA, actual11=(test_data_pon$ED11)*(max(df_pon$ED11)-min(df_pon$ED11))+min(df_pon$ED11), prediction11=(predict_net_test11_pon$net.result)*(max(df_pon$ED11)-min(df_pon$ED11))+min(df_pon$ED11))
resultsn12_pon <- data.frame(DATAD=test_data_pon$DATA, actual12=(test_data_pon$ED12)*(max(df_pon$ED12)-min(df_pon$ED12))+min(df_pon$ED12), prediction12=(predict_net_test12_pon$net.result)*(max(df_pon$ED12)-min(df_pon$ED12))+min(df_pon$ED12))
resultsn13_pon <- data.frame(DATAD=test_data_pon$DATA, actual13=(test_data_pon$ED13)*(max(df_pon$ED13)-min(df_pon$ED13))+min(df_pon$ED13), prediction13=(predict_net_test13_pon$net.result)*(max(df_pon$ED13)-min(df_pon$ED13))+min(df_pon$ED13))
resultsn14_pon <- data.frame(DATAD=test_data_pon$DATA, actual14=(test_data_pon$ED14)*(max(df_pon$ED14)-min(df_pon$ED14))+min(df_pon$ED14), prediction14=(predict_net_test14_pon$net.result)*(max(df_pon$ED14)-min(df_pon$ED14))+min(df_pon$ED14))
resultsn15_pon <- data.frame(DATAD=test_data_pon$DATA, actual15=(test_data_pon$ED15)*(max(df_pon$ED15)-min(df_pon$ED15))+min(df_pon$ED15), prediction15=(predict_net_test15_pon$net.result)*(max(df_pon$ED15)-min(df_pon$ED15))+min(df_pon$ED15))
resultsn16_pon <- data.frame(DATAD=test_data_pon$DATA, actual16=(test_data_pon$ED16)*(max(df_pon$ED16)-min(df_pon$ED16))+min(df_pon$ED16), prediction16=(predict_net_test16_pon$net.result)*(max(df_pon$ED16)-min(df_pon$ED16))+min(df_pon$ED16))
resultsn17_pon <- data.frame(DATAD=test_data_pon$DATA, actual17=(test_data_pon$ED17)*(max(df_pon$ED17)-min(df_pon$ED17))+min(df_pon$ED17), prediction17=(predict_net_test17_pon$net.result)*(max(df_pon$ED17)-min(df_pon$ED17))+min(df_pon$ED17))
resultsn18_pon <- data.frame(DATAD=test_data_pon$DATA, actual18=(test_data_pon$ED18)*(max(df_pon$ED18)-min(df_pon$ED18))+min(df_pon$ED18), prediction18=(predict_net_test18_pon$net.result)*(max(df_pon$ED18)-min(df_pon$ED18))+min(df_pon$ED18))
resultsn19_pon <- data.frame(DATAD=test_data_pon$DATA, actual19=(test_data_pon$ED19)*(max(df_pon$ED19)-min(df_pon$ED19))+min(df_pon$ED19), prediction19=(predict_net_test19_pon$net.result)*(max(df_pon$ED19)-min(df_pon$ED19))+min(df_pon$ED19))
resultsn20_pon <- data.frame(DATAD=test_data_pon$DATA, actual20=(test_data_pon$ED20)*(max(df_pon$ED20)-min(df_pon$ED20))+min(df_pon$ED20), prediction20=(predict_net_test20_pon$net.result)*(max(df_pon$ED20)-min(df_pon$ED20))+min(df_pon$ED20))
resultsn21_pon <- data.frame(DATAD=test_data_pon$DATA, actual21=(test_data_pon$ED21)*(max(df_pon$ED21)-min(df_pon$ED21))+min(df_pon$ED21), prediction21=(predict_net_test21_pon$net.result)*(max(df_pon$ED21)-min(df_pon$ED21))+min(df_pon$ED21))
resultsn22_pon <- data.frame(DATAD=test_data_pon$DATA, actual22=(test_data_pon$ED22)*(max(df_pon$ED22)-min(df_pon$ED22))+min(df_pon$ED22), prediction22=(predict_net_test22_pon$net.result)*(max(df_pon$ED22)-min(df_pon$ED22))+min(df_pon$ED22))
resultsn23_pon <- data.frame(DATAD=test_data_pon$DATA, actual23=(test_data_pon$ED23)*(max(df_pon$ED23)-min(df_pon$ED23))+min(df_pon$ED23), prediction23=(predict_net_test23_pon$net.result)*(max(df_pon$ED23)-min(df_pon$ED23))+min(df_pon$ED23))
resultsn24_pon <- data.frame(DATAD=test_data_pon$DATA, actual24=(test_data_pon$ED24)*(max(df_pon$ED24)-min(df_pon$ED24))+min(df_pon$ED24), prediction24=(predict_net_test24_pon$net.result)*(max(df_pon$ED24)-min(df_pon$ED24))+min(df_pon$ED24))


#remove.packages("neuralnet")
install.packages("dplyr")
library("dplyr")
DATAJ='2018/12/31'
#j=filter(resultsn1_pon, DATAD=='2010/01/13')
r_pon=data.frame(t(data.frame(cbind(filter(resultsn1_pon, DATAD==DATAJ)[1,2]), (filter(resultsn2_pon, DATAD==DATAJ)[1,2]), (filter(resultsn3_pon, DATAD==DATAJ)[1,2]), (filter(resultsn4_pon, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn5_pon, DATAD==DATAJ)[1,2]), (filter(resultsn6_pon, DATAD==DATAJ)[1,2]), (filter(resultsn7_pon, DATAD==DATAJ)[1,2]), (filter(resultsn8_pon, DATAD==DATAJ)[1,2]), (filter(resultsn9_pon, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn10_pon, DATAD==DATAJ)[1,2]), (filter(resultsn11_pon, DATAD==DATAJ)[1,2]),  (filter(resultsn12_pon, DATAD==DATAJ)[1,2]), (filter(resultsn13_pon, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn14_pon, DATAD==DATAJ)[1,2]), (filter(resultsn15_pon, DATAD==DATAJ)[1,2]), (filter(resultsn16_pon, DATAD==DATAJ)[1,2]), (filter(resultsn17_pon, DATAD==DATAJ)[1,2]), (filter(resultsn18_pon, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn19_pon, DATAD==DATAJ)[1,2]), (filter(resultsn20_pon, DATAD==DATAJ)[1,2]), (filter(resultsn21_pon, DATAD==DATAJ)[1,2]), (filter(resultsn22_pon, DATAD==DATAJ)[1,2]), (filter(resultsn23_pon, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn24_pon, DATAD==DATAJ)[1,2]))))

names(r_pon)[1] <- "Rzeczywista_wartosc_zapotrzebowania"
#row.names(j)[1] <- "Zapotrzebowanie1"
row.names(j)[2] <- "Zapotrzebowanie2"
row.names(j)[3] <- "Zapotrzebowanie3"
row.names(j)[4] <- "Zapotrzebowanie4"
row.names(j)[5] <- "Zapotrzebowanie5"
row.names(j)[6] <- "Zapotrzebowanie6"
row.names(j)[7] <- "Zapotrzebowanie7"
row.names(j)[8] <- "Zapotrzebowanie8"
row.names(j)[9] <- "Zapotrzebowanie9"
row.names(j)[10] <- "Zapotrzebowanie10"
row.names(j)[11] <- "Zapotrzebowanie11"
row.names(j)[12] <- "Zapotrzebowanie12"
row.names(j)[13] <- "Zapotrzebowanie13"
row.names(j)[14] <- "Zapotrzebowanie14"
row.names(j)[15] <- "Zapotrzebowanie15"
row.names(j)[16] <- "Zapotrzebowanie16"
row.names(j)[17] <- "Zapotrzebowanie17"
row.names(j)[18] <- "Zapotrzebowanie18"
row.names(j)[19] <- "Zapotrzebowanie19"
row.names(j)[20] <- "Zapotrzebowanie20"
row.names(j)[21] <- "Zapotrzebowanie21"
row.names(j)[22] <- "Zapotrzebowanie22"
row.names(j)[23] <- "Zapotrzebowanie23"
row.names(j)[24] <- "Zapotrzebowanie24"


r_pon <- cbind(Godzina=c(1:24), r_pon)

p_pon=data.frame(t(data.frame(cbind(filter(resultsn1_pon, DATAD==DATAJ)[1,3]), (filter(resultsn2_pon, DATAD==DATAJ)[1,3]), (filter(resultsn3_pon, DATAD==DATAJ)[1,3]), (filter(resultsn4_pon, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn5_pon, DATAD==DATAJ)[1,3]), (filter(resultsn6_pon, DATAD==DATAJ)[1,3]), (filter(resultsn7_pon, DATAD==DATAJ)[1,3]), (filter(resultsn8_pon, DATAD==DATAJ)[1,3]), (filter(resultsn9_pon, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn10_pon, DATAD==DATAJ)[1,3]), (filter(resultsn11_pon, DATAD==DATAJ)[1,3]),  (filter(resultsn12_pon, DATAD==DATAJ)[1,3]), (filter(resultsn13_pon, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn14_pon, DATAD==DATAJ)[1,3]), (filter(resultsn15_pon, DATAD==DATAJ)[1,3]), (filter(resultsn16_pon, DATAD==DATAJ)[1,3]), (filter(resultsn17_pon, DATAD==DATAJ)[1,3]), (filter(resultsn18_pon, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn19_pon, DATAD==DATAJ)[1,3]), (filter(resultsn20_pon, DATAD==DATAJ)[1,3]), (filter(resultsn21_pon, DATAD==DATAJ)[1,3]), (filter(resultsn22_pon, DATAD==DATAJ)[1,3]), (filter(resultsn23_pon, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn24_pon, DATAD==DATAJ)[1,3]))))

r_pon <- cbind(r_pon, p_pon[,1])
names(r_pon)[3] <- "Prognozowana_wartosc_zapotrzebowania"

library("ggplot2")
library("ggalt")
ggplot(data=r_pon, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania)) + geom_xspline(color='steelblue', size=1.05, lineend = "round") + geom_line(data=r_pon, 
                                                                                                                                                      aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), linetype=2, color='violetred', size=1.05) +
  xlab("Godzina doby") + ylab("Zapotrzebowanie [MW]") + 
  ggtitle("Porównanie rzeczywistego i prognozowanego zu¿ycia energii") +
  theme(plot.title = element_text(hjust = 0.5, face="italic")) + scale_x_discrete(limit = c(1:24)) +
  theme(panel.background = element_rect(fill = 'azure2', colour = "#6D9EC1",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))  #+ annotate("text", x = 14, y = 1000, label = "Rzeczywista wartoœæ zapotrzebowania", color="pink")

predict_net_test_start1_pon <- predict_net_test1_pon$net.result*(max(df$ED1)-min(df$ED1))+min(df$ED1)
predict_net_test_start2_pon <- predict_net_test2_pon$net.result*(max(df$ED2)-min(df$ED2))+min(df$ED2)
predict_net_test_start3_pon <- predict_net_test3_pon$net.result*(max(df$ED3)-min(df$ED3))+min(df$ED3)
predict_net_test_start4_pon <- predict_net_test4_pon$net.result*(max(df$ED4)-min(df$ED4))+min(df$ED4)
predict_net_test_start5_pon <- predict_net_test5_pon$net.result*(max(df$ED5)-min(df$ED5))+min(df$ED5)
predict_net_test_start6_pon <- predict_net_test6_pon$net.result*(max(df$ED6)-min(df$ED6))+min(df$ED6)
predict_net_test_start7_pon <- predict_net_test7_pon$net.result*(max(df$ED7)-min(df$ED7))+min(df$ED7)
predict_net_test_start8_pon <- predict_net_test8_pon$net.result*(max(df$ED8)-min(df$ED8))+min(df$ED8)
predict_net_test_start9_pon <- predict_net_test9_pon$net.result*(max(df$ED9)-min(df$ED9))+min(df$ED9)
predict_net_test_start10_pon <- predict_net_test10_pon$net.result*(max(df$ED10)-min(df$ED10))+min(df$ED10)
predict_net_test_start11_pon <- predict_net_test11_pon$net.result*(max(df$ED11)-min(df$ED11))+min(df$ED11)
predict_net_test_start12_pon <- predict_net_test12_pon$net.result*(max(df$ED12)-min(df$ED12))+min(df$ED12)
predict_net_test_start13_pon <- predict_net_test13_pon$net.result*(max(df$ED13)-min(df$ED13))+min(df$ED13)
predict_net_test_start14_pon <- predict_net_test14_pon$net.result*(max(df$ED14)-min(df$ED14))+min(df$ED14)
predict_net_test_start15_pon <- predict_net_test15_pon$net.result*(max(df$ED15)-min(df$ED15))+min(df$ED15)
predict_net_test_start16_pon <- predict_net_test16_pon$net.result*(max(df$ED16)-min(df$ED16))+min(df$ED16)
predict_net_test_start17_pon <- predict_net_test17_pon$net.result*(max(df$ED17)-min(df$ED17))+min(df$ED17)
predict_net_test_start18_pon <- predict_net_test18_pon$net.result*(max(df$ED18)-min(df$ED18))+min(df$ED18)
predict_net_test_start19_pon <- predict_net_test19_pon$net.result*(max(df$ED19)-min(df$ED19))+min(df$ED19)
predict_net_test_start20_pon <- predict_net_test20_pon$net.result*(max(df$ED20)-min(df$ED20))+min(df$ED20)
predict_net_test_start21_pon <- predict_net_test21_pon$net.result*(max(df$ED21)-min(df$ED21))+min(df$ED21)
predict_net_test_start22_pon <- predict_net_test22_pon$net.result*(max(df$ED22)-min(df$ED22))+min(df$ED22)
predict_net_test_start23_pon <- predict_net_test23_pon$net.result*(max(df$ED23)-min(df$ED23))+min(df$ED23)
predict_net_test_start24_pon <- predict_net_test24_pon$net.result*(max(df$ED24)-min(df$ED24))+min(df$ED24)

test_start1_pon <- as.data.frame((test_data_pon$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1))
test_start2_pon <- as.data.frame((test_data_pon$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
test_start3_pon <- as.data.frame((test_data_pon$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
test_start4_pon <- as.data.frame((test_data_pon$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
test_start5_pon <- as.data.frame((test_data_pon$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
test_start6_pon <- as.data.frame((test_data_pon$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
test_start7_pon <- as.data.frame((test_data_pon$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
test_start8_pon <- as.data.frame((test_data_pon$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
test_start9_pon <- as.data.frame((test_data_pon$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
test_start10_pon <- as.data.frame((test_data_pon$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
test_start11_pon <- as.data.frame((test_data_pon$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
test_start12_pon <- as.data.frame((test_data_pon$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
test_start13_pon <- as.data.frame((test_data_pon$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
test_start14_pon <- as.data.frame((test_data_pon$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
test_start15_pon <- as.data.frame((test_data_pon$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
test_start16_pon <- as.data.frame((test_data_pon$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
test_start17_pon <- as.data.frame((test_data_pon$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
test_start18_pon <- as.data.frame((test_data_pon$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
test_start19_pon <- as.data.frame((test_data_pon$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
test_start20_pon <- as.data.frame((test_data_pon$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
test_start21_pon <- as.data.frame((test_data_pon$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
test_start22_pon <- as.data.frame((test_data_pon$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
test_start23_pon <- as.data.frame((test_data_pon$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
test_start24_pon <- as.data.frame((test_data_pon$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24))

MSE.net_data1_pon <- sum((test_start1_pon-predict_net_test_start1_pon)^2/nrow(test_start1_pon))
MSE.net_data2_pon <- sum((test_start2_pon-predict_net_test_start2_pon)^2/nrow(test_start2_pon))
MSE.net_data3_pon <- sum((test_start3_pon-predict_net_test_start3_pon)^2/nrow(test_start3_pon))
MSE.net_data4_pon <- sum((test_start4_pon-predict_net_test_start4_pon)^2/nrow(test_start4_pon))
MSE.net_data5_pon <- sum((test_start5_pon-predict_net_test_start5_pon)^2/nrow(test_start5_pon))
MSE.net_data6_pon <- sum((test_start6_pon-predict_net_test_start6_pon)^2/nrow(test_start6_pon))
MSE.net_data7_pon <- sum((test_start7_pon-predict_net_test_start7_pon)^2/nrow(test_start7_pon))
MSE.net_data8_pon <- sum((test_start8_pon-predict_net_test_start8_pon)^2/nrow(test_start8_pon))
MSE.net_data9_pon <- sum((test_start9_pon-predict_net_test_start9_pon)^2/nrow(test_start9_pon))
MSE.net_data10_pon <- sum((test_start10_pon-predict_net_test_start10_pon)^2/nrow(test_start10_pon))
MSE.net_data11_pon <- sum((test_start11_pon-predict_net_test_start11_pon)^2/nrow(test_start11_pon))
MSE.net_data12_pon <- sum((test_start12_pon-predict_net_test_start12_pon)^2/nrow(test_start12_pon))
MSE.net_data13_pon <- sum((test_start13_pon-predict_net_test_start13_pon)^2/nrow(test_start13_pon))
MSE.net_data14_pon <- sum((test_start14_pon-predict_net_test_start14_pon)^2/nrow(test_start14_pon))
MSE.net_data15_pon <- sum((test_start15_pon-predict_net_test_start15_pon)^2/nrow(test_start15_pon))
MSE.net_data16_pon <- sum((test_start16_pon-predict_net_test_start16_pon)^2/nrow(test_start16_pon))
MSE.net_data17_pon <- sum((test_start17_pon-predict_net_test_start17_pon)^2/nrow(test_start17_pon))
MSE.net_data18_pon <- sum((test_start18_pon-predict_net_test_start18_pon)^2/nrow(test_start18_pon))
MSE.net_data19_pon <- sum((test_start19_pon-predict_net_test_start19_pon)^2/nrow(test_start19_pon))
MSE.net_data20_pon <- sum((test_start20_pon-predict_net_test_start20_pon)^2/nrow(test_start20_pon))
MSE.net_data21_pon <- sum((test_start21_pon-predict_net_test_start21_pon)^2/nrow(test_start21_pon))
MSE.net_data22_pon <- sum((test_start22_pon-predict_net_test_start22_pon)^2/nrow(test_start22_pon))
MSE.net_data23_pon <- sum((test_start23_pon-predict_net_test_start23_pon)^2/nrow(test_start23_pon))
MSE.net_data24_pon <- sum((test_start24_pon-predict_net_test_start24_pon)^2/nrow(test_start24_pon))

#ponizej suma mse dla poszzcegolnych godzin dla tych wszystkich dat
MSE=data.frame(t(data.frame(cbind(MSE.net_data1_pon, MSE.net_data2_pon, MSE.net_data3_pon, 
                                  MSE.net_data4_pon, MSE.net_data5_pon, MSE.net_data6_pon, 
                                  MSE.net_data7_pon, MSE.net_data8_pon, MSE.net_data9_pon, 
                                  MSE.net_data10_pon, MSE.net_data11_pon, MSE.net_data12_pon, 
                                  MSE.net_data13_pon, MSE.net_data14_pon, MSE.net_data15_pon, 
                                  MSE.net_data16_pon, MSE.net_data17_pon, MSE.net_data18_pon, 
                                  MSE.net_data19_pon, MSE.net_data20_pon, MSE.net_data21_pon, 
                                  MSE.net_data22_pon, MSE.net_data23_pon, MSE.net_data24_pon))))

names(MSE)[1] <- "MSE"


ME.net_data <- cbind(test_data_pon[,1], 
                     (test_start1_pon-predict_net_test_start1_pon), 
                     (test_start2_pon-predict_net_test_start2_pon), 
                     (test_start3_pon-predict_net_test_start3_pon), 
                     (test_start4_pon-predict_net_test_start4_pon), 
                     (test_start5_pon-predict_net_test_start5_pon), 
                     (test_start6_pon-predict_net_test_start6_pon), 
                     (test_start7_pon-predict_net_test_start7_pon), 
                     (test_start8_pon-predict_net_test_start8_pon), 
                     (test_start9_pon-predict_net_test_start9_pon), 
                     (test_start10_pon-predict_net_test_start10_pon), 
                     (test_start11_pon-predict_net_test_start11_pon), 
                     (test_start12_pon-predict_net_test_start12_pon), 
                     (test_start13_pon-predict_net_test_start13_pon), 
                     (test_start14_pon-predict_net_test_start14_pon), 
                     (test_start15_pon-predict_net_test_start15_pon), 
                     (test_start16_pon-predict_net_test_start16_pon), 
                     (test_start17_pon-predict_net_test_start17_pon), 
                     (test_start18_pon-predict_net_test_start18_pon), 
                     (test_start19_pon-predict_net_test_start19_pon), 
                     (test_start20_pon-predict_net_test_start20_pon), 
                     (test_start21_pon-predict_net_test_start21_pon), 
                     (test_start22_pon-predict_net_test_start22_pon), 
                     (test_start23_pon-predict_net_test_start23_pon), 
                     (test_start24_pon-predict_net_test_start24_pon))
ME.net_data$"ME [MW]" <-(rowSums(ME.net_data[,2:25]))/24

names(ME.net_data)[1] <- "ME"



RMSE.net_data <- cbind(test_data_pon[,1], 
                       (test_start1_pon-predict_net_test_start1_pon)^2, 
                       (test_start2_pon-predict_net_test_start2_pon)^2, 
                       (test_start3_pon-predict_net_test_start3_pon)^2, 
                       (test_start4_pon-predict_net_test_start4_pon)^2, 
                       (test_start5_pon-predict_net_test_start5_pon)^2, 
                       (test_start6_pon-predict_net_test_start6_pon)^2, 
                       (test_start7_pon-predict_net_test_start7_pon)^2, 
                       (test_start8_pon-predict_net_test_start8_pon)^2, 
                       (test_start9_pon-predict_net_test_start9_pon)^2, 
                       (test_start10_pon-predict_net_test_start10_pon)^2, 
                       (test_start11_pon-predict_net_test_start11_pon)^2, 
                       (test_start12_pon-predict_net_test_start12_pon)^2, 
                       (test_start13_pon-predict_net_test_start13_pon)^2, 
                       (test_start14_pon-predict_net_test_start14_pon)^2, 
                       (test_start15_pon-predict_net_test_start15_pon)^2, 
                       (test_start16_pon-predict_net_test_start16_pon)^2, 
                       (test_start17_pon-predict_net_test_start17_pon)^2, 
                       (test_start18_pon-predict_net_test_start18_pon)^2, 
                       (test_start19_pon-predict_net_test_start19_pon)^2, 
                       (test_start20_pon-predict_net_test_start20_pon)^2, 
                       (test_start21_pon-predict_net_test_start21_pon)^2, 
                       (test_start22_pon-predict_net_test_start22_pon)^2, 
                       (test_start23_pon-predict_net_test_start23_pon)^2, 
                       (test_start24_pon-predict_net_test_start24_pon)^2)
RMSE.net_data$"RMSE [MW]" <-sqrt(rowSums(RMSE.net_data[,2:25])/24)

names(RMSE.net_data)[1] <- "RMSE"



MPE.net_data <- cbind(test_data_pon[,1], 
                      (test_start1_pon-predict_net_test_start1_pon)/(test_start1_pon), 
                      (test_start2_pon-predict_net_test_start2_pon)/(test_start2_pon), 
                      (test_start3_pon-predict_net_test_start3_pon)/(test_start3_pon), 
                      (test_start4_pon-predict_net_test_start4_pon)/(test_start4_pon), 
                      (test_start5_pon-predict_net_test_start5_pon)/(test_start5_pon), 
                      (test_start6_pon-predict_net_test_start6_pon)/(test_start6_pon), 
                      (test_start7_pon-predict_net_test_start7_pon)/(test_start7_pon), 
                      (test_start8_pon-predict_net_test_start8_pon)/(test_start8_pon), 
                      (test_start9_pon-predict_net_test_start9_pon)/(test_start9_pon), 
                      (test_start10_pon-predict_net_test_start10_pon)/(test_start10_pon), 
                      (test_start11_pon-predict_net_test_start11_pon)/(test_start11_pon), 
                      (test_start12_pon-predict_net_test_start12_pon)/(test_start12_pon), 
                      (test_start13_pon-predict_net_test_start13_pon)/(test_start13_pon), 
                      (test_start14_pon-predict_net_test_start14_pon)/(test_start14_pon), 
                      (test_start15_pon-predict_net_test_start15_pon)/(test_start15_pon), 
                      (test_start16_pon-predict_net_test_start16_pon)/(test_start16_pon), 
                      (test_start17_pon-predict_net_test_start17_pon)/(test_start17_pon), 
                      (test_start18_pon-predict_net_test_start18_pon)/(test_start18_pon), 
                      (test_start19_pon-predict_net_test_start19_pon)/(test_start19_pon), 
                      (test_start20_pon-predict_net_test_start20_pon)/(test_start20_pon), 
                      (test_start21_pon-predict_net_test_start21_pon)/(test_start21_pon), 
                      (test_start22_pon-predict_net_test_start22_pon)/(test_start22_pon), 
                      (test_start23_pon-predict_net_test_start23_pon)/(test_start23_pon), 
                      (test_start24_pon-predict_net_test_start24_pon)/(test_start24_pon))

MPE.net_data$"MPE [%]" <-(rowSums(MPE.net_data[,2:25]))/24*100

names(MPE.net_data)[1] <- "MPE"

MAE.net_data <- cbind(test_data_pon[,1], 
                      abs(test_start1_pon-predict_net_test_start1_pon), 
                      abs(test_start2_pon-predict_net_test_start2_pon), 
                      abs(test_start3_pon-predict_net_test_start3_pon), 
                      abs(test_start4_pon-predict_net_test_start4_pon), 
                      abs(test_start5_pon-predict_net_test_start5_pon), 
                      abs(test_start6_pon-predict_net_test_start6_pon), 
                      abs(test_start7_pon-predict_net_test_start7_pon), 
                      abs(test_start8_pon-predict_net_test_start8_pon), 
                      abs(test_start9_pon-predict_net_test_start9_pon), 
                      abs(test_start10_pon-predict_net_test_start10_pon), 
                      abs(test_start11_pon-predict_net_test_start11_pon), 
                      abs(test_start12_pon-predict_net_test_start12_pon), 
                      abs(test_start13_pon-predict_net_test_start13_pon), 
                      abs(test_start14_pon-predict_net_test_start14_pon), 
                      abs(test_start15_pon-predict_net_test_start15_pon), 
                      abs(test_start16_pon-predict_net_test_start16_pon), 
                      abs(test_start17_pon-predict_net_test_start17_pon), 
                      abs(test_start18_pon-predict_net_test_start18_pon), 
                      abs(test_start19_pon-predict_net_test_start19_pon), 
                      abs(test_start20_pon-predict_net_test_start20_pon), 
                      abs(test_start21_pon-predict_net_test_start21_pon), 
                      abs(test_start22_pon-predict_net_test_start22_pon), 
                      abs(test_start23_pon-predict_net_test_start23_pon), 
                      abs(test_start24_pon-predict_net_test_start24_pon))

MAE.net_data$"MAE [MW]" <-(rowSums(MAE.net_data[,2:25]))/24

names(MAE.net_data)[1] <- "MAE"


MAPE.net_data <- cbind(test_data_pon[,1], 
                       abs((test_start1_pon-predict_net_test_start1_pon)/(test_start1_pon)), 
                       abs((test_start2_pon-predict_net_test_start2_pon)/(test_start2_pon)), 
                       abs((test_start3_pon-predict_net_test_start3_pon)/(test_start3_pon)), 
                       abs((test_start4_pon-predict_net_test_start4_pon)/(test_start4_pon)), 
                       abs((test_start5_pon-predict_net_test_start5_pon)/(test_start5_pon)), 
                       abs((test_start6_pon-predict_net_test_start6_pon)/(test_start6_pon)), 
                       abs((test_start7_pon-predict_net_test_start7_pon)/(test_start7_pon)), 
                       abs((test_start8_pon-predict_net_test_start8_pon)/(test_start8_pon)), 
                       abs((test_start9_pon-predict_net_test_start9_pon)/(test_start9_pon)), 
                       abs((test_start10_pon-predict_net_test_start10_pon)/(test_start10_pon)), 
                       abs((test_start11_pon-predict_net_test_start11_pon)/(test_start11_pon)), 
                       abs((test_start12_pon-predict_net_test_start12_pon)/(test_start12_pon)), 
                       abs((test_start13_pon-predict_net_test_start13_pon)/(test_start13_pon)), 
                       abs((test_start14_pon-predict_net_test_start14_pon)/(test_start14_pon)), 
                       abs((test_start15_pon-predict_net_test_start15_pon)/(test_start15_pon)), 
                       abs((test_start16_pon-predict_net_test_start16_pon)/(test_start16_pon)), 
                       abs((test_start17_pon-predict_net_test_start17_pon)/(test_start17_pon)), 
                       abs((test_start18_pon-predict_net_test_start18_pon)/(test_start18_pon)), 
                       abs((test_start19_pon-predict_net_test_start19_pon)/(test_start19_pon)), 
                       abs((test_start20_pon-predict_net_test_start20_pon)/(test_start20_pon)), 
                       abs((test_start21_pon-predict_net_test_start21_pon)/(test_start21_pon)), 
                       abs((test_start22_pon-predict_net_test_start22_pon)/(test_start22_pon)), 
                       abs((test_start23_pon-predict_net_test_start23_pon)/(test_start23_pon)), 
                       abs((test_start24_pon-predict_net_test_start24_pon)/(test_start24_pon)))

MAPE.net_data$"MAPE [%]" <-(rowSums(MAPE.net_data[,2:25]))/24*100

names(MAPE.net_data)[1] <- "MAPE"

errors.net_data <- cbind(test_data_pon[,1], ME.net_data[,26], RMSE.net_data[,26], MPE.net_data[,26], MAE.net_data[,26], MAPE.net_data[,26], abs((test_start1_pon-predict_net_test_start1_pon)/(test_start1_pon)))                    
colnames(errors.net_data) <- c("Data","ME [MW]", "RMSE [MW]", "MPE [%]", "MAE [MW]", "MAPE [%]")
errors.net_data <- errors.net_data[,1:6]

##########################SOBOTA
df_sob=data.frame(df[df$SOB==1 & df$NT!=1,])

max_data_sob <- apply(df_sob[,8:103], 2, max)
min_data_sob <- apply(df_sob[,8:103], 2, min)
data_scaled_sob <- as.data.frame(scale(df_sob[,8:103], center=min_data_sob, scale=max_data_sob - min_data_sob))
data_scaled_sob <- cbind(df_sob[,1:7], data_scaled_sob)

index_sob=sample(1:nrow(df_sob), round(0.70*nrow(df_sob)))
train_data_sob <-as.data.frame(data_scaled_sob[index_sob,])
test_data_sob <-as.data.frame(data_scaled_sob[-index_sob,])

install.packages("neuralnet")
library("neuralnet")

net_data1_sob <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data2_sob <-neuralnet(ED2~TEMP2+ZACHM2+OPAD2, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data3_sob <-neuralnet(ED3~TEMP3+ZACHM3+OPAD3, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data4_sob <-neuralnet(ED4~TEMP4+ZACHM4+OPAD4, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data5_sob <-neuralnet(ED5~TEMP5+ZACHM5+OPAD5, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data6_sob <-neuralnet(ED6~TEMP6+ZACHM6+OPAD6, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data7_sob <-neuralnet(ED7~TEMP7+ZACHM7+OPAD7, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data8_sob <-neuralnet(ED8~TEMP8+ZACHM8+OPAD8, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data9_sob <-neuralnet(ED9~TEMP9+ZACHM9+OPAD9, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data10_sob <-neuralnet(ED10~TEMP10+ZACHM10+OPAD10, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data11_sob <-neuralnet(ED11~TEMP11+ZACHM11+OPAD11, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data12_sob <-neuralnet(ED12~TEMP12+ZACHM12+OPAD12, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data13_sob <-neuralnet(ED13~TEMP13+ZACHM13+OPAD13, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data14_sob <-neuralnet(ED14~TEMP14+ZACHM14+OPAD14, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data15_sob <-neuralnet(ED15~TEMP15+ZACHM15+OPAD15, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data16_sob <-neuralnet(ED16~TEMP16+ZACHM16+OPAD16, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data17_sob <-neuralnet(ED17~TEMP17+ZACHM17+OPAD17, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data18_sob <-neuralnet(ED18~TEMP18+ZACHM18+OPAD18, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data19_sob <-neuralnet(ED19~TEMP19+ZACHM19+OPAD19, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data20_sob <-neuralnet(ED20~TEMP20+ZACHM20+OPAD20, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data21_sob <-neuralnet(ED21~TEMP21+ZACHM21+OPAD21, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data22_sob <-neuralnet(ED22~TEMP22+ZACHM22+OPAD22, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data23_sob <-neuralnet(ED23~TEMP23+ZACHM23+OPAD23, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)
net_data24_sob <-neuralnet(ED24~TEMP24+ZACHM24+OPAD24, data=train_data_sob, hidden=10, linear.output=TRUE, threshold=0.01)

net_data1_sob$result.matrix
net_data2_sob$result.matrix
net_data3_sob$result.matrix
net_data4_sob$result.matrix
net_data5_sob$result.matrix
net_data6_sob$result.matrix
net_data7_sob$result.matrix
net_data8_sob$result.matrix
net_data9_sob$result.matrix
net_data10_sob$result.matrix
net_data11_sob$result.matrix
net_data12_sob$result.matrix
net_data13_sob$result.matrix
net_data14_sob$result.matrix
net_data15_sob$result.matrix
net_data16_sob$result.matrix
net_data17_sob$result.matrix
net_data18_sob$result.matrix
net_data19_sob$result.matrix
net_data20_sob$result.matrix
net_data21_sob$result.matrix
net_data22_sob$result.matrix
net_data23_sob$result.matrix
net_data24_sob$result.matrix

plot(net_data1_sob)
plot(net_data2_sob)
plot(net_data3_sob)
plot(net_data4_sob)
plot(net_data5_sob)
plot(net_data6_sob)
plot(net_data7_sob)
plot(net_data8_sob)
plot(net_data9_sob)
plot(net_data10_sob)
plot(net_data11_sob)
plot(net_data12_sob)
plot(net_data13_sob)
plot(net_data14_sob)
plot(net_data15_sob)
plot(net_data16_sob)
plot(net_data17_sob)
plot(net_data18_sob)
plot(net_data19_sob)
plot(net_data20_sob)
plot(net_data21_sob)
plot(net_data22_sob)
plot(net_data23_sob)
plot(net_data24_sob)

library("NeuralNetTools")

garson(net_data1_sob)
garson(net_data2_sob)
garson(net_data3_sob)
garson(net_data4_sob)
garson(net_data5_sob)
garson(net_data6_sob)
garson(net_data7_sob)
garson(net_data8_sob)
garson(net_data9_sob)
garson(net_data10_sob)
garson(net_data11_sob)
garson(net_data12_sob)
garson(net_data13_sob)
garson(net_data14_sob)
garson(net_data15_sob)
garson(net_data16_sob)
garson(net_data17_sob)
garson(net_data18_sob)
garson(net_data19_sob)
garson(net_data20_sob)
garson(net_data21_sob)
garson(net_data22_sob)
garson(net_data23_sob)
garson(net_data24_sob)

predict_net_test1_sob <- (compute(net_data1_sob, test_data_sob[,8:103]))
predict_net_test2_sob <- compute(net_data2_sob, test_data_sob[,8:103])
predict_net_test3_sob <- compute(net_data3_sob, test_data_sob[,8:103])
predict_net_test4_sob <- compute(net_data4_sob, test_data_sob[,8:103])
predict_net_test5_sob <- compute(net_data5_sob, test_data_sob[,8:103])
predict_net_test6_sob <- compute(net_data6_sob, test_data_sob[,8:103])
predict_net_test7_sob <- compute(net_data7_sob, test_data_sob[,8:103])
predict_net_test8_sob <- compute(net_data8_sob, test_data_sob[,8:103])
predict_net_test9_sob <- compute(net_data9_sob, test_data_sob[,8:103])
predict_net_test10_sob <- compute(net_data10_sob, test_data_sob[,8:103])
predict_net_test11_sob <- compute(net_data11_sob, test_data_sob[,8:103])
predict_net_test12_sob <- compute(net_data12_sob, test_data_sob[,8:103])
predict_net_test13_sob <- compute(net_data13_sob, test_data_sob[,8:103])
predict_net_test14_sob <- compute(net_data14_sob, test_data_sob[,8:103])
predict_net_test15_sob <- compute(net_data15_sob, test_data_sob[,8:103])
predict_net_test16_sob <- compute(net_data16_sob, test_data_sob[,8:103])
predict_net_test17_sob <- compute(net_data17_sob, test_data_sob[,8:103])
predict_net_test18_sob <- compute(net_data18_sob, test_data_sob[,8:103])
predict_net_test19_sob <- compute(net_data19_sob, test_data_sob[,8:103])
predict_net_test20_sob <- compute(net_data20_sob, test_data_sob[,8:103])
predict_net_test21_sob <- compute(net_data21_sob, test_data_sob[,8:103])
predict_net_test22_sob <- compute(net_data22_sob, test_data_sob[,8:103])
predict_net_test23_sob <- compute(net_data23_sob, test_data_sob[,8:103])
predict_net_test24_sob <- compute(net_data24_sob, test_data_sob[,8:103])

resultsn1_sob <- data.frame(DATAD=test_data_sob$DATA, actual1=(test_data_sob$ED1)*(max(df_sob$ED1)-min(df_sob$ED1))+min(df_sob$ED1), prediction1=(predict_net_test1_sob$net.result)*(max(df_sob$ED1)-min(df_sob$ED1))+min(df_sob$ED1), TEMPERATURA1=test_data_sob$TEMP1*(max(df_sob$TEMP1)-min(df_sob$TEMP1))+min(df_sob$TEMP1))
resultsn2_sob <- data.frame(DATAD=test_data_sob$DATA, actual2=(test_data_sob$ED2)*(max(df_sob$ED2)-min(df_sob$ED2))+min(df_sob$ED2), prediction2=(predict_net_test2_sob$net.result)*(max(df_sob$ED2)-min(df_sob$ED2))+min(df_sob$ED2))
resultsn3_sob <- data.frame(DATAD=test_data_sob$DATA, actual3=(test_data_sob$ED3)*(max(df_sob$ED3)-min(df_sob$ED3))+min(df_sob$ED3), prediction3=(predict_net_test3_sob$net.result)*(max(df_sob$ED3)-min(df_sob$ED3))+min(df_sob$ED3))
resultsn4_sob <- data.frame(DATAD=test_data_sob$DATA, actual4=(test_data_sob$ED4)*(max(df_sob$ED4)-min(df_sob$ED4))+min(df_sob$ED4), prediction4=(predict_net_test4_sob$net.result)*(max(df_sob$ED4)-min(df_sob$ED4))+min(df_sob$ED4))
resultsn5_sob <- data.frame(DATAD=test_data_sob$DATA, actual5=(test_data_sob$ED5)*(max(df_sob$ED5)-min(df_sob$ED5))+min(df_sob$ED5), prediction5=(predict_net_test5_sob$net.result)*(max(df_sob$ED5)-min(df_sob$ED5))+min(df_sob$ED5))
resultsn6_sob <- data.frame(DATAD=test_data_sob$DATA, actual6=(test_data_sob$ED6)*(max(df_sob$ED6)-min(df_sob$ED6))+min(df_sob$ED6), prediction6=(predict_net_test6_sob$net.result)*(max(df_sob$ED6)-min(df_sob$ED6))+min(df_sob$ED6))
resultsn7_sob <- data.frame(DATAD=test_data_sob$DATA, actual7=(test_data_sob$ED7)*(max(df_sob$ED7)-min(df_sob$ED7))+min(df_sob$ED7), prediction7=(predict_net_test7_sob$net.result)*(max(df_sob$ED7)-min(df_sob$ED7))+min(df_sob$ED7))
resultsn8_sob <- data.frame(DATAD=test_data_sob$DATA, actual8=(test_data_sob$ED8)*(max(df_sob$ED8)-min(df_sob$ED8))+min(df_sob$ED8), prediction8=(predict_net_test8_sob$net.result)*(max(df_sob$ED8)-min(df_sob$ED8))+min(df_sob$ED8))
resultsn9_sob <- data.frame(DATAD=test_data_sob$DATA, actual9=(test_data_sob$ED9)*(max(df_sob$ED9)-min(df_sob$ED9))+min(df_sob$ED9), prediction9=(predict_net_test9_sob$net.result)*(max(df_sob$ED9)-min(df_sob$ED9))+min(df_sob$ED9))
resultsn10_sob <- data.frame(DATAD=test_data_sob$DATA, actual10=(test_data_sob$ED10)*(max(df_sob$ED10)-min(df_sob$ED10))+min(df_sob$ED10), prediction10=(predict_net_test10_sob$net.result)*(max(df_sob$ED10)-min(df_sob$ED10))+min(df_sob$ED10))
resultsn11_sob <- data.frame(DATAD=test_data_sob$DATA, actual11=(test_data_sob$ED11)*(max(df_sob$ED11)-min(df_sob$ED11))+min(df_sob$ED11), prediction11=(predict_net_test11_sob$net.result)*(max(df_sob$ED11)-min(df_sob$ED11))+min(df_sob$ED11))
resultsn12_sob <- data.frame(DATAD=test_data_sob$DATA, actual12=(test_data_sob$ED12)*(max(df_sob$ED12)-min(df_sob$ED12))+min(df_sob$ED12), prediction12=(predict_net_test12_sob$net.result)*(max(df_sob$ED12)-min(df_sob$ED12))+min(df_sob$ED12))
resultsn13_sob <- data.frame(DATAD=test_data_sob$DATA, actual13=(test_data_sob$ED13)*(max(df_sob$ED13)-min(df_sob$ED13))+min(df_sob$ED13), prediction13=(predict_net_test13_sob$net.result)*(max(df_sob$ED13)-min(df_sob$ED13))+min(df_sob$ED13))
resultsn14_sob <- data.frame(DATAD=test_data_sob$DATA, actual14=(test_data_sob$ED14)*(max(df_sob$ED14)-min(df_sob$ED14))+min(df_sob$ED14), prediction14=(predict_net_test14_sob$net.result)*(max(df_sob$ED14)-min(df_sob$ED14))+min(df_sob$ED14))
resultsn15_sob <- data.frame(DATAD=test_data_sob$DATA, actual15=(test_data_sob$ED15)*(max(df_sob$ED15)-min(df_sob$ED15))+min(df_sob$ED15), prediction15=(predict_net_test15_sob$net.result)*(max(df_sob$ED15)-min(df_sob$ED15))+min(df_sob$ED15))
resultsn16_sob <- data.frame(DATAD=test_data_sob$DATA, actual16=(test_data_sob$ED16)*(max(df_sob$ED16)-min(df_sob$ED16))+min(df_sob$ED16), prediction16=(predict_net_test16_sob$net.result)*(max(df_sob$ED16)-min(df_sob$ED16))+min(df_sob$ED16))
resultsn17_sob <- data.frame(DATAD=test_data_sob$DATA, actual17=(test_data_sob$ED17)*(max(df_sob$ED17)-min(df_sob$ED17))+min(df_sob$ED17), prediction17=(predict_net_test17_sob$net.result)*(max(df_sob$ED17)-min(df_sob$ED17))+min(df_sob$ED17))
resultsn18_sob <- data.frame(DATAD=test_data_sob$DATA, actual18=(test_data_sob$ED18)*(max(df_sob$ED18)-min(df_sob$ED18))+min(df_sob$ED18), prediction18=(predict_net_test18_sob$net.result)*(max(df_sob$ED18)-min(df_sob$ED18))+min(df_sob$ED18))
resultsn19_sob <- data.frame(DATAD=test_data_sob$DATA, actual19=(test_data_sob$ED19)*(max(df_sob$ED19)-min(df_sob$ED19))+min(df_sob$ED19), prediction19=(predict_net_test19_sob$net.result)*(max(df_sob$ED19)-min(df_sob$ED19))+min(df_sob$ED19))
resultsn20_sob <- data.frame(DATAD=test_data_sob$DATA, actual20=(test_data_sob$ED20)*(max(df_sob$ED20)-min(df_sob$ED20))+min(df_sob$ED20), prediction20=(predict_net_test20_sob$net.result)*(max(df_sob$ED20)-min(df_sob$ED20))+min(df_sob$ED20))
resultsn21_sob <- data.frame(DATAD=test_data_sob$DATA, actual21=(test_data_sob$ED21)*(max(df_sob$ED21)-min(df_sob$ED21))+min(df_sob$ED21), prediction21=(predict_net_test21_sob$net.result)*(max(df_sob$ED21)-min(df_sob$ED21))+min(df_sob$ED21))
resultsn22_sob <- data.frame(DATAD=test_data_sob$DATA, actual22=(test_data_sob$ED22)*(max(df_sob$ED22)-min(df_sob$ED22))+min(df_sob$ED22), prediction22=(predict_net_test22_sob$net.result)*(max(df_sob$ED22)-min(df_sob$ED22))+min(df_sob$ED22))
resultsn23_sob <- data.frame(DATAD=test_data_sob$DATA, actual23=(test_data_sob$ED23)*(max(df_sob$ED23)-min(df_sob$ED23))+min(df_sob$ED23), prediction23=(predict_net_test23_sob$net.result)*(max(df_sob$ED23)-min(df_sob$ED23))+min(df_sob$ED23))
resultsn24_sob <- data.frame(DATAD=test_data_sob$DATA, actual24=(test_data_sob$ED24)*(max(df_sob$ED24)-min(df_sob$ED24))+min(df_sob$ED24), prediction24=(predict_net_test24_sob$net.result)*(max(df_sob$ED24)-min(df_sob$ED24))+min(df_sob$ED24))


#remove.packages("neuralnet")
install.packages("dplyr")
library("dplyr")
DATAJ='2010/05/29'
#j=filter(resultsn1_sob, DATAD=='2010/01/13')
r_sob=data.frame(t(data.frame(cbind(filter(resultsn1_sob, DATAD==DATAJ)[1,2]), (filter(resultsn2_sob, DATAD==DATAJ)[1,2]), (filter(resultsn3_sob, DATAD==DATAJ)[1,2]), (filter(resultsn4_sob, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn5_sob, DATAD==DATAJ)[1,2]), (filter(resultsn6_sob, DATAD==DATAJ)[1,2]), (filter(resultsn7_sob, DATAD==DATAJ)[1,2]), (filter(resultsn8_sob, DATAD==DATAJ)[1,2]), (filter(resultsn9_sob, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn10_sob, DATAD==DATAJ)[1,2]), (filter(resultsn11_sob, DATAD==DATAJ)[1,2]),  (filter(resultsn12_sob, DATAD==DATAJ)[1,2]), (filter(resultsn13_sob, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn14_sob, DATAD==DATAJ)[1,2]), (filter(resultsn15_sob, DATAD==DATAJ)[1,2]), (filter(resultsn16_sob, DATAD==DATAJ)[1,2]), (filter(resultsn17_sob, DATAD==DATAJ)[1,2]), (filter(resultsn18_sob, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn19_sob, DATAD==DATAJ)[1,2]), (filter(resultsn20_sob, DATAD==DATAJ)[1,2]), (filter(resultsn21_sob, DATAD==DATAJ)[1,2]), (filter(resultsn22_sob, DATAD==DATAJ)[1,2]), (filter(resultsn23_sob, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn24_sob, DATAD==DATAJ)[1,2]))))

names(r_sob)[1] <- "Rzeczywista_wartosc_zapotrzebowania"
#row.names(j)[1] <- "Zapotrzebowanie1"
row.names(j)[2] <- "Zapotrzebowanie2"
row.names(j)[3] <- "Zapotrzebowanie3"
row.names(j)[4] <- "Zapotrzebowanie4"
row.names(j)[5] <- "Zapotrzebowanie5"
row.names(j)[6] <- "Zapotrzebowanie6"
row.names(j)[7] <- "Zapotrzebowanie7"
row.names(j)[8] <- "Zapotrzebowanie8"
row.names(j)[9] <- "Zapotrzebowanie9"
row.names(j)[10] <- "Zapotrzebowanie10"
row.names(j)[11] <- "Zapotrzebowanie11"
row.names(j)[12] <- "Zapotrzebowanie12"
row.names(j)[13] <- "Zapotrzebowanie13"
row.names(j)[14] <- "Zapotrzebowanie14"
row.names(j)[15] <- "Zapotrzebowanie15"
row.names(j)[16] <- "Zapotrzebowanie16"
row.names(j)[17] <- "Zapotrzebowanie17"
row.names(j)[18] <- "Zapotrzebowanie18"
row.names(j)[19] <- "Zapotrzebowanie19"
row.names(j)[20] <- "Zapotrzebowanie20"
row.names(j)[21] <- "Zapotrzebowanie21"
row.names(j)[22] <- "Zapotrzebowanie22"
row.names(j)[23] <- "Zapotrzebowanie23"
row.names(j)[24] <- "Zapotrzebowanie24"


r_sob <- cbind(Godzina=c(1:24), r_sob)

p_sob=data.frame(t(data.frame(cbind(filter(resultsn1_sob, DATAD==DATAJ)[1,3]), (filter(resultsn2_sob, DATAD==DATAJ)[1,3]), (filter(resultsn3_sob, DATAD==DATAJ)[1,3]), (filter(resultsn4_sob, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn5_sob, DATAD==DATAJ)[1,3]), (filter(resultsn6_sob, DATAD==DATAJ)[1,3]), (filter(resultsn7_sob, DATAD==DATAJ)[1,3]), (filter(resultsn8_sob, DATAD==DATAJ)[1,3]), (filter(resultsn9_sob, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn10_sob, DATAD==DATAJ)[1,3]), (filter(resultsn11_sob, DATAD==DATAJ)[1,3]),  (filter(resultsn12_sob, DATAD==DATAJ)[1,3]), (filter(resultsn13_sob, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn14_sob, DATAD==DATAJ)[1,3]), (filter(resultsn15_sob, DATAD==DATAJ)[1,3]), (filter(resultsn16_sob, DATAD==DATAJ)[1,3]), (filter(resultsn17_sob, DATAD==DATAJ)[1,3]), (filter(resultsn18_sob, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn19_sob, DATAD==DATAJ)[1,3]), (filter(resultsn20_sob, DATAD==DATAJ)[1,3]), (filter(resultsn21_sob, DATAD==DATAJ)[1,3]), (filter(resultsn22_sob, DATAD==DATAJ)[1,3]), (filter(resultsn23_sob, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn24_sob, DATAD==DATAJ)[1,3]))))

r_sob <- cbind(r_sob, p_sob[,1])
names(r_sob)[3] <- "Prognozowana_wartosc_zapotrzebowania"

library("ggplot2")
library("ggalt")
ggplot(data=r_sob, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania)) + geom_xspline(color='steelblue', size=1.05, lineend = "round") + geom_line(data=r_sob, 
                                                                                                                                                      aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), linetype=2, color='violetred', size=1.05) +
  xlab("Godzina doby") + ylab("Zapotrzebowanie [MW]") + 
  ggtitle("Porównanie rzeczywistego i prognozowanego zu¿ycia energii") +
  theme(plot.title = element_text(hjust = 0.5, face="italic")) + scale_x_discrete(limit = c(1:24)) +
  theme(panel.background = element_rect(fill = 'azure2', colour = "#6D9EC1",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))  #+ annotate("text", x = 14, y = 1000, label = "Rzeczywista wartoœæ zapotrzebowania", color="pink")

predict_net_test_start1_sob <- predict_net_test1_sob$net.result*(max(df$ED1)-min(df$ED1))+min(df$ED1)
predict_net_test_start2_sob <- predict_net_test2_sob$net.result*(max(df$ED2)-min(df$ED2))+min(df$ED2)
predict_net_test_start3_sob <- predict_net_test3_sob$net.result*(max(df$ED3)-min(df$ED3))+min(df$ED3)
predict_net_test_start4_sob <- predict_net_test4_sob$net.result*(max(df$ED4)-min(df$ED4))+min(df$ED4)
predict_net_test_start5_sob <- predict_net_test5_sob$net.result*(max(df$ED5)-min(df$ED5))+min(df$ED5)
predict_net_test_start6_sob <- predict_net_test6_sob$net.result*(max(df$ED6)-min(df$ED6))+min(df$ED6)
predict_net_test_start7_sob <- predict_net_test7_sob$net.result*(max(df$ED7)-min(df$ED7))+min(df$ED7)
predict_net_test_start8_sob <- predict_net_test8_sob$net.result*(max(df$ED8)-min(df$ED8))+min(df$ED8)
predict_net_test_start9_sob <- predict_net_test9_sob$net.result*(max(df$ED9)-min(df$ED9))+min(df$ED9)
predict_net_test_start10_sob <- predict_net_test10_sob$net.result*(max(df$ED10)-min(df$ED10))+min(df$ED10)
predict_net_test_start11_sob <- predict_net_test11_sob$net.result*(max(df$ED11)-min(df$ED11))+min(df$ED11)
predict_net_test_start12_sob <- predict_net_test12_sob$net.result*(max(df$ED12)-min(df$ED12))+min(df$ED12)
predict_net_test_start13_sob <- predict_net_test13_sob$net.result*(max(df$ED13)-min(df$ED13))+min(df$ED13)
predict_net_test_start14_sob <- predict_net_test14_sob$net.result*(max(df$ED14)-min(df$ED14))+min(df$ED14)
predict_net_test_start15_sob <- predict_net_test15_sob$net.result*(max(df$ED15)-min(df$ED15))+min(df$ED15)
predict_net_test_start16_sob <- predict_net_test16_sob$net.result*(max(df$ED16)-min(df$ED16))+min(df$ED16)
predict_net_test_start17_sob <- predict_net_test17_sob$net.result*(max(df$ED17)-min(df$ED17))+min(df$ED17)
predict_net_test_start18_sob <- predict_net_test18_sob$net.result*(max(df$ED18)-min(df$ED18))+min(df$ED18)
predict_net_test_start19_sob <- predict_net_test19_sob$net.result*(max(df$ED19)-min(df$ED19))+min(df$ED19)
predict_net_test_start20_sob <- predict_net_test20_sob$net.result*(max(df$ED20)-min(df$ED20))+min(df$ED20)
predict_net_test_start21_sob <- predict_net_test21_sob$net.result*(max(df$ED21)-min(df$ED21))+min(df$ED21)
predict_net_test_start22_sob <- predict_net_test22_sob$net.result*(max(df$ED22)-min(df$ED22))+min(df$ED22)
predict_net_test_start23_sob <- predict_net_test23_sob$net.result*(max(df$ED23)-min(df$ED23))+min(df$ED23)
predict_net_test_start24_sob <- predict_net_test24_sob$net.result*(max(df$ED24)-min(df$ED24))+min(df$ED24)

test_start1_sob <- as.data.frame((test_data_sob$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1))
test_start2_sob <- as.data.frame((test_data_sob$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
test_start3_sob <- as.data.frame((test_data_sob$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
test_start4_sob <- as.data.frame((test_data_sob$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
test_start5_sob <- as.data.frame((test_data_sob$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
test_start6_sob <- as.data.frame((test_data_sob$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
test_start7_sob <- as.data.frame((test_data_sob$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
test_start8_sob <- as.data.frame((test_data_sob$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
test_start9_sob <- as.data.frame((test_data_sob$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
test_start10_sob <- as.data.frame((test_data_sob$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
test_start11_sob <- as.data.frame((test_data_sob$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
test_start12_sob <- as.data.frame((test_data_sob$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
test_start13_sob <- as.data.frame((test_data_sob$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
test_start14_sob <- as.data.frame((test_data_sob$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
test_start15_sob <- as.data.frame((test_data_sob$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
test_start16_sob <- as.data.frame((test_data_sob$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
test_start17_sob <- as.data.frame((test_data_sob$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
test_start18_sob <- as.data.frame((test_data_sob$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
test_start19_sob <- as.data.frame((test_data_sob$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
test_start20_sob <- as.data.frame((test_data_sob$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
test_start21_sob <- as.data.frame((test_data_sob$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
test_start22_sob <- as.data.frame((test_data_sob$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
test_start23_sob <- as.data.frame((test_data_sob$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
test_start24_sob <- as.data.frame((test_data_sob$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24))

MSE.net_data1_sob <- sum((test_start1_sob-predict_net_test_start1_sob)^2/nrow(test_start1_sob))
MSE.net_data2_sob <- sum((test_start2_sob-predict_net_test_start2_sob)^2/nrow(test_start2_sob))
MSE.net_data3_sob <- sum((test_start3_sob-predict_net_test_start3_sob)^2/nrow(test_start3_sob))
MSE.net_data4_sob <- sum((test_start4_sob-predict_net_test_start4_sob)^2/nrow(test_start4_sob))
MSE.net_data5_sob <- sum((test_start5_sob-predict_net_test_start5_sob)^2/nrow(test_start5_sob))
MSE.net_data6_sob <- sum((test_start6_sob-predict_net_test_start6_sob)^2/nrow(test_start6_sob))
MSE.net_data7_sob <- sum((test_start7_sob-predict_net_test_start7_sob)^2/nrow(test_start7_sob))
MSE.net_data8_sob <- sum((test_start8_sob-predict_net_test_start8_sob)^2/nrow(test_start8_sob))
MSE.net_data9_sob <- sum((test_start9_sob-predict_net_test_start9_sob)^2/nrow(test_start9_sob))
MSE.net_data10_sob <- sum((test_start10_sob-predict_net_test_start10_sob)^2/nrow(test_start10_sob))
MSE.net_data11_sob <- sum((test_start11_sob-predict_net_test_start11_sob)^2/nrow(test_start11_sob))
MSE.net_data12_sob <- sum((test_start12_sob-predict_net_test_start12_sob)^2/nrow(test_start12_sob))
MSE.net_data13_sob <- sum((test_start13_sob-predict_net_test_start13_sob)^2/nrow(test_start13_sob))
MSE.net_data14_sob <- sum((test_start14_sob-predict_net_test_start14_sob)^2/nrow(test_start14_sob))
MSE.net_data15_sob <- sum((test_start15_sob-predict_net_test_start15_sob)^2/nrow(test_start15_sob))
MSE.net_data16_sob <- sum((test_start16_sob-predict_net_test_start16_sob)^2/nrow(test_start16_sob))
MSE.net_data17_sob <- sum((test_start17_sob-predict_net_test_start17_sob)^2/nrow(test_start17_sob))
MSE.net_data18_sob <- sum((test_start18_sob-predict_net_test_start18_sob)^2/nrow(test_start18_sob))
MSE.net_data19_sob <- sum((test_start19_sob-predict_net_test_start19_sob)^2/nrow(test_start19_sob))
MSE.net_data20_sob <- sum((test_start20_sob-predict_net_test_start20_sob)^2/nrow(test_start20_sob))
MSE.net_data21_sob <- sum((test_start21_sob-predict_net_test_start21_sob)^2/nrow(test_start21_sob))
MSE.net_data22_sob <- sum((test_start22_sob-predict_net_test_start22_sob)^2/nrow(test_start22_sob))
MSE.net_data23_sob <- sum((test_start23_sob-predict_net_test_start23_sob)^2/nrow(test_start23_sob))
MSE.net_data24_sob <- sum((test_start24_sob-predict_net_test_start24_sob)^2/nrow(test_start24_sob))

#ponizej suma mse dla poszzcegolnych godzin dla tych wszystkich dat
MSE=data.frame(t(data.frame(cbind(MSE.net_data1_sob, MSE.net_data2_sob, MSE.net_data3_sob, 
                                  MSE.net_data4_sob, MSE.net_data5_sob, MSE.net_data6_sob, 
                                  MSE.net_data7_sob, MSE.net_data8_sob, MSE.net_data9_sob, 
                                  MSE.net_data10_sob, MSE.net_data11_sob, MSE.net_data12_sob, 
                                  MSE.net_data13_sob, MSE.net_data14_sob, MSE.net_data15_sob, 
                                  MSE.net_data16_sob, MSE.net_data17_sob, MSE.net_data18_sob, 
                                  MSE.net_data19_sob, MSE.net_data20_sob, MSE.net_data21_sob, 
                                  MSE.net_data22_sob, MSE.net_data23_sob, MSE.net_data24_sob))))

names(MSE)[1] <- "MSE"


ME.net_data <- cbind(test_data_sob[,1], 
                     (test_start1_sob-predict_net_test_start1_sob), 
                     (test_start2_sob-predict_net_test_start2_sob), 
                     (test_start3_sob-predict_net_test_start3_sob), 
                     (test_start4_sob-predict_net_test_start4_sob), 
                     (test_start5_sob-predict_net_test_start5_sob), 
                     (test_start6_sob-predict_net_test_start6_sob), 
                     (test_start7_sob-predict_net_test_start7_sob), 
                     (test_start8_sob-predict_net_test_start8_sob), 
                     (test_start9_sob-predict_net_test_start9_sob), 
                     (test_start10_sob-predict_net_test_start10_sob), 
                     (test_start11_sob-predict_net_test_start11_sob), 
                     (test_start12_sob-predict_net_test_start12_sob), 
                     (test_start13_sob-predict_net_test_start13_sob), 
                     (test_start14_sob-predict_net_test_start14_sob), 
                     (test_start15_sob-predict_net_test_start15_sob), 
                     (test_start16_sob-predict_net_test_start16_sob), 
                     (test_start17_sob-predict_net_test_start17_sob), 
                     (test_start18_sob-predict_net_test_start18_sob), 
                     (test_start19_sob-predict_net_test_start19_sob), 
                     (test_start20_sob-predict_net_test_start20_sob), 
                     (test_start21_sob-predict_net_test_start21_sob), 
                     (test_start22_sob-predict_net_test_start22_sob), 
                     (test_start23_sob-predict_net_test_start23_sob), 
                     (test_start24_sob-predict_net_test_start24_sob))
ME.net_data$"ME [MW]" <-(rowSums(ME.net_data[,2:25]))/24

names(ME.net_data)[1] <- "ME"



RMSE.net_data <- cbind(test_data_sob[,1], 
                       (test_start1_sob-predict_net_test_start1_sob)^2, 
                       (test_start2_sob-predict_net_test_start2_sob)^2, 
                       (test_start3_sob-predict_net_test_start3_sob)^2, 
                       (test_start4_sob-predict_net_test_start4_sob)^2, 
                       (test_start5_sob-predict_net_test_start5_sob)^2, 
                       (test_start6_sob-predict_net_test_start6_sob)^2, 
                       (test_start7_sob-predict_net_test_start7_sob)^2, 
                       (test_start8_sob-predict_net_test_start8_sob)^2, 
                       (test_start9_sob-predict_net_test_start9_sob)^2, 
                       (test_start10_sob-predict_net_test_start10_sob)^2, 
                       (test_start11_sob-predict_net_test_start11_sob)^2, 
                       (test_start12_sob-predict_net_test_start12_sob)^2, 
                       (test_start13_sob-predict_net_test_start13_sob)^2, 
                       (test_start14_sob-predict_net_test_start14_sob)^2, 
                       (test_start15_sob-predict_net_test_start15_sob)^2, 
                       (test_start16_sob-predict_net_test_start16_sob)^2, 
                       (test_start17_sob-predict_net_test_start17_sob)^2, 
                       (test_start18_sob-predict_net_test_start18_sob)^2, 
                       (test_start19_sob-predict_net_test_start19_sob)^2, 
                       (test_start20_sob-predict_net_test_start20_sob)^2, 
                       (test_start21_sob-predict_net_test_start21_sob)^2, 
                       (test_start22_sob-predict_net_test_start22_sob)^2, 
                       (test_start23_sob-predict_net_test_start23_sob)^2, 
                       (test_start24_sob-predict_net_test_start24_sob)^2)
RMSE.net_data$"RMSE [MW]" <-sqrt(rowSums(RMSE.net_data[,2:25])/24)

names(RMSE.net_data)[1] <- "RMSE"



MPE.net_data <- cbind(test_data_sob[,1], 
                      (test_start1_sob-predict_net_test_start1_sob)/(test_start1_sob), 
                      (test_start2_sob-predict_net_test_start2_sob)/(test_start2_sob), 
                      (test_start3_sob-predict_net_test_start3_sob)/(test_start3_sob), 
                      (test_start4_sob-predict_net_test_start4_sob)/(test_start4_sob), 
                      (test_start5_sob-predict_net_test_start5_sob)/(test_start5_sob), 
                      (test_start6_sob-predict_net_test_start6_sob)/(test_start6_sob), 
                      (test_start7_sob-predict_net_test_start7_sob)/(test_start7_sob), 
                      (test_start8_sob-predict_net_test_start8_sob)/(test_start8_sob), 
                      (test_start9_sob-predict_net_test_start9_sob)/(test_start9_sob), 
                      (test_start10_sob-predict_net_test_start10_sob)/(test_start10_sob), 
                      (test_start11_sob-predict_net_test_start11_sob)/(test_start11_sob), 
                      (test_start12_sob-predict_net_test_start12_sob)/(test_start12_sob), 
                      (test_start13_sob-predict_net_test_start13_sob)/(test_start13_sob), 
                      (test_start14_sob-predict_net_test_start14_sob)/(test_start14_sob), 
                      (test_start15_sob-predict_net_test_start15_sob)/(test_start15_sob), 
                      (test_start16_sob-predict_net_test_start16_sob)/(test_start16_sob), 
                      (test_start17_sob-predict_net_test_start17_sob)/(test_start17_sob), 
                      (test_start18_sob-predict_net_test_start18_sob)/(test_start18_sob), 
                      (test_start19_sob-predict_net_test_start19_sob)/(test_start19_sob), 
                      (test_start20_sob-predict_net_test_start20_sob)/(test_start20_sob), 
                      (test_start21_sob-predict_net_test_start21_sob)/(test_start21_sob), 
                      (test_start22_sob-predict_net_test_start22_sob)/(test_start22_sob), 
                      (test_start23_sob-predict_net_test_start23_sob)/(test_start23_sob), 
                      (test_start24_sob-predict_net_test_start24_sob)/(test_start24_sob))

MPE.net_data$"MPE [%]" <-(rowSums(MPE.net_data[,2:25]))/24*100

names(MPE.net_data)[1] <- "MPE"

MAE.net_data <- cbind(test_data_sob[,1], 
                      abs(test_start1_sob-predict_net_test_start1_sob), 
                      abs(test_start2_sob-predict_net_test_start2_sob), 
                      abs(test_start3_sob-predict_net_test_start3_sob), 
                      abs(test_start4_sob-predict_net_test_start4_sob), 
                      abs(test_start5_sob-predict_net_test_start5_sob), 
                      abs(test_start6_sob-predict_net_test_start6_sob), 
                      abs(test_start7_sob-predict_net_test_start7_sob), 
                      abs(test_start8_sob-predict_net_test_start8_sob), 
                      abs(test_start9_sob-predict_net_test_start9_sob), 
                      abs(test_start10_sob-predict_net_test_start10_sob), 
                      abs(test_start11_sob-predict_net_test_start11_sob), 
                      abs(test_start12_sob-predict_net_test_start12_sob), 
                      abs(test_start13_sob-predict_net_test_start13_sob), 
                      abs(test_start14_sob-predict_net_test_start14_sob), 
                      abs(test_start15_sob-predict_net_test_start15_sob), 
                      abs(test_start16_sob-predict_net_test_start16_sob), 
                      abs(test_start17_sob-predict_net_test_start17_sob), 
                      abs(test_start18_sob-predict_net_test_start18_sob), 
                      abs(test_start19_sob-predict_net_test_start19_sob), 
                      abs(test_start20_sob-predict_net_test_start20_sob), 
                      abs(test_start21_sob-predict_net_test_start21_sob), 
                      abs(test_start22_sob-predict_net_test_start22_sob), 
                      abs(test_start23_sob-predict_net_test_start23_sob), 
                      abs(test_start24_sob-predict_net_test_start24_sob))

MAE.net_data$"MAE [MW]" <-(rowSums(MAE.net_data[,2:25]))/24

names(MAE.net_data)[1] <- "MAE"


MAPE.net_data <- cbind(test_data_sob[,1], 
                       abs((test_start1_sob-predict_net_test_start1_sob)/(test_start1_sob)), 
                       abs((test_start2_sob-predict_net_test_start2_sob)/(test_start2_sob)), 
                       abs((test_start3_sob-predict_net_test_start3_sob)/(test_start3_sob)), 
                       abs((test_start4_sob-predict_net_test_start4_sob)/(test_start4_sob)), 
                       abs((test_start5_sob-predict_net_test_start5_sob)/(test_start5_sob)), 
                       abs((test_start6_sob-predict_net_test_start6_sob)/(test_start6_sob)), 
                       abs((test_start7_sob-predict_net_test_start7_sob)/(test_start7_sob)), 
                       abs((test_start8_sob-predict_net_test_start8_sob)/(test_start8_sob)), 
                       abs((test_start9_sob-predict_net_test_start9_sob)/(test_start9_sob)), 
                       abs((test_start10_sob-predict_net_test_start10_sob)/(test_start10_sob)), 
                       abs((test_start11_sob-predict_net_test_start11_sob)/(test_start11_sob)), 
                       abs((test_start12_sob-predict_net_test_start12_sob)/(test_start12_sob)), 
                       abs((test_start13_sob-predict_net_test_start13_sob)/(test_start13_sob)), 
                       abs((test_start14_sob-predict_net_test_start14_sob)/(test_start14_sob)), 
                       abs((test_start15_sob-predict_net_test_start15_sob)/(test_start15_sob)), 
                       abs((test_start16_sob-predict_net_test_start16_sob)/(test_start16_sob)), 
                       abs((test_start17_sob-predict_net_test_start17_sob)/(test_start17_sob)), 
                       abs((test_start18_sob-predict_net_test_start18_sob)/(test_start18_sob)), 
                       abs((test_start19_sob-predict_net_test_start19_sob)/(test_start19_sob)), 
                       abs((test_start20_sob-predict_net_test_start20_sob)/(test_start20_sob)), 
                       abs((test_start21_sob-predict_net_test_start21_sob)/(test_start21_sob)), 
                       abs((test_start22_sob-predict_net_test_start22_sob)/(test_start22_sob)), 
                       abs((test_start23_sob-predict_net_test_start23_sob)/(test_start23_sob)), 
                       abs((test_start24_sob-predict_net_test_start24_sob)/(test_start24_sob)))

MAPE.net_data$"MAPE [%]" <-(rowSums(MAPE.net_data[,2:25]))/24*100

names(MAPE.net_data)[1] <- "MAPE"

errors.net_data <- cbind(test_data_sob[,1], ME.net_data[,26], RMSE.net_data[,26], MPE.net_data[,26], MAE.net_data[,26], MAPE.net_data[,26], abs((test_start1_sob-predict_net_test_start1_sob)/(test_start1_sob)))                    
colnames(errors.net_data) <- c("Data","ME [MW]", "RMSE [MW]", "MPE [%]", "MAE [MW]", "MAPE [%]")
errors.net_data <- errors.net_data[,1:6]


##########################NIEDZIELA
df_ndz=data.frame(df[df$NDZ==1 & df$NT!=1,])

max_data_ndz <- apply(df_ndz[,8:103], 2, max)
min_data_ndz <- apply(df_ndz[,8:103], 2, min)
data_scaled_ndz <- as.data.frame(scale(df_ndz[,8:103], center=min_data_ndz, scale=max_data_ndz - min_data_ndz))
data_scaled_ndz <- cbind(df_ndz[,1:7], data_scaled_ndz)

index_ndz=sample(1:nrow(df_ndz), round(0.70*nrow(df_ndz)))
train_data_ndz <-as.data.frame(data_scaled_ndz[index_ndz,])
test_data_ndz <-as.data.frame(data_scaled_ndz[-index_ndz,])

install.packages("neuralnet")
library("neuralnet")

net_data1_ndz <-neuralnet(ED1~TEMP1+ZACHM1+OPAD1, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data2_ndz <-neuralnet(ED2~TEMP2+ZACHM2+OPAD2, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data3_ndz <-neuralnet(ED3~TEMP3+ZACHM3+OPAD3, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data4_ndz <-neuralnet(ED4~TEMP4+ZACHM4+OPAD4, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data5_ndz <-neuralnet(ED5~TEMP5+ZACHM5+OPAD5, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data6_ndz <-neuralnet(ED6~TEMP6+ZACHM6+OPAD6, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data7_ndz <-neuralnet(ED7~TEMP7+ZACHM7+OPAD7, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data8_ndz <-neuralnet(ED8~TEMP8+ZACHM8+OPAD8, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data9_ndz <-neuralnet(ED9~TEMP9+ZACHM9+OPAD9, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data10_ndz <-neuralnet(ED10~TEMP10+ZACHM10+OPAD10, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data11_ndz <-neuralnet(ED11~TEMP11+ZACHM11+OPAD11, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data12_ndz <-neuralnet(ED12~TEMP12+ZACHM12+OPAD12, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data13_ndz <-neuralnet(ED13~TEMP13+ZACHM13+OPAD13, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data14_ndz <-neuralnet(ED14~TEMP14+ZACHM14+OPAD14, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data15_ndz <-neuralnet(ED15~TEMP15+ZACHM15+OPAD15, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data16_ndz <-neuralnet(ED16~TEMP16+ZACHM16+OPAD16, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data17_ndz <-neuralnet(ED17~TEMP17+ZACHM17+OPAD17, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data18_ndz <-neuralnet(ED18~TEMP18+ZACHM18+OPAD18, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data19_ndz <-neuralnet(ED19~TEMP19+ZACHM19+OPAD19, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data20_ndz <-neuralnet(ED20~TEMP20+ZACHM20+OPAD20, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data21_ndz <-neuralnet(ED21~TEMP21+ZACHM21+OPAD21, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data22_ndz <-neuralnet(ED22~TEMP22+ZACHM22+OPAD22, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data23_ndz <-neuralnet(ED23~TEMP23+ZACHM23+OPAD23, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)
net_data24_ndz <-neuralnet(ED24~TEMP24+ZACHM24+OPAD24, data=train_data_ndz, hidden=10, linear.output=TRUE, threshold=0.01)

net_data1_ndz$result.matrix
net_data2_ndz$result.matrix
net_data3_ndz$result.matrix
net_data4_ndz$result.matrix
net_data5_ndz$result.matrix
net_data6_ndz$result.matrix
net_data7_ndz$result.matrix
net_data8_ndz$result.matrix
net_data9_ndz$result.matrix
net_data10_ndz$result.matrix
net_data11_ndz$result.matrix
net_data12_ndz$result.matrix
net_data13_ndz$result.matrix
net_data14_ndz$result.matrix
net_data15_ndz$result.matrix
net_data16_ndz$result.matrix
net_data17_ndz$result.matrix
net_data18_ndz$result.matrix
net_data19_ndz$result.matrix
net_data20_ndz$result.matrix
net_data21_ndz$result.matrix
net_data22_ndz$result.matrix
net_data23_ndz$result.matrix
net_data24_ndz$result.matrix

plot(net_data1_ndz)
plot(net_data2_ndz)
plot(net_data3_ndz)
plot(net_data4_ndz)
plot(net_data5_ndz)
plot(net_data6_ndz)
plot(net_data7_ndz)
plot(net_data8_ndz)
plot(net_data9_ndz)
plot(net_data10_ndz)
plot(net_data11_ndz)
plot(net_data12_ndz)
plot(net_data13_ndz)
plot(net_data14_ndz)
plot(net_data15_ndz)
plot(net_data16_ndz)
plot(net_data17_ndz)
plot(net_data18_ndz)
plot(net_data19_ndz)
plot(net_data20_ndz)
plot(net_data21_ndz)
plot(net_data22_ndz)
plot(net_data23_ndz)
plot(net_data24_ndz)

library("NeuralNetTools")

garson(net_data1_ndz)
garson(net_data2_ndz)
garson(net_data3_ndz)
garson(net_data4_ndz)
garson(net_data5_ndz)
garson(net_data6_ndz)
garson(net_data7_ndz)
garson(net_data8_ndz)
garson(net_data9_ndz)
garson(net_data10_ndz)
garson(net_data11_ndz)
garson(net_data12_ndz)
garson(net_data13_ndz)
garson(net_data14_ndz)
garson(net_data15_ndz)
garson(net_data16_ndz)
garson(net_data17_ndz)
garson(net_data18_ndz)
garson(net_data19_ndz)
garson(net_data20_ndz)
garson(net_data21_ndz)
garson(net_data22_ndz)
garson(net_data23_ndz)
garson(net_data24_ndz)

predict_net_test1_ndz <- (compute(net_data1_ndz, test_data_ndz[,8:103]))
predict_net_test2_ndz <- compute(net_data2_ndz, test_data_ndz[,8:103])
predict_net_test3_ndz <- compute(net_data3_ndz, test_data_ndz[,8:103])
predict_net_test4_ndz <- compute(net_data4_ndz, test_data_ndz[,8:103])
predict_net_test5_ndz <- compute(net_data5_ndz, test_data_ndz[,8:103])
predict_net_test6_ndz <- compute(net_data6_ndz, test_data_ndz[,8:103])
predict_net_test7_ndz <- compute(net_data7_ndz, test_data_ndz[,8:103])
predict_net_test8_ndz <- compute(net_data8_ndz, test_data_ndz[,8:103])
predict_net_test9_ndz <- compute(net_data9_ndz, test_data_ndz[,8:103])
predict_net_test10_ndz <- compute(net_data10_ndz, test_data_ndz[,8:103])
predict_net_test11_ndz <- compute(net_data11_ndz, test_data_ndz[,8:103])
predict_net_test12_ndz <- compute(net_data12_ndz, test_data_ndz[,8:103])
predict_net_test13_ndz <- compute(net_data13_ndz, test_data_ndz[,8:103])
predict_net_test14_ndz <- compute(net_data14_ndz, test_data_ndz[,8:103])
predict_net_test15_ndz <- compute(net_data15_ndz, test_data_ndz[,8:103])
predict_net_test16_ndz <- compute(net_data16_ndz, test_data_ndz[,8:103])
predict_net_test17_ndz <- compute(net_data17_ndz, test_data_ndz[,8:103])
predict_net_test18_ndz <- compute(net_data18_ndz, test_data_ndz[,8:103])
predict_net_test19_ndz <- compute(net_data19_ndz, test_data_ndz[,8:103])
predict_net_test20_ndz <- compute(net_data20_ndz, test_data_ndz[,8:103])
predict_net_test21_ndz <- compute(net_data21_ndz, test_data_ndz[,8:103])
predict_net_test22_ndz <- compute(net_data22_ndz, test_data_ndz[,8:103])
predict_net_test23_ndz <- compute(net_data23_ndz, test_data_ndz[,8:103])
predict_net_test24_ndz <- compute(net_data24_ndz, test_data_ndz[,8:103])

resultsn1_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual1=(test_data_ndz$ED1)*(max(df_ndz$ED1)-min(df_ndz$ED1))+min(df_ndz$ED1), prediction1=(predict_net_test1_ndz$net.result)*(max(df_ndz$ED1)-min(df_ndz$ED1))+min(df_ndz$ED1), TEMPERATURA1=test_data_ndz$TEMP1*(max(df_ndz$TEMP1)-min(df_ndz$TEMP1))+min(df_ndz$TEMP1))
resultsn2_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual2=(test_data_ndz$ED2)*(max(df_ndz$ED2)-min(df_ndz$ED2))+min(df_ndz$ED2), prediction2=(predict_net_test2_ndz$net.result)*(max(df_ndz$ED2)-min(df_ndz$ED2))+min(df_ndz$ED2))
resultsn3_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual3=(test_data_ndz$ED3)*(max(df_ndz$ED3)-min(df_ndz$ED3))+min(df_ndz$ED3), prediction3=(predict_net_test3_ndz$net.result)*(max(df_ndz$ED3)-min(df_ndz$ED3))+min(df_ndz$ED3))
resultsn4_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual4=(test_data_ndz$ED4)*(max(df_ndz$ED4)-min(df_ndz$ED4))+min(df_ndz$ED4), prediction4=(predict_net_test4_ndz$net.result)*(max(df_ndz$ED4)-min(df_ndz$ED4))+min(df_ndz$ED4))
resultsn5_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual5=(test_data_ndz$ED5)*(max(df_ndz$ED5)-min(df_ndz$ED5))+min(df_ndz$ED5), prediction5=(predict_net_test5_ndz$net.result)*(max(df_ndz$ED5)-min(df_ndz$ED5))+min(df_ndz$ED5))
resultsn6_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual6=(test_data_ndz$ED6)*(max(df_ndz$ED6)-min(df_ndz$ED6))+min(df_ndz$ED6), prediction6=(predict_net_test6_ndz$net.result)*(max(df_ndz$ED6)-min(df_ndz$ED6))+min(df_ndz$ED6))
resultsn7_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual7=(test_data_ndz$ED7)*(max(df_ndz$ED7)-min(df_ndz$ED7))+min(df_ndz$ED7), prediction7=(predict_net_test7_ndz$net.result)*(max(df_ndz$ED7)-min(df_ndz$ED7))+min(df_ndz$ED7))
resultsn8_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual8=(test_data_ndz$ED8)*(max(df_ndz$ED8)-min(df_ndz$ED8))+min(df_ndz$ED8), prediction8=(predict_net_test8_ndz$net.result)*(max(df_ndz$ED8)-min(df_ndz$ED8))+min(df_ndz$ED8))
resultsn9_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual9=(test_data_ndz$ED9)*(max(df_ndz$ED9)-min(df_ndz$ED9))+min(df_ndz$ED9), prediction9=(predict_net_test9_ndz$net.result)*(max(df_ndz$ED9)-min(df_ndz$ED9))+min(df_ndz$ED9))
resultsn10_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual10=(test_data_ndz$ED10)*(max(df_ndz$ED10)-min(df_ndz$ED10))+min(df_ndz$ED10), prediction10=(predict_net_test10_ndz$net.result)*(max(df_ndz$ED10)-min(df_ndz$ED10))+min(df_ndz$ED10))
resultsn11_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual11=(test_data_ndz$ED11)*(max(df_ndz$ED11)-min(df_ndz$ED11))+min(df_ndz$ED11), prediction11=(predict_net_test11_ndz$net.result)*(max(df_ndz$ED11)-min(df_ndz$ED11))+min(df_ndz$ED11))
resultsn12_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual12=(test_data_ndz$ED12)*(max(df_ndz$ED12)-min(df_ndz$ED12))+min(df_ndz$ED12), prediction12=(predict_net_test12_ndz$net.result)*(max(df_ndz$ED12)-min(df_ndz$ED12))+min(df_ndz$ED12))
resultsn13_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual13=(test_data_ndz$ED13)*(max(df_ndz$ED13)-min(df_ndz$ED13))+min(df_ndz$ED13), prediction13=(predict_net_test13_ndz$net.result)*(max(df_ndz$ED13)-min(df_ndz$ED13))+min(df_ndz$ED13))
resultsn14_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual14=(test_data_ndz$ED14)*(max(df_ndz$ED14)-min(df_ndz$ED14))+min(df_ndz$ED14), prediction14=(predict_net_test14_ndz$net.result)*(max(df_ndz$ED14)-min(df_ndz$ED14))+min(df_ndz$ED14))
resultsn15_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual15=(test_data_ndz$ED15)*(max(df_ndz$ED15)-min(df_ndz$ED15))+min(df_ndz$ED15), prediction15=(predict_net_test15_ndz$net.result)*(max(df_ndz$ED15)-min(df_ndz$ED15))+min(df_ndz$ED15))
resultsn16_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual16=(test_data_ndz$ED16)*(max(df_ndz$ED16)-min(df_ndz$ED16))+min(df_ndz$ED16), prediction16=(predict_net_test16_ndz$net.result)*(max(df_ndz$ED16)-min(df_ndz$ED16))+min(df_ndz$ED16))
resultsn17_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual17=(test_data_ndz$ED17)*(max(df_ndz$ED17)-min(df_ndz$ED17))+min(df_ndz$ED17), prediction17=(predict_net_test17_ndz$net.result)*(max(df_ndz$ED17)-min(df_ndz$ED17))+min(df_ndz$ED17))
resultsn18_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual18=(test_data_ndz$ED18)*(max(df_ndz$ED18)-min(df_ndz$ED18))+min(df_ndz$ED18), prediction18=(predict_net_test18_ndz$net.result)*(max(df_ndz$ED18)-min(df_ndz$ED18))+min(df_ndz$ED18))
resultsn19_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual19=(test_data_ndz$ED19)*(max(df_ndz$ED19)-min(df_ndz$ED19))+min(df_ndz$ED19), prediction19=(predict_net_test19_ndz$net.result)*(max(df_ndz$ED19)-min(df_ndz$ED19))+min(df_ndz$ED19))
resultsn20_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual20=(test_data_ndz$ED20)*(max(df_ndz$ED20)-min(df_ndz$ED20))+min(df_ndz$ED20), prediction20=(predict_net_test20_ndz$net.result)*(max(df_ndz$ED20)-min(df_ndz$ED20))+min(df_ndz$ED20))
resultsn21_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual21=(test_data_ndz$ED21)*(max(df_ndz$ED21)-min(df_ndz$ED21))+min(df_ndz$ED21), prediction21=(predict_net_test21_ndz$net.result)*(max(df_ndz$ED21)-min(df_ndz$ED21))+min(df_ndz$ED21))
resultsn22_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual22=(test_data_ndz$ED22)*(max(df_ndz$ED22)-min(df_ndz$ED22))+min(df_ndz$ED22), prediction22=(predict_net_test22_ndz$net.result)*(max(df_ndz$ED22)-min(df_ndz$ED22))+min(df_ndz$ED22))
resultsn23_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual23=(test_data_ndz$ED23)*(max(df_ndz$ED23)-min(df_ndz$ED23))+min(df_ndz$ED23), prediction23=(predict_net_test23_ndz$net.result)*(max(df_ndz$ED23)-min(df_ndz$ED23))+min(df_ndz$ED23))
resultsn24_ndz <- data.frame(DATAD=test_data_ndz$DATA, actual24=(test_data_ndz$ED24)*(max(df_ndz$ED24)-min(df_ndz$ED24))+min(df_ndz$ED24), prediction24=(predict_net_test24_ndz$net.result)*(max(df_ndz$ED24)-min(df_ndz$ED24))+min(df_ndz$ED24))


#remove.packages("neuralnet")
install.packages("dplyr")
library("dplyr")
DATAJ='2010/08/09'
#j=filter(resultsn1_ndz, DATAD=='2010/01/13')
r_ndz=data.frame(t(data.frame(cbind(filter(resultsn1_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn2_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn3_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn4_ndz, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn5_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn6_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn7_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn8_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn9_ndz, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn10_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn11_ndz, DATAD==DATAJ)[1,2]),  (filter(resultsn12_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn13_ndz, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn14_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn15_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn16_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn17_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn18_ndz, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn19_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn20_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn21_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn22_ndz, DATAD==DATAJ)[1,2]), (filter(resultsn23_ndz, DATAD==DATAJ)[1,2]), 
                              (filter(resultsn24_ndz, DATAD==DATAJ)[1,2]))))

names(r_ndz)[1] <- "Rzeczywista_wartosc_zapotrzebowania"
#row.names(j)[1] <- "Zapotrzebowanie1"
row.names(j)[2] <- "Zapotrzebowanie2"
row.names(j)[3] <- "Zapotrzebowanie3"
row.names(j)[4] <- "Zapotrzebowanie4"
row.names(j)[5] <- "Zapotrzebowanie5"
row.names(j)[6] <- "Zapotrzebowanie6"
row.names(j)[7] <- "Zapotrzebowanie7"
row.names(j)[8] <- "Zapotrzebowanie8"
row.names(j)[9] <- "Zapotrzebowanie9"
row.names(j)[10] <- "Zapotrzebowanie10"
row.names(j)[11] <- "Zapotrzebowanie11"
row.names(j)[12] <- "Zapotrzebowanie12"
row.names(j)[13] <- "Zapotrzebowanie13"
row.names(j)[14] <- "Zapotrzebowanie14"
row.names(j)[15] <- "Zapotrzebowanie15"
row.names(j)[16] <- "Zapotrzebowanie16"
row.names(j)[17] <- "Zapotrzebowanie17"
row.names(j)[18] <- "Zapotrzebowanie18"
row.names(j)[19] <- "Zapotrzebowanie19"
row.names(j)[20] <- "Zapotrzebowanie20"
row.names(j)[21] <- "Zapotrzebowanie21"
row.names(j)[22] <- "Zapotrzebowanie22"
row.names(j)[23] <- "Zapotrzebowanie23"
row.names(j)[24] <- "Zapotrzebowanie24"


r_ndz <- cbind(Godzina=c(1:24), r_ndz)

p_ndz=data.frame(t(data.frame(cbind(filter(resultsn1_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn2_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn3_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn4_ndz, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn5_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn6_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn7_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn8_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn9_ndz, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn10_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn11_ndz, DATAD==DATAJ)[1,3]),  (filter(resultsn12_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn13_ndz, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn14_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn15_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn16_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn17_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn18_ndz, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn19_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn20_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn21_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn22_ndz, DATAD==DATAJ)[1,3]), (filter(resultsn23_ndz, DATAD==DATAJ)[1,3]), 
                              (filter(resultsn24_ndz, DATAD==DATAJ)[1,3]))))

r_ndz <- cbind(r_ndz, p_ndz[,1])
names(r_ndz)[3] <- "Prognozowana_wartosc_zapotrzebowania"

library("ggplot2")
library("ggalt")
ggplot(data=r_ndz, aes(x=Godzina, y=Rzeczywista_wartosc_zapotrzebowania)) + geom_xspline(color='steelblue', size=1.05, lineend = "round") + geom_line(data=r_ndz, 
                                                                                                                                                      aes(x=Godzina, y=Prognozowana_wartosc_zapotrzebowania), linetype=2, color='violetred', size=1.05) +
  xlab("Godzina doby") + ylab("Zapotrzebowanie [MW]") + 
  ggtitle("Porównanie rzeczywistego i prognozowanego zu¿ycia energii") +
  theme(plot.title = element_text(hjust = 0.5, face="italic")) + scale_x_discrete(limit = c(1:24)) +
  theme(panel.background = element_rect(fill = 'azure2', colour = "#6D9EC1",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))  #+ annotate("text", x = 14, y = 1000, label = "Rzeczywista wartoœæ zapotrzebowania", color="pink")

predict_net_test_start1_ndz <- predict_net_test1_ndz$net.result*(max(df$ED1)-min(df$ED1))+min(df$ED1)
predict_net_test_start2_ndz <- predict_net_test2_ndz$net.result*(max(df$ED2)-min(df$ED2))+min(df$ED2)
predict_net_test_start3_ndz <- predict_net_test3_ndz$net.result*(max(df$ED3)-min(df$ED3))+min(df$ED3)
predict_net_test_start4_ndz <- predict_net_test4_ndz$net.result*(max(df$ED4)-min(df$ED4))+min(df$ED4)
predict_net_test_start5_ndz <- predict_net_test5_ndz$net.result*(max(df$ED5)-min(df$ED5))+min(df$ED5)
predict_net_test_start6_ndz <- predict_net_test6_ndz$net.result*(max(df$ED6)-min(df$ED6))+min(df$ED6)
predict_net_test_start7_ndz <- predict_net_test7_ndz$net.result*(max(df$ED7)-min(df$ED7))+min(df$ED7)
predict_net_test_start8_ndz <- predict_net_test8_ndz$net.result*(max(df$ED8)-min(df$ED8))+min(df$ED8)
predict_net_test_start9_ndz <- predict_net_test9_ndz$net.result*(max(df$ED9)-min(df$ED9))+min(df$ED9)
predict_net_test_start10_ndz <- predict_net_test10_ndz$net.result*(max(df$ED10)-min(df$ED10))+min(df$ED10)
predict_net_test_start11_ndz <- predict_net_test11_ndz$net.result*(max(df$ED11)-min(df$ED11))+min(df$ED11)
predict_net_test_start12_ndz <- predict_net_test12_ndz$net.result*(max(df$ED12)-min(df$ED12))+min(df$ED12)
predict_net_test_start13_ndz <- predict_net_test13_ndz$net.result*(max(df$ED13)-min(df$ED13))+min(df$ED13)
predict_net_test_start14_ndz <- predict_net_test14_ndz$net.result*(max(df$ED14)-min(df$ED14))+min(df$ED14)
predict_net_test_start15_ndz <- predict_net_test15_ndz$net.result*(max(df$ED15)-min(df$ED15))+min(df$ED15)
predict_net_test_start16_ndz <- predict_net_test16_ndz$net.result*(max(df$ED16)-min(df$ED16))+min(df$ED16)
predict_net_test_start17_ndz <- predict_net_test17_ndz$net.result*(max(df$ED17)-min(df$ED17))+min(df$ED17)
predict_net_test_start18_ndz <- predict_net_test18_ndz$net.result*(max(df$ED18)-min(df$ED18))+min(df$ED18)
predict_net_test_start19_ndz <- predict_net_test19_ndz$net.result*(max(df$ED19)-min(df$ED19))+min(df$ED19)
predict_net_test_start20_ndz <- predict_net_test20_ndz$net.result*(max(df$ED20)-min(df$ED20))+min(df$ED20)
predict_net_test_start21_ndz <- predict_net_test21_ndz$net.result*(max(df$ED21)-min(df$ED21))+min(df$ED21)
predict_net_test_start22_ndz <- predict_net_test22_ndz$net.result*(max(df$ED22)-min(df$ED22))+min(df$ED22)
predict_net_test_start23_ndz <- predict_net_test23_ndz$net.result*(max(df$ED23)-min(df$ED23))+min(df$ED23)
predict_net_test_start24_ndz <- predict_net_test24_ndz$net.result*(max(df$ED24)-min(df$ED24))+min(df$ED24)

test_start1_ndz <- as.data.frame((test_data_ndz$ED1)*(max(df$ED1)-min(df$ED1))+min(df$ED1))
test_start2_ndz <- as.data.frame((test_data_ndz$ED2)*(max(df$ED2)-min(df$ED2))+min(df$ED2))
test_start3_ndz <- as.data.frame((test_data_ndz$ED3)*(max(df$ED3)-min(df$ED3))+min(df$ED3))
test_start4_ndz <- as.data.frame((test_data_ndz$ED4)*(max(df$ED4)-min(df$ED4))+min(df$ED4))
test_start5_ndz <- as.data.frame((test_data_ndz$ED5)*(max(df$ED5)-min(df$ED5))+min(df$ED5))
test_start6_ndz <- as.data.frame((test_data_ndz$ED6)*(max(df$ED6)-min(df$ED6))+min(df$ED6))
test_start7_ndz <- as.data.frame((test_data_ndz$ED7)*(max(df$ED7)-min(df$ED7))+min(df$ED7))
test_start8_ndz <- as.data.frame((test_data_ndz$ED8)*(max(df$ED8)-min(df$ED8))+min(df$ED8))
test_start9_ndz <- as.data.frame((test_data_ndz$ED9)*(max(df$ED9)-min(df$ED9))+min(df$ED9))
test_start10_ndz <- as.data.frame((test_data_ndz$ED10)*(max(df$ED10)-min(df$ED10))+min(df$ED10))
test_start11_ndz <- as.data.frame((test_data_ndz$ED11)*(max(df$ED11)-min(df$ED11))+min(df$ED11))
test_start12_ndz <- as.data.frame((test_data_ndz$ED12)*(max(df$ED12)-min(df$ED12))+min(df$ED12))
test_start13_ndz <- as.data.frame((test_data_ndz$ED13)*(max(df$ED13)-min(df$ED13))+min(df$ED13))
test_start14_ndz <- as.data.frame((test_data_ndz$ED14)*(max(df$ED14)-min(df$ED14))+min(df$ED14))
test_start15_ndz <- as.data.frame((test_data_ndz$ED15)*(max(df$ED15)-min(df$ED15))+min(df$ED15))
test_start16_ndz <- as.data.frame((test_data_ndz$ED16)*(max(df$ED16)-min(df$ED16))+min(df$ED16))
test_start17_ndz <- as.data.frame((test_data_ndz$ED17)*(max(df$ED17)-min(df$ED17))+min(df$ED17))
test_start18_ndz <- as.data.frame((test_data_ndz$ED18)*(max(df$ED18)-min(df$ED18))+min(df$ED18))
test_start19_ndz <- as.data.frame((test_data_ndz$ED19)*(max(df$ED19)-min(df$ED19))+min(df$ED19))
test_start20_ndz <- as.data.frame((test_data_ndz$ED20)*(max(df$ED20)-min(df$ED20))+min(df$ED20))
test_start21_ndz <- as.data.frame((test_data_ndz$ED21)*(max(df$ED21)-min(df$ED21))+min(df$ED21))
test_start22_ndz <- as.data.frame((test_data_ndz$ED22)*(max(df$ED22)-min(df$ED22))+min(df$ED22))
test_start23_ndz <- as.data.frame((test_data_ndz$ED23)*(max(df$ED23)-min(df$ED23))+min(df$ED23))
test_start24_ndz <- as.data.frame((test_data_ndz$ED24)*(max(df$ED24)-min(df$ED24))+min(df$ED24))

MSE.net_data1_ndz <- sum((test_start1_ndz-predict_net_test_start1_ndz)^2/nrow(test_start1_ndz))
MSE.net_data2_ndz <- sum((test_start2_ndz-predict_net_test_start2_ndz)^2/nrow(test_start2_ndz))
MSE.net_data3_ndz <- sum((test_start3_ndz-predict_net_test_start3_ndz)^2/nrow(test_start3_ndz))
MSE.net_data4_ndz <- sum((test_start4_ndz-predict_net_test_start4_ndz)^2/nrow(test_start4_ndz))
MSE.net_data5_ndz <- sum((test_start5_ndz-predict_net_test_start5_ndz)^2/nrow(test_start5_ndz))
MSE.net_data6_ndz <- sum((test_start6_ndz-predict_net_test_start6_ndz)^2/nrow(test_start6_ndz))
MSE.net_data7_ndz <- sum((test_start7_ndz-predict_net_test_start7_ndz)^2/nrow(test_start7_ndz))
MSE.net_data8_ndz <- sum((test_start8_ndz-predict_net_test_start8_ndz)^2/nrow(test_start8_ndz))
MSE.net_data9_ndz <- sum((test_start9_ndz-predict_net_test_start9_ndz)^2/nrow(test_start9_ndz))
MSE.net_data10_ndz <- sum((test_start10_ndz-predict_net_test_start10_ndz)^2/nrow(test_start10_ndz))
MSE.net_data11_ndz <- sum((test_start11_ndz-predict_net_test_start11_ndz)^2/nrow(test_start11_ndz))
MSE.net_data12_ndz <- sum((test_start12_ndz-predict_net_test_start12_ndz)^2/nrow(test_start12_ndz))
MSE.net_data13_ndz <- sum((test_start13_ndz-predict_net_test_start13_ndz)^2/nrow(test_start13_ndz))
MSE.net_data14_ndz <- sum((test_start14_ndz-predict_net_test_start14_ndz)^2/nrow(test_start14_ndz))
MSE.net_data15_ndz <- sum((test_start15_ndz-predict_net_test_start15_ndz)^2/nrow(test_start15_ndz))
MSE.net_data16_ndz <- sum((test_start16_ndz-predict_net_test_start16_ndz)^2/nrow(test_start16_ndz))
MSE.net_data17_ndz <- sum((test_start17_ndz-predict_net_test_start17_ndz)^2/nrow(test_start17_ndz))
MSE.net_data18_ndz <- sum((test_start18_ndz-predict_net_test_start18_ndz)^2/nrow(test_start18_ndz))
MSE.net_data19_ndz <- sum((test_start19_ndz-predict_net_test_start19_ndz)^2/nrow(test_start19_ndz))
MSE.net_data20_ndz <- sum((test_start20_ndz-predict_net_test_start20_ndz)^2/nrow(test_start20_ndz))
MSE.net_data21_ndz <- sum((test_start21_ndz-predict_net_test_start21_ndz)^2/nrow(test_start21_ndz))
MSE.net_data22_ndz <- sum((test_start22_ndz-predict_net_test_start22_ndz)^2/nrow(test_start22_ndz))
MSE.net_data23_ndz <- sum((test_start23_ndz-predict_net_test_start23_ndz)^2/nrow(test_start23_ndz))
MSE.net_data24_ndz <- sum((test_start24_ndz-predict_net_test_start24_ndz)^2/nrow(test_start24_ndz))

#ponizej suma mse dla poszzcegolnych godzin dla tych wszystkich dat
MSE=data.frame(t(data.frame(cbind(MSE.net_data1_ndz, MSE.net_data2_ndz, MSE.net_data3_ndz, 
                                  MSE.net_data4_ndz, MSE.net_data5_ndz, MSE.net_data6_ndz, 
                                  MSE.net_data7_ndz, MSE.net_data8_ndz, MSE.net_data9_ndz, 
                                  MSE.net_data10_ndz, MSE.net_data11_ndz, MSE.net_data12_ndz, 
                                  MSE.net_data13_ndz, MSE.net_data14_ndz, MSE.net_data15_ndz, 
                                  MSE.net_data16_ndz, MSE.net_data17_ndz, MSE.net_data18_ndz, 
                                  MSE.net_data19_ndz, MSE.net_data20_ndz, MSE.net_data21_ndz, 
                                  MSE.net_data22_ndz, MSE.net_data23_ndz, MSE.net_data24_ndz))))

names(MSE)[1] <- "MSE"


ME.net_data <- cbind(test_data_ndz[,1], 
                     (test_start1_ndz-predict_net_test_start1_ndz), 
                     (test_start2_ndz-predict_net_test_start2_ndz), 
                     (test_start3_ndz-predict_net_test_start3_ndz), 
                     (test_start4_ndz-predict_net_test_start4_ndz), 
                     (test_start5_ndz-predict_net_test_start5_ndz), 
                     (test_start6_ndz-predict_net_test_start6_ndz), 
                     (test_start7_ndz-predict_net_test_start7_ndz), 
                     (test_start8_ndz-predict_net_test_start8_ndz), 
                     (test_start9_ndz-predict_net_test_start9_ndz), 
                     (test_start10_ndz-predict_net_test_start10_ndz), 
                     (test_start11_ndz-predict_net_test_start11_ndz), 
                     (test_start12_ndz-predict_net_test_start12_ndz), 
                     (test_start13_ndz-predict_net_test_start13_ndz), 
                     (test_start14_ndz-predict_net_test_start14_ndz), 
                     (test_start15_ndz-predict_net_test_start15_ndz), 
                     (test_start16_ndz-predict_net_test_start16_ndz), 
                     (test_start17_ndz-predict_net_test_start17_ndz), 
                     (test_start18_ndz-predict_net_test_start18_ndz), 
                     (test_start19_ndz-predict_net_test_start19_ndz), 
                     (test_start20_ndz-predict_net_test_start20_ndz), 
                     (test_start21_ndz-predict_net_test_start21_ndz), 
                     (test_start22_ndz-predict_net_test_start22_ndz), 
                     (test_start23_ndz-predict_net_test_start23_ndz), 
                     (test_start24_ndz-predict_net_test_start24_ndz))
ME.net_data$"ME [MW]" <-(rowSums(ME.net_data[,2:25]))/24

names(ME.net_data)[1] <- "ME"



RMSE.net_data <- cbind(test_data_ndz[,1], 
                       (test_start1_ndz-predict_net_test_start1_ndz)^2, 
                       (test_start2_ndz-predict_net_test_start2_ndz)^2, 
                       (test_start3_ndz-predict_net_test_start3_ndz)^2, 
                       (test_start4_ndz-predict_net_test_start4_ndz)^2, 
                       (test_start5_ndz-predict_net_test_start5_ndz)^2, 
                       (test_start6_ndz-predict_net_test_start6_ndz)^2, 
                       (test_start7_ndz-predict_net_test_start7_ndz)^2, 
                       (test_start8_ndz-predict_net_test_start8_ndz)^2, 
                       (test_start9_ndz-predict_net_test_start9_ndz)^2, 
                       (test_start10_ndz-predict_net_test_start10_ndz)^2, 
                       (test_start11_ndz-predict_net_test_start11_ndz)^2, 
                       (test_start12_ndz-predict_net_test_start12_ndz)^2, 
                       (test_start13_ndz-predict_net_test_start13_ndz)^2, 
                       (test_start14_ndz-predict_net_test_start14_ndz)^2, 
                       (test_start15_ndz-predict_net_test_start15_ndz)^2, 
                       (test_start16_ndz-predict_net_test_start16_ndz)^2, 
                       (test_start17_ndz-predict_net_test_start17_ndz)^2, 
                       (test_start18_ndz-predict_net_test_start18_ndz)^2, 
                       (test_start19_ndz-predict_net_test_start19_ndz)^2, 
                       (test_start20_ndz-predict_net_test_start20_ndz)^2, 
                       (test_start21_ndz-predict_net_test_start21_ndz)^2, 
                       (test_start22_ndz-predict_net_test_start22_ndz)^2, 
                       (test_start23_ndz-predict_net_test_start23_ndz)^2, 
                       (test_start24_ndz-predict_net_test_start24_ndz)^2)
RMSE.net_data$"RMSE [MW]" <-sqrt(rowSums(RMSE.net_data[,2:25])/24)

names(RMSE.net_data)[1] <- "RMSE"



MPE.net_data <- cbind(test_data_ndz[,1], 
                      (test_start1_ndz-predict_net_test_start1_ndz)/(test_start1_ndz), 
                      (test_start2_ndz-predict_net_test_start2_ndz)/(test_start2_ndz), 
                      (test_start3_ndz-predict_net_test_start3_ndz)/(test_start3_ndz), 
                      (test_start4_ndz-predict_net_test_start4_ndz)/(test_start4_ndz), 
                      (test_start5_ndz-predict_net_test_start5_ndz)/(test_start5_ndz), 
                      (test_start6_ndz-predict_net_test_start6_ndz)/(test_start6_ndz), 
                      (test_start7_ndz-predict_net_test_start7_ndz)/(test_start7_ndz), 
                      (test_start8_ndz-predict_net_test_start8_ndz)/(test_start8_ndz), 
                      (test_start9_ndz-predict_net_test_start9_ndz)/(test_start9_ndz), 
                      (test_start10_ndz-predict_net_test_start10_ndz)/(test_start10_ndz), 
                      (test_start11_ndz-predict_net_test_start11_ndz)/(test_start11_ndz), 
                      (test_start12_ndz-predict_net_test_start12_ndz)/(test_start12_ndz), 
                      (test_start13_ndz-predict_net_test_start13_ndz)/(test_start13_ndz), 
                      (test_start14_ndz-predict_net_test_start14_ndz)/(test_start14_ndz), 
                      (test_start15_ndz-predict_net_test_start15_ndz)/(test_start15_ndz), 
                      (test_start16_ndz-predict_net_test_start16_ndz)/(test_start16_ndz), 
                      (test_start17_ndz-predict_net_test_start17_ndz)/(test_start17_ndz), 
                      (test_start18_ndz-predict_net_test_start18_ndz)/(test_start18_ndz), 
                      (test_start19_ndz-predict_net_test_start19_ndz)/(test_start19_ndz), 
                      (test_start20_ndz-predict_net_test_start20_ndz)/(test_start20_ndz), 
                      (test_start21_ndz-predict_net_test_start21_ndz)/(test_start21_ndz), 
                      (test_start22_ndz-predict_net_test_start22_ndz)/(test_start22_ndz), 
                      (test_start23_ndz-predict_net_test_start23_ndz)/(test_start23_ndz), 
                      (test_start24_ndz-predict_net_test_start24_ndz)/(test_start24_ndz))

MPE.net_data$"MPE [%]" <-(rowSums(MPE.net_data[,2:25]))/24*100

names(MPE.net_data)[1] <- "MPE"

MAE.net_data <- cbind(test_data_ndz[,1], 
                      abs(test_start1_ndz-predict_net_test_start1_ndz), 
                      abs(test_start2_ndz-predict_net_test_start2_ndz), 
                      abs(test_start3_ndz-predict_net_test_start3_ndz), 
                      abs(test_start4_ndz-predict_net_test_start4_ndz), 
                      abs(test_start5_ndz-predict_net_test_start5_ndz), 
                      abs(test_start6_ndz-predict_net_test_start6_ndz), 
                      abs(test_start7_ndz-predict_net_test_start7_ndz), 
                      abs(test_start8_ndz-predict_net_test_start8_ndz), 
                      abs(test_start9_ndz-predict_net_test_start9_ndz), 
                      abs(test_start10_ndz-predict_net_test_start10_ndz), 
                      abs(test_start11_ndz-predict_net_test_start11_ndz), 
                      abs(test_start12_ndz-predict_net_test_start12_ndz), 
                      abs(test_start13_ndz-predict_net_test_start13_ndz), 
                      abs(test_start14_ndz-predict_net_test_start14_ndz), 
                      abs(test_start15_ndz-predict_net_test_start15_ndz), 
                      abs(test_start16_ndz-predict_net_test_start16_ndz), 
                      abs(test_start17_ndz-predict_net_test_start17_ndz), 
                      abs(test_start18_ndz-predict_net_test_start18_ndz), 
                      abs(test_start19_ndz-predict_net_test_start19_ndz), 
                      abs(test_start20_ndz-predict_net_test_start20_ndz), 
                      abs(test_start21_ndz-predict_net_test_start21_ndz), 
                      abs(test_start22_ndz-predict_net_test_start22_ndz), 
                      abs(test_start23_ndz-predict_net_test_start23_ndz), 
                      abs(test_start24_ndz-predict_net_test_start24_ndz))

MAE.net_data$"MAE [MW]" <-(rowSums(MAE.net_data[,2:25]))/24

names(MAE.net_data)[1] <- "MAE"


MAPE.net_data <- cbind(test_data_ndz[,1], 
                       abs((test_start1_ndz-predict_net_test_start1_ndz)/(test_start1_ndz)), 
                       abs((test_start2_ndz-predict_net_test_start2_ndz)/(test_start2_ndz)), 
                       abs((test_start3_ndz-predict_net_test_start3_ndz)/(test_start3_ndz)), 
                       abs((test_start4_ndz-predict_net_test_start4_ndz)/(test_start4_ndz)), 
                       abs((test_start5_ndz-predict_net_test_start5_ndz)/(test_start5_ndz)), 
                       abs((test_start6_ndz-predict_net_test_start6_ndz)/(test_start6_ndz)), 
                       abs((test_start7_ndz-predict_net_test_start7_ndz)/(test_start7_ndz)), 
                       abs((test_start8_ndz-predict_net_test_start8_ndz)/(test_start8_ndz)), 
                       abs((test_start9_ndz-predict_net_test_start9_ndz)/(test_start9_ndz)), 
                       abs((test_start10_ndz-predict_net_test_start10_ndz)/(test_start10_ndz)), 
                       abs((test_start11_ndz-predict_net_test_start11_ndz)/(test_start11_ndz)), 
                       abs((test_start12_ndz-predict_net_test_start12_ndz)/(test_start12_ndz)), 
                       abs((test_start13_ndz-predict_net_test_start13_ndz)/(test_start13_ndz)), 
                       abs((test_start14_ndz-predict_net_test_start14_ndz)/(test_start14_ndz)), 
                       abs((test_start15_ndz-predict_net_test_start15_ndz)/(test_start15_ndz)), 
                       abs((test_start16_ndz-predict_net_test_start16_ndz)/(test_start16_ndz)), 
                       abs((test_start17_ndz-predict_net_test_start17_ndz)/(test_start17_ndz)), 
                       abs((test_start18_ndz-predict_net_test_start18_ndz)/(test_start18_ndz)), 
                       abs((test_start19_ndz-predict_net_test_start19_ndz)/(test_start19_ndz)), 
                       abs((test_start20_ndz-predict_net_test_start20_ndz)/(test_start20_ndz)), 
                       abs((test_start21_ndz-predict_net_test_start21_ndz)/(test_start21_ndz)), 
                       abs((test_start22_ndz-predict_net_test_start22_ndz)/(test_start22_ndz)), 
                       abs((test_start23_ndz-predict_net_test_start23_ndz)/(test_start23_ndz)), 
                       abs((test_start24_ndz-predict_net_test_start24_ndz)/(test_start24_ndz)))

MAPE.net_data$"MAPE [%]" <-(rowSums(MAPE.net_data[,2:25]))/24*100

names(MAPE.net_data)[1] <- "MAPE"

errors.net_data <- cbind(test_data_ndz[,1], ME.net_data[,26], RMSE.net_data[,26], MPE.net_data[,26], MAE.net_data[,26], MAPE.net_data[,26], abs((test_start1_ndz-predict_net_test_start1_ndz)/(test_start1_ndz)))                    
colnames(errors.net_data) <- c("Data","ME [MW]", "RMSE [MW]", "MPE [%]", "MAE [MW]", "MAPE [%]")
errors.net_data <- errors.net_data[,1:6]

