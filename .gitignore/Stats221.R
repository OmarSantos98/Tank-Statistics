

#Omar Santos
#STATS211 



# Part 2
#determine bean absolute difference
#test estimator for n=30,100 and m=500,5000

#Part 3
#construct %=95 confidence interval for m, simualate 1000 sets
#Try n = 30, 100 and m = 500, 5000.

#first experiment, m=500 n=30
#stores the values for the both MADs and confidence intervals
value1 = list()
value11=list()
tvalues=list()

#does it 1000 times
 for(i in 1:1000){
 
 #gets 30 numbers out of 500 without replacement
   x1=sample(1:500,30,replace=F) 
   
   #gets the max number out of those 30
   estimate1=max(x1)
   
   #standard deviation
  s1=sd(x1)
  
  #gets mean of those umbers
  m1=mean(x1)
  
  #our estimator  2 times the mean -1
   mhat1=2*mean(x1)-1
   
   #MAD for our estimator
   y=abs(mhat1-500)
   
   #MAD for using max value as estimator
   z=abs(estimate1-500)
   
   #stores our MAD estimator
   value1[i]=y
   
   #stores the max estimator
   value11[i]=z
   
   #gets confidence interval range
   error1 <- qt(0.975,df=30-1)*s1/sqrt(30)
   #LEFT BOUND
   left1 <- mhat1 - error1
   # right bound
   right1 <- mhat1 + error1
   #checks if inside , returns true or false
   inside1 <- left1 <= 500 && 500 <= right1
   #stores true or false
   tvalues[i]=inside1
   
 
 }
 #sum all the mean values stores
 sumValue = sum(unlist(value1))
  sumValue1 = sum(unlist(value11))
  
  #divides mean values added and divides by 1000
 exp1 = sumValue/1000
 maxexp1=sumValue1/1000
 #returns number of true values
 true1=length(tvalues[tvalues==TRUE])
 
 
# second experiment, m=500 n=100
 value2 = list()
  value22 = list()
  tvalues2=list()
  for(i in 1:1000){
 
    x2=sample(1:500,100,replace=F)
      estimate2=max(x2)
    s2=sd(x2)
    m2=mean(x2)
 
    mhat2=2*mean(x2)-1
    y=abs(mhat2-500)
    z=abs(estimate2-500)
    value2[i]=y
    value22[i]=z
   
    
    error2 <- qnorm(0.975)*s2/sqrt(100)
    left2 <- mhat2 - error2
    right2 <- mhat2 + error2
    inside2 <- left2 <= 500 && 500 <= right2
     tvalues2[i]=inside2
 
  }
 
 
 sumValue = sum(unlist(value2))
 sumValue2 = sum(unlist(value22))
 exp2 = sumValue/1000
  maxexp2=sumValue2/1000
 true2= length(tvalues2[tvalues2==TRUE])

#third, m=5000 n=30
value3 = list()
value33 = list()
tvalue3=list()
 for(i in 1:1000){
 
   x3=sample(1:5000,30,replace=F)
     estimate3=max(x3)
   s3=sd(x3)
   m3=mean(x3)
 
   mhat3=2*mean(x3)-1
   y=abs(mhat3-5000)
   z=abs(estimate3-5000)
   value3[i]=y
   value33[i]=z
   
   error3 <- qt(0.975,df=30-1)*s3/sqrt(30)
   left3 <- mhat3 - error3
   right3 <- mhat3 + error3
   inside3 <- left3 <= 5000 && 5000 <= right3
   tvalue3[i]=inside3
   
   
 
 }
 
 sumValue = sum(unlist(value3))
  sumValue3 = sum(unlist(value33))
 exp3 = sumValue/1000
  maxexp3=sumValue3/1000
   true3= length(tvalue3[tvalue3==TRUE])
 
#fourth experiment, m=5000 n=100
 value4 = list()
 value44 = list()
 tvalue4=list()
  for(i in 1:1000){
 
    x4=sample(1:5000,100,replace=F)
      estimate4=max(x4)
    s4=sd(x4)
    m4=mean(x4)
 
    mhat4=2*mean(x4)-1
    y=abs(mhat4-5000)
    z=abs(estimate4-5000)
    value4[i]=y
    value44[i]=z
    
    error4 <- qnorm(0.975)*s4/sqrt(100)
    left4 <- mhat4 - error4
    right4 <- mhat4 + error4
    inside4 <- left4 <= 5000 && 5000 <= right4
    tvalue4[i]=inside4
 
  }
 
 sumValue = sum(unlist(value4))
  sumValue4 = sum(unlist(value44))
 exp4 = sumValue/1000
  maxexp4=sumValue4/1000
     true4= length(tvalue4[tvalue4==TRUE])
 

#DOES THE GRAPHING
par(mfrow=c(2,2))

experiments <-c(exp1,maxexp1)
barplot(experiments,main="n=30,m=500", xlab="Estimators",  
    ylab="MAD", names.arg=c("m hat","max estimator"), 
    border="blue", density=c(10,40))


experiments2 <-c(exp2,maxexp2)
barplot(experiments2,main="n=100,m=500", xlab="Estimators",  
    ylab="MAD", names.arg=c("m hat","max estimator"), 
    border="blue", density=c(10,40))

experiments3 <-c(exp3,maxexp3)
barplot(experiments3,main="n=30,m=5000", xlab="Estimators",  
    ylab="MAD", names.arg=c("m hat","max estimator"), 
    border="blue", density=c(10,40))


experiments4 <-c(exp4,maxexp4)
barplot(experiments4,main="n=100,m=5000", xlab="Estimators",  
    ylab="MAD", names.arg=c("m hat","max estimator"), 
    border="blue", density=c(10,40))



par(mfrow=c(2,2))


experiments1 <-c(true1,1000-true1)
barplot(experiments1,main="Confidence Interval n=30,m=500", xlab="TRUE OR FALSE",  
    ylab="AMOUNT OF TIMES INSIDE INTERVAL", names.arg=c("TRUE","FALSE"), 
    border="blue", density=c(30,60))


experiments22 <-c(true2,1000-true2)
barplot(experiments22,main=" Confidence Interval n=100,m=500", xlab="",  
    ylab="AMOUNT OF TIMES INSIDE INTERVAL", names.arg=c("TRUE","FALSE"), 
    border="blue", density=c(30,60))

experiments33 <-c(true3,maxexp3)
barplot(experiments33,main="Confidence Interval n=30,m=5000", xlab="",  
    ylab="AMOUNT OF TIMES INSIDE INTERVAL", names.arg=c("TRUE","FALSE"), 
    border="blue", density=c(30,60))


experiments44 <-c(true4,maxexp4)
barplot(experiments44,main="Confidence Interval n=100,m=5000", xlab="",  
    ylab="AMOUNT OF TIMES INSIDE INTERVAL", names.arg=c("TRUE","FALSE"), 
    border="blue", density=c(30,60))



