path<- "C:\\Users\\CHIRAG\\Downloads\\ACADgILd"
setwd(path)
library(readr)
bank_full<-read_delim("bank-full.csv",";",escape_double = F,trim_ws = T)
bank_full1<-read_delim("bank-additional-full.csv",";",escape_double = F,trim_ws = T)
summary(bank_full)
is.na(bank_full)
View(bank_full1)
#deep check of NA value 
if(length(which(is.na(bank_full$age)==TRUE)>0))
{
  print("missing values found in specified column")
}else
  print("everything is ok")

if(length(which(is.na(bank_full$marital)==TRUE)>0))
{
  print("missing values found in specified column")
}else
  print("everything is ok")

head(bank_full)
install.packages("Amelia")
library(Amelia)
missmap(bank_full,main = "missing data - blank",col = c("red","grey"),legend = FALSE)
# there is no red line in the visual, its safe to say there are no missing values 

# a. check the relation between jobs and default 

with(bank_full,chisq.test(job,default))
with(bank_full,table(job,default))

#H0 = the two varialbes are independent  
#Ha = the two variables are dependent 
#after getting the results we can see that p value <0.05 so we reject the Null hypothesis

# b.Is there any significant difference in duration of last call between people having housing loan or not?

with(bank_full,chisq.test(duration,housing))

#H0 = there is no significant diffrence between people having a loan vs people dont a have loan
#Ha = there is significant diffrence between people having a loan vs people dont a have loan
#after getting the results we can see that p value >0.05 so we accept the Null hypothesis


# c. Is there any association between consumer price index and consumer?

with(bank_full1, chisq.test(cons.price.idx,cons.conf.idx))

#H0 = there is no association between consumer price and consumer
#Ha = there is a association between consumer price and consumer
#after getting the results we can see that p value < 0.05 so we reject the Null hypothesis


# d.Is the employment variation rate consistent across job types?

with(bank_full1, chisq.test(job,emp.var.rate))

#H0 = employment variation rate is consistent 
#Ha = employment variation rate is not consistent 
#after getting the results we can see that p value < 0.05 so we reject the Null hypothesis


#e.Is the employment variation rate same across education?

with(bank_full1, chisq.test( education,emp.var.rate))

#H0 = employment variation rate is same across education 
#Ha = employment variation rate is not same across education
#after getting the results we can see that p value < 0.05 so we reject the Null hypothesis



