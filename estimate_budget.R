
#########################
## Poll Details
## link here - http://www.ropercenter.uconn.edu/CFIDE/cf/action/ipoll/abstract.cfm?keyword=&keywordoptions=1&exclude=&excludeOptions=1&topic=Any&organization=CNN&label=&fromdate=3/1/2011&toDate=3/20/2011&&archno=USORCCNN2011-004&start=summary
## directory with poll data and topline - https://github.com/kyledropp/Budgets.git
#########################


#########################
## Read Poll
## Need Internet connection
#########################

install.packages('RCurl') #if you don't have package
install.packages('foreign') #if you don't have package
install.packages('car') #if you don't have package
install.packages('survey') #if you don't have package

library(RCurl)
library(foreign)
library(car)
library(survey)

link = 'https://raw.githubusercontent.com/kyledropp/Budgets/master/cnn_budget.csv'
link.data <- getURL(link)                
df <- read.csv(textConnection(link.data))

dim(df)
names(df)
head(df)

#########################
## Budget percentage questions
## Results from survey
## http://i2.cdn.turner.com/cnn/2011/images/03/31/rel4m.pdf
## 1= less than 1%; 2= 1-5%; 3 = 6-10%; 4=11-20%; 5=21-30%; 6=31-50%; 7=50%+; 9=no opinion; 99=not asked
#########################

table(df$qq39a)
table(df$qq39b)
table(df$qq39c)
table(df$qq39d)
table(df$qq39e)
table(df$qq39f)
table(df$qq39g)
table(df$qq39h)
table(df$qq39i)

#########################
## Recode budget variables

df$qq39aNET = recode(df$qq39a,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39bNET = recode(df$qq39b,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39cNET = recode(df$qq39c,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39dNET = recode(df$qq39d,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39eNET = recode(df$qq39e,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39fNET = recode(df$qq39f,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39gNET = recode(df$qq39g,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39hNET = recode(df$qq39h,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39iNET = recode(df$qq39i,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")
df$qq39jNET = recode(df$qq39i,"1=1;2=2;3=3;4=4;5=5;6=6;7=7;else=NA")


#########################
## Recode demographic variables

df$age = ifelse(df$d2==-1,NA,df$d2)  ## drop people who refused
df$hisp = ifelse(df$d4==9,NA,df$d4)
df$white = ifelse(df$d5_1==1,1,0)  # yes=1, no=0
df$black = ifelse(df$d5_2==1,1,0)  # yes=1, no=0
df$educ = ifelse(df$d1==9,NA,df$d1)




#########################
## Add survey weights to the data
svy1 = svydesign(ids=~1,data=df,weights=~weight)

## These proportions basically match page 2 in http://i2.cdn.turner.com/cnn/2011/images/03/31/rel4m.pdf
## Slightly different because I dropped the don't know response
prop.table(svytable(~qq39aNET,svy1))
prop.table(svytable(~qq39bNET,svy1))


#########################
## Regression of perceived spending (7=50%+; 1=less than 1%) on background characteristics

## more educated, men say less goes to foreign aid
summary(lm(qq39eNET~age+educ+sex+factor(d7a),data=df))

## more educated, men, democrats say less goes to welfare
summary(lm(qq39gNET~age+educ+sex+factor(d7a),data=df))







