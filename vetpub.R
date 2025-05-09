
a<- c(1,2,3)
a
mean(a)

#### 패키지(package) ####

# 패키지 설치

install.packages("ggplot2")# 괄호안에 설치할 패키지 이름 입력, 앞 뒤에 따옴표를 넣어야함
library("ggplot2") # 패키지를 로드하기, 따옴표를 넣어도 되고 안 넣어도 됨

#### R 데이터 프레임 다루기 ####
### 외부데이터 이용하기
### csv 파일 불러오기
# csv란? 대부분 프로그램에서 사용하는 범용 데이터 파일 
# CSV : Comma-Seperated Values)

## 데이터 설명 : 병원에 방문한 개들에게 프로바이오틱스를 먹이기 전, 
## blautia 균의 양을 측정하고 프로바이오틱스를 먹인후 blautia 균의 양을 측정하여 
## 영양제의 치료효과를 확인함
## id : 방문견 번호
## group: low- 저용양  mid- 중간용량  control- 먹이지 않음
## sex : 성별,  F=Female/암컷  M=Male/수컷 
## age : 나이, 단위는 년
## bw : 몸무게  body weight,  단위는 kg
## before : 프로바이오틱스를 먹기전 blautia 균의 양
## after : 프로바이오틱스를 먹은후 blautia 균의 양


## 데이터 프레임 파악하기  
## 데이터가 주어질때 가장 먼저 하는일 : 데이터의 전반적인 구조 파악
setwd("D:/vetpub_R")
getwd()
dog <- read.csv("csv_data.csv")
dog

head(dog) # 앞에서 부터 6행까지 출력
summary(dog) # 전체 요약

mean(dog$before) #평균 mean
sd(dog$before) # 표준편차, standard deviation

mean(dog$after)
sd(dog$after)

mean(dog$after)

# dog를 데이터 테이블화
install.packages("data.table")
library("data.table")

dt<-data.table(dog)
# 그룹별 프로바이오틱스 급이 전 blautia 균량 평균 및 표준편차
dt[,list(mean=mean(Blautia_before),sd=sd(Blautia_before)),by=group]
# 그룹별 프로바이오틱스 급이 후 blautia  균량 평균 및 표준편차
dt[,list(mean=mean(Blautia_after),sd=sd(Blautia_after)),by=group]

#### 그래프 그려보기 ####

## ggplot(데이터이름) +  geom_기능(aes(변수정보))

## scatter plot 점그래프
# dog 데이터에서  x 축은 age로 y 는 bw로
ggplot(dog)+
  geom_point(aes(x=age, y=Blautia_after))

# 사이즈 3, 색깔 파란색, 모양 X자
ggplot(dog)+
  geom_point(aes(x=age, y=Blautia_after),size=3,color="blue",shape=4)

## boxplot
ggplot(data=dog, aes(x=group, y=Blautia_before))+
  geom_boxplot()

ggplot(data=dog, aes(x=group, y=Blautia_after))+
  geom_boxplot()


# group별로 색깔 부여
ggplot(data=dog, aes(x=group, y=Blautia_before, color=group))+
  geom_boxplot()

# box 안에 group색깔 부여
ggplot(data=dog, aes(x=group, y=Blautia_before, fill= group))+
  geom_boxplot()

# boxplot과 점그래프를 한번에
ggplot(data=dog, aes(x=group, y=Blautia_before))+
  geom_boxplot()+
  geom_jitter(size=1.5, width=0.2,height=1.5)

#### 데이터 통계분석 ####

dog

#### 첫번째 통계분석
## anova test
## 3 개의 다른 군에서 각 데이터 간의 통계적으로 차이점이 있는지 파악하는 분석

## dog  데이터에서 group 별로 before, 프로바이오틱스 먹기 전 blautia 균량간의 통계적 차이를 분석
before.aov <- aov(Blautia_before ~ group, data = dog)
summary(before.aov)

## dog  데이터에서 group 별로 after, 프로바이오틱스 먹기 후 blautia 균량간의 통계적 차이를 분석
after.aov <- aov(Blautia_after ~ group, data = dog)
summary(after.aov)

## 결과 해석
## p value < 0.05 : 통계적으로 유의한 차이
## p value > 0.05 : 통계적으로 유의미한 차이가 없다. 
## 프로바이오틱스를 먹기 전 blautia 균량간의 통계적 차이는 없었으나 
## 먹은후 그룹간에 통계적으로 유의한 균량의 차이를 보였다.

#### 2. paired t test 
## 같은 군내에서 프로바이오틱스 섭취전 후 간의 통계적인 차이가 있는지를 확인하는 분석

## 먼저 데이터프레임을 분해
# 저용량군 추출
low <- subset(dog,subset = group == 'low')
low

# low 에서 beforenormal, low 에서 treat 간의 paired-t-test
t.test(low$Blautia_before, low$Blautia_after, paired = TRUE)

## 결과해석
## 프로바이오틱스 투입 전후로 통계적으로 유의한 균량의 차이를 보였다.
## paired= TRUE 명령어 -> 같은 열에 있는 데이터를 짝지어서 분석할 때 쓰는 명령어
## 만약 데이터 숫자가 다르면 생략

# 중간용량군 추출
mid <- subset(dog,subset = group == 'mid')
mid
t.test(mid$Blautia_before, mid$Blautia_after, paired = TRUE)


# 대조군군 추출
con <- subset(dog,subset = group == 'control')
con
t.test(con$Blautia_before, con$Blautia_after, paired = TRUE)
