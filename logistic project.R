install.packages("VGAM")

#분석에 필요한 패키지 불러오기


library(VGAM)
library(haven)
library(foreign)

# 데이터 불러오기
spss_data <- read.spss("/Users/leeseunghoon/Desktop/Hoon/SSU/2-2/회귀분석2/Project/2003-2021_KGSS_07012022_public_v1_07012022.sav",to.data.frame=T, reencode='utf-8')
write.csv(spss_data,"spss")

# 필요한 변수선택
spss_data <- spss_data[,c("EMPLY","SEX","AGE","EDUC")]

# 성별(SEX), 종교(RELIG), 혼인상태(MARTIAL), 연령(AGE), 주관적 계층의식(RANK), 자녀 남아여아 선호도(APPCCSXB), 이상적인 자녀 수(IDLCHDN14), 응답자학력(EDUC), 동거가족 수(HOMPOP), 전반적인 환경에 대한 걱정(GRNCON), 응답자의 자녀 수(KIDNUM10), 14년도 총선 참여여부(VOTE 14),월평균가구소득(INCOM0), 사회에 대한 신뢰(RELIABLE), 정치진보보수성향(PARTYLR), 한국경제에 대한 만족(SATECO), 사회경제적 성공여부(SUCCESS), 월평균 가구소득_범주형(INCOME), 지지정당(PRTYID14), 한국경제에 대한 전망

# 결측치 제거
spss_data = na.omit(spss_data)

# 결측치 제거 확인
colSums(is.na(spss_data))



#[본론]
#[코드 및 실행결과]
#1. 변수 선택을 통한 최적 모형 적합
#코드
# 최대 모형 적합
log.fit1 <- glm(factor(EMPLY)~factor(SEX)+factor(RELIG)+AGE+RANK+factor(APPCCSXB)+IDLCHDN14+EDUC+HOMPOP+KIDNUM10+factor(VOTE14)+INCOM0+RELIABLE+factor(PARTYLR)+factor(SUCCESS)+factor(PRTYID14), family = binomial, data = spss_data)

# 상수항만을 사용하는 모형 적합
log.fit2 <- glm(factor(EMPLY)~1, family = binomial, data = spss_data)

# 단계적 선택법
bestmodel_both = step(log.fit1, direction="both")
bestmodel_both$formula
# factor(EMPLY) ~ factor(SEX) + AGE + factor(APPCCSXB) + EDUC

# 전진 선택법
bestmodel_forward = step(log.fit2, direction="forward",scope=list(lower=log.fit2,upper=log.fit1))
bestmodel_forward$formula
# factor(EMPLY) ~ factor(SEX) + EDUC + factor(APPCCSXB) + AGE

# 후진제거법
bestmodel_backward = step(log.fit1, direction="backward")
bestmodel_backward$formula
# factor(EMPLY) ~ factor(SEX) + AGE + factor(APPCCSXB) + EDUC


#실행결과
# 세 가지 방법 모두 같은 결과를 나타냄.





#2. 일반화 선형 모형을 이용한 로지스틱 회귀분석

#코드
# 2가 미취업이라 pi가 취업확률이 되게 설정
spss_data$EMPLY = relevel(factor(spss_data$EMPLY),ref = 2)

# 기준범주를 상관없음으로 변경
spss_data$APPCCSXB = relevel(factor(spss_data$APPCCSXB),ref=3) 

#1. 최적모형 적합

glm.fit = glm(EMPLY ~ factor(SEX) + EDUC + AGE,family = binomial(link=logit),data=spss_data)
summary(glm.fit) # deviance  = 1692.0

qchisq(0.95,1331) # 1416.987 

#2. 연속형 변수의 재곱항을 추가한 모형의 적합

glm.fit2 = glm(EMPLY ~ factor(SEX) + EDUC + APPCCSXB + AGE + ECOPROS + I(EDUC^2) + I(AGE^2) + I(ECOPROS^2),family = binomial(link=logit),data=spss_data)
summary(glm.fit2) # 1440.2 
qchisq(0.95,1327) # 1412.86 

#3. ECOPROS 변수를 제거한 모형의 적합

glm.fit3 = glm(EMPLY ~ factor(SEX) + EDUC + APPCCSXB + AGE + I(EDUC^2) + I(AGE^2),family = binomial(link=logit),data=spss_data)
summary(glm.fit3) # 1444.2 
qchisq(0.95,1329) # 1414.923 

anova(glm.fit3,glm.fit2, test = "Chisq") 
anova(glm.fit,glm.fit3, test = "Chisq") 
#실행결과

#1. 최적모형 적합 결과
glm.fit = glm(EMPLY ~ factor(SEX) + EDUC + APPCCSXB + AGE + ECOPROS,family = binomial(link=logit),data=spss_data)
summary(glm.fit) # deviance  = 1692.0

qchisq(0.95,1331) # 1416.987

# => deviance가 높아 적합도가 낮다고 판단됨.


#2. 연속형 변수의 재곱항을 추가한 모형의 적합결과
glm.fit2 = glm(EMPLY ~ factor(SEX) + EDUC + APPCCSXB + AGE + ECOPROS + I(EDUC^2) + I(AGE^2) + I(ECOPROS^2),family = binomial(link=logit),data=spss_data)
summary(glm.fit2) # 1440.2 

qchisq(0.95,1327) # deviance =  1412.86 


#3. ECOPROS 변수를 제거한 모형의 적합 결과
glm.fit3 = glm(EMPLY ~ factor(SEX) + EDUC + APPCCSXB + AGE + I(EDUC^2) + I(AGE^2),family = binomial(link=logit),data=spss_data)
summary(glm.fit3)

qchisq(0.95,1329) # deviance =  1414.923


#4.  glm.fit3,glm.fit2 모형 비교
anova(glm.fit3,glm.fit2, test = "Chisq")
# => 두 모형이 같다는 귀무가설을 채택하여 변수가 더 적은 glm.fit3 모형을 선택


#5. glm.fit,glm.fit3 모형 비교

anova(glm.fit,glm.fit3, test = "Chisq") # glm.fit3 선택
#=> 두 모형이 같다는 귀무가설을 기각하여  glm.fit3 모형을 선택

#결과해석
#glm.fit3의 적합 결과 해석



#3. 비례-오즈 모형을 통한 다항 로지스틱 회귀분석

#코드
Emply_2 = rep(c("Empolyed","Unempolyed"),3); Emply_2 = factor(Emply_2); Emply_2 = relevel(Emply_2,ref="Empolyed")
Income = rep(c("Low_Income","Middle_Income","High_Income"),each=2); Income = factor(Income); Income = relevel(Income,ref="High_Income")


attach(spss_data)
b.Success_1 = c(sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 1 ] )),sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 2])),sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 2] )),sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 1 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 2] )))

b.Success_2 = c(sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 1 ] )),sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 2])),sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 2] )),sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 2 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 2] )))

b.Success_3 = c(sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 1 ] )),sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 2])),sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 2] )),sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 3 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 2] )))

b.Success_4 = c(sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 1 ] )),sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 2])),sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 2] )),sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 4 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 2] )))

b.Success_5 = c(sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 1 ] )),sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 0|INCOME ==1|INCOME ==2|INCOME ==3|INCOME ==4|INCOME ==5|INCOME ==6|INCOME ==7) & EMPLY == 2])),sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 8|INCOME ==9|INCOME ==10|INCOME ==11|INCOME ==12|INCOME ==13|INCOME ==14) & EMPLY == 2] )),sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 1] )),sum(table(SUCCESS[SUCCESS == 5 & (INCOME == 15|INCOME ==16|INCOME ==17|INCOME ==18|INCOME ==19|INCOME ==20|INCOME ==21) & EMPLY == 2] )))
detach(spss_data)

# 분할표 생성
Success2 = data.frame(Income,Emply_2,b.Success_1,b.Success_2,b.Success_3,b.Success_4,b.Success_5); Success2
fit.P2 = vglm(cbind(b.Success_1,b.Success_2,b.Success_3,b.Success_4,b.Success_5) ~ Income + Emply_2, family = cumulative(parallel=TRUE), data = Success2)
summary(fit.P2)
data.frame(Income,Emply_2,fitted(fit.P2))
#실행결과
#1. 분할표 생성결과

Income         Emply_2    Success_1 Success_2 Success_3 Success_4 Success_5
1    Low_Income     Empolyed           4              46             195              49               8
2    Low_Income   Unempolyed         6              55             181              52              12
3  Middle_Income   Empolyed           2              77             193              33               4
4  Middle_Income Unempolyed         8              36             109              14               4
5   High_Income     Empolyed           9              68               80               8                4
6   High_Income   Unempolyed         2              17              46                3                0



#2. 비례오즈 모형 적합결과

Call:
  vglm(formula = cbind(b.Success_1, b.Success_2, b.Success_3, b.Success_4, 
                       b.Success_5) ~ Income + Emply_2, family = cumulative(parallel = TRUE), 
       data = Success2)

Coefficients: 
  Estimate Std. Error z value Pr(>|z|)    
(Intercept):1       -3.03533    0.20988 -14.462  < 2e-16 ***
  (Intercept):2       -0.35892    0.13047  -2.751  0.00594 ** 
  (Intercept):3        2.63517    0.15259  17.270  < 2e-16 ***
  (Intercept):4        4.57247    0.22288  20.515  < 2e-16 ***
  IncomeLow_Income    -1.16967    0.15587  -7.504 6.19e-14 ***
  IncomeMiddle_Income -0.65641    0.15601  -4.208 2.58e-05 ***
  Emply_2Unempolyed   -0.04437    0.11323  -0.392  0.69517    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Names of linear predictors: logitlink(P[Y<=1]), logitlink(P[Y<=2]), logitlink(P[Y<=3]), logitlink(P[Y<=4])

Residual deviance: 26.1374 on 17 degrees of freedom

Log-likelihood: -61.743 on 17 degrees of freedom

Number of Fisher scoring iterations: 4 

Warning: Hauck-Donner effect detected in the following estimate(s):
  '(Intercept):1', '(Intercept):4'


Exponentiated coefficients:
  IncomeLow_Income IncomeMiddle_Income   Emply_2Unempolyed 
0.3104693           0.5187119           0.9566000


3. 각각의 확률 계산

Income        Emply_2     Success_1   Success_2   Success_3  Success_4   Success_5
1    Low_Income   Empolyed    0.01470139   0.1634989   0.6341727  0.15541891  0.03220810
2    Low_Income Unempolyed  0.01407233   0.1577227   0.6337211  0.16086370  0.03362022
3 Middle_Income   Empolyed   0.02432229   0.2416167   0.6126107  0.10191997  0.01953034
4 Middle_Income Unempolyed 0.02329129   0.2340765   0.6163676  0.10586633  0.02039833
5   High_Income   Empolyed    0.04585495   0.3653665   0.5218699  0.05668196  0.01022675
6   High_Income Unempolyed  0.04395231   0.3565702   0.5297449  0.05904685  0.01068577




