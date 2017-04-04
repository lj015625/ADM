#Calculating Information Gain
# Entropy
# 9 out of 15  5 out of 14
Info_Buy<-((-9/14)*log2(9/14))-((5/14)*log2(5/14)) #expected information
print(Info_Buy)

#information needed
# under 30 bought vs not bought
Info_Age_Under30<-((-2/5)*log2(2/5))-((3/5)*log2(3/5))
print(Info_Age_Under30)

Info_Age_31_40<-((-4/4)*log2(4/4))
print(Info_Age_31_40)

Info_Age_Over40<-((-3/5)*log2(3/5))-((2/5)*log2(2/5))
print(Info_Age_Over40)

# add three Entropy bought
Info_Age <- (5/14)*Info_Age_Under30 + (4/14)*Info_Age_31_40 + (5/14)*Info_Age_Over40
print(Info_Age)

# information gain = EntropyA - EntropyB
Info_Buy - Info_Age #information gained.


# Gain Ratio

Split_Info = -((5/14)*log2(5/14))-((4/14)*log2(4/14))-((5/14)*log2(5/14))
Gain_Ratio_Age = (Info_Buy-Info_Age)/Split_Info
print(Gain_Ratio_Age)


#Calculating Gini Gain

Gini_Buy<-1-((9/14)^2+(5/14)^2)
print(Gini_Buy)

Gini_Age_Under30<-1-((2/5)^2+(3/5)^2)
Gini_Age_31_40<-1-((4/4)^2+(0/4)^2)
Gini_Age_Over_40<-1-((3/5)^2+(2/5)^2)

print(Gini_Age_Under30)
print(Gini_Age_31_40)
print(Gini_Age_Over_40)

Gini_Age <- (5/14)*Gini_Age_Under30 + (4/14)*Gini_Age_31_40 + (5/14)*Gini_Age_Over_40
print(Gini_Age)

print(Gini_Buy - Gini_Age)



