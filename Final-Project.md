Final Project - Heart Failure Analysis
================
David Olander
2022-12-16

# Introduction:

Heart failure is a syndrome that is a result of functional or structural
impairment of ventricles which can result in insufficient cardiac
output, and the heart fails to keep up with the needs of the body.^1
Cardiovascular diseases, or CVDs, is an umbrella term to describe all
the diseases of the heart or of the blood vessels; this includes heart
disease, heart attack, stroke, arrhythmia, and other heart or blood
vessel conditions.^2 It is estimated that approximately 17.9 million
lives are lost yearly due to CVDs worldwide.^3 This ranks as the number
one leading cause of death worldwide, higher than cancer. Heart failure
is usually an event caused by CVDs, and to explore this topic, I chose a
dataset to look at the association between certain variables and the
outcome of death from heart failure.

To explore this topic and conduct an analysis, I obtained a dataset from
Kaggle that included data on clinical data about heart failure. The
dataset is pre-processed and includes data for 299 patients with heart
failure in 2015. The variables that are included in this dataset are
age, anemia status, creatinine phosphokinase levels, diabetes status,
ejection fraction, high blood pressure status, platelet counts
(kiloplatelets/mL), serum creatinine levels in blood (mg/dL), serum
sodium levels in blood (mEq/L), sex, smoking status, and death event.

For my analysis, I have decided to answer the question of: what
variables are associated with death from heart failure, how can we model
this, and what is a sufficient prediction model to possibly predict the
outcome of death?

For my analysis, I will assess the association between ejection
fraction, serum sodium, and serum creatinine on the outcome, death from
heart failure, and while controlling for age. Because the outcome of
death from heart failure is binary, where death = 1 and no death = 0, I
decided to measure the association by fitting a logistic regression
model and obtain an odds ratio. The reason why I was interested in these
variables are a result of my exploratory data analysis and getting to
know my data better, where I determined that these covariates would be
important to include in my model. Additionally, I will create a couple
prediction models from the data to assess the logistic regression
covariates I included to including all the covariates.

## Exploratory Analysis:

![](Final-Project_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Figure 1: To begin my exploratory data analysis, I wanted to look at a plot to see variable importance to get a sense of what variables may be important to consider when looking at the outcome of death. While I ended up look at all the variables in my exploratory analysis, this gave me a start to what I should maybe consider when looking at association and modeling the relationship. The bigger dots suggest more variable importance, so I took note of this.

![](Final-Project_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Figure 2,3,4,5: Here are 4 plots of the varaible distributions that I found to be the most interesting and important. I looked at the distributions of each variable and grouped by death outcome to see if there was anything I could see different from the 2 group (death and no death). Note: more EDA can be found in the appendix

# Results (Route 2):

## Logistic Regression and Association:

To begin with association tests and logistic regression, I began fitting
various models. I first checked and verified assumptions for fitting a
logistic regression model, and proceeded. For my first model, I began
simple and had just age to predict the death outcome. Age was a
statistically significant variable for this model, and following *(Table
1)*, it had an AIC value of 359.993. An AIC value, or Akaike Information
Criteria, helps assess a model fit in relation to other models, hence
why I decided to assess my models this way. A lower AIC value relative
to its other models means a better fit. Next, I fit a model, model2,
with age and ejection fraction, and found that both covariates were also
statisitcally significant, with and the model having an AIC value of
333.4015, suggesting improvement to our model. Continuing, I fit another
model, model3, adding serum sodium level to the model, and found it was
also statistically significant, with an AIC value of 329.95. This
continues to show that the model is imporving fit. Furthermore, I
continued this with model4, adding on serum creatinine, and then finally
model5, which included an interaction term of age to help assess effect
modification since age is a confounder.

When looking at the AIC values and comparing them to each of the models,
we can see that model4, which included age, ejection fraction, serum
sodium, and serum creatinine to assess association to death from heart
failure had the best fit *(Table 2)*. Preference is given to the model
with the lowest AIC value. Furthermore, likelihood ratio tests were used
to compare a reduced model to a full model, where the null hypothesis
suggests that the reduced model is sufficient to model and the
alternative hypothesis suggests that the full model is preferred *(Table
3)*. In other words, I compared the two lowest AIC value models to see
which model would be preferred, and model4 was still better than model3
as we rejected the null hypothesis. Additionally, I ran another
likelihood ratio test to compare model5 and model4, and we failed to
reject the null hypothesis and that model4 is still preferred *(Table
4)*.

For our findings given the model selection of model4, our data shows
that those with one-unit higher in age have a 1.05 times the odds of
death associated with heart failure occurring compared to those with
one-unit lower, on average, holding all other covariates constant, with
a 95% CI: \[1.028, 1.079\]. This suggests that with age, the odds of
death is greater. Additionally, those with a one-unit higher in ejection
fraction have a 0.935 times the odds of death associated with heart
failure occurring compared to those with one-unit lower, on average,
holding all other covariates constant, with a 95% CI: \[0.908, 0.961\].
This suggests that higher ejection fractions have a protective effect on
heart failure. Those with one-unit higher in serum sodium levels have a
0.953 times the odds of death associated with heart failure occurring
compared to those with one-unit lower, on average, holding all other
covariates constant. However, the 95% CI \[0.894, 1.016\] includes 1,
suggesting that this may not significant. Finally, those with one-unit
higher in serum creatinine levels have a 1.88 times the odds of death
associated with heart failure occurring compared to those with one-unit
lower, on average, holding all other covariates constant, with a 95% CI:
\[1.406, 2.655\].

## Prediction Model:

Next, I wanted to get a prediction model and assess the accuracy. To do
this, given that the dataset is small, I decided that splitting up the
data into 80/20, with 80% of the data being the train set, and then 20%
being the test set, I would see which if the model I fit would be good
in prediction. I also decided to compare this to another model including
all the covariates to see which model would do better (although, this
model would result in over-fitting since it includes all the covariates,
so I wanted to see if my lower model would perform just as well or even
better). So I trained my model4 with the train set and we can see that
that model4 had a test set accuracy of 0.7699 *(Table 5)*. In comparison
to a model with all the covariates included in it, the test set accuracy
was 0.7657 *(Table 6)*. While both are very close in accuracy, it is
fair to say that the model4 that was developed was more accurate and has
a better fit, and may be more generalizable to a greater population
since it did not overfit. Overall, I am satisfied with the model that I
created and believe that its accuracy in predicting heart failure is
relatively high since there are so many different factors that can play
a part in leading up to death from heart failure.

# Conclusion:

The purpose of my analysis was to assess the association between
ejection fraction, serum sodium, and serum creatinine on the outcome,
death from heart failure, and while controlling for age, and model this.
Also, to create a prediction model from the data to assess the logistic
regression covariates I included versus including all the covariates.

To conclude, I have determined that the variables most associated with
death from heart disease are age, ejection fraction, serum creatinine,
and serum sodium levels. To best model this, I ran a logistic regression
model to best fit the binary nature of the outcome of death from heart
failure. I assessed various models and compared them with AIC and
likelihood ratio tests. I determined that a model with heart disease are
age, ejection fraction, serum creatinine, and serum sodium levels. In
parallel to this, I wanted to assess the prediction ability of the model
I have created in comparison to a model that included all the
covariates. I ultimately determined that the model that I created, which
had fewer covariates, was just as accurate as a model with all the
covariates. However, I came to the logical conclusion that the model
with all the covariates would likely be overfitting the data and would
not be generalizable; therefore, my model is likely to be more
generalizable and accurate to the real world.

I would say that my analysis was successful with fitting a regression
model and doing an association test, as well as doing a prediction
model. However, I would say that the time constraint hindered my
analysis as I chose a fairly small data set that may not be
representative of the US population or really any population. Therefore,
if I had more time, I would chose a different dataset that included many
more observations. I would also like to explore applying kNN or random
forest to the bigger dataset and comparing it to the approach of using
logistic regression. In other words, I would like to see if the logistic
regression would be just as close or even better than the models.

### TABLE 1: Shows the AIC values for various fitted logistic regression models

    ##     TABLE_1  model_1  model_2  model_3  model_4  model_5
    ## 1 AIC VALUE 359.9928 333.4015 329.9527 313.0898 315.5374

### TABLE 2: Logistic Regression Model fit with age, ejection fraction, serum sodium, and serum creatining

    ##                           OR      2.5 %       97.5 %    Estimate Std. Error
    ## (Intercept)       59.9748292 0.01004444 4.353118e+05  4.09392496 4.45542181
    ## age                1.0527795 1.02812862 1.079299e+00  0.05143385 0.01234615
    ## ejection_fraction  0.9356515 0.90872881 9.612421e-01 -0.06651218 0.01428788
    ## serum_sodium       0.9534492 0.89372396 1.015543e+00 -0.04766916 0.03238247
    ## serum_creatinine   1.8808690 1.40580940 2.655471e+00  0.63173389 0.15843305
    ##                      z value     Pr(>|z|)
    ## (Intercept)        0.9188636 3.581669e-01
    ## age                4.1659835 3.100131e-05
    ## ejection_fraction -4.6551457 3.237517e-06
    ## serum_sodium      -1.4720668 1.410028e-01
    ## serum_creatinine   3.9873869 6.680503e-05

### TABLE 3: Likelihood Ratio Test to assess model covariates

    ## Analysis of Deviance Table
    ## 
    ## Model 1: death ~ age + ejection_fraction + serum_sodium
    ## Model 2: death ~ age + ejection_fraction + serum_sodium + serum_creatinine
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1       295     321.95                          
    ## 2       294     303.09  1   18.863 1.405e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### TABLE 4: Likelihood Ratio Test to assess model covariates

    ## Analysis of Deviance Table
    ## 
    ## Model 1: death ~ age + ejection_fraction + serum_sodium + serum_creatinine
    ## Model 2: death ~ age + ejection_fraction + age * ejection_fraction + age * 
    ##     serum_sodium + age * serum_creatinine
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       294     303.09                     
    ## 2       291     299.54  3   3.5523    0.314

### TABLE 5: Prediction Model4

![](Final-Project_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

### TABLE 6: Prediction Model with all covariates

![](Final-Project_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# References:

1.  NHLBI. Know the Differences: Cardiovascular Disease, Heart Disease,
    Coronary Heart Disease. NHLBI - NIH. Know the Differences
    Cardiovascular Disease, Heart Disease, Coronary Heart Disease.
    Published 2019.

2.  WHO. Cardiovascular diseases. World Health Organization.
    <https://www.who.int/health-topics/cardiovascular-diseases/#tab=tab_1>.
    Published 2020.

3.  Gaziano TA, Bitton A, Anand S, Abrahams-Gessel S, Murphy A. Growing
    epidemic of coronary heart disease in low- and middle-income
    countries. Curr Probl Cardiol. 2010;35(2):72-115.
    <doi:10.1016/j.cpcardiol.2009.10.002>

4.  <https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package>

# Appendix:

``` r
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(ggplot2)
library(corrplot)
library(glmnet)
library(HistData)
library(caret)
library(randomForest)
library(gridExtra)

#Function to draw confusion matrix [source 4]:
draw_confusion_matrix <- function(cm) { 

  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)

  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)

  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')

  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)

  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
} 
```

``` r
## Loading in data:
heart_failure <- read.csv("heart_failure.csv")
```

``` r
# Exploratory Data Analysis:

## Look at Data Structure:
heart_failure %>% head()
```

    ##   age anaemia creatinine_phosphokinase diabetes ejection_fraction
    ## 1  75       0                      582        0                20
    ## 2  55       0                     7861        0                38
    ## 3  65       0                      146        0                20
    ## 4  50       1                      111        0                20
    ## 5  65       1                      160        1                20
    ## 6  90       1                       47        0                40
    ##   high_blood_pressure platelets serum_creatinine serum_sodium sex smoking time
    ## 1                   1    265000              1.9          130   1       0    4
    ## 2                   0    263358              1.1          136   1       0    6
    ## 3                   0    162000              1.3          129   1       1    7
    ## 4                   0    210000              1.9          137   1       0    7
    ## 5                   0    327000              2.7          116   0       0    8
    ## 6                   1    204000              2.1          132   1       1    8
    ##   DEATH_EVENT
    ## 1           1
    ## 2           1
    ## 3           1
    ## 4           1
    ## 5           1
    ## 6           1

``` r
str(heart_failure)
```

    ## 'data.frame':    299 obs. of  13 variables:
    ##  $ age                     : num  75 55 65 50 65 90 75 60 65 80 ...
    ##  $ anaemia                 : int  0 0 0 1 1 1 1 1 0 1 ...
    ##  $ creatinine_phosphokinase: int  582 7861 146 111 160 47 246 315 157 123 ...
    ##  $ diabetes                : int  0 0 0 0 1 0 0 1 0 0 ...
    ##  $ ejection_fraction       : int  20 38 20 20 20 40 15 60 65 35 ...
    ##  $ high_blood_pressure     : int  1 0 0 0 0 1 0 0 0 1 ...
    ##  $ platelets               : num  265000 263358 162000 210000 327000 ...
    ##  $ serum_creatinine        : num  1.9 1.1 1.3 1.9 2.7 2.1 1.2 1.1 1.5 9.4 ...
    ##  $ serum_sodium            : int  130 136 129 137 116 132 137 131 138 133 ...
    ##  $ sex                     : int  1 1 1 1 0 1 1 1 0 1 ...
    ##  $ smoking                 : int  0 0 1 0 0 1 0 1 0 1 ...
    ##  $ time                    : int  4 6 7 7 8 8 10 10 10 10 ...
    ##  $ DEATH_EVENT             : int  1 1 1 1 1 1 1 1 1 1 ...

``` r
## Renaming Variable(s):
heart_failure <- heart_failure %>%
  rename(death = DEATH_EVENT)

heart_failure <- subset(heart_failure, select = -c(time))

corr <- cor(heart_failure)
corrplot(corr)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
## Number of Variables:
length(heart_failure)
```

    ## [1] 12

``` r
## Number of Rows:
nrow(heart_failure)
```

    ## [1] 299

``` r
## Distribution of Certain Variables:
### Age
heart_failure %>%
  ggplot(aes(x = age)) + geom_histogram(bins = 15) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Age")
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
heart_failure %>%
  ggplot(aes(x = age, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "identity") +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Age by Death Outcome", color = "Death Event") +
  theme_bw()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-Project_files/figure-gfm/unnamed-chunk-15-3.png)<!-- -->

``` r
summary(heart_failure$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   40.00   51.00   60.00   60.83   70.00   95.00

``` r
### Creatinine Phosphokinase
heart_failure %>%
  ggplot(aes(x = creatinine_phosphokinase)) + geom_histogram() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +   
  ggtitle("Creatinine Phosphokinase")
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Final-Project_files/figure-gfm/unnamed-chunk-15-4.png)<!-- -->

``` r
summary(heart_failure$creatinine_phosphokinase)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    23.0   116.5   250.0   581.8   582.0  7861.0

``` r
### Platelets
heart_failure %>%
  ggplot(aes(x = platelets)) + geom_bar() + ylim(0,2) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Platelets")
```

    ## Warning: Removed 27 rows containing missing values (`geom_bar()`).

![](Final-Project_files/figure-gfm/unnamed-chunk-15-5.png)<!-- -->

``` r
heart_failure %>%
  ggplot(aes(x = platelets, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "identity", bins = 10) +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Platelet by Death Outcome", color = "Death Event") +
  theme_bw()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-6.png)<!-- -->

``` r
summary(heart_failure$platelets)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   25100  212500  262000  263358  303500  850000

``` r
## Ejection Fraction
hist(heart_failure$ejection_fraction)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-7.png)<!-- -->

``` r
heart_failure %>%
  ggplot(aes(x = ejection_fraction, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "identity", bins = 10) +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Ejection Fraction by Death Outcome", color = "Death Event") +
  theme_bw()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-8.png)<!-- -->

``` r
## Serum Sodium:
heart_failure %>%
  ggplot(aes(x = serum_sodium, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "identity", bins = 10) +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Serum Sodium by Death Outcome", color = "Death Event") +
  theme_bw()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-9.png)<!-- -->

``` r
## Diabetes:
heart_failure %>%
  ggplot(aes(x = diabetes, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "dodge", bins = 10) +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Diabetes by Death Outcome", color = "Death Event") +
  theme_bw()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-10.png)<!-- -->

``` r
## Smoking:
heart_failure %>%
  ggplot(aes(x = smoking, group = factor(death), fill = factor(death))) + 
  geom_histogram(position = "dodge", bins = 10) +
  scale_fill_discrete(name = "Death Event", labels = c("No death", "Death")) +
  labs(title = "Smoking by Death Outcome", color = "Death Event") +
  theme_bw()
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-11.png)<!-- -->

``` r
## Checking Missing Data:
sum(is.na(heart_failure))
```

    ## [1] 0

``` r
## Count of Death Events:
sum(heart_failure$death)
```

    ## [1] 96

``` r
# Graphs:
plot(heart_failure$age, heart_failure$diabetes)
```

![](Final-Project_files/figure-gfm/unnamed-chunk-15-12.png)<!-- -->

# Logistic Regression, Model Selection, Association:

``` r
death_factor <- as.factor(heart_failure$death)

# With Just Age as Covariate:
model1 <- glm(death ~ age, data = heart_failure, family = "binomial")
logOR.scale <- summary(model1)$coefficients
OR.scale <- exp(cbind(OR = coef(model1), confint(model1)))
```

    ## Waiting for profiling to be done...

``` r
cbind(OR.scale, logOR.scale)
```

    ##                     OR      2.5 %     97.5 %    Estimate Std. Error   z value
    ## (Intercept) 0.02587886 0.00620156 0.09979039 -3.65432880 0.70661721 -5.171582
    ## age         1.04806947 1.02602488 1.07168816  0.04694987 0.01107111  4.240755
    ##                 Pr(>|z|)
    ## (Intercept) 2.321205e-07
    ## age         2.227688e-05

``` r
model1$aic
```

    ## [1] 359.9928

``` r
# Age and Ejection Fraction:
model2 <- glm(death ~ age + ejection_fraction, data = heart_failure, family = "binomial")
logOR.scale <- summary(model2)$coefficients
OR.scale <- exp(cbind(OR = coef(model2), confint(model2)))
```

    ## Waiting for profiling to be done...

``` r
cbind(OR.scale, logOR.scale)
```

    ##                          OR      2.5 %    97.5 %    Estimate Std. Error
    ## (Intercept)       0.1661441 0.03381588 0.7859550 -1.79489972 0.79987731
    ## age               1.0574074 1.03374437 1.0830121  0.05582006 0.01183810
    ## ejection_fraction 0.9362606 0.91069761 0.9604511 -0.06586146 0.01353153
    ##                     z value     Pr(>|z|)
    ## (Intercept)       -2.243969 2.483441e-02
    ## age                4.715287 2.413699e-06
    ## ejection_fraction -4.867258 1.131573e-06

``` r
model2$aic
```

    ## [1] 333.4015

``` r
# Age, EF, and Sodium:
model3 <- glm(death ~ age + ejection_fraction + serum_sodium, data = heart_failure, family = "binomial")
logOR.scale <- summary(model3)$coefficients
OR.scale <- exp(cbind(OR = coef(model3), confint(model3)))
```

    ## Waiting for profiling to be done...

``` r
cbind(OR.scale, logOR.scale)
```

    ##                             OR     2.5 %       97.5 %    Estimate Std. Error
    ## (Intercept)       2917.5327680 0.6777970 1.792618e+07  7.97849360 4.33689957
    ## age                  1.0561640 1.0323735 1.081873e+00  0.05464347 0.01190790
    ## ejection_fraction    0.9407637 0.9149182 9.652999e-01 -0.06106324 0.01363379
    ## serum_sodium         0.9301466 0.8728174 9.885517e-01 -0.07241306 0.03160279
    ##                     z value     Pr(>|z|)
    ## (Intercept)        1.839677 6.581570e-02
    ## age                4.588841 4.457137e-06
    ## ejection_fraction -4.478815 7.505841e-06
    ## serum_sodium      -2.291351 2.194315e-02

``` r
model3$aic
```

    ## [1] 329.9527

``` r
#Age, EF, and serum_sodium, serum_creatinine:
model4 <- glm(death ~ age + ejection_fraction + serum_sodium + serum_creatinine, data = heart_failure, family = "binomial")
logOR.scale <- summary(model4)$coefficients
OR.scale <- exp(cbind(OR = coef(model4), confint(model4)))
```

    ## Waiting for profiling to be done...

``` r
cbind(OR.scale, logOR.scale)
```

    ##                           OR      2.5 %       97.5 %    Estimate Std. Error
    ## (Intercept)       59.9748292 0.01004444 4.353118e+05  4.09392496 4.45542181
    ## age                1.0527795 1.02812862 1.079299e+00  0.05143385 0.01234615
    ## ejection_fraction  0.9356515 0.90872881 9.612421e-01 -0.06651218 0.01428788
    ## serum_sodium       0.9534492 0.89372396 1.015543e+00 -0.04766916 0.03238247
    ## serum_creatinine   1.8808690 1.40580940 2.655471e+00  0.63173389 0.15843305
    ##                      z value     Pr(>|z|)
    ## (Intercept)        0.9188636 3.581669e-01
    ## age                4.1659835 3.100131e-05
    ## ejection_fraction -4.6551457 3.237517e-06
    ## serum_sodium      -1.4720668 1.410028e-01
    ## serum_creatinine   3.9873869 6.680503e-05

``` r
model4$aic
```

    ## [1] 313.0898

``` r
# Interaction Term:
model5 <- glm(death ~ age + ejection_fraction + age*ejection_fraction+ age*serum_sodium + age*serum_creatinine, data = heart_failure, family = "binomial")


# LRT to see if reduced or full model preferred:
anova(model3, model4, test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: death ~ age + ejection_fraction + serum_sodium
    ## Model 2: death ~ age + ejection_fraction + serum_sodium + serum_creatinine
    ##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
    ## 1       295     321.95                          
    ## 2       294     303.09  1   18.863 1.405e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# LRT to see if interaction terms needed:
anova(model4, model5, test = "Chisq")
```

    ## Analysis of Deviance Table
    ## 
    ## Model 1: death ~ age + ejection_fraction + serum_sodium + serum_creatinine
    ## Model 2: death ~ age + ejection_fraction + age * ejection_fraction + age * 
    ##     serum_sodium + age * serum_creatinine
    ##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
    ## 1       294     303.09                     
    ## 2       291     299.54  3   3.5523    0.314

# Prediction:

``` r
set.seed(1)
dt = sort(sample(nrow(heart_failure), nrow(heart_failure)*.8))
train <- heart_failure[dt,]
test <- heart_failure[-dt,]

training_model <- glm(death ~ ., data = train, family = "binomial")

results <- train %>% 
  mutate(pred_prob_model = predict(training_model, newdata = train, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0))

results$death <- as.factor(results$death)
results$pred_outcome_model <- as.factor(results$pred_outcome_model)

confusionMatrix(results$pred_outcome_model, results$death)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 148  40
    ##          1  16  35
    ##                                           
    ##                Accuracy : 0.7657          
    ##                  95% CI : (0.7068, 0.8179)
    ##     No Information Rate : 0.6862          
    ##     P-Value [Acc > NIR] : 0.004183        
    ##                                           
    ##                   Kappa : 0.4042          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.002116        
    ##                                           
    ##             Sensitivity : 0.9024          
    ##             Specificity : 0.4667          
    ##          Pos Pred Value : 0.7872          
    ##          Neg Pred Value : 0.6863          
    ##              Prevalence : 0.6862          
    ##          Detection Rate : 0.6192          
    ##    Detection Prevalence : 0.7866          
    ##       Balanced Accuracy : 0.6846          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
training_model <- glm(death ~ age + ejection_fraction + serum_sodium + serum_creatinine, data = heart_failure, family = "binomial")

results <- train %>% 
  mutate(pred_prob_model = predict(training_model, newdata = train, type = "response")) %>% 
  mutate(pred_outcome_model = ifelse(pred_prob_model >= 0.5, 1,0))

results$death <- as.factor(results$death)
results$pred_outcome_model <- as.factor(results$pred_outcome_model)


confusionMatrix(results$pred_outcome_model, results$death)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 148  39
    ##          1  16  36
    ##                                           
    ##                Accuracy : 0.7699          
    ##                  95% CI : (0.7112, 0.8217)
    ##     No Information Rate : 0.6862          
    ##     P-Value [Acc > NIR] : 0.002680        
    ##                                           
    ##                   Kappa : 0.4172          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.003012        
    ##                                           
    ##             Sensitivity : 0.9024          
    ##             Specificity : 0.4800          
    ##          Pos Pred Value : 0.7914          
    ##          Neg Pred Value : 0.6923          
    ##              Prevalence : 0.6862          
    ##          Detection Rate : 0.6192          
    ##    Detection Prevalence : 0.7824          
    ##       Balanced Accuracy : 0.6912          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

# 
