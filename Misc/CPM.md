Stroke Clinical Predictive Model
================
Tosin Dairo
MAY 25, 2020

## Clinical Predictive Model classifier for stroke

Stroke-CPM is a tool that takes information available about a patient
and their observed predictive factors, and makes a prediction regarding
their diagnosis and causal factors.

  - Diagnosis – detects presence of stroke currently
  - Causal Factors – assess whether observed predictive factor(s) is/are
    likely to be contributors to the development of stroke presently or
    in the future

<br></br>

#### Observed Factors:

  - Gender
  - Age
  - Hypertension
  - Heart Disease
  - Ever Married
  - Work Type
  - Residence Type
  - Average Glucose Level
  - BMI
  - Smoking
    Status

<br></br>

``` r
{{knitr::spin_child('init.R')}}
```

``` r
head(d)
```

    ##      id gender age hypertension heart_disease ever_married    work_type
    ## 1 30669   Male   3            0             0           No     children
    ## 2 30468   Male  58            1             0          Yes      Private
    ## 3 16523 Female   8            0             0           No      Private
    ## 4 56543 Female  70            0             0          Yes      Private
    ## 5 46136   Male  14            0             0           No Never_worked
    ## 6 32257 Female  47            0             0          Yes      Private
    ##   Residence_type avg_glucose_level  bmi  smoking_status stroke
    ## 1          Rural             95.12 18.0                      0
    ## 2          Urban             87.96 39.2    never smoked      0
    ## 3          Urban            110.89 17.6                      0
    ## 4          Rural             69.04 35.9 formerly smoked      0
    ## 5          Rural            161.28 19.1                      0
    ## 6          Urban            210.95 50.1                      0

#### Missing Values

``` r
d$smoking_status <- na_if(d$smoking_status, "")
```

``` python
print(r.d.isnull().sum())
```

    ## id                       0
    ## gender                   0
    ## age                      0
    ## hypertension             0
    ## heart_disease            0
    ## ever_married             0
    ## work_type                0
    ## Residence_type           0
    ## avg_glucose_level        0
    ## bmi                   1462
    ## smoking_status       13292
    ## stroke                   0
    ## dtype: int64

``` python
fig, ax = plt.subplots(figsize=(14,12))
sns.heatmap(r.d.isnull(),yticklabels=False,cbar=False,cmap='viridis') 
```

![](CPM_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
d$gender <- as.factor(d$gender)
d$ever_married <- as.factor(d$ever_married)
d$work_type <- as.factor(d$work_type)
d$Residence_type <- as.factor(d$Residence_type)
d$smoking_status <- as.factor(d$smoking_status)
str(d)
```

    ## 'data.frame':    43400 obs. of  12 variables:
    ##  $ id               : int  30669 30468 16523 56543 46136 32257 52800 41413 15266 28674 ...
    ##  $ gender           : Factor w/ 3 levels "Female","Male",..: 2 2 1 1 2 1 1 1 1 1 ...
    ##  $ age              : num  3 58 8 70 14 47 52 75 32 74 ...
    ##  $ hypertension     : int  0 1 0 0 0 0 0 0 0 1 ...
    ##  $ heart_disease    : int  0 0 0 0 0 0 0 1 0 0 ...
    ##  $ ever_married     : Factor w/ 2 levels "No","Yes": 1 2 1 2 1 2 2 2 2 2 ...
    ##  $ work_type        : Factor w/ 5 levels "children","Govt_job",..: 1 4 4 4 3 4 4 5 4 5 ...
    ##  $ Residence_type   : Factor w/ 2 levels "Rural","Urban": 1 2 2 1 1 2 2 1 1 2 ...
    ##  $ avg_glucose_level: num  95.1 88 110.9 69 161.3 ...
    ##  $ bmi              : num  18 39.2 17.6 35.9 19.1 50.1 17.7 27 32.3 54.6 ...
    ##  $ smoking_status   : Factor w/ 4 levels "","formerly smoked",..: NA 3 NA 2 NA NA 2 3 4 3 ...
    ##  $ stroke           : int  0 0 0 0 0 0 0 0 0 0 ...

After checking for missing values, smoking status and bmi had missing
values which could lead drawing an inaccurate inference about the stroke
classification

#### Multiple Imputation for Missing Data

> To prevent bias in prediction, missing values are predicted and
> imputed into the stroke dataset

Using MICE package, Predictive Mean Matching and Poly Regreession
technique are used to imput data missing at random from smoking status
and
    bmi

``` r
imp <- mice(d, seed = 3333)
```

    ## Warning: Number of logged events: 25

``` r
imp$method
```

    ##                id            gender               age      hypertension 
    ##                ""                ""                ""                "" 
    ##     heart_disease      ever_married         work_type    Residence_type 
    ##                ""                ""                ""                "" 
    ## avg_glucose_level               bmi    smoking_status            stroke 
    ##                ""             "pmm"         "polyreg"                ""

``` r
dx <- mice::complete(imp)
```

``` python
print(r.dx.isnull().sum())
```

    ## id                   0
    ## gender               0
    ## age                  0
    ## hypertension         0
    ## heart_disease        0
    ## ever_married         0
    ## work_type            0
    ## Residence_type       0
    ## avg_glucose_level    0
    ## bmi                  0
    ## smoking_status       0
    ## stroke               0
    ## dtype: int64

``` python
fig, ax = plt.subplots(figsize=(14,12))
sns.heatmap(r.dx.isnull(),yticklabels=False,cbar=False,cmap='viridis') 
```

![](CPM_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

#### Exploratory Analysis

Stroke class from exploratory pairplot below shows imbalance in stroke
outcome, hypertension predictor and heart. Fitting a model on this
imbalanced dataset would lead to overfitting whereby the trained model
crams the train set data and predict wrongly on external datasets

``` python
fig, ax = plt.subplots(figsize=(14,12))
sns.pairplot(r.dx)
```

    ## <seaborn.axisgrid.PairGrid object at 0x12c0e24a8>

``` python
plt.show()
```

    ## /Volumes/Loopdisk/Dev/PyDsc/env/lib/python3.6/site-packages/matplotlib/figure.py:2299: UserWarning: This figure includes Axes that are not compatible with tight_layout, so results might be incorrect.
    ##   warnings.warn("This figure includes Axes that are not compatible "

![](CPM_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` python
sns.countplot(x='stroke', data = r.dx)
plt.title("Stroke Class Distribution")
plt.show()
```

    ## /Volumes/Loopdisk/Dev/PyDsc/env/lib/python3.6/site-packages/matplotlib/figure.py:2299: UserWarning: This figure includes Axes that are not compatible with tight_layout, so results might be incorrect.
    ##   warnings.warn("This figure includes Axes that are not compatible "

![](CPM_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
{{knitr::spin_child('label_encode.R')}}
```

In order for us to predict the minority class in the imbalanced
variable, the factor variables need to be encoded to vector types to
enable prediction through oversampleing of minority class.

``` r
result <- encode.fit_transform(dx)

dx_ <- result[[2]]
# encode.transform()
```

> Checking proportion of imbalance in the beelow variables

``` r
tab_df(table(dx_$stroke), title = "Stroke Class")
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

Stroke
Class

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">

0

</th>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">

1

</th>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; modelcolumn0 ">

42617

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double;  ">

783

</td>

</tr>

</table>

``` r
tab_df(table(dx_$hypertension), title = "Hypertension Class")
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

Hypertension
Class

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">

0

</th>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">

1

</th>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; modelcolumn0 ">

39339

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double;  ">

4061

</td>

</tr>

</table>

``` r
tab_df(table(dx_$heart_disease), title = "Heart Disease Class")
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

Heart Disease
Class

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">

0

</th>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">

1

</th>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; modelcolumn0 ">

41338

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double;  ">

2062

</td>

</tr>

</table>

``` r
dx_$hypertension <- as.factor(dx_$hypertension)
dx_$heart_disease <- as.factor(dx_$heart_disease)
dx_$stroke <- as.factor(dx_$stroke)
dx_$gender <- as.factor(dx_$gender)
dx_$ever_married <- as.factor(dx_$ever_married)
dx_$work_type <- as.factor(dx_$work_type)
dx_$Residence_type <- as.factor(dx_$Residence_type)
dx_$smoking_status <- as.factor(dx_$smoking_status)
str(dx_)
```

#### Class Imbalance

To predict the minoritty class in stroke, the train set data is expose
to SMOTE package inorder to use K-Nearest Neighbour for predicting
miority
class

``` r
## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
set.seed(3333)

balanced_dx_ <-SMOTE(stroke ~., dx_, perc.over = 2100 , k = 5, perc.under = 160)

tab_df(table(balanced_dx_$stroke), title = "Stroke Class")
```

<table style="border-collapse:collapse; border:none;">

<caption style="font-weight: bold; text-align:left;">

Stroke
Class

</caption>

<tr>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; text-align:left; ">

0

</th>

<th style="border-top: double; text-align:center; font-style:italic; font-weight:normal; padding:0.2cm; border-bottom:1px solid black; ">

1

</th>

</tr>

<tr>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; border-bottom: double; modelcolumn0 ">

26308

</td>

<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center; border-bottom: double;  ">

17226

</td>

</tr>

</table>

### Cross Validation:

For good predictions of stroke, a model must be well calibrated and have
high discrimination. The input data was divided into two groups, in
order to fit the data in one group and validate it in the other.This
helps to reducing training bias.

``` r
# Random sample indexes
train_index <- sample(1:nrow(balanced_dx_), 0.8 * nrow(balanced_dx_))
test_index <- setdiff(1:nrow(balanced_dx_), train_index)

# Build X_train, X_test
X_train <- balanced_dx_[train_index, 2:12]

X_test <- balanced_dx_[test_index, 2:12]
```

#### Model Training - Logistic Regression

To classify patients who have stroke or do not have stroke and to check
for contributing causal factors to stroke, a binary classifier is built
to predict the cases of stroke. State-of-the-art binary classifier
Logistic Regression is applied. As per the TRIPOD guidelines, we would
need to report this stroke clinical predictive model in sufficient
detail to allow for reproducability on a totally dufferent dataset.

``` r
set.seed(3333)
model <- glm (stroke ~ ., data=X_train, family = binomial)
summary(model)
```

``` r
stepAIC(model, direction='both')
```

``` r
model_select <- glm(formula = stroke ~ gender + age + hypertension + heart_disease + 
    ever_married + work_type + avg_glucose_level + smoking_status, 
    family = binomial, data = X_train)
summary(model_select)
```

    ## 
    ## Call:
    ## glm(formula = stroke ~ gender + age + hypertension + heart_disease + 
    ##     ever_married + work_type + avg_glucose_level + smoking_status, 
    ##     family = binomial, data = X_train)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.3419  -0.4227  -0.1431   0.4958   3.3866  
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)       -5.960e+00  1.961e-01 -30.400  < 2e-16 ***
    ## gender2           -2.268e-01  3.338e-02  -6.794 1.09e-11 ***
    ## gender3           -1.277e+01  4.381e+02  -0.029    0.977    
    ## age                9.584e-02  1.395e-03  68.683  < 2e-16 ***
    ## hypertension1      1.363e+00  3.759e-02  36.253  < 2e-16 ***
    ## heart_disease1     1.826e+00  4.370e-02  41.785  < 2e-16 ***
    ## ever_married2     -1.546e+00  4.509e-02 -34.293  < 2e-16 ***
    ## work_type2        -4.424e-01  2.051e-01  -2.157    0.031 *  
    ## work_type3        -1.114e+01  8.998e+01  -0.124    0.901    
    ## work_type4        -1.089e-01  2.079e-01  -0.524    0.600    
    ## work_type5        -7.825e-02  2.076e-01  -0.377    0.706    
    ## avg_glucose_level  4.674e-03  3.211e-04  14.557  < 2e-16 ***
    ## smoking_status2    2.021e-01  3.817e-02   5.294 1.20e-07 ***
    ## smoking_status3    4.055e-01  4.341e-02   9.341  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 46792  on 34826  degrees of freedom
    ## Residual deviance: 23305  on 34813  degrees of freedom
    ## AIC: 23333
    ## 
    ## Number of Fisher Scoring iterations: 13

Using the stepwise AIC technique, the trained model for stroke has
dropped the variables BMI and Reesident Type as they are not
contributing factors that would lead to stroke. Stroke classification is
non-preedicttive from the above 2 variables, and they have
non-significant p-values which are \> 0.05.

``` r
## Predict the Values
predict <- predict(model, X_test, type = 'response')

## Create Confusion Matrix
table(X_test$stroke, predict > 0.5)
```

    ##    
    ##     FALSE TRUE
    ##   0  4678  630
    ##   1   617 2782

#### Discrimination of Logistic Regression Classifier

Using the Discrimination, we check if trained CPM has a simultaneously
high sensitivity and specificity. A receiver operator characteristic
(ROC) curve is ploted using sensitivity against ‘1-specificity’ across
the full range of potential cutpoints at 93%.

``` r
#ROCR Curve
ROCRpred <- prediction(predict, X_test$stroke)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
p <- plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0, b=1)
```

![](CPM_files/figure-gfm/unnamed-chunk-24-1.png)<!-- --> The ROC curve
shows further to the top left of the graph, this indicates a great test.

``` r
auc(roc(X_test$stroke ~ predict))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Area under the curve: 0.9307

``` r
hoslem.test(x = X_test$stroke, y=predict, g=10)
```

    ## Warning in Ops.factor(1, y): '-' not meaningful for factors

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  X_test$stroke, predict
    ## X-squared = 8707, df = 8, p-value < 2.2e-16

To determine how far to the top left of the graph the curve is, the Area
Under the Curve (AUC) is calculated

So the AUC for the Logistic Regression Classifier is 0.92 which
represents a near perfect discrimination

#### Calibration of Logistic Regression Classifier

Calibration using Hosmer and Lemeshow goodness of fit test shows
significant p-value which indicates model validity

#### Model Training - Random Forest

To further check for a better prediction classifier, a Random Forest
model is built check for contributing causal factors to stroke and also
to classify patients who have stroke or do not have stroke. As per the
TRIPOD guidelines, we would need to report this stroke clinical
predictive model in sufficient detail to allow for reproducability on a
totally dufferent dataset.

``` r
set.seed(3333)
rf = randomForest(stroke ~ ., 
                  ntree = 500,
                   data = X_train)
rf
```

    ## 
    ## Call:
    ##  randomForest(formula = stroke ~ ., data = X_train, ntree = 500) 
    ##                Type of random forest: classification
    ##                      Number of trees: 500
    ## No. of variables tried at each split: 3
    ## 
    ##         OOB estimate of  error rate: 10.65%
    ## Confusion matrix:
    ##       0     1 class.error
    ## 0 19064  1936  0.09219048
    ## 1  1772 12055  0.12815506

``` r
plot(rf) 
```

![](CPM_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
varImp(rf)
```

``` r
## Important variables according to the model
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Variable Importance")
```

![](CPM_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

> After applying Random Forest classifier algorithm, all contributing
> causal factors were kept by the model and rated in order of importance
> as seen in the above plot

``` r
predicted.response <- predict(rf, X_test)


confusionMatrix(data=predicted.response,  
                reference=X_test$stroke)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 4844  447
    ##          1  464 2952
    ##                                           
    ##                Accuracy : 0.8954          
    ##                  95% CI : (0.8888, 0.9017)
    ##     No Information Rate : 0.6096          
    ##     P-Value [Acc > NIR] : <2e-16          
    ##                                           
    ##                   Kappa : 0.7804          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.596           
    ##                                           
    ##             Sensitivity : 0.9126          
    ##             Specificity : 0.8685          
    ##          Pos Pred Value : 0.9155          
    ##          Neg Pred Value : 0.8642          
    ##              Prevalence : 0.6096          
    ##          Detection Rate : 0.5563          
    ##    Detection Prevalence : 0.6077          
    ##       Balanced Accuracy : 0.8905          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

#### Discrimination of Random Forest Classifier

Using the Discrimination, we check if trained CPM has a simultaneously
high sensitivity and specificity. A receiver operator characteristic
(ROC) curve is ploted using sensitivity against ‘1-specificity’ across
the full range of potential cutpoints at 88%.

``` r
#ROCR Curve
ROCRpred_rf <- prediction(as.numeric(predicted.response), X_test$stroke)
ROCRperf_rf <- performance(ROCRpred_rf, 'tpr','fpr')
plot(ROCRperf_rf, colorize = TRUE, text.adj = c(-0.2,1.7))
abline(a=0, b=1)
```

![](CPM_files/figure-gfm/unnamed-chunk-30-1.png)<!-- --> The ROC curve
shows further to the top left of the graph, this indicates a great test,
but is slightly lower than the Logistic Regression Classifier.

``` r
auc(roc(X_test$stroke ~ as.numeric(predicted.response)))
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

    ## Area under the curve: 0.8905

``` r
hoslem.test(x = X_test$stroke, y=as.numeric(predicted.response), g=10)
```

    ## Warning in Ops.factor(1, y): '-' not meaningful for factors

    ## 
    ##  Hosmer and Lemeshow goodness of fit (GOF) test
    ## 
    ## data:  X_test$stroke, as.numeric(predicted.response)
    ## X-squared = 8707, df = 8, p-value < 2.2e-16

To determine how far to the top left of the graph the curve is, the Area
Under the Curve (AUC) is calculated

So the AUC for the Random Forest classifier is 0.89 which represents a
near perfect discrimination

#### Calibration of Logistic Regression Classifier

Calibration using Hosmer and Lemeshow goodness of fit test shows
significant p-value which indicates model validity

#### Conclusion

Overall logistic regression was selected as the best model to predict if
a patient can have stroke or not. To achieve this and reduce the
potential of bias and oveerfitting of trained model, imbalanced data are
sampled because it is a common hurdle in healthcare. Also performed
multiple imputation to predict missing data of smoke status in other
ways as well
