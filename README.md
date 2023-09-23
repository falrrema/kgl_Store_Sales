# Kaggle Competition: Store Sales - Time Series Forecasting
[Kaggle competition of the ongoing Store Sales - Time Series Forecasting](https://www.kaggle.com/competitions/store-sales-time-series-forecasting/leaderboard)

## Objective

- Learn time series forecasting analysis and techniques.
- Update to new R and Python packages for time series forecasting.
- Refresh machine learning time series forecasting.
- Refresh on python coding.
- Help and guide my data science team in their learning path.

### Week 1

- Setup mostly, we downloaded the data and uploaded it to deltalake in databricks.
- Created functions for easy loading and saving the files.
- Basic EDA:
  1. How much information does the training have? How many days?
  2. How many stores we have? Do we have information of the stores for the whole period? Completeness.
  3. How many family of products do we have? Do we have a family that has little data?
  4. What is the testing time frame?

Answers we found:

1. We have about 4.6 years of data. Around 1.679 days. 
2. There are 54 stores in total. We have data for all the timeframe, however when we group by store and sum all its sales, and filter for sales above zero you find that there are 7 stores that have less than 90% of data of days. There is even one store (52) that has 118 days of information or 7% of data. Problematic stores are 52 (7%), 22 (40%), 42 (43%), 21 (44%), 29 (52%), 20 (54%), 53 (69%).
3. There are 33 families of products. We do have information in all of them, however some products are barely sold, particularly two families `BOOKS`and `BABY CARE`are sold 287 (17%) and 891 (53%) days respectively.
4. The testing data set spans 15 days after the end of the training set from 2017-08-16 to 2017-08-31.

### Week 2

Our team focused to complete the recommended tutorial of the competition, which teaches about time series forecasting. It can be found here:

[Time series forecasting](https://www.kaggle.com/learn/time-series)

Also this tutorial included a submission and the score of that submission was 0.51 measure with RMSLE. 

### Week 3

I suggested focusing in developing a good cross-validation pipeline. I shared links to different articles both for python or R about ways to do time series crossvalidation:

- [4 Things to Do When Applying Cross-Validation with Time Series](https://towardsdatascience.com/4-things-to-do-when-applying-cross-validation-with-time-series-c6a5674ebf3a)
- [Cross Validation in Time Series](https://medium.com/@soumyachess1496/cross-validation-in-time-series-566ae4981ce4)
- [How To Correctly Perform Cross-Validation For Time Series](https://towardsdatascience.com/how-to-correctly-perform-cross-validation-for-time-series-b083b869e42c)
- [Time Series Cross Validation](https://rpubs.com/crossxwill/time-series-cv)

Also recommended some datacamp courses on the subject:

- [Time Series-Crossvalidation in R](https://campus.datacamp.com/courses/forecasting-in-r/benchmark-methods-and-forecast-accuracy?ex=9)
- [Cross-validation timeseries data python](https://campus.datacamp.com/courses/machine-learning-for-time-series-data-in-python/validating-and-inspecting-time-series-models?ex=6)

We decided block time series crossvalidation is the way to go, and the group constructed different functions to apply CV. I discover ModelTime library in R and found that it was easy to create the block time CV using the function sliding_period(). 

### Week 4

This week the idea was to upload a new submission with the things learn so far, CV the hypothesis to check for improvements. So far no new improvements where made, but there were efforts to understand better how the forecasting is done with linear regressions. 

I personally did the time series forecasting datacamp course in R. 

### Week 5

This week we kept on discussing about how to improve the base model uploaded in the tutorial. The team found that a several of variables are predictive such as changes in oil price, holidays and holidays by region, and train window timeframe. There was a lot of discussion concerning how linear regression is able to predict for multiple days, stores and categories simultaneously. Some of the teams where training multiple models iterating by store and family of products. This has lead them to worse scores in there CV. 

In my case I decided to replicate the tutorial in my databricks python notebook, for the sake to understand what was happening underneath. I implemented a class for blocktimeseriesCrossvalidation and several functions to imitate the works of sliding_period() in R successfully. I experimented with a series of training timeframes and found that for the seasonality model propose in the notebook there was a sweet spot around 100 days of training. The tutorial submitted a 200 days training period, so I made the change and the score improved to 0.495. 

I learned a lot of python in the process, and supported significantly by chaGPT. Particularly, I understood the way to do Multi-output Regression. 

I suggested the team two things:

1. To think about the hierarchical attributes that the dataset has. Meaning, that it could be better instead to predict the stores sales and cascade down the sales to the family of products multiplying by vector of weights. These weights can be the average composition of the product in that stores, or a moving weigth average.
2. Understand were the model is making mistakes. Its important to make diagnostics of every hypothesis, and understand which stores or families or periods have the biggest errors. If there is a pattern then it can generate new insights for new models.

### Week 6-7 

Vacations!

### Week 7 

Still on vacations but manage to find time to work out a hypotesis. This week I have work mainly in R using the `modeltime` library. You can fin more in this link:

- [Modeltime](https://business-science.github.io/modeltime/index.html)
- [Modeltime Resampling](https://business-science.github.io/modeltime.resample/)

This library is an extension of the [tidymodels](https://www.tidymodels.org/) ecosystem which is great and I have never used it extensively. 

Previous weeks I have focused on using a linear regression to train and predict on all the data, stores and family are converted to features and some time features were created also. Since the data set has hierarchical properties I wanted to pursue the idea to predict store sales and cascade down to family sales by looking back the percentage of sales that correspond to the family in a lookback window. Say for example, `GROCERIES I` usually correspond to 25% sales for store 1 looking back 2 weeks, then I asign that percentage to that family. Then, when predicting whole store sales I just multiply the percentage to get family sales. The limitations is that the percentage is static.

**Hypothesis 1:**

*I can improve sales forecasting by a top down approach of stores sales prediction and cascading the sales to family using a rolling sales percentage. This forecasting will be improved also by adding new features such as changes in day to day oil prices, time features, holidays and store level characteristics (cluster and type).*

I first did a simple EDA of this hypothesis you can find that analysis in `S8_H1_EDA.R`. I calculated for train the whole day store sales and then the percentage of sales of each family product. Then aggregated the percentages by a two week frame for all train. Since there are about 1600 training days, that means 118 biweeks, and since there are 32 families there is a total of ~3700 biweeks-family to evaluate the percentage variation. I found there is relevant variation withing two weeks frame:

|var_desc   | n_prom|  pct|
|:----------|------:|----:|
|menor 20%  |    643| 29.0|
|menor 50%  |    880| 39.7|
|menor 80%  |    512| 23.1|
|menor 100% |    184|  8.3|

Only 20% biweeks-family has a variation of its percentage below 20% (there were no under 10%), only 8% has between 80-100%. This strategy may work, but I will have to experiment with different look-back frames (not only two week). 

#### Modeling

I made a block time series crossvalidation with 150 days of training and 15 days of test. A total of 10 slices were made with these characteristics:

|.id     |.key     |min_date   |max_date   |    n| sum_sales| sum_prom|diff     |
|:-------|:--------|:----------|:----------|----:|---------:|--------:|:--------|
|Slice01 |training |2017-03-04 |2017-07-31 | 8100| 129478086|  1885574|149 days |
|Slice01 |testing  |2017-08-01 |2017-08-15 |  810|  12433323|   160278|14 days  |
|Slice02 |training |2016-09-18 |2017-02-15 | 8100| 126497045|  2003464|150 days |
|Slice02 |testing  |2017-02-16 |2017-03-02 |  810|  12375999|   162018|14 days  |
|Slice03 |training |2016-04-05 |2016-09-01 | 8100| 115499431|  1390679|149 days |
|Slice03 |testing  |2016-09-02 |2016-09-16 |  810|  11663417|   129889|14 days  |
|Slice04 |training |2015-10-21 |2016-03-19 | 8100| 118298760|   723888|150 days |
|Slice04 |testing  |2016-03-20 |2016-04-03 |  810|  11735073|    81278|14 days  |
|Slice05 |training |2015-05-08 |2015-10-04 | 8100| 107657202|   519688|149 days |
|Slice05 |testing  |2015-10-05 |2015-10-19 |  810|  11539689|    56977|14 days  |
|Slice06 |training |2014-11-22 |2015-04-21 | 8100|  85749072|   283646|150 days |
|Slice06 |testing  |2015-04-22 |2015-05-06 |  810|   7795975|    30643|14 days  |
|Slice07 |training |2014-06-09 |2014-11-05 | 8100|  86967388|   288440|149 days |
|Slice07 |testing  |2014-11-06 |2014-11-20 |  810|   9673110|    37007|14 days  |
|Slice08 |training |2013-12-24 |2014-05-23 | 8100|  77896800|     9423|150 days |
|Slice08 |testing  |2014-05-24 |2014-06-07 |  810|   6859857|     7935|14 days  |
|Slice09 |training |2013-07-11 |2013-12-07 | 8100|  58456165|        0|149 days |
|Slice09 |testing  |2013-12-08 |2013-12-22 |  810|   7841931|        0|14 days  |
|Slice10 |training |2013-01-26 |2013-06-24 | 8100|  55073882|        0|149 days |
|Slice10 |testing  |2013-06-25 |2013-07-09 |  810|   5789107|        0|14 days  |

I trained 5 models (`Linear regression`, `Glmnet`, `Xgboost`, `Prophet Boost`, `Lightgbm`) all available through `tidymodels` and `modeltime` libraries. 

*Feature engineering*: I added features like `onpromotion`, delta prices of oil (daily, weekly), and added features store-level `cluster` and `type`. Last, but not least the holidays. 

|date       | store_nbr|     sales| onpromotion| delta_oil_1d| delta_oil_7d|type |cluster | holiday|
|:----------|---------:|---------:|-----------:|------------:|------------:|:----|:-------|-------:|
|2013-01-11 |         1|  5494.016|           0|        -0.21|         0.48|D    |13      |       0|
|2013-01-11 |         2|  6459.785|           0|        -0.21|         0.48|D    |13      |       0|
|2013-01-11 |         3| 14997.531|           0|        -0.21|         0.48|D    |8       |       0|
|2013-01-11 |         4|  6109.589|           0|        -0.21|         0.48|D    |9       |       0|
|2013-01-11 |         5|  6379.973|           0|        -0.21|         0.48|D    |4       |       0|

I fabricated a recipe that fabricated more features and log-transformed the sales:

```
recipe_spec <- recipe(sales ~ ., template) %>%
  step_timeseries_signature(date) %>%
  step_select(sales, date, store_nbr, onpromotion, delta_oil_1d, delta_oil_7d,
              type, cluster, holiday, date_half, date_quarter, date_month,
              date_day, date_wday, date_week, type, cluster) %>%
  step_mutate(store_nbr = factor(store_nbr)) %>%
  step_dummy(all_nominal()) %>% # Everything to dummy
  step_log(sales, offset = 1, base = 10)

```

Probably it would have been better to dummify also the date features like date_day, or day_week. I will test it in the future. 

Overall the performance was very similar between boosting models as expected:

![Resample Performance](https://github.com/falrrema/kgl_Store_Sales/blob/main/Extra/Results_Resample_H1.png)