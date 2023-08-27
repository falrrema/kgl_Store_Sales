# kgl_Store_Sales
Kaggle competition of the ongoing Store Sales - Time Series Forecasting.

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




