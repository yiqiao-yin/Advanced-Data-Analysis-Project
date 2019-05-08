# Advanced Data Analysis Project

This github repo stores the project for Advanced Data Analysis course for Columbia University. 

Info:
- Course: STATS GR5291
- Term: Spring 2019
- Professor: Hammou Elbarmi
- This github repo is also used as a submission to the project. 

Team: 
- Lin, Xiaotong (MA, Columbia University)
- Yin, Yiqiao (MA, Columbia University)

Content:
- For full report, we want to direct audience [here](https://github.com/yiqiao-yin/Advanced-Data-Analysis-Project/blob/master/docs/Final_Report.pdf). 
- For quick debrief, one can gloss over the following.

## Intro

The goal of this project is to investigate the hourly wage from the 1985 Current Population Survey (CPS) dataset and find out that its determinant variables. By taking 0.05 as the significant level in the whole project and through a diverse group of non-parametric and parametric methods, such as correlation tests and two-sample tests, the results show that experience and education are the two most important and informative variables that they are by themselves positively influence hourly wage. For example, an employee’s hourly wage will increase $1.21/hour if he or she has one more year of education background or increase $0.68/hour with one more year experience while keeping other variables the same. However, education and experience are negatively associated with each other. Additionally, tests on Contingency Table suggest that males with the same experience level receive the higher hourly wage in more detail. Therefore, we propose the solution to predict the hourly wage of a person by Linear Regression with 10 variables, including age, education, experience, sex, south, union, sector, sex-experience, age-experience, and age-education. On average, we are able to predict hourly wages within $4.23/hour.

## Data

The 1985 CPS dataset consists of a random sample of 534 persons in 1985 with information on their wages and other characteristics of the workers, including age, the number of years of education, years of work experience, sex, ethnic background, marital status, occupation, the region of residence and union membership. We can consider wage as the response variable and all others as the predictor variables.

<p align="center">
  <img src="https://github.com/yiqiao-yin/Advanced-Data-Analysis-Project/blob/master/figs/fig-1.PNG">
</p>

## Exploratory Analysis

An exploratory analysis of the data is conducted by presenting the simple scatter plot of all the variables. The below graph shows the positively correlated relationship between wage & education and experience & age can be detected. This means, for example, as a person’s education level increases in years, it is more likely for him or her to obtain higher wage. However, we can see that education and experience have a negatively correlated relationship, which corresponds the pattern that the more years one a person spent in school the fewer years he or she can devote in the industry. These relationships will be further verified by the correlation test later on.

<p align="center">
  <img src="https://github.com/yiqiao-yin/Advanced-Data-Analysis-Project/blob/master/figs/fig-2.PNG">
</p>

## Selective Results

This table presents the comparison of the performance of different statistical methods through the lenses of the 5-fold cross-validation. For example, we observe that the Linear Regression model is able to predict hourly wages for each candidate in the data set and on average these predictions are within the range of $4.395/hour. With the average hourly wage to be $10, this is a rather realistic approach. As we can see above, even Principle Component does not help us to generate better variables (Root of MSE: 5.29) because it obscures the performance of linear regression itself (Root of MSE: 4.40). Therefore, in the following part of this project, we decide to use nonparametric methods, including two sample tests, correlation tests, and interaction test to further analyze the relationship between variables and look for the determinant ones.

<p align="center">
  <img src="https://github.com/yiqiao-yin/Advanced-Data-Analysis-Project/blob/master/figs/fig-3.PNG">
</p>

## Quick Summary

We conclude that wage is positively correlated with education and education is negatively correlated with experience. Also, age is strongly positively correlated with experience. These are identical with the results above that an employee’s hourly wage will increase with the increase of education and experience, but decrease with the increase of age. Hence, companies prefer younger people with more education and experience.

<p align="center">
  <img src="https://github.com/yiqiao-yin/Advanced-Data-Analysis-Project/blob/master/figs/fig-4.PNG">
</p>
