Uber_case_analysis
================
Jackyieym
2024-04-20

## Introduction

This project is conduct under the Google Data Analytics Capstone and
follows Track B, which is performing own case study. The company I
choose is Uber, an American multinational transportation company that
provides ride-hailing services, courier services, food delivery, and
freight transport. The goal of this project is to analyse how some of
the fundemental factors of people affect their usage of Uber and provide
insights to Uber’s strategy in tailoring their ride-hailing service.

## Data collection

The data used in this project is collected from survey answer. The
survey was distributed in the university campus and received 32
responses. The questions asked about the gender, their major, usage of
Uber, frequency, price, number of people, tier of services, main
alternative, rating on important and improvement factors, including
price, safety, convenient and customer services. Due to the data was
collected in campus, its respondents are mostly limited to university
students.

## Question

Some question are set up for analysis using `chisq.test` 1. Is gender
related to the score of important factors when using Uber In this
question, `Importance_Price`, `Importance_Price`, `Importance_Price`,
`Importance_Price` are selected for analysis, representing Price,
Safety, Convenient and Customer Service respectively to `Gender`. For
null hypothesis (H0), no relation between gender and the score of
important factors For alternative hypothesis (H1), there is relation
between gender and the score of important factors

## Setting up Environment

``` r
library(ggplot2)
library(ggcorrplot)
library(nFactors)
library(GGally)
library(dplyr)
library(patchwork)
library(readr)
```

Load the data file

``` r
UberProjectRawData <- read_csv("./UberProjectRawData.csv")
```

    ## # A tibble: 32 × 18
    ##    `Response ID` Gender  Major OnlineTaxiService Frequency Time  Cost  NumPeople
    ##            <dbl> <chr>   <chr> <chr>             <chr>     <chr> <chr>     <dbl>
    ##  1     127966762 Female  Fina… Yes               Often     Morn… $50-…         1
    ##  2     127992777 Male    Acco… Yes               Sometimes Noon… $50-…         2
    ##  3     128824959 Male    Fina… Yes               Selfdom   Midn… $75-…         1
    ##  4     129122186 Male    Acco… Yes               Always    Midn… $125…         1
    ##  5     129123976 Prefer… Mark… Yes               Often     Midn… more…         2
    ##  6     129123992 Male    Fina… Yes               Selfdom   Midn… more…         1
    ##  7     129124004 Female  Entr… Yes               Always    Night $100…         1
    ##  8     129124015 Female  Mark… Yes               Sometimes Night $100…         2
    ##  9     129124024 Male    I am… Yes               Sometimes Midn… more…         4
    ## 10     129124046 Male    Huma… Yes               Selfdom   Midn… more…         3
    ## # ℹ 22 more rows
    ## # ℹ 10 more variables: `Std-or-Prem` <chr>, MainTrans <chr>,
    ## #   Importance_Price <dbl>, Importance_Safety <dbl>, Importance_Conv <dbl>,
    ## #   Importance_Cus <dbl>, Improve_Price <dbl>, Improve_Safety <dbl>,
    ## #   Improve_Conv <dbl>, Improve_Cus <dbl>

## Data cleaning

After filtering out those did not use Uber, columns required for
analysis are selected and arrange according to gender.

``` r
s1<-UberProjectRawData %>% 
  filter(OnlineTaxiService=="Yes") %>% 
  select(2,11,12,13,14) %>% 
  arrange(Gender)
```

    ## # A tibble: 29 × 5
    ##    Gender Importance_Price Importance_Safety Importance_Conv Importance_Cus
    ##    <chr>             <dbl>             <dbl>           <dbl>          <dbl>
    ##  1 Female                5                 5               5              5
    ##  2 Female                5                 4               5              3
    ##  3 Female                3                 3               5              2
    ##  4 Female                1                 3               5              4
    ##  5 Female                5                 5               4              4
    ##  6 Female                5                 4               3              5
    ##  7 Female                5                 5               4              3
    ##  8 Female                3                 3               3              3
    ##  9 Female                5                 3               4              2
    ## 10 Female                5                 5               4              3
    ## # ℹ 19 more rows

## Data visualisation

To have a quick glance on the data to get the rating by the respondents,
we can use bar chart to represent the vote of rating on different
factors by genders. Putting four chart together will have a better
comparison.

``` r
bar_Price<-ggplot(s1, aes(x=Importance_Price, fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Safety<-ggplot(s1, aes(x=Importance_Safety,fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Conv<-ggplot(s1, aes(x=Importance_Conv,fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Cus<-ggplot(s1, aes(x=Importance_Cus, fill=Gender))+
  geom_bar(position = position_dodge(preserve = "single"))
bar_Price+bar_Safety+bar_Conv+bar_Cus+
  plot_annotation("Votes on score of different factors per gender (frequency)", 
                  theme=theme(plot.title=element_text(hjust=0.5)))
```

![](Uber_case_analysis_files/figure-gfm/frequency%20bar%20chart-1.png)<!-- -->

However, due to some catagory of gender have less response in absolute
number compare to other options, their vote on rating may
under-represented. Therefore, converting the data into proportion will
provide a better representation.

``` r
port1<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Price), margin=1)))
port2<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Safety), margin=1)))
port3<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Conv), margin=1)))
port4<-as.data.frame(with(s1, prop.table(table(Gender, Importance_Cus), margin=1)))

porp_bar_Price<-ggplot(port1, aes(x=Importance_Price, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Safety<-ggplot(port2, aes(x=Importance_Safety, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Conv<-ggplot(port3, aes(x=Importance_Conv, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Cus<-ggplot(port4, aes(x=Importance_Cus, y=Freq, fill=Gender))+
  geom_col(position="dodge")+
  labs(y="Proportion of count")
porp_bar_Price+porp_bar_Safety+porp_bar_Conv+porp_bar_Cus+
  plot_annotation("Votes on score of different factors per gender (proportion)", 
                  theme=theme(plot.title=element_text(hjust=0.5)))
```

![](Uber_case_analysis_files/figure-gfm/proportion%20bar%20chart-1.png)<!-- -->

## Testing relationship

Performing the `chisq.test` can provide us the relationship between the
gender and the factors

``` r
chisq.test(s1$Gender, s1$Importance_Price)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  s1$Gender and s1$Importance_Price
    ## X-squared = 4.7338, df = 6, p-value = 0.5784

``` r
chisq.test(s1$Gender, s1$Importance_Safety)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  s1$Gender and s1$Importance_Safety
    ## X-squared = 5.5935, df = 6, p-value = 0.4702

``` r
chisq.test(s1$Gender, s1$Importance_Conv)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  s1$Gender and s1$Importance_Conv
    ## X-squared = 3.9441, df = 6, p-value = 0.6842

``` r
chisq.test(s1$Gender, s1$Importance_Cus)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  s1$Gender and s1$Importance_Cus
    ## X-squared = 5.4759, df = 8, p-value = 0.7057

## Conclusion for question 1

Because p-value of all factors is greater than 0.05, H0 is not rejected
and meaning there is no relation between gender and the score of
important factors.
