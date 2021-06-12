Predicting Pentecostal Affilitation within Latin America using Subset of
the Questionnaire of a Large-Scale Representative Survey
================
Gustavo Arruda
6/4/2021

``` r
knitr::opts_chunk$set(warning = FALSE, message = FALSE)

library(haven)
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.3     v purrr   0.3.4
    ## v tibble  3.1.0     v dplyr   1.0.4
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.4.0     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.0.5

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
library(MASS)
```

    ## Warning: package 'MASS' was built under R version 4.0.5

    ## 
    ## Attaching package: 'MASS'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     select

``` r
library(knitr)
set.seed(123456789)
```

# Preparing data

``` r
rel_vars <- c('Q28a', 'Q28c', 'Q46a', 'Q55a',
  'Q55b', 'Q55d', 'Q55e', 'Q55f',
  'Q60a', 'Q60e', 'Q60f', 'Q60j')
rel_df <-
  read_sav("Religion in Latin America Dataset.sav",
           col_select = all_of(rel_vars))

rel_tot <- as.data.frame(rel_df) %>%
  zap_label() %>%
  zap_labels() %>%
  mutate_all(funs(as.numeric(str_replace_all(., "2", "0")))) %>%
  na_if(., "98") %>%
  na_if(., "99")

items_tot <- rel_tot[3:12]
rel_tot <- rel_tot %>%
  mutate(score = rowSums(items_tot))

rel_df <- as.data.frame(rel_df) %>%
  zap_missing() %>%
  zap_label() %>%
  zap_labels() %>%
  drop_na() %>%
  mutate_all(funs(as.numeric(str_replace_all(., "2", "0")))) %>%
  na_if(., "98") %>%
  na_if(., "99") %>%
  drop_na()

items <- rel_df[3:12]
rel_df <- rel_df %>%
  mutate(score = rowSums(items))


model_P = lda(Q28a ~ score, data = rel_df)
predictions_P = predict(model_P)
tab_lda_P = table(rel_df$Q28a,predictions_P$class)

cutoff <- ceiling((5.349445 + 6.590104) / 2)

# Cross-Validation, from example on Canvas.
n = nrow(rel_df)
nt = 2000
neval=n-nt
rep=100
hitr=dim(rep)
for(i in 1:rep){
  train=sample(1:n,nt)
  model=lda(Q28a ~ score, data = rel_df[train,])
  predict(model,rel_df[-train,])$class
  tab_lda=table(rel_df$Q28a[-train],
                predict(model,rel_df[-train,])$class)
  hitr[i]=sum(diag(tab_lda))/neval
  }
```

# Objectives

    This project seeks to develop and validate a scale to measure Pentecostal affiliation among Latin American Christians. To accomplish that, I am gathering dichotomous questions from a large-scale survey conducted by the Pew Research Center, chosen by their face validity in relation to the literature on the Pentecostal movement, estimating their composite reliability, calculated with the coefficient alpha, and their criterium validity, estimated with Discriminant Analysis to a self-identification question from the same questionnaire. With such a scale, we could explore Pentecostal affiliation and its political ramifications among different social groups considering more factors than only self-identification.

# Literature Review

Contradicting previous sociological views on secularization, which
predicted the decline of the importance of religion in the political
arena as modernization progressed, since the 1980s religion has
re-awakened in the public arena, both in the United States and in Brazil
(Casanova 1994). In the United States, the Evangelical and
Fundamentalist revival characterized the New Religious Right, backing
the Moral Majority, prompting continued analytical relevance within
sociology and political science (Hackett and Lindsay 2008; Kellstedt and
Smidt 1991). Yet, debates over the appropriate way to identify and
measure alignment and identification with Evangelicalism or Protestant
Fundamentalism persist, which partially stems from a broader difficulty
in establishing appropriate measures of religious identification
(Hackett 2014; Hackett and Lindsay 2008; Kellstedt and Smidt 1991; Smith
1990; Steensland, Park et al 2000). The strategy often revolves around
using surveys to calculate correlations between Evangelical or
Fundamentalist self-identification with scales of attitudes and
behaviors theoretically associated with these religious movements
(Hackett and Lindsay 2008; Kellstedt and Smidt 1991; Smith 1990;
Steensland, Park et al 2000).

Meanwhile, in Brazil, the Pentecostal movement, which shares multiple
characteristics with the American Evangelical and Fundamentalists, has
presented extraordinary growth since mid-20th century. In many
countries, the Pentecostal movement ended up characterized by support to
the military dictatorships of the 1970s and 1980s (Chesnut 1997; Martin
1990; Stoll 1990). More recently, in Brazil they now represent a third
of the population, with proportional representation in Congress and
increasing weight in presidential elections (Nicolau 2014). In the 2018
election of the authoritarian populist President Jair Bolsonaro, who won
with 55%, Pentecostals disproportionally voted for him when compared to
Catholics and the religiously unaffiliated. By using the “Religion in
Latin America” 2014 survey conducted by the Pew Research Center, we have
the opportunity to extend the exploration of methodological nuances in
measuring denominational affiliation among Christian groups, both by
self-identification and attitudinal scales, from the United States to
other social contexts like Latin America. That would allow us to refine
our knowledge on the political influence of the Pentecostal movement,
and its reverberations to global democracy.

# Methods

The data used in this project comes from the “Religion in Latin America”
2014 survey published by the Pew Research Center, a nonpartisan think
tank that researches public opinion with an expertise in religion. The
survey, through 3 different languages (Spanish, Portuguese and Guarani),
included nationally representative samples from 18 different countries,
which together include more than 95% of the population in Latin America.
In total, the dataset included 30,326 observations, going to 20,221
observations after removing all empty responses from the variables of
interest.

For my dependent variable I selected a dichotomous item indicating
self-identification with Pentecostalism, along with 10 other dichotomous
variables including attitudes and behaviors pertaining to Pentecostal
theology. I then created a composite score with the 10 items, which
presented face validity according to the literature on the theme. The
question asking whether the participant self-identifies as Pentecostal
was only applied to Latin American participants already self-identifying
as Christians. That means that my scale, if valid, would only have its
validity measured to differentiate participant’s religious attitudes and
behaviors among strands of Christianism within Latin America. Religious
identification might change drastically among cultures, even within a
single country (Hackett 2014; Steensland, Park et al 2000), which would
require this scale to be evaluated for other populations for their
results to be validated.

After that, I calculated the coefficient alpha as a measure of
reliability and item statistics of the 10-item composite test, using the
function “alpha” in the R package “psych”. The coefficient alpha
estimates internal consistency, showing whether the different items in
the scale measure statistically correlated underlying constructs,
whichever they may be. That, on its own hand, helps us to estimate the
reliability of scale items in measuring our construct of interest.

At last, to evaluate the fit of a scale composed by the 10 items of the
survey to classify and predict religious affiliation with Pentecostalism
I used Discriminant Analysis (DA), using the function “lda” from the R
package “MASS”. Two sub-groups — Non-Pentecostals and Pentecostals —
were established according to a self-identification question included in
the same survey. The DA model created a linear function that, when
applied to each individual test results, creates a discriminant score
that maximizes the difference between its sub-group means. The average
between the two sub-group means characterizes the cut-off value of the
DA model, which is used to classify each score to the Non-Pentecostal or
Pentecostal group depending on its standing above or below the cut-off.
I evaluated the fit of the model by cross-validating it, which entails
applying it to various random subsamples of the data, before verifying
the proportion of cases that it predicts correctly, also named hit
ratio. The linear function calculated by the DA model ultimately helps
us to understand the strength of the relationship between the composite
score of the scale I proposed and the self-identification measure.
Discriminant Analysis is most often used to verify criterium validity
with multiple predictors, which is not the case here given that I am
using a single composite score as the independent variable. Still, I
believe that DA might still be a good estimative of criterium validity
for my purposes given I am trying to differentiate between two
categorial dependent variables.

# Results

When looking at the output of the reliability analysis I conducted in R,
we see that the coefficient alpha of the 10-item scale I constructed,
pulling from the questionnaire written by the Pew Research for the 2014
“Religion in Latin America” survey, was 0.68. The alpha coefficient is
the average of all the possible split-half reliabilities, which is
constructed from the covariance of all possible item pairs. The
reliability of a scale can also be interpreted as its correlation with
itself. Additionally, the item statistics show that dropping each of all
the items would not increase the remaining reliability, which is a good
indicator that none of the individual items is substantially decreasing
the total coefficient alpha. Conversely, dropping each of all the items
would also not decrease the remaining reliability, which shows that all
of them have relatively similar item total reliabilities, meaning the
correlation between each item with the entire scale.

``` r
print(alpha(items_tot, cumulative = TRUE))
```

    ## 
    ## Reliability analysis   
    ## Call: alpha(x = items_tot, cumulative = TRUE)
    ## 
    ##   raw_alpha std.alpha G6(smc) average_r S/N    ase mean sd median_r
    ##       0.68      0.68     0.7      0.18 2.2 0.0026  5.1  2     0.13
    ## 
    ##  lower alpha upper     95% confidence boundaries
    ## 0.68 0.68 0.69 
    ## 
    ##  Reliability if an item is dropped:
    ##      raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
    ## Q46a      0.69      0.69    0.71      0.20 2.2   0.0026 0.020 0.130
    ## Q55a      0.63      0.64    0.66      0.16 1.8   0.0032 0.017 0.099
    ## Q55b      0.65      0.66    0.68      0.18 1.9   0.0029 0.017 0.129
    ## Q55d      0.63      0.64    0.66      0.17 1.8   0.0031 0.017 0.108
    ## Q55e      0.64      0.65    0.66      0.17 1.8   0.0030 0.018 0.106
    ## Q55f      0.66      0.67    0.69      0.18 2.0   0.0028 0.019 0.129
    ## Q60a      0.68      0.68    0.69      0.19 2.1   0.0027 0.018 0.130
    ## Q60e      0.66      0.66    0.66      0.18 1.9   0.0028 0.016 0.130
    ## Q60f      0.67      0.66    0.68      0.18 2.0   0.0027 0.018 0.121
    ## Q60j      0.66      0.66    0.67      0.18 1.9   0.0028 0.018 0.119
    ## 
    ##  Item statistics 
    ##          n raw.r std.r r.cor r.drop  mean   sd
    ## Q46a 25778  0.41  0.37  0.21   0.19 0.827 0.38
    ## Q55a 30226  0.65  0.61  0.56   0.48 0.352 0.48
    ## Q55b 30113  0.49  0.51  0.43   0.37 0.107 0.31
    ## Q55d 30103  0.62  0.59  0.54   0.46 0.272 0.44
    ## Q55e 30190  0.59  0.57  0.51   0.44 0.219 0.41
    ## Q55f 29992  0.44  0.48  0.37   0.32 0.088 0.28
    ## Q60a 30221  0.34  0.43  0.33   0.24 0.975 0.16
    ## Q60e 29760  0.50  0.52  0.47   0.33 0.874 0.33
    ## Q60f 29427  0.51  0.49  0.41   0.29 0.715 0.45
    ## Q60j 30022  0.48  0.52  0.44   0.32 0.895 0.31
    ## 
    ## Non missing response frequency for each item
    ##         0    1 miss
    ## Q46a 0.17 0.83 0.15
    ## Q55a 0.65 0.35 0.00
    ## Q55b 0.89 0.11 0.01
    ## Q55d 0.73 0.27 0.01
    ## Q55e 0.78 0.22 0.00
    ## Q55f 0.91 0.09 0.01
    ## Q60a 0.02 0.98 0.00
    ## Q60e 0.13 0.87 0.02
    ## Q60f 0.29 0.71 0.03
    ## Q60j 0.10 0.90 0.01

The hit ratio was moderately predictive, giving 77.23% of correct
predictions, relatively higher than the 50% expected hit ratio. Among
100 Christian participants, about 77% would be appropriately identified
as associated with the Pentecostal movement or not. When looking at the
confusion table, we see that the ratio of false negatives is much higher
than the ratio of false positives, around 40% for the first versus 21%
for the second. That means that the scale is much more efficient in
distinguishing who is not affiliated with Pentecostalism than who is
affiliated with it. The cut-off value is 6, which would be the score to
classify affiliation with the Pentecostal movement among Christian
individuals in Latin America.

``` r
print(tab_lda_P)
```

    ##    
    ##         0     1
    ##   0 14707   603
    ##   1  3998   913

``` r
print(sum(diag(tab_lda_P))/nrow(rel_df))
```

    ## [1] 0.7724643

``` r
print(model_P)
```

    ## Call:
    ## lda(Q28a ~ score, data = rel_df)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.7571337 0.2428663 
    ## 
    ## Group means:
    ##      score
    ## 0 5.349445
    ## 1 6.590104
    ## 
    ## Coefficients of linear discriminants:
    ##             LD1
    ## score 0.5919371

``` r
paste('Cutoff: ', cutoff)
```

    ## [1] "Cutoff:  6"

``` r
print(mean(hitr))
```

    ## [1] 0.7723363

# Discussion

Overall, the scale of Pentecostal affiliation that I proposed presents
moderate reliability and moderate criterium validity. However, it is
important to consider the broad range of populations included in the
validation of this scale when evaluating its usefulness— 18 nationally
representative surveys spanning 3 different languages—, which means that
it moderately measures the construct of Pentecostal affiliation among
various populations and contexts. Although this scale presents moderate
predictive power, and seems to include moderately predictive individual
items, I would suggest as next steps the development and testing of
additional items as a way to improve the overall scale performance in
reliability and validity measures, particularly to diminish its
false-negative rates. Another venue of further exploration would be
testing the reliability and validity of this scale for more specific
populations, given that it possibly holds more significant
classificatory power for some social groups than others. On the
downside, it is important to notice that the usage of
self-identification as a criterium for construct validity remains
contested, given the complexity of identification and affiliation in
regards to a religious movement. For instance, changes in wording result
in significant differences in interpretation for different populations
(Hackett 2014; Hackett and Lindsay 2008).

# References

Casanova, Jose. 1994. Public Religions in the Modern World. 2nd
ed. Chicago, IL: University of Chicago Press.

Chesnut, R. Andrew. 1997. Born Again in Brazil: The Pentecostal Boom and
the Pathogens of Poverty. 1st Editio. New Brunswick: Rutgers University
Press.

Hackett, Conrad. 2014. “Seven Things to Consider When Measuring
Religious Identity.” Religion 44(3):396–413. doi:
10.1080/0048721X.2014.903647.

HACKETT, CONRAD, and D. MICHAEL LINDSAY. 2008. “Measuring
Evangelicalism: Consequences of Different Operationalization
Strategies.” Journal for the Scientific Study of Religion 47(3):499–514.
doi: <https://doi.org/10.1111/j.1468-5906.2008.00423.x>.

Kellstedt, Lyman, and Corwin Smidt. 1991. “Measuring Fundamentalism: An
Analysis of Different Operational Strategies.” Journal for the
Scientific Study of Religion 30(3):259–78. doi: 10.2307/1386972.

Martin, David. 1990. Tongues of Fire: Explosion of Protestantism in
Latin America. London, England: Blackwell.

Nicolau, Jairo. 2014. “Determinantes Do Voto No Primeiro Turno Das
Eleições Presidenciais Brasileiras de 2010: Uma Análise Exploratória.”
Opiniao Publica 20(3):311–25. doi: 10.1590/1807-01912014203311.

Pew Research Center. 2014. “Appendix A: Methodology” Religion in Latin
America. Retrieved
(<https://www.pewforum.org/2014/11/13/appendix-a-methodology/>). Smith,
Tom W. 1990. “Classifying Protestant Denominations.” Review of Religious
Research 31(3):225–45. doi: 10.2307/3511614.

Steensland, Brian, Jerry Z. Park, Mark D. Regnerus, Lynn D. Robinson, W.
Bradford Wilcox, and Robert D. Woodberry. 2000.

“The Measure of American Religion: Toward Improving the State of the
Art.” Social Forces 79(1):291–318. doi: 10.2307/2675572.

Stoll, David. 1990. Is Latin America Turning Protestant?: The Politics
of Evangelical Growth. Berkeley, CA: University of California Press.

# Appendix A – Test Items

## CRITERIUM VARIABLE (PENTECOSTAL SELF-IDENTIFICATION)

ASK IF CHRISTIAN (QCURREL = 1,2 OR QCURRELc=1) (IF CHRISTIAN, INCLUDING
CATHOLICS AND ALL SELF-IDENTIFIED CHRISTIANS) Q28 Would you describe
yourself as a (INSERT ITEM), or not? And would you describe yourself as
a (INSERT ITEM) or not?

1.  pentecostal
2.  NO ITEM B
3.  charismatic ASK IF PROTESTANT (CURREL = 2)
4.  “born again”

1 Yes 2 No 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

## CONSTRUCT/PREDICTOR VARIABLES (PENTECOSTAL ATTITUDES AND BEHAVIORS)

### DICHOTOMOUS (10 QUESTIONS)

ASK ALL Q55 Have you ever (INSERT)

1.  experienced or witnessed a divine healing of an illness or injury?
2.  given or interpreted prophecy?
3.  NO ITEM C
4.  received a direct revelation from God?
5.  experienced or witnessed the devil or evil spirits being driven out
    of a person?
6.  spoken or prayed in tongues?

1 Yes 2 No 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

ASK ALL Q60 Which, if any, of the following do you believe? Do you
believe (INSERT), or not? (READ LIST)

1.  in God?
2.  in Heaven?
3.  in Hell?
4.  in Miracles?

1 Yes 2 No 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

ASK IF ATTENDS RELIGIOUS SERVICES MORE THAN NEVER (Q45 = 1 THRU 5) Q46
And does the church or house of worship where you most often attend
religious services (INSERT)? (READ LIST)

1.  Try to bring others into the faith by spreading the message of
    Christ?

1 Yes 2 No 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)
