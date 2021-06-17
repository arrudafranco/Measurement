Factor Analysis of Religious Participation Variables versus Democratic
Participation Variables in Latin America
================
Gustavo Arruda
6/4/2021

``` r
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
library(knitr)
library(GPArotation)

knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

# Preparing Data

``` r
rel_vars <- c('Q22', 'Q8', 'Q45',
              'Q66b', 'Q66c', 'Q17',
              'Q48a','Q48b', 'Q48c')

rel_df <-
  read_sav("Religion in Latin America Dataset.sav",
           col_select = all_of(rel_vars))

rel_df <- as.data.frame(rel_df) %>%
  zap_missing() %>%
  zap_label() %>%
  zap_labels() %>%
  drop_na() %>%
  na_if(., "98") %>%
  na_if(., "99") %>%
  drop_na()
```

# Religious Participation versus Democratic Participation

Contradicting previous sociological views on secularization, which
predicted the decline of the importance of religion in the political
arena as modernization progressed, since the 1980s religion has
re-awakened in the public arena, both in the United States and in Brazil
(Casanova 1994). In the United States, the Evangelical and
Fundamentalist revival characterized the New Religious Right, backing
the Moral Majority, prompting continued analytical relevance within
sociology and political science (Hackett and Lindsay 2008; Kellstedt and
Smidt 1991). Meanwhile, in Brazil, the Pentecostal movement, which
shares multiple characteristics with the American Evangelical and
Fundamentalists, has presented extraordinary growth since mid-20th
century. In many countries, the Pentecostal movement ended up
characterized by support to the military dictatorships of the 1970s and
1980s (Chesnut 1997; Martin 1990; Stoll 1990). More recently, in Brazil
they now represent a third of the population, with proportional
representation in Congress and increasing weight in presidential
elections (Nicolau 2014), given their high political articulation and
cohesion which allow for cheap and effective campaigns (Baptista 2009).
In the 2018 election of the authoritarian populist President Jair
Bolsonaro, who won with 55%, Pentecostals disproportionally voted for
him when compared to Catholics and the religiously unaffiliated.

Simultaneously, social scientific traditions on democratic participation
drawing from a Tocquevillian perspective draw attention to the role of
institutional religious involvement. This current understands free civic
association as a necessary connective tissue to maintain the smooth
functioning of a democracy, stimulating trust, cooperation and
productivity while avoiding the descension of democratic freedom into
chaos. Free civic association is then correlated with both democratic
values, meaning attitudes of support towards democracies, and behaviors
that characterize political participation, for instance voting, writing
letters or organizing a rally. Perspectives associated with this
tradition ranged from preoccupation with the erosion of social ties
accompanying secularization (Putnam 2000), to the operationalization of
civic skills learnt in religious settings to measure political
participation (Brady, Verba et al 1995). To extend these quantitative
analyses to a Latin American context, I decided to use Spearman
correlations and Principal Factor Analysis on the large-scale survey
“Religion in Latin America” conducted by the Pew Research Center in
2014, which includes both religious and political participation
variables.

# Methods

## Polychoric Factor Analysis and Spearman Correlation

The data used in this project comes from the “Religion in Latin America”
2014 survey published by the Pew Research Center, a nonpartisan think
tank that researches public opinion with an expertise in religion. The
survey, through 3 different languages (Spanish, Portuguese and Guarani),
included nationally representative samples from 18 different countries,
which together include more than 95% of the population in Latin America.
In total, the dataset included 30,326 observations, going to 24,341
observations after removing all empty responses from the variables of
interest.

This paper uses principal Spearman correlations and principal factor
analysis (PFA), built with polychoric correlations, to explore the
relationship among 9 variables, ordinal and dichotomous, from the 2014
“Religion in Latin America” survey conducted by the non-partisan think
tank Pew Research Center. I grounded the variable selection on
literature concerning civic association in religious contexts, political
participation and democratic values. Three of the variables regard
“secular politics” more strictly, two of them measuring political
participation, either by community involvement or political interest,
and the remaining one measuring support to democracy versus a strong
leader. The other six variables measure involvement with church
activities, from attendance to leadership roles.

Principal factor analysis is a technique that allow us to uncover latent
dimensions in a dataset, thus facilitating the comprehension of a
relatively large number of variables at the same time. PFA does not
assume any prior theory or dependent variable, configurating an
exploratory data analysis method rather than a confirmatory one. Using
polychoric and Spearman correlations rather than Pearson’s, on the other
hand, allows us to analyze a set of ordinal and dichotomous variables,
given Pearson correlation assume continuous variables with linear
relationships. Polychoric correlations assume data points to be binned
continuous values on a normal distribution, which permits a correlation
estimation of ordinal and dichotomous variables, while Spearman
correlations assume only monotonicity, or a relationship that either
strictly increases or decreases, not linearity. At last, I used a
Varimax orthogonal rotation on my PFA model a way to facilitate the
analytic distinction among factors.

# Results

The first issue that stands out is the low communality of the variables
measuring democratic values (Q17) or political participation (Q8) and
political interest (Q22). That means that the resulting PFA model
accounts little for the variance of any of those variables. As expected,
however, most of the religious participation variables show relatively
high communalities, except perhaps for Q45, which measures church
attendance, and Q66c, which measures conversations about own’s faith or
God with people from other religions. One possible explanation is that
the two items might be too broad to measure the underlying construct of
religious participation, which the other variables might be measuring.
Additionally, when looking at the proportion of variance in the model
explained by each factor, we see a sharp decline between Factor 2 and
Factor 3 (0.18 to 0.04), which justifies not including any additional
factors to the model. Given that the items selected from the “Religion
in Latin America” survey (Pew Research Center 2014) all indicate
decrease in participation as the scale number increases, we might
interpret positive correlations as going in the direction of decrease in
participation or affiliation as well.

``` r
cor(rel_df, method = 'spearman')
```

    ##              Q8          Q17         Q22         Q45        Q48a       Q48b
    ## Q8   1.00000000  0.040721487 0.109435085 0.092165186 0.099989476 0.10762950
    ## Q17  0.04072149  1.000000000 0.050429081 0.010028712 0.002086722 0.01945003
    ## Q22  0.10943509  0.050429081 1.000000000 0.000568316 0.010969638 0.02390719
    ## Q45  0.09216519  0.010028712 0.000568316 1.000000000 0.305401401 0.29757930
    ## Q48a 0.09998948  0.002086722 0.010969638 0.305401401 1.000000000 0.49431382
    ## Q48b 0.10762950  0.019450027 0.023907194 0.297579298 0.494313820 1.00000000
    ## Q48c 0.08801497  0.023762679 0.034521691 0.196067917 0.364885835 0.46834243
    ## Q66b 0.15007920 -0.007421158 0.010354089 0.483616171 0.321704670 0.32832500
    ## Q66c 0.12304040 -0.002291214 0.037652250 0.362768640 0.240810955 0.24113573
    ##            Q48c         Q66b         Q66c
    ## Q8   0.08801497  0.150079204  0.123040400
    ## Q17  0.02376268 -0.007421158 -0.002291214
    ## Q22  0.03452169  0.010354089  0.037652250
    ## Q45  0.19606792  0.483616171  0.362768640
    ## Q48a 0.36488584  0.321704670  0.240810955
    ## Q48b 0.46834243  0.328324998  0.241135731
    ## Q48c 1.00000000  0.230059198  0.179678175
    ## Q66b 0.23005920  1.000000000  0.573933144
    ## Q66c 0.17967818  0.573933144  1.000000000

``` r
fa(rel_df, rotate = 'varimax', cor = 'poly', weight = NULL,
   nfactors = 3, warnings = TRUE, fm = 'pa')
```

    ## Factor Analysis using method =  pa
    ## Call: fa(r = rel_df, nfactors = 3, rotate = "varimax", warnings = TRUE, 
    ##     fm = "pa", cor = "poly", weight = NULL)
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##        PA1   PA2   PA3    h2   u2 com
    ## Q8    0.13  0.14  0.33 0.146 0.85 1.7
    ## Q17   0.03 -0.03  0.17 0.032 0.97 1.1
    ## Q22  -0.02  0.02  0.40 0.157 0.84 1.0
    ## Q45   0.46  0.44 -0.01 0.401 0.60 2.0
    ## Q48a  0.78  0.30  0.06 0.698 0.30 1.3
    ## Q48b  0.88  0.30  0.14 0.890 0.11 1.3
    ## Q48c  0.77  0.26  0.22 0.705 0.30 1.4
    ## Q66b  0.38  0.83  0.02 0.836 0.16 1.4
    ## Q66c  0.25  0.67  0.08 0.521 0.48 1.3
    ## 
    ##                        PA1  PA2  PA3
    ## SS loadings           2.41 1.60 0.37
    ## Proportion Var        0.27 0.18 0.04
    ## Cumulative Var        0.27 0.45 0.49
    ## Proportion Explained  0.55 0.37 0.09
    ## Cumulative Proportion 0.55 0.91 1.00
    ## 
    ## Mean item complexity =  1.4
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  36  and the objective function was  3.56 with Chi Square of  86600.03
    ## The degrees of freedom for the model are 12  and the objective function was  0.02 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.01 
    ## The df corrected root mean square of the residuals is  0.02 
    ## 
    ## The harmonic number of observations is  24341 with the empirical chi square  157.2  with prob <  2e-27 
    ## The total number of observations was  24341  with Likelihood Chi Square =  453.28  with prob <  1.9e-89 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.985
    ## RMSEA index =  0.039  and the 90 % confidence intervals are  0.036 0.042
    ## BIC =  332.08
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    PA1  PA2   PA3
    ## Correlation of (regression) scores with factors   0.93 0.89  0.55
    ## Multiple R square of scores with factors          0.86 0.79  0.30
    ## Minimum correlation of possible factor scores     0.72 0.57 -0.41

Factor 1 shows a high positive correlation among occupying different
church leadership positions, a moderate correlation with church
attendance and with participation in scripture study or praying groups
(SSPG), and a low but observable correlation with community cooperation
and disposition to talk about religion with people from different
faiths. Factor 2 is similar to Factor 1, but inverts between church
leadership and SSPG the moderate and high associations. Finally, Factor
3, which has low eigenvalue thus not offering the same sturdiness than
the other two factors, shows more sizeable correlation with the
democratic participation variables (support to democracy, political
interest and community cooperation), in conjunction with relatively low
correlations with SSPG.

The Spearman correlation matrix repeats some of the same patterns. The
only correlations above 10% among the explicitly democratic
participation variables are between Q8 and Q22 (community cooperation
and political interest), Q8 and Q48b (community cooperation and church
leadership), Q8 and Q66b (community cooperation and SSPG), and Q8 and
Q66c (community cooperation and religious conversation with other
faiths). Overall, the political participation variable that seems most
associated with the rest is community participation, which seems
partially aligned with a social capital perspective on civic
association. With the regards to the other two political variables, it
is uncertain whether the association between religious and political
participation in this dataset was low, or whether the validity of the
constructs I am using are low. For future exploration I would suggest
considering additional variables as to increase the sturdiness of the
model.

# References

Baptista, Saulo. 2009. Pentecostais E Neopentecostais Na Política
Brasileira: Um Estudo Sobre Cultura Política, Estado E Atores Coletivos
Religiosos No Brasil. Sao Paulo: Instituto Metodista Izabela Hendrix.

Brady, Henry E., Sidney Verba, and Kay Lehman Schlozman. 1995. “Beyond
Ses: A Resource Model of Political Participation.” The American
Political Science Review 89(2):271–94. doi: 10.2307/2082425.

Casanova, Jose. 1994. Public Religions in the Modern World. 2nd
ed. Chicago, IL: University of Chicago Press.

Chesnut, R. Andrew. 1997. Born Again in Brazil: The Pentecostal Boom and
the Pathogens of Poverty. 1st Editio. New Brunswick: Rutgers University
Press.

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
(<https://www.pewforum.org/2014/11/13/appendix-a-methodology/>). Putnam,
Robert D. 2000. Bowling Alone. New York: Simon & Schuster.

Stoll, David. 1990. Is Latin America Turning Protestant?: The Politics
of Evangelical Growth. Berkeley, CA: University of California Press.

# Appendix A - Questionnaire

## Democratic Participation

-   ASK ALL Q17 Some feel that we should rely on a democratic form of
    government to solve our country’s problems. Others feel that we
    should rely on a leader with a strong hand to solve our country’s
    problems. Which comes closer to your opinion?

1 Democratic form of government 2 Strong leader  
98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

-   ASK ALL Q22 Would you say you follow what’s going on in government
    and public affairs (READ )?

1 Most of the time 2 Some of the time 3 Only now and then, OR 4 Hardly
at all 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

-   ASK ALL Q8 How often, if at all, do you work with other people in
    your neighborhood to improve conditions in your community? Do you do
    this (READ)? 1 Often 2 Sometimes 3 Rarely OR, 3 Never 98 Don’t know
    (DO NOT READ) 99 Refused (DO NOT READ)

## Religious Participation

-   ASK ALL Q45 Aside from weddings and funerals how often do you attend
    religious services… more than once a week, once a week, once or
    twice a month, a few times a year, seldom, or never?

1 More than once a week 2 Once a week 3 Once or twice a month 4 A few
times a year 5 Seldom 6 Never (SKIP TO Q52) 98 Don’t know (DO NOT READ)
99 Refused (DO NOT READ)

-   ASK IF ATTENDS RELIGIOUS SERVICES MORE THAN NEVER (Q45 = 1 THRU 5)
    Q48 Thinking of the church or place of worship that you attend most
    often, are you \[INSERT\], or not?

1.  a member of the church council there?
2.  the leader of any small groups or ministries there?
3.  a teacher in Sunday school or other religious education classes?

1 Yes 2 No 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)

-   ASK ALL Q66 Please tell me how often you do each of the following.
    Would you say at least once a week, once or twice a month, several
    times a year, seldom, or never? (READ LIST) (SHOW CARD)

2.  participate in prayer groups or scripture study groups
3.  share your faith or views on God with people from other religions

1 At least once a week 2 Once or twice a month 3 Several times a year 4
Seldom OR 5 Never 98 Don’t know (DO NOT READ) 99 Refused (DO NOT READ)
