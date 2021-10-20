---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
description: Nothing better than a good movie with some nice popcorn, especially on a rainy day which as we all know we get a lot in London. Even better is a good talk about different movies and directors. That being sad, let's take a look on the ratings between to of the greatest movie directors of all time.
draft: false
image: movie.jpg
keywords: ""
slug: movies
title: Movies
---


# IMDB ratings: Differences between directors

Recall the IMBD ratings data. I would like you to explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. I have already calculated the confidence intervals for the mean ratings of these two directors and as you can see they overlap. 


```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```

First, I would like you to reproduce this graph. You may find `geom_errorbar()` and `geom_rect()` useful.

In addition, you will run a hpothesis test. You should use both the `t.test` command and the `infer` package to simulate from a null distribution, where you assume zero difference between the two.

> Before anything, write down the null and alternative hypotheses, as well as the resulting test statistic and the associated t-stat or p-value. At the end of the day, what do you conclude?

You can load the data and examine its structure

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies)
```

Your R code and analysis should go here. If you want to insert a blank chunk of R code you can just hit `Ctrl/Cmd+Alt+I` 

First, I would like you to reproduce this graph. You may find `geom_errorbar()` and `geom_rect()` useful:
```{r}
S_directors <- movies %>%
  select(director, rating) %>%
  filter(director == "Steven Spielberg" | director == "Tim Burton") %>%
  group_by(director) %>%
  summarize(avg = mean(rating), 
            sd = sd(rating), 
            count = n(),
            tstat = qt(0.975, count-1),
            se = sd/sqrt(count),
            lower_ci = avg - tstat*se, 
            upper_ci = avg + tstat*se) 
S_directors

ggplot(S_directors, aes(x = avg,
                        y = reorder(director, avg), 
                        color = director)) +
  
  geom_errorbar(aes(xmin = lower_ci, 
                    xmax = upper_ci), 
                width = 0.1) +
  
  labs(title = "Do Spielberg and Burton have the same IMDB Ratings",
       subtitle = "95% confidence intervals overlap",
       x = "Mean IMDB Rating",
       y = "") +
  
  geom_text(aes(label=round(lower_ci,2)), 
            vjust=-1.5,
            hjust=3,
            angle=0, 
            color="black", 
            size=7) +
  
  geom_text(aes(label=round(upper_ci,2)), 
            vjust=-1.5,
            hjust=-2,
            angle=0, 
            color="black", 
            size=7) +
  
  geom_text(aes(label=round(avg,2)), 
            vjust=-1, 
            colour= "black", 
            size= 11)+
  
  geom_point() +
  theme_classic()+
  theme(legend.position = "none") +
  annotate("rect", 
           fill = "grey", 
           alpha = 0.5, 
           xmin=7.27, 
           xmax= 7.33, 
           ymin=-Inf, 
           ymax = Inf)


```

In addition, you will run a hpothesis test. You should use both the `t.test` command and the `infer` package to simulate from a null distribution, where you assume zero difference between the two:

```{r}
#The null hypothesis: both directors have the same budget for their films
#The alternative hypothesis: both directors don't have the same budget for their films

Null_testing <- movies %>%
  select(director, budget) %>%
  filter(director == "Tim Burton" | director == "Steven Spielberg")

t.test(budget ~ director, data =Null_testing)

```
```{r}
budget_null_init<- Null_testing%>%
specify(budget ~ director)%>%
calculate(stat="diff in means",order=c("Steven Spielberg","Tim Burton")) 

#Test Simulation on the null distribution
budget_null<-Null_testing%>%
specify(budget ~ director)%>%
hypothesize(null="independence")%>%
generate(reps=1000,type="permute")%>%
  
calculate(stat="diff in means",order=c("Steven Spielberg","Tim Burton"))

#Null distribution Visualization and get the p value
budget_null %>% 
visualize() +
  theme_classic()+
  shade_p_value(obs_stat = budget_null_init,
                direction = "two-sided") 

budget_null %>% get_p_value(obs_stat = budget_null_init, direction = "two_sided")

```
