---
title: "Application Task 2"
author: "Shreya Shakya"
date: "06/23/2019"
output: 
  html_document: 
    keep_md: yes
---

### Load packages


```r
library(tidyverse)
library(ggplot2)
```

### Load data


```r
pokemon <- read_csv("data/pokemon.csv")
```

### 2.1: How do Pokémon heights change after evolution?

Calculate the difference in heights pre and post evolution and save this as a new variable in the dataset. Calculate the percentage of Pokémon that grew during evolution.

```r
(poke <- 
pokemon %>%
  mutate(height_diff = height_new - height) %>%
  mutate(grow_percent = ((height_new - height)/(height))*100))
```

```
## # A tibble: 75 x 29
##    name  species    cp    hp weight height power_up_stardu… power_up_candy
##    <chr> <chr>   <dbl> <dbl>  <dbl>  <dbl>            <dbl>          <dbl>
##  1 Pidg… Pidgey    384    56   2.31  0.34              2500              2
##  2 Pidg… Pidgey    366    54   1.67  0.290             2500              2
##  3 Pidg… Pidgey    353    55   1.94  0.3               3000              3
##  4 Pidg… Pidgey    338    51   1.73  0.31              3000              3
##  5 Pidg… Pidgey    242    45   1.44  0.27              1900              2
##  6 Pidg… Pidgey    129    35   2.07  0.35               800              1
##  7 Pidg… Pidgey     10    10   0.92  0.25               200              1
##  8 Pidg… Pidgey     25    14   2.72  0.37               200              1
##  9 Pidg… Pidgey     24    13   2.07  0.32               200              1
## 10 Pidg… Pidgey    161    35   1.45  0.31              1000              1
## # … with 65 more rows, and 21 more variables: attack_weak <chr>,
## #   attack_weak_type <chr>, attack_weak_value <dbl>, attack_strong <chr>,
## #   attack_strong_type <chr>, attack_strong_value <dbl>, cp_new <dbl>,
## #   hp_new <dbl>, weight_new <dbl>, height_new <dbl>,
## #   power_up_stardust_new <dbl>, power_up_candy_new <dbl>,
## #   attack_weak_new <chr>, attack_weak_type_new <chr>,
## #   attack_weak_value_new <dbl>, attack_strong_new <chr>,
## #   attack_strong_type_new <chr>, attack_strong_value_new <dbl>,
## #   notes <chr>, height_diff <dbl>, grow_percent <dbl>
```

Visualize the distribution of change in height by species


```r
library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## The following object is masked from 'package:purrr':
## 
##     compact
```

```r
cdat <- ddply(poke,"species", summarise, height_diff.mean=mean(height_diff))

ggplot(data = poke, aes(x = height_diff,fill=species)) +
  geom_density(alpha = 0.4) +
    geom_vline(data=cdat, aes(xintercept=height_diff.mean,  colour=species),linetype="dashed", size=1)
```

![](application02_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggplot(data = poke, aes(x = height_diff,fill=species)) +
  geom_density(alpha = 0.4) +
facet_wrap(~species)  +
    geom_vline(data=cdat, aes(xintercept=height_diff.mean,  colour=species),linetype="dashed", size=1)
```

![](application02_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
ggplot(data = poke, aes(x = height_diff,fill=species)) +
  geom_histogram(position = "identity")
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](application02_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
ggplot(data = poke) +
  geom_histogram(mapping = aes(x = height_diff,fill=species))+
     facet_wrap(~species)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](application02_files/figure-html/unnamed-chunk-2-4.png)<!-- -->

**How change in height varies both in general and across species**

*In general, different species have different spread for height difference. The center for height difference between each species is in a different location. The differences in the locations indicate that the mean height difference are different for all species. Weedle species tends to have lower difference in height growth whereas Pidgey tends to have higher difference in height growth among all the species. Eevee seems to vary in growth as the spread is wider, but most of the values are clustered towards the left side. Caterpie also seems to have lower difference in height growth. 

Looking at height difference in  each of the species, most values for Caterpie species are close to 0.4 and values further away are rarer. The distribution is almost symmetric and values falls between 0.3 to 0.5. For Eevee species, the long tail extends to right while most of the values are clustered to left. Pidgey species most of the values are close to 0.8 and values further away are rare. The distribution is roughly symmetric and values falls between 0.57 to 1.0. Similarly for Weedle is most of the value is at 0.3. The distribution is roughly symmetric and values falls between 0.2 to 0.42.*

### 2.2: Recreate this plot


```r
p <-poke %>%
  filter(species != c("Weedle")) %>%
  group_by(species, attack_weak) %>%
  tally() %>%
  arrange(desc(n))
p
```

```
## # A tibble: 6 x 3
## # Groups:   species [3]
##   species  attack_weak      n
##   <chr>    <chr>        <int>
## 1 Pidgey   Tackle          22
## 2 Pidgey   Quick Attack    17
## 3 Caterpie Bug Bite         5
## 4 Caterpie Tackle           5
## 5 Eevee    Tackle           4
## 6 Eevee    Quick Attack     2
```

```r
ggplot(data=p,aes(x=species, y=n, fill=attack_weak)) +
  coord_flip() +
    geom_bar(stat="identity" , position=position_dodge()) +
  ylab("Frequency")
```

![](application02_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


### 2.3: Do Pokémon tend to change their stronger attack post-evolution?

Calculate the relative frequency of post-evolution stronger attack based on the pre-evolution stronger attack. Provide a discussion.Another way to read this is, "For each pre-evo attack, what is the percentage breakdown of each post-Evo attack?" This could be a table or plot.


```r
poke <- poke %>%
  mutate(rel_freq_strong = attack_strong_value_new / sum(attack_strong_value))

poke %>%
  select(species, attack_strong,attack_strong_value,attack_strong_new,attack_strong_value_new,rel_freq_strong) %>%
  mutate(rel_freq_strong = attack_strong_value_new / sum(attack_strong_value))
```

```
## # A tibble: 75 x 6
##    species attack_strong attack_strong_v… attack_strong_n… attack_strong_v…
##    <chr>   <chr>                    <dbl> <chr>                       <dbl>
##  1 Pidgey  Aerial Ace                  30 Air Cutter                     30
##  2 Pidgey  Twister                     25 Air Cutter                     30
##  3 Pidgey  Aerial Ace                  30 Air Cutter                     30
##  4 Pidgey  Air Cutter                  30 Air Cutter                     30
##  5 Pidgey  Air Cutter                  30 Twister                        25
##  6 Pidgey  Air Cutter                  30 Aerial Ace                     30
##  7 Pidgey  Air Cutter                  30 Air Cutter                     30
##  8 Pidgey  Twister                     25 Air Cutter                     30
##  9 Pidgey  Twister                     25 Twister                        25
## 10 Pidgey  Twister                     25 Aerial Ace                     30
## # … with 65 more rows, and 1 more variable: rel_freq_strong <dbl>
```

```r
#ggplot(data = poke, aes(x = rel_freq_strong,fill=species)) +
  #geom_density(alpha=0.35)

cross <- table(poke$attack_strong_new,poke$attack_strong)
round(prop.table(cross,2)*100,digits=2)
```

```
##              
##               Aerial Ace Air Cutter Body Slam    Dig Struggle  Swift
##   Aerial Ace       36.36      38.46      0.00   0.00     0.00   0.00
##   Air Cutter       36.36      38.46      0.00   0.00     0.00   0.00
##   Aqua Tail         0.00       0.00     33.33   0.00     0.00 100.00
##   Fire Blast        0.00       0.00      0.00  50.00     0.00   0.00
##   Heat Wave         0.00       0.00     66.67   0.00     0.00   0.00
##   Struggle          0.00       0.00      0.00   0.00   100.00   0.00
##   Thunderbolt       0.00       0.00      0.00  50.00     0.00   0.00
##   Twister          27.27      23.08      0.00   0.00     0.00   0.00
##              
##               Twister
##   Aerial Ace    46.67
##   Air Cutter    26.67
##   Aqua Tail      0.00
##   Fire Blast     0.00
##   Heat Wave      0.00
##   Struggle       0.00
##   Thunderbolt    0.00
##   Twister       26.67
```
*The above graph shows the table for each pre-evo attack, the percentage breakdown of each post-Evo attack. Each column represents the pre evolution attack and each observation represents the post evolution attack.*

*For pre-evo attack Aerial Ace air, the percentage breakdown of its post evolution attack is 36.36%, 36.36% and 27.27% on Aerial Ace, Air Cutter and Twister respectively.*

*For pre-evo attack Cutter Body, the percentage breakdown of its post evolution attack is 38.46%, 38.46% and 23.08% on Aerial Ace, Air Cutter and Twister respectively.*

*For pre-evo attack Body slam, the percentage breakdown of its post evolution attack is 33.33% and 67.67% on Aqua Tail and Heat Wave respectively.*

*For pre-evo attack Dig, the percentage breakdown of its post evolution attack is 50% and 50% on Fire Blast and Thunderbolt respectively.*

*For pre-evo attack Struggle, the percentage breakdown of its post evolution attack is 100% on Struggle itself.*

*For pre-evo attack Swift, the percentage breakdown of its post evolution attack is 100% on Aqua Tail respectively.*

*For pre-evo attack Twister, the percentage breakdown of its post evolution attack is 47.67%, 27.67% and 27.67% on Aerial Ace, Air Cutter and Twister respectively.*


### 2.4: Relationship between categorical variables

Pick two categorical variables and construct a plot that depicts the relationship between them. 


```r
ggplot(poke, aes(x=attack_strong_type,y =attack_strong_value)) + geom_bar(aes(fill = attack_strong), stat = "identity", position = "dodge") 
```

![](application02_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#+facet_wrap(~species)
```

*Here, attack strong value for several different attack strong type are graphed. Within each original category i.e attack strong type, the data is categorized further based on a second categorical variable attack_strong. These "attack_strong” categories are created within each of the four “attack strong type” categories, and each of the seven categories-within-a-category has its own bar. While the original categories are distinguished using the original labeling system along the x-axis, the second categorical variable is determined using a color scheme.*

*Dragon attack type has attack strong value of around 23. Twister is the only name of the pre-evolution stronger attack under Dragon attack_type. Similary for flying and ground attack type, attack strong value is around 25 and 65 respectively. Aerial Ace and Air cutter strong attack is of type flying. Dig is the strong attack for Ground type.For the normal attack strong type, the attack strong value differs according to different attack_strong names. Body slam attack strong has strong value of around 40, Struggle has value aroung 18 and Swift has value around 25 which all lies under Normal attack_strong_type.*


### 2.5: Comparative boxplots

Pick a numerical and a categorical variable and construct a side-by-side boxplot depicting their relationship.


```r
boxplot(cp~species,
data=poke,
main="Pre-evolution Combat Power for different kind of species",
xlab="Types of species",
ylab="Combat Power"
)
```

![](application02_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

*The above graph shows the relation between different species and their pre evolution combat power. The box plot for Eevee is much higher compared to other boxplots which means that combat power for Eevee is much higher than rest of the species. The box plot of caterpie, pidgey and weedle have roughly same median, but different distribution. The box plot is comparatively short for Eevee which suggests that overall Evee have a similar range combat power. The box plot for Pidgey is comparatively tall and it suggests that these species hold quite different range of combat power. The distribution for Eevee is skewed left, most of the Eevee has higher than average combat power. Whereas the distribution for Pidgey, Caterpie and Weedle is fairly symmetrical. There are no obvious outliers in any of the samples.*

### 2.6: Violin plots

What do the violin plots reveal that the boxplots do not? What features are apparent in the boxplots, but not in the violin plots?


```r
 ggplot(poke, aes(species,cp,fill=species))+
  geom_violin(alpha=0.3) +
  geom_boxplot(width=0.1) 
```

![](application02_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
   #+geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7)
```

*Similar to boxplot, Voilin plots can also be used to show the relationship between species and their pre evolution combat power. From above graph, we can see that these plots are more informative compared to previous boxplot. Box plot can only be used to show the summary statistics such as mean, median, interquartile ranges. Whereas, voilin plots can show these summary statistics along with the distribution of the data.*

*We can see that there is a very high density around the mean for caterpie species and appears to be roughly symmetric. It is similarly for species Weedle. For Eevee we can clearly see that distribution is left skewed. For Pidgey the graph shows that it has high density for cp of around 100 and cp of around 340. It appears to have bimodal distribution which is not seen in the boxplot.*


### 2.7: Characteristics of an evolved Pokémon combat power

What characteristics correspond to an evolved Pokémon with a high combat power? You do not need to come up with an exhaustive list, but you should walk through your reasoning as you answer this question. Include all relevant summary statistics and visualizations. You should look at pairings of variables that you think might impact cp and explore if these contribute to higher values. Again, tables or plots will help you answer this.

*The evolved pokemon with high combat power could be analyzed by looking at the correlation between each of the variable*


```r
library(corrplot)
```

```
## corrplot 0.84 loaded
```

```r
corr_data <- poke[, sapply(poke, is.numeric)]
M <- cor(corr_data)
c2 <- corrplot(M,type = "upper",order = "hclust", method = "square", tl.cex = 0.75, cl.cex = 1)
```

![](application02_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
library(corrr)
data <- pokemon[, sapply(pokemon, is.numeric)]
data %>% correlate(use="pairwise.complete.obs") %>% focus(cp) %>% data.frame()
```

```
## 
## Correlation method: 'pearson'
## Missing treated using: 'pairwise.complete.obs'
```

```
##                    rowname          cp
## 1                       hp  0.87607609
## 2                   weight  0.45483415
## 3                   height -0.01717795
## 4        power_up_stardust  0.77385620
## 5           power_up_candy  0.64479192
## 6        attack_weak_value  0.33527977
## 7      attack_strong_value  0.61326426
## 8                   cp_new  0.96101924
## 9                   hp_new  0.90764010
## 10              weight_new  0.05786113
## 11              height_new  0.20066976
## 12   power_up_stardust_new  0.77385620
## 13      power_up_candy_new  0.64479192
## 14   attack_weak_value_new  0.06568735
## 15 attack_strong_value_new  0.65424077
```

```r
data %>% correlate() %>% focus(cp_new) %>%
  ggplot(aes(x = rowname, y = cp_new)) +
    geom_bar(stat = "identity") +
    ylab("Correlation with cp_new") +
    xlab("Variables") +
  theme(axis.text.x=element_text(angle=90,margin = margin(-0.15, unit = "cm"),vjust =1))
```

```
## 
## Correlation method: 'pearson'
## Missing treated using: 'pairwise.complete.obs'
```

![](application02_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
data %>% correlate() %>% focus(cp) %>%
  ggplot(aes(x = rowname, y = cp)) +
    geom_bar(stat = "identity") +
    ylab("Correlation with cp_new") +
    xlab("Variables") +
  theme(axis.text.x=element_text(angle=90,margin = margin(-0.15, unit = "cm"),vjust =1))
```

```
## 
## Correlation method: 'pearson'
## Missing treated using: 'pairwise.complete.obs'
```

![](application02_files/figure-html/unnamed-chunk-9-3.png)<!-- -->
**From the above analysis, we can conclude that mostly the variables HP, power_up_stardust (new and old) and attack_strong_value_new has impact on CP. CP is highly postively correlated with Hp old and Hp new with correlation of 0.88 and 0.91 respectively. Similarly, Cp has postive correlation with power_up_stardust (old and new) with correlation of 0.77. These variables are explored more below.**


```r
library("dplyr")
ggplot(data = poke, aes(x = hp, y = cp, color=species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

![](application02_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
ggplot(data = poke, aes(x = power_up_stardust, y = cp, color=species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
```

![](application02_files/figure-html/unnamed-chunk-10-2.png)<!-- -->

*For all the species we can see that as hp increases cp tends to increase postively. Similarly as the power_up_stardust increases, Cp also tends to increase positively. Therefore, we can conclude that evolved Pokémon with a high combat power highly depends on how difficult it is to weaken the pokemon and stardust required to power up the Pokemon.*
