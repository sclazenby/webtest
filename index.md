## Welcome to GitHub Pages

You can use the [editor on GitHub](https://github.com/sclazenby/webtest/edit/gh-pages/index.md) to maintain and preview the content for your website in Markdown files.

Whenever you commit to this repository, GitHub Pages will run [Jekyll](https://jekyllrb.com/) to rebuild the pages in your site, from the content in your Markdown files.

### Markdown

Markdown is a lightweight and easy-to-use syntax for styling your writing. It includes conventions for

```markdown
Syntax highlighted code block

# Header 1
## Header 2
### Header 3

- Bulleted
- List

1. Numbered
2. List

**Bold** and _Italic_ and `Code` text

[Link](url) and ![Image](src)
```

For more details see [GitHub Flavored Markdown](https://guides.github.com/features/mastering-markdown/).

### Jekyll Themes

Your Pages site will use the layout and styles from the Jekyll theme you have selected in your [repository settings](https://github.com/sclazenby/webtest/settings). The name of this theme is saved in the Jekyll `_config.yml` configuration file.

### Support or Contact

Having trouble with Pages? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) or [contact support](https://github.com/contact) and we’ll help you sort it out.

## Overview, Motivation, and Related Work

While the COVID-19 pandemic marches on, there continues to be a dearth of evidence that illustrates **how SARS-CoV-2 impacts men and women differently, and why these trends might vary over space.** When data first emerged from China, the virus was dubbed “a man killer,” for men were 2.4 times as likely to die from the virus. A similar trend was then observed in Italy, where a disproportionate amount of men were dying from the virus (men were representing over 70% of the deaths). Initial hypotheses for the observed health outcomes ran the gamut from differences in smoking and health-seeking behaviors to immunological differences and variances in ACE 2 receptors for the coronavirus. 

As COVID-19 continued to spread, researchers and journalists began to investigate other gendered impacts of the pandemic. After studying the rise in domestic abuse, caregiving and homeschooling responsibilities, and exposure to the virus through over-representation in ‘essential work,’ many have determined that the virus was indeed exacerbating inequities and a “disaster for feminism.” The problem, at that time, was that the paucity of sex-disaggregated data made this difficult to prove and nearly impossible to act on.

The good news, according to Global Health 5050, is that in recent months, more and more countries (now 79) have begun to **report sex-disaggregated case and mortality data** -- acknowledging that this is integral to understanding the virus and informing a strong COVID-19 response. While the prevailing hypothesis today is that more men die from COVID-19 even if and when more women are exposed, this trend is not ubiquitous (in Vietnam, for example, just 37% of the deaths are male, compared to Bangladesh where this number is 77%). **This project seeks to illustrate the observed differences, and explore potential factors that could explain what we are witnessing globally and in the US.**

This project was inspired by the [Global Health 5050](https://globalhealth5050.org/) , as well as the work out of [Harvard’s GenderSci Lab](https://www.genderscilab.org/gender-and-sex-in-covid19). 

*Note: sex-disaggregated data does not report or account for gender identity, therefore data are absent on the impact of COVID-19 on transgender and non-binary people. Some efforts are underway to redress this gap, but for the scope of this project we will use and sex and gender interchangeably.* 

### Plots for Global Case and Death Data

These plots give us a first sense how the ratio of male to female COVID-19 cases and deaths differs across Region. For example, while there have been more male cases reported where sex-disaggregated data is available, this plot shows that a cluster of European countries have observed more female cases and a cluster of Asian countries are at the other extreme. Because we are looking at ratios, these scatterplots have limited value. The next visualizations attempt to provide a more granular understanding of global trends.

```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Loading data
global_data = read_csv("global5050_clean.csv")
countries = read_csv("Country_Region_data.csv")
```
```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Scatterplot for Global Case Data
confirmed_cases = read_csv("Global Confirmed cases.csv")
confirmed_cases <- select(confirmed_cases, "country", "male", "female") %>%
  rename(Country = 'country')
confirmed_cases <- left_join(confirmed_cases, countries)
confirmed_cases <- select(confirmed_cases, "Country", "male", "female", "Region")
c <- confirmed_cases %>% 
  ggplot(aes(male, female, label = "")) +
  geom_text_repel() +
  xlab("Cases % Male") +
  ylab("Cases % Female") +
  xlim(0,100) +
  ylim(0,100) +
  ggtitle("Global COVID-19 Case Data Disaggregated by Sex") + 
  theme_economist()
c + geom_point(aes(color = Region), size = 3)
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Global Deaths Scatterplot
global_deaths = read_csv("Global Deaths.csv")
global_deaths <- select(global_deaths, "country", "male", "female") %>%
  rename(Country = 'country')
global_deaths <- left_join(global_deaths, countries)
global_deaths <- select(global_deaths, "Country", "male", "female", "Region")
global_deaths <- subset(global_deaths, global_deaths$male!=0.00 & global_deaths$female!=0.00)
d <- global_deaths %>% 
  ggplot(aes(male, female, label = "")) +
  geom_text_repel() +
  xlab("Deaths % Male") +
  ylab("Deaths % Female") +
  ggtitle("Global COVID-19 Death Data Disaggregated by Sex") + 
  xlim(0,100) +
  ylim(0,100) +
  theme_economist()
  
d + geom_point(aes(color = Region), size = 3)
```

### Global Case Data

This stacked barplot gives us a slightly better sense of how these sex ratios vary between countries, but because a limited number of case data has been disaggregated by sex (many countries only started in recent months) we will also want to look at this by percentage. We can see that the US has the greatest number of COVID-19 cases disaggregated by sex, followed by India. 


```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Stacked Barplot for Global Cases 
dat <- select(global_data, "Country", "Cases where sex-disaggregated data is available", "Cases (% male)", "Cases (% female)") %>%
  rename(country = 'Country') %>%
  drop_na() %>%
  rename(total_cases = 'Cases where sex-disaggregated data is available') %>%
  rename(male_pct = "Cases (% male)") %>%
  rename(female_pct = "Cases (% female)") %>%
  mutate(male_cases = total_cases * male_pct) %>%
  mutate(female_cases = total_cases * female_pct) %>%
  mutate(country_fct = reorder(factor(country), male_pct)) 
dat <- select(dat, "country", "male_pct", "male_cases", "female_cases", "country_fct")
#converting to long format
dat_long <- dat %>%
  pivot_longer(cols = c("male_cases", "female_cases"),
                        names_to = "gender",
                        values_to = "cases")
```

```{r, fig.height = 12, fig.width = 8, message=FALSE, warning=FALSE, echo=FALSE}
#Cases in Absolute Numbers
dat_long %>%
  ggplot(aes(x = country_fct, y = cases, fill = gender)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) +
  ylab("Cases where Sex-Disaggregated Data Available") +
  ggtitle("Global COVID-19 Cases Where Sex-Disagregated Data Available") + 
  xlab("Countries")
```

### Global Case Data By Percentage

This stacked barplot gives us a better sense of the sex ratios for COVID-19 cases where sex-disaggregated data are available and how these trends are not universal. For example, while the majority of cases in Qatar and Singapore have been male, countries like Guernsey, Gabon and the Ukraine have observed the opposite trend. This also gives us an understanding that for many countries, the ratio of male to female COVID-19 cases is nearly 1:1. 

```{r, fig.height = 12, fig.width = 8, message=FALSE, warning=FALSE, echo=FALSE}
#Cases by Percent
dat_long %>%
  ggplot(aes(x = country_fct, y = cases, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) + 
  ylab("Cases where Sex-Disaggregated Data Available by %") +
  ggtitle("Global COVID-19 Cases Where Sex-Disagregated Data Available by %") +
  xlab("Countries")
```


```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Stacked Barplot for Global Cases 
dat_death <- select(global_data, "Country", "Deaths where sex-disaggregated data is available", "Deaths (% male)", "Deaths (% female)") %>%
  rename(country = 'Country') %>%
  drop_na() %>%
  rename(total_deaths = "Deaths where sex-disaggregated data is available") %>%
  rename(male_death_pct = "Deaths (% male)") %>%
  rename(female_death_pct = "Deaths (% female)")
dat_death <- transform(dat_death, male_death_pct = as.numeric(male_death_pct))
dat_death <- transform(dat_death, female_death_pct = as.numeric(female_death_pct))
dat_death <- dat_death %>%
  mutate(male_deaths = total_deaths * male_death_pct) %>%
  mutate(female_deaths = total_deaths * female_death_pct) %>%
  mutate(country_fct = reorder(factor(country), male_death_pct)) 
dat_death <- select(dat_death, "country", "male_death_pct", "male_deaths", "female_deaths", "country_fct")
#converting to long format
dat_death_long <- dat_death %>%
  pivot_longer(cols = c("male_deaths", "female_deaths"),
                        names_to = "gender",
                        values_to = "deaths")
```

### Global Death Data 

Our initial hypothesis was that we would see more male deaths from COVID-19 than female deaths for the reasons stated in the background section. This stacked bar chart provides some insight into country trends, but again, because sex-disaggregated death data is limited, we will also want to look at the data by percentage. We see that the US has the greatest number of deaths disaggregated by sex, followed by Brazil and Mexico -- all of which have seen more men dying from COVID-19 than women. 
```{r, fig.height = 12, fig.width = 8, message=FALSE, warning=FALSE, echo=FALSE}
#Deaths in Absolute Numbers
dat_death_long %>%
  ggplot(aes(x = country_fct, y = deaths, fill = gender)) +
  geom_bar(position = "stack", stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) +
  ylab("Deaths where Sex-Disaggregated Data Available") +
  ggtitle("Global COVID-19 Deaths Where Sex-Disagregated Data Available") +
  xlab("Countries")
```

### Global Death Data By Percentage

This stacked barplot gives us a better sense of the sex ratios for COVID-19 deaths where sex-disaggregated data are available and how these trends are not universal. Unlike case data, the majority of countries have reported > 50% of their COVID-19 deaths being male with countries like Chad, Yemen and Bangladesh reporting over 75%. However, there are outliers - countries like Lebanon, Vietnam and Guernsey have reported more female deaths than male deaths from COVID-19. Future analyses ought to control for baseline population characteristics. 

```{r, fig.height = 12, fig.width = 8, message=FALSE, warning=FALSE, echo=FALSE}
#Deaths by Percent
dat_death_long %>%
  ggplot(aes(x = country_fct, y = deaths, fill = gender)) +
  geom_bar(position = "fill", stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_text(size = 8)) + 
  ylab("Deaths where Sex-Disaggregated Data Available by %") +
  ggtitle("Global COVID-19 Deaths Where Sex-Disagregated Data Available by %") +
  xlab("Countries")
```

```{r, message=FALSE, warning=FALSE, echo=FALSE}
#Number of Countries with Sex-Disaggregated COVID-19 Data 
global_data <- select(global_data, "Case & death data by sex?") %>%
  rename(reporting = 'Case & death data by sex?')
sum(global_data$reporting == "Yes")
sum(global_data$reporting == "No")
sum(global_data$reporting == "Partial")
```
### The Case for Sex-Disaggregated Data
While these visualizations illuminate some trends, we also see the need for more sex-disaggregated data to ascertain a better understanding of the true trends. According to Global Health 5050, as of December, there are 85 countries reporting complete sex-disaggregated COVID-19 data, 48 countries reporting partial data and 53 countries reporting no sex-disaggregated COVID-19 data.

