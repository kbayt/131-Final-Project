# PART 2 - DESCRIPTIVE ANALYSIS: NUMERIC / factor 

#SCORE
# distribution of score !
movies1 %>%
  ggplot(aes(x = score, fill=discrete_runtime)) +
  geom_histogram(bins = 60) +
  theme_bw()

#CORRELATION MATRIX !
# numeric variables and correlation to score
movies1 %>%
  select(where(is.numeric)) %>%
  cor() %>%
  corrplot(type = 'lower', diag = FALSE)

#VOTES !
# relationship btwn votes and score 
movies1 %>%
  ggplot(aes(x=votes, y=score)) +
  geom_point() +
  coord_flip()

#RUNTIME !
# relationship btwn runtime and score
movies1 %>%
  ggplot(aes(x = runtime, y = score)) +
  geom_point() +
  stat_summary(fun.y=mean, colour="red", geom="line")

# relationship btwn score and year released !
movies1 %>%
  ggplot(aes(x=year_released, y = score)) +
  geom_point() +
  stat_summary(fun.y=mean, colour="red", geom="line")

#BUDGET
# relationship btwn score and budget
movies1 %>%
  ggplot(aes(x=budget, y = score)) +
  geom_point()

#GROSS INCOME 
# relationship btwn score and gross income
movies1 %>%
  ggplot(aes(x=gross, y = score)) +
  geom_point() 

# NON NUMERIC

#RATING
# bar chart for distribution of rating
ggplot(data = movies1) +
  geom_bar(mapping = aes(x = rating))

# boxplot distribution of score for different levels of rating
movies1 %>%
  ggplot(aes(x = rating, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = 'Rating') +
  theme_bw()
# hisotogram of score by rating
movies1 %>%
  ggplot(aes(x=score)) +
  geom_histogram(bins=30, color = "white") +
  facet_wrap(~rating)+
  labs(title = "Histogram of Score by Rating")


#MONTH RELEASED
# boxplpot distribution of score for different month 
movies1 %>%
  ggplot(aes(x=month_released, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Month Released") +
  theme_bw()

#GENRE 
# distribution of genre
movies1 %>%
  ggplot(aes(x = forcats::fct_infreq(genre))) +
  geom_histogram(stat='count') +
  labs(y = "Count", x = 'Genre') +
  theme_bw() +
  coord_flip()

movies1 %>%
  ggplot(aes(x=genre, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Genre") +
  theme_bw() +
  coord_flip()

# !
movies1 %>%
  ggplot(aes(x=score)) +
  geom_histogram(bins=30, color = "white") +
  facet_wrap(~genre)+
  labs(title = "Histogram of Score by Genre")

# COMPANY
# sort by most frequenct (code borrowed from student final project)
company_frequent <- movies1 %>%
  group_by(company) %>%
  count() %>%
  arrange(n) %>%
  filter(n >= 20) %>%
  pull(company)

view(company_frequent)
movies1 %>%
  filter(company %in% company_frequent) %>%
  ggplot(aes(x=forcats::fct_infreq(company))) +
  geom_histogram(bins=60, stat='count') +
  coord_flip() +
  labs(y="Count", x = "Company") +
  theme_bw()

movies1 %>%
  filter(company %in% company_frequent) %>%
  ggplot(aes(x=company, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Company") +
  coord_flip() +
  theme_bw()

movies1 %>%
  filter(company %in% company_frequent) %>%
  ggplot(aes(x=score)) +
  geom_histogram(bins=30, color = "white") +
  facet_wrap(~company)+
  labs(title = "Histogram of Score by Company")


# STAR
star_frequent <- movies1 %>%
  group_by(star) %>%
  count() %>%
  arrange(n) %>%
  filter(n >= 20) %>%
  pull(star)

view(star_frequent)

movies1 %>%
  filter(star %in% star_frequent) %>%
  ggplot(aes(x=forcats::fct_infreq(star))) +
  geom_histogram(bins = 60, stat = 'count') +
  coord_flip() + 
  labs(x= "Star", y = 'Count') +
  theme_bw()

movies1 %>%
  filter(star %in% star_frequent) %>%
  ggplot(aes(x=star, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Star") +
  coord_flip() +
  theme_bw()

# WRITER
writer_frequent <- movies1 %>%
  group_by(writer) %>%
  count() %>%
  arrange(n) %>%
  filter(n >= 10) %>%
  pull(writer)

view(writer_frequent)

movies1 %>%
  filter(writer %in% writer_frequent) %>%
  ggplot(aes(x=forcats::fct_infreq(writer))) +
  geom_histogram(bins = 60, stat = 'count') +
  coord_flip() +
  labs(x="Writer", y="Count") +
  theme_bw()

movies1 %>%
  filter(writer %in% writer_frequent) %>%
  ggplot(aes(x=writer, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Writer") +
  coord_flip() +
  theme_bw()

movies1 %>%
  filter(writer %in% writer_frequent) %>%
  ggplot(aes(x=score)) +
  geom_histogram(bins=30, color = "white") +
  facet_wrap(~writer)+
  labs(title = "Histogram of Score by Writer")


# DIRECTOR
director_frequent <- movies1 %>%
  group_by(director) %>%
  count() %>%
  arrange(n) %>%
  filter(n >= 15) %>%
  pull(director)

view(director_frequent)

movies1 %>%
  filter(director %in% director_frequent) %>%
  ggplot(aes(x=forcats::fct_infreq(director))) +
  geom_histogram(bins = 60, stat = 'count') +
  coord_flip() +
  labs(x="Director", y="Count") +
  theme_bw()

movies1 %>%
  filter(director %in% director_frequent) %>%
  ggplot(aes(x=director, y = score)) +
  geom_boxplot() +
  labs(y = "Score", x = "Director") +
  coord_flip() +
  theme_bw()

movies1 %>%
  filter(director %in% director_frequent) %>%
  ggplot(aes(x=score)) +
  geom_histogram(bins=30, color = "white") +
  facet_wrap(~director) +
  labs(title = "Histogram of Score by Director")

# SCORE, VOTES, RUNTIME
# why rating not important for model
movies1 %>%
  ggplot(aes(x=votes, y=score, color=rating)) +
  geom_point()

movies1 %>%
  ggplot(aes(x=votes, y=score)) +
  geom_point() +
  facet_wrap(~rating)

movies1 %>%
  ggplot(aes(x=runtime, y=score, color=rating)) +
  geom_point()

movies1 %>%
  ggplot(aes(x=runtime, y=score)) +
  geom_point() +
  facet_wrap(~rating)

movies1 %>%
  ggplot(aes(x=score, fill=genre)) +
  geom_histogram(color='white')

movies1 %>%
  ggplot(aes(x=genre, fill=rating)) +
  geom_bar() +
  coord_flip()

#INTERACTIONS: company + genre
movies1 %>%
  filter(company %in% company_frequent) %>%
  ggplot(aes(x=forcats::fct_infreq(company), fill=genre)) +
  geom_bar() +
  coord_flip() +
  labs(y="Count", x="Company")
# top 100 scores by company
movies1 %>%
  filter(company %in% company_frequent) %>%
  arrange(-score) %>%
  select(score, company, genre) %>%
  head(100) %>%
  ggplot(aes(x=forcats::fct_infreq(company), fill=genre)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Top 100 scores by Company and Genre",
       x = "Company")
# bottom 100 scores by company and genre
movies1 %>%
  filter(company %in% company_frequent) %>%
  arrange(score) %>%
  select(score, company, genre) %>%
  head(100) %>%
  ggplot(aes(x=forcats::fct_infreq(company), fill=genre)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Bottom 100 scores by Company and Genre",
       x = "Company")

# top 100 scores by runtime 
                  
view(discrete_runtime)
view(movies1)
movies1 %>%
  filter(company %in% company_frequent) %>%
  arrange(-score) %>%
  select(score, company, genre, star, discrete_runtime, name) %>%
  head(100) %>%
  ggplot(aes(x=forcats::fct_infreq(company), fill=discrete_runtime)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Top 100 scores by Company and Runtime",
       x = "Company")

# top 100 scores by runtime 
movies1 %>%
  arrange(-score) %>%
  select(score, discrete_runtime) %>%
  head(100) %>%
  ggplot(aes(x = score, fill=discrete_runtime)) +
  geom_histogram() +
  theme_bw() +
  labs(title = "Top 100 scores by Runtime")
# bottom 100 score by runtime 
movies1 %>%
  arrange(score) %>%
  select(score, discrete_runtime) %>%
  head(100) %>%
  ggplot(aes(x = score, fill=discrete_runtime)) +
  geom_histogram() +
  theme_bw() +
  labs(title = "Bottom 100 scores by Runtime")

