#load libraries
library(tidyverse)
library(broom)
library(car)
library(stringr)
library(dplyr)
library(partykit)
library(entropy)

############################################
#     READING DATA
############################################

# define negated %in% operator
`%!in%` <- Negate(`%in%`)

wubuy_data_raw <- read_csv('Noun Occurance Database.csv')


# filter for acts and states
wubuy_data <- filter(wubuy_data_raw, `Predicate Class` %in% c('act', 'state', 'adj'))

# convert database into counts

# make new tibble to store values
frequency_table <- tribble(~`Noun Group`, ~`Body Part`, ~Connected, ~Count, ~Incorporated, ~Focus, ~Topic, ~`Core Role`, ~`Unmarked Oblique`, ~`Marked Oblique`, ~Act, ~State, ~Adj, ~Determiner, ~Possession, ~`Implied Possession`)

# for each noun group find list of values
for(grouping in unique(wubuy_data$`Noun Grouping`)){
  # add new row
  frequency_table <- add_row(frequency_table, 
                             `Noun Group` = grouping,
                             `Body Part` = unique(filter(wubuy_data, `Noun Grouping` == grouping)$`Body Part`)[1],
                             Connected = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$Connected == 'yes')),
                             Count = count(filter(wubuy_data, `Noun Grouping` == grouping))$n ,
                             Incorporated = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$Incorporated == 'yes')),
                             Focus = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$`Focus/Topic` == 'focus')),
                             Topic = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$`Focus/Topic` == 'topic')),
                             `Core Role` = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `argument function` %in% c('A', 'S', 'O')))$n,
                             `Unmarked Oblique` = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `argument function` == 'OBL'))$n,
                             `Marked Oblique` = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `argument function` %!in% c('A', 'S', 'O', 'OBL')))$n,
                             Act = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `Predicate Class` == 'act'))$n,
                             State = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `Predicate Class` == 'state'))$n,
                             Adj = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `Predicate Class` == 'adj'))$n,
                             Determiner = count(filter(filter(wubuy_data, `Noun Grouping` == grouping), `determiner co-occurance` != 'no'))$n,
                             Possession = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$`Possession status` %in% c('part/whole', 'possessor raising', 'X-REL Y'))),
                             `Implied Possession` = length(which(filter(wubuy_data, `Noun Grouping` == grouping)$`Possession status` %in% c('part/whole', 'possessor raising', 'non overt', 'X-REL Y'))))
}

# make counts into rates
frequency_table <- mutate(frequency_table, `Incorporation Rate` = (Incorporated/Count))
frequency_table <- mutate(frequency_table, `Focus Rate` = Focus / Count)
frequency_table <- mutate(frequency_table, `Topic Rate` = Topic / Count)
frequency_table <- mutate(frequency_table, `Connected Percent` = Connected / Count)
frequency_table <- mutate(frequency_table, `Core Role Percent` = `Core Role`/Count)
frequency_table <- mutate(frequency_table, `Unmarked Oblique Role Percent` = `Unmarked Oblique`/Count)
frequency_table <- mutate(frequency_table, `Marked Oblique Role Percent` = `Marked Oblique`/Count)
frequency_table <- mutate(frequency_table, `Oblique Role Percent` = (`Unmarked Oblique` + `Marked Oblique`)/Count)
frequency_table <- mutate(frequency_table, `Act Percent` = Act/Count)
frequency_table <- mutate(frequency_table, `State Percent` = State/Count)
frequency_table <- mutate(frequency_table, `Adj Percent` = Adj/Count)
frequency_table <- mutate(frequency_table, `Determiner Percent` = Determiner/Count)
frequency_table <- mutate(frequency_table, `Possessed Percent` = Possession/Count)
frequency_table <- mutate(frequency_table, `Implied Possessed Percent` = `Implied Possession`/Count)


# Filter out words with less then 10 instances
frequency_table_trimmed <- filter(frequency_table, Count >= 10)

#histogram of incorporability
ggplot(frequency_table_trimmed, aes(x = `Incorporation Rate`)) + geom_histogram() + theme_minimal()

#descriptive stats

mean(frequency_table_trimmed$`Incorporation Rate`)
sd(frequency_table_trimmed$`Incorporation Rate`)

#simple t-test

t.test(frequency_table_trimmed$`Incorporation Rate`, mu = 0)

############################################
#     NARRATOR EFFECT
############################################
narrator_data <- tribble(~Narrator, ~Count, ~Incorporated)

# for each narrator, count variables and add new row to tibble
for (nar in unique(wubuy_data$Narrator)){
  narrator_data <- add_row(narrator_data,
                           Narrator = nar,
                           Count = count(filter(wubuy_data, Narrator == nar)),
                           Incorporated = length(which(filter(wubuy_data, Narrator == nar)$Incorporated == 'yes')))
}

narrator_data <- mutate(narrator_data, `Percent Incorporated` = Incorporated/Count$n)

# rate of incorporation compared to mean
narrator_data %>% mutate(Narrator = str_wrap(Narrator, width = 10)) %>% ggplot(aes(x = Narrator, y = `Percent Incorporated`)) + 
  geom_point() + 
  theme_minimal() + 
  geom_hline(yintercept = mean(narrator_data$`Percent Incorporated`)) + 
  geom_text(aes(0.65, mean(`Percent Incorporated`), 
                label = round(mean(`Percent Incorporated`), 2), vjust = -1)) + 
  ggtitle("Percent Incorporated by Narrator")

# box plot of narrators incorporation rate
narrator_data %>% ggplot(aes(x = `Percent Incorporated`)) + 
  geom_histogram() + theme_minimal()

############################################
#     TOPIC/GENRE
############################################
genre_data <- tribble(~Genre, ~Count, ~Incorporated)

for (genre in unique(wubuy_data$Genre)){
  genre_data <- add_row(genre_data, 
                        Genre = genre,
                        Count = count(filter(wubuy_data, Genre == genre))$n,
                        Incorporated = length(which(filter(wubuy_data, Genre == genre)$Incorporated == 'yes')))
}

genre_data <- mutate(genre_data, `Incorporation Rate` = Incorporated/Count)

genre_data

genre_data %>% ggplot(aes(x = Genre, y = `Incorporation Rate`)) + 
  geom_point() + 
  theme_minimal() + 
  geom_hline(yintercept = mean(genre_data$`Incorporation Rate`)) + 
  geom_text(aes(0.65, mean(`Incorporation Rate`), label = round(mean(`Incorporation Rate`), 2), vjust = -1)) +
  ggtitle("Percent Incorporated by Topic") +
  xlab('Topic') +
  ylab('Percent Incorporated')


############################################
#     GENRE AND NARRATOR
############################################
Narrators <- rep(sort(unique(wubuy_data$Narrator)), times = length(unique(wubuy_data$Genre)))
Genres <- rep(sort(unique(wubuy_data$Genre)), each = length(unique(wubuy_data$Narrator)))

count <- rep(0, 48)
incorp <- rep(0, 48)

for(i in 1:length(narrators)){
  count[i] <- count(filter(wubuy_data, Narrator == narrators[i] & Genre == genres[i]))$n
  incorp[i] <- count(filter(wubuy_data, Narrator == narrators[i] & Genre == genres[i] & Incorporated == 'yes'))$n
}

gen_nar_total <- tibble(genres, narrators, count, incorp)
gen_nar_total

gen_nar_total <- mutate(gen_nar_total, `Percent Incorporated` = incorp/count)
gen_nar_total


#plot incorporation rate for each genre by speaker; filtering out na vals
gen_nar_total %>% filter(!is.na(`Percent Incorporated`)) %>% 
  ggplot(aes(x = genres, y = `Percent Incorporated`, color = narrators)) + 
  stat_summary(geom = 'point', fun = 'mean', col = 'black', 
               size = 2, shape = 24, fill = 'red') +
  geom_point() + theme_minimal() + 
  ggtitle('Percent Incorporated by Topic for each Narrator With Mean') + 
  xlab('Topic') + 
  labs(color = 'Narrator')

gen_nar_total %>% filter(!is.na(`Percent Incorporated`)) %>% 
  mutate(narrators = str_wrap(narrators, width = 10)) %>%
  ggplot(aes(color = genres, y = `Percent Incorporated`, x = narrators)) + 
  stat_summary(geom = 'point', fun = 'mean', col = 'black', 
               size = 2, shape = 24, fill = 'red') +
  geom_point() + theme_minimal() + 
  ggtitle('Percent Incorporated by Narrator for each Topic With Mean') + 
  labs(color = 'Topic') +
  xlab('Narrator')

wubuy_data %>% mutate(Narrator = str_wrap(Narrator, width = 10)) %>%
  ggplot(aes(fill=Genre, x = Narrator)) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  scale_fill_brewer(palette = 'PuOr') +
  ggtitle('Proportion of Target Noun Count by Topic') + 
  labs(fill = 'Topic') +
  ylab('Proportion')

############################################
#     FOCUS
############################################
incorporate_mdl <- lm(`Incorporation Rate` ~ `Focus Rate`, data = frequency_table_trimmed)
summary(incorporate_mdl)

incorporate_mdl_body <- lm(`Incorporation Rate` ~ `Focus Rate`, data = filter(frequency_table_trimmed, `Body Part` == 'body part'))
summary(incorporate_mdl_body)

incorporate_mdl_generic <- lm(`Incorporation Rate` ~ `Focus Rate`, data = filter(frequency_table_trimmed, `Body Part` != 'body part'))
summary(incorporate_mdl_generic)

frequency_table_trimmed %>% ggplot(aes(x = `Focus Rate`, y = `Incorporation Rate`)) + 
  geom_point(aes(col = `Body Part`, shape = `Body Part`)) + geom_smooth(method = 'lm') + theme_minimal() + ggtitle('Linear Model of Focus vs Incorporability') + geom_hline(yintercept = 0) + xlab('Focus Percent')

frequency_table_trimmed %>% ggplot(aes(x = `Focus Rate`, y = `Incorporation Rate`)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  theme_minimal() + 
  ggtitle('Linear Model of Focus vs Incorporability') + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  xlab('Percent Focused') + 
  ylab('Percent Incorporated')

frequency_table_trimmed %>% ggplot(aes(x = `Focus Rate`, y = `Incorporation Rate`, colour = `Body Part`, shape = `Body Part`)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  theme_minimal() + 
  ggtitle('Linear Model of Focus vs Incorporability') + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  xlab('Percent Focused') + 
  ylab('Percent Incorporated')

############################################
#     TOPIC
############################################
topic_mdl <- lm(`Incorporation Rate` ~ `Topic Rate`, data = frequency_table_trimmed)
summary(topic_mdl)

frequency_table_trimmed %>% ggplot(aes(x = `Topic Rate`, y = `Incorporation Rate`)) + 
  geom_point(aes(col = `Body Part`, shape = `Body Part`)) + geom_smooth(method = 'lm') + theme_minimal() + ggtitle('Linear Model of Focus vs Incorporability') + geom_hline(yintercept = 0) + xlab('Focus Percent')

frequency_table_trimmed %>% ggplot(aes(x = `Topic Rate`, y = `Incorporation Rate`)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  theme_minimal() + 
  ggtitle('Linear Model of Topic vs Incorporability') + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  xlab('Percent Topical') + 
  ylab('Percent Incorporated')

############################################
#     NEWNESS
############################################

#deal w/ copulars
wubuy_data_cop <- mutate(wubuy_data_raw, Incorporated = ifelse(`Predicate Class` == 'cop', 'copula', Incorporated))

presentational <- filter(wubuy_data_cop, Presentational == 'yes')

presentational %>% filter(`First occurance` == 'yes') %>% filter(Incorporated == 'yes')
presentational %>% filter(`First occurance` == 'yes')

#remove presentational
wubuy_data_cop <- filter(wubuy_data_cop, Presentational != 'yes')

wubuy_data_cop %>% filter(`Noun Grouping` %in% frequency_table_trimmed$`Noun Group`) %>%
  ggplot(aes(fill = Incorporated, x = `First occurance`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal()

wubuy_data_cop %>% filter(`Noun Grouping` %in% frequency_table_trimmed$`Noun Group`) %>%
  filter(`Body Part` == 'body part') %>%
  ggplot(aes(fill = Incorporated, x = `First occurance`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal()

wubuy_data_cop %>% filter(`Noun Grouping` %in% frequency_table_trimmed$`Noun Group`) %>%
  filter(`Body Part` != 'body part') %>%
  ggplot(aes(fill = Incorporated, x = `First occurance`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal()

wubuy_non_body <- wubuy_data_cop %>% 
  filter(`Noun Grouping` %in% frequency_table_trimmed$`Noun Group`) %>%
  filter(`Body Part` != 'body part')

wubuy_body <- wubuy_data_cop %>% 
  filter(`Noun Grouping` %in% frequency_table_trimmed$`Noun Group`) %>%
  filter(`Body Part` == 'body part')

newness_data <- tribble(~Nouns, ~New, ~Count, ~`Count Incorporated`, ~`Count Free`, ~`Count Copula`)

for (set in c('yes', 'no')){
  
  newness_data <- add_row(newness_data,
                          Nouns = 'all',
                          New = set,
                          Count = count(filter(wubuy_data_cop, `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(wubuy_data_cop, `First occurance` == set), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(wubuy_data_cop, `First occurance` == set), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(wubuy_data_cop, `First occurance` == set), Incorporated == 'copula'))$n)
  
  newness_data <- add_row(newness_data,
                          Nouns = 'body',
                          New = set,
                          Count = count(filter(filter(wubuy_data_cop, `Body Part` == 'body part'), `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'copula'))$n)
  
  newness_data <- add_row(newness_data,
                          Nouns = 'generic',
                          New = set,
                          Count = count(filter(filter(wubuy_data_cop, `Body Part` != 'body part'), `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(filter(wubuy_data_cop, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'copula'))$n)
}
newness_data <- mutate(newness_data, `Percent Incorporated` = `Count Incorporated`/Count, `Percent Free` = `Count Free`/Count, `Percent Copula` = `Count Copula`/Count)

wubuy_data_cop_nposs <- filter(wubuy_data_cop, `Possession status` %!in% c('part/whole', 'possessor raising'))

newness_data_nposs <- tribble(~Nouns, ~New, ~Count, ~`Count Incorporated`, ~`Count Free`, ~`Count Copula`)
for (set in c('yes', 'no')){
  
  newness_data_nposs <- add_row(newness_data_nposs,
                          Nouns = 'all',
                          New = set,
                          Count = count(filter(wubuy_data_cop_nposs, `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), Incorporated == 'copula'))$n)
  
  newness_data_nposs <- add_row(newness_data_nposs,
                          Nouns = 'body',
                          New = set,
                          Count = count(filter(filter(wubuy_data_cop_nposs, `Body Part` == 'body part'), `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` == 'body part'), Incorporated == 'copula'))$n)
  
  newness_data_nposs <- add_row(newness_data_nposs,
                          Nouns = 'generic',
                          New = set,
                          Count = count(filter(filter(wubuy_data_cop_nposs, `Body Part` != 'body part'), `First occurance` == set))$n,
                          `Count Incorporated` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'yes'))$n,
                          `Count Free` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'no'))$n,
                          `Count Copula` = count(filter(filter(filter(wubuy_data_cop_nposs, `First occurance` == set), `Body Part` != 'body part'), Incorporated == 'copula'))$n)
}
newness_data_nposs <- mutate(newness_data_nposs, `Percent Incorporated` = `Count Incorporated`/Count, `Percent Free` = `Count Free`/Count, `Percent Copula` = `Count Copula`/Count)


# all nouns4
new_incor_percent <- (count(filter(filter(wubuy_data_cop, `First occurance` == 'yes'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_data_cop, `First occurance` == 'yes'))$n)
old_incorp_percent <- (count(filter(filter(wubuy_data_cop, `First occurance` == 'no'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_data_cop, `First occurance` == 'no'))$n)

# filter out low frequency nouns
new_incor_percent_trimmed <- (count(filter(filter(filter(wubuy_data, `Noun Grouping` %in% frequency_table_trimmed$`Noun Group`), `First occurance` == 'yes'), Incorporated == 'yes'))$n) / 
  (count(filter(filter(wubuy_data, `Noun Grouping` %in% frequency_table_trimmed$`Noun Group`), `First occurance` == 'yes'))$n)
old_incorp_percent_trimmed <- (count(filter(filter(filter(wubuy_data, `Noun Grouping` %in% frequency_table_trimmed$`Noun Group`), `First occurance` == 'no'), Incorporated == 'yes'))$n) / 
  (count(filter(filter(wubuy_data, `Noun Grouping` %in% frequency_table_trimmed$`Noun Group`), `First occurance` == 'no'))$n)


# all body parts
new_incor_percent_body <- (count(filter(filter(wubuy_body, `First occurance` == 'yes'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_body, `First occurance` == 'yes'))$n)
old_incorp_percent_body <- (count(filter(filter(wubuy_body, `First occurance` == 'no'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_body, `First occurance` == 'no'))$n)

# all non body parts
new_incor_percent_non_body <- (count(filter(filter(wubuy_non_body, `First occurance` == 'yes'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_non_body, `First occurance` == 'yes'))$n)
old_incorp_percent_non_body <- (count(filter(filter(wubuy_non_body, `First occurance` == 'no'), Incorporated == 'yes'))$n) / 
  (count(filter(wubuy_non_body, `First occurance` == 'no'))$n)

############################################
#     BODY PART VS NON BODY PART
############################################
frequency_table_trimmed %>% ggplot(aes(x = `Body Part`, y = `Incorporation Rate`, fill = `Body Part`)) + 
  geom_boxplot() + theme_minimal() +
  ylab('Percent Incorporated') + 
  ggtitle('Body Part Incorporation vs. Non Body Part Incorporation')

quantile(filter(frequency_table_trimmed, `Body Part` == 'body part')$`Incorporation Rate`)
range(filter(frequency_table_trimmed, `Body Part` == 'body part')$`Incorporation Rate`)
quantile(filter(frequency_table_trimmed, `Body Part` != 'body part')$`Incorporation Rate`)
range(filter(frequency_table_trimmed, `Body Part` != 'body part')$`Incorporation Rate`)

noun_type_mdl <- lm(`Incorporation Rate` ~ `Body Part`, data = frequency_table_trimmed)
summary(noun_type_mdl)

type_preds <- tibble(`Body Part` = unique(frequency_table_trimmed$`Body Part`))
type_preds$fit <- predict(noun_type_mdl, type_preds)
type_preds

##########################################################################
#         predicate class effect
##########################################################################

#count number of instances for each predicate
intr_pred_data <- tribble(~Predicate, ~`Predicate Class`, ~Count, ~Incorporated, ~`Body Part Count`, ~`Body Part Incorporated`, ~`Non Body Part Count`, ~`Non Body Part Incorporated`)

for (predicate in unique(wubuy_data$Predicate)){
  intr_pred_data <- add_row(intr_pred_data,
    Predicate = predicate,
    `Predicate Class` = unique(filter(filter(wubuy_data,Transativity != 'trans'), Predicate == predicate)$`Predicate Class`)[1],
    Count = count(filter(filter(wubuy_data,Transativity != 'trans'),Predicate == predicate))$n,
    Incorporated = count(filter(filter(filter(wubuy_data,Transativity != 'trans'),Incorporated == 'yes'),Predicate == predicate))$n,
    `Body Part Count` = count(filter(filter(filter(wubuy_data,Transativity != 'trans'),`Body Part` == 'body part'),Predicate == predicate))$n,
    `Body Part Incorporated` = count(filter(filter(filter(filter(wubuy_data,Transativity != 'trans'),`Body Part` == 'body part'),Incorporated == 'yes'),Predicate == predicate))$n,
    `Non Body Part Count` = count(filter(filter(filter(wubuy_data,Transativity != 'trans'),`Body Part` != 'body part'),Predicate == predicate))$n,
    `Non Body Part Incorporated` = count(filter(filter(filter(filter(wubuy_data,Transativity != 'trans'),`Body Part` != 'body part'),Incorporated == 'yes'),Predicate == predicate))$n
  )
}
intr_pred_data <- mutate(pred_data, `Percent Incorporated` = Incorporated/Count,
                    `Percent Incorporated Body` = `Body Part Incorporated`/`Body Part Count`,
                    `Percent Incorporated Non Body` = `Non Body Part Incorporated`/`Non Body Part Count`)

intr_pred_data <- filter(pred_data, Count >= 10)

pred_data <- tribble(~Predicate, ~`Predicate Class`, ~Count, ~Incorporated, ~`Body Part Count`, ~`Body Part Incorporated`, ~`Non Body Part Count`, ~`Non Body Part Incorporated`)

for (predicate in unique(wubuy_data$Predicate)){
  pred_data <- add_row(pred_data,
                       Predicate = predicate,
                       `Predicate Class` = unique(filter(wubuy_data, Predicate == predicate)$`Predicate Class`)[1],
                       Count = count(filter(wubuy_data,Predicate == predicate))$n,
                       Incorporated = count(filter(filter(wubuy_data,Incorporated == 'yes'),Predicate == predicate))$n,
                       `Body Part Count` = count(filter(filter(wubuy_data,`Body Part` == 'body part'),Predicate == predicate))$n,
                       `Body Part Incorporated` = count(filter(filter(filter(wubuy_data,`Body Part` == 'body part'),Incorporated == 'yes'),Predicate == predicate))$n,
                       `Non Body Part Count` = count(filter(filter(wubuy_data,`Body Part` != 'body part'),Predicate == predicate))$n,
                       `Non Body Part Incorporated` = count(filter(filter(filter(wubuy_data,`Body Part` != 'body part'),Incorporated == 'yes'),Predicate == predicate))$n
  )
}
pred_data <- mutate(pred_data, `Percent Incorporated` = Incorporated/Count,
                    `Percent Incorporated Body` = `Body Part Incorporated`/`Body Part Count`,
                    `Percent Incorporated Non Body` = `Non Body Part Incorporated`/`Non Body Part Count`)

#predicate linear model
pred_md <- lm(`Percent Incorporated` ~ `Predicate Class`, intr_pred_data)
summary(pred_md)

pred_md_body <- lm(`Percent Incorporated Body` ~ `Predicate Class`, intr_pred_data)
summary(pred_md_body)

pred_md_non_body <- lm(`Percent Incorporated Non Body` ~ `Predicate Class`, intr_pred_data)
summary(pred_md_non_body)

#all nouns
wubuy_data %>% filter(`Predicate Class` %in% c('act', 'adj', 'state')) %>%
  filter(Transativity != 'trans') %>%
  ggplot(aes(fill = Incorporated, x = `Predicate Class`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  ggtitle('Relative Incorporation Rates Of Predicate Classes') + 
  geom_text(stat='count', aes(label=..count..),position = 'fill', vjust = 1) + 
  ylab('Percent')
#count total values
class_count <- wubuy_data %>% filter(Transativity != 'trans') %>% count(`Predicate Class`) # get count of predicate classes
class_count <- transmute(class_count, `Predicate Class` = `Predicate Class`, Total = n)
incorp_count <- wubuy_data %>% filter(Transativity != 'trans') %>% filter(Incorporated == 'yes') %>% count(`Predicate Class`)
incorp_count <- transmute(incorp_count, `Predicate Class` = `Predicate Class`, Incorporated = n)


#join counts and calculate percentage
vclass_count <- left_join(class_count, incorp_count)
vclass_count <- mutate(vclass_count, Free = Total - Incorporated)
vclass_count <- mutate(vclass_count, `Percent Incorporated` = (Incorporated/Total))
vclass_count

#body parts
wubuy_data %>% filter(`Predicate Class` %in% c('act', 'adj', 'state')) %>%
  filter(Transativity != 'trans') %>%
  filter(`Body Part` == 'body part') %>%
  ggplot(aes(fill = Incorporated, x = `Predicate Class`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  ggtitle('Relative Incorporation Rates Of Predicate Classes For Body Parts') + 
  geom_text(stat='count', aes(label=..count..),position = 'fill', vjust = 1) + 
  ylab('Percent')
class_count_body <- wubuy_data %>% filter(`Body Part` == 'body part') %>% filter(Transativity != 'trans') %>% count(`Predicate Class`) # get count of predicate classes
class_count_body <- transmute(class_count_body, `Predicate Class` = `Predicate Class`, Total = n)
incorp_count_body <- wubuy_data %>% filter(`Body Part` == 'body part') %>% filter(Transativity != 'trans') %>% filter(Incorporated == 'yes') %>% count(`Predicate Class`)
incorp_count_body <- transmute(incorp_count_body, `Predicate Class` = `Predicate Class`, Incorporated = n)


#join counts and calculate percentage
vclass_count_body <- left_join(class_count_body, incorp_count_body)
vclass_count_body <- mutate(vclass_count_body, Free = Total - Incorporated)
vclass_count_body <- mutate(vclass_count_body, `Percent Incorporated` = (Incorporated/Total))
vclass_count_body



#non body parts
wubuy_data %>% filter(`Predicate Class` %in% c('act', 'adj', 'state')) %>%
  filter(Transativity != 'trans') %>%
  filter(`Body Part` != 'body part') %>%
  ggplot(aes(fill = Incorporated, x = `Predicate Class`)) + 
  geom_bar(position = 'fill') + 
  theme_minimal() + 
  ggtitle('Relative Incorporation Rates Of Predicate Classes For Non Body Parts') + 
  geom_text(stat='count', aes(label=..count..),position = 'fill', vjust = 1) + 
  ylab('Percent')
class_count_nonbody <- wubuy_data %>% filter(`Body Part` != 'body part') %>% filter(Transativity != 'trans') %>% count(`Predicate Class`) # get count of predicate classes
class_count_nonbody <- transmute(class_count_nonbody, `Predicate Class` = `Predicate Class`, Total = n)
incorp_count_nonbody <- wubuy_data %>% filter(`Body Part` != 'body part') %>% filter(Transativity != 'trans') %>% filter(Incorporated == 'yes') %>% count(`Predicate Class`)
incorp_count_nonbody <- transmute(incorp_count_nonbody, `Predicate Class` = `Predicate Class`, Incorporated = n)


#join counts and calculate percentage
vclass_count_nonbody <- left_join(class_count_nonbody, incorp_count_nonbody)
vclass_count_nonbody <- mutate(vclass_count_nonbody, Free = Total - Incorporated)
vclass_count_nonbody <- mutate(vclass_count_nonbody, `Percent Incorporated` = (Incorporated/Total))
vclass_count_nonbody

##########################################################################
#         ARGUMENT FUNCTION
##########################################################################

wubuy_data %>% 
  mutate(`argument function` = ifelse(`argument function` %in% c('A', 'O', 'OBL', 'S'), `argument function`, 'Marked OBL'))%>% 
  ggplot(aes(fill = Incorporated, x = `argument function`)) + 
  geom_bar() + 
  theme_minimal()

# w/ body parts only
wubuy_data %>% filter(`Body Part` == 'body part') %>%
  mutate(`argument function` = ifelse(`argument function` %in% c('A', 'O', 'OBL', 'S'), `argument function`, 'Marked OBL'))%>% 
  ggplot(aes(fill = Incorporated, x = `argument function`)) + 
  geom_bar() + 
  theme_minimal()

# w/o body parts
wubuy_data %>% filter(`Body Part` != 'body part') %>%
  mutate(`argument function` = ifelse(`argument function` %in% c('A', 'O', 'OBL', 'S'), `argument function`, 'Marked OBL'))%>% 
  ggplot(aes(fill = Incorporated, x = `argument function`)) + 
  geom_bar() + 
  theme_minimal()

wubuy_data %>% count(`argument function`) %>% print(n = Inf)

#group argument functions together
wubuy_data_funct_filter <- wubuy_data %>% mutate(`Argument Function` = ifelse(`argument function` %in% c('A', 'O', 'OBL', 'S'), `argument function`, 'Marked OBL'))

arg_funct <- tribble(~`Argument Function`, ~Count, ~Incorporated)

for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double')
  arg_funct <- add_row(arg_funct,
                       `Argument Function` = arg,
                       Count = count(temp)$n,
                       Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct <- mutate(arg_funct, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct

#& `Possession status` != 'part/whole')

arg_funct_bp <- tribble(~`Argument Function`, ~Count, ~Incorporated)
for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double') , `Body Part` == 'body part')
  arg_funct_bp <- add_row(arg_funct_bp,
                       `Argument Function` = arg,
                       Count = count(temp)$n,
                       Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct_bp <- mutate(arg_funct_bp, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct_bp



arg_funct_nbp <- tribble(~`Argument Function`, ~Count, ~Incorporated)
for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double'), `Body Part` != 'body part')
  arg_funct_nbp <- add_row(arg_funct_nbp,
                          `Argument Function` = arg,
                          Count = count(temp)$n,
                          Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct_nbp <- mutate(arg_funct_nbp, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct_nbp

## WITHOUT PART/WHOLE and POSS RAISISNG

arg_funct_nposs <- tribble(~`Argument Function`, ~Count, ~Incorporated)
for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double' & `Possession status` %!in% c('possessor raising','part/whole'))
  arg_funct_nposs <- add_row(arg_funct_nposs,
                       `Argument Function` = arg,
                       Count = count(temp)$n,
                       Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct_nposs <- mutate(arg_funct_nposs, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct_nposs

arg_funct_nposs_bp <- tribble(~`Argument Function`, ~Count, ~Incorporated)
for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double'  & `Possession status` %!in% c('possessor raising','part/whole')) , `Body Part` == 'body part')
  arg_funct_nposs_bp <- add_row(arg_funct_nposs_bp,
                          `Argument Function` = arg,
                          Count = count(temp)$n,
                          Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct_nposs_bp <- mutate(arg_funct_nposs_bp, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct_nposs_bp



arg_funct_nposs_nbp <- tribble(~`Argument Function`, ~Count, ~Incorporated)
for(arg in c('A', 'O', 'OBL', 'S', 'Marked OBL')){
  temp <- filter(filter(wubuy_data_funct_filter, `Argument Function` == arg & `External Mod` != 'double'  & `Possession status` %!in% c('possessor raising','part/whole')), `Body Part` != 'body part')
  arg_funct_nposs_nbp <- add_row(arg_funct_nposs_nbp,
                           `Argument Function` = arg,
                           Count = count(temp)$n,
                           Incorporated = count(filter(temp, Incorporated == 'yes'), `argument function` = arg)$n)
}
arg_funct_nposs_nbp <- mutate(arg_funct_nposs_nbp, Free = Count - Incorporated, `Percent Incorporated` = Incorporated/Count)
arg_funct_nposs_nbp


#############################################################################
#             POSESSION
#############################################################################

connected_mdl <- lm(`Incorporation Rate` ~ `Connected Percent`, data = filter(frequency_table_trimmed, `Body Part` == 'body part'))
summary(connected_mdl)

possessed_mdl <- lm(`Incorporation Rate` ~`Possessed Percent`, data = frequency_table_trimmed)
summary(possessed_mdl)

frequency_table_trimmed %>%
  ggplot(aes(x = `Possessed Percent`, y = `Incorporation Rate`)) + 
  geom_point() + geom_smooth(method = lm) + theme_minimal()

possessed_bp_mdl <- lm(`Incorporation Rate` ~ `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` == 'body part'))
summary(possessed_bp_mdl)

possessed_nbp_mdl <- lm(`Incorporation Rate` ~ `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` != 'body part'))
summary(possessed_nbp_mdl)

frequency_table_trimmed %>%
  ggplot(aes(x = `Possessed Percent`, y = `Incorporation Rate`, color = `Body Part`, shape = `Body Part`)) + 
  geom_point() + geom_smooth(method = lm) + theme_minimal() + geom_hline(yintercept = 0)

# exclude voice from non body parts
possessed_nbpnv_mdl <- lm(`Incorporation Rate` ~ `Possessed Percent`, data = filter(filter(frequency_table_trimmed, `Noun Group` != 'voice'), `Body Part` != 'body part'))
summary(possessed_nbp_mdl)

frequency_table_trimmed %>% filter(`Noun Group` != 'voice') %>%
  ggplot(aes(x = `Possessed Percent`, y = `Incorporation Rate`, color = `Body Part`)) + 
  geom_point() + geom_smooth(method = lm) + theme_minimal()

############################################################################################
#       Mixed Linear Effect Model
############################################################################################

multible_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` * `Possessed Percent`, data = frequency_table_trimmed)
summary(multible_mdl)

multible_bod_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` * `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` == 'body part'))
summary(multible_bod_mdl)

multible_nonbod_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` * `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` != 'body part'))
summary(multible_nonbod_mdl)

multible_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` + `Possessed Percent`, data = frequency_table_trimmed)
summary(multible_mdl)

multible_bod_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` + `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` == 'body part'))
summary(multible_bod_mdl)

multible_nonbod_mdl <- lm(`Incorporation Rate` ~ `Focus Rate` + `Possessed Percent`, data = filter(frequency_table_trimmed, `Body Part` != 'body part'))
summary(multible_nonbod_mdl)

###########################################################################################
#       CONDITIONAL INFERENCE TREE MODEL
###########################################################################################

#convert to factors
wubuy_factor <- mutate(wubuy_data, 
                       Incorporated = factor(Incorporated), 
                       `Body Part` = factor(`Body Part`) , 
                       `Focus/Topic` = factor(`Focus/Topic`), 
                       `argument function` = factor(ifelse(`argument function` %in% c('A', 'O', 'OBL', 'S'), `argument function`, 'OBL')),
                       Possessed = factor(ifelse(`Possession status` %in% c('X-REL Y', 'part/whole', 'possessor raising'), 'yes', 'no')),
                       `Possessor Raised` = factor(ifelse(`Possession status` %in% c('part/whole', 'possessor raising'), 'yes', 'no')),
                       Possession = factor(ifelse(`Possession status` %in% c('X-REL Y', 'part/whole', 'possessor raising'), `Possession status`, 'no')),
                       `First occurance` = factor(`First occurance`),
                       `<wara> in verb` = factor(`<wara> in verb`),
                       `Predicate Class` = factor(`Predicate Class`),
                       `First occurance` = factor(`First occurance`),
                       `determiner co-occurance` = factor(`determiner co-occurance`),
                       Presentational = factor(Presentational))

#all variables considered
output.tree <- ctree(Incorporated ~ `Body Part` + `Focus/Topic` + `argument function` + Possession + `argument function` + `First occurance` + Presentational + `Predicate Class`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Body Part` + `Focus/Topic` + `argument function` + `Possessor Raised` + `argument function` + `First occurance` + Presentational, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Body Part` + `Focus/Topic` + `argument function` + Possessed + `argument function` + `First occurance` + Presentational, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Body Part` + `First occurance` + `argument function` + Possessed + Presentational, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Body Part` + `Focus/Topic` + `argument function`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~  `Focus/Topic` + Possessed + `Predicate Class`, data = filter(wubuy_factor, `External Mod` != 'double'))
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Focus/Topic` + `First occurance` + Presentational + `Possessor Raised`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `Focus/Topic` + `argument function` + Possessed + `Body Part`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `argument function` + `First occurance` + Possessed + `Body Part`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(Incorporated ~ `argument function` + `Predicate Class` + Possessed + `Body Part`, data = filter(wubuy_factor, `Possession status` %!in% c('possessor raising', 'part/whole')))
plot(output.tree)

output.tree <- ctree(Incorporated ~ `argument function` + `Predicate Class` + Possessed + `Body Part`, data = wubuy_factor)
plot(output.tree)

output.tree <- ctree(`argument function` ~ `Predicate Class` + Possessed + `Body Part` + `Focus/Topic`, data = filter(wubuy_factor, `External Mod` == 'double'))
plot(output.tree)









