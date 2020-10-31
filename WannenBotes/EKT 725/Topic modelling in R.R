#TOPIC MODELLING IN R#
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gutenbergr)
library(stringr)
library(scales)
library(rJava)
library(mallet)



#### Step 1: Importing the data ####

data("AssociatedPress") #The data originates from the 'topicmodel' package.
AssociatedPress #The data consists of 2246 news articles (documents) from an American news agency. 
                #There are a total of 10 437 terms in the corpus.

#### Step 2: FITTING AN LDA MODEL TO THE DATA ####
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234)) # set a seed so that the output of the model is predictable
ap_lda

#### A little bit of information on the LDA model ####
#What the LDA model does, is...

#### Step 3: Extracting the per-topic per-word probabilites ####
ap_topics <- tidy(ap_lda, matrix = "beta") #Obtaining the probabilities that a term belongs to one of the $k$ topics.
ap_topics

ap_top_terms <- ap_topics %>% #Determining the top 10 terms in each topic
  group_by(topic) %>% 
  top_n(10, beta) %>% #This gives us the top 10 words and the probability that they are assigned to topic 1 or 2.
  ungroup() %>%
  arrange(topic, -beta) #Arranging the top 10 words per topic, by their topic.

ap_top_terms %>% #Plotting a visual representation of the terms, and their $\beta$ (probability) of appearing in a topic.
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#We may deduce that the first topic is that of business and fincance, and that the second topic is that of politics.
#We also see that the words 'people' and 'new' are highly probable to appear in either of the two topics. 
#This demonstrates the soft clustering nature of LDA.

####Step 4: Considering the greatest differences in probabilites ($\betas$) between topic 1 and 2.####

beta_spread <- ap_topics %>% 
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread
#By considering the log ratio of the greatest difference between probabilities give us further insight into the nature of the 
#two topics identified by the LDA model.


####Step 5: Extracting the per-document per-topic information####
ap_documents <- tidy(ap_lda, matrix = "gamma") #Gamma is the proportion of the terms in a document that come from topic 1
ap_documents

#We see from this, that document 6 consists almost entirely of words from topic 2, and that document 9 consists almost 
#entirely of words from topic 1.

tidy(AssociatedPress) %>% #Let's take a look at document 6 to see if it does speak about business (or, if the words therein refer to business)
  filter(document == 6) %>%
  arrange(desc(count))
#By looking at the most common words in document 6, it can be confirmed that this is a topic 2 (political) document.

#Recall that $\betas$ may be interpreted as the probability of a word coming from topic k. Thus,
#if a document consists of words that have very high $\betas$ for topic k, then the $\gamma$ for topic k will in turn be high!


####_____________________________________________________________________________________________________________________________####


####The great library heist - another example####

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")  #The titles of the books we would like to investigate.

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")




# divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

####Casting our word_counts df into a DocumentTermMatrix####

#We need to do this in order to have the topicmodel package use these chapters to perform topic modelling.

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm #Now our data is in a form to be analysed by an LDA model

####Applying an LDA on the four books data####

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

####Investigating per-topic per-word probabilities####

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>% #Finding the top 10 probability terms for each of the four topics.
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

library(ggplot2)

top_terms %>% #A visualisation of the top terms per topic
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

####Investigating per-document per-topic probabilities####

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

#Splitting the book title and chapter number, and conisdering the $\gamma$ of each.
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma
  
# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

####Brief interpretation of results####
#Above, we generate a visual representation of the gammas for all the book chapters. For pride and prejudice, nearly all the 
#chapters that truly are from pride and prejudice, have a gamma of 1. There are no misclassifications here. We also see no
#misclassifications for the chapters (documents) to the books (topics) of 'The War of the Worlds' and 'Twenty Thousand 
#Leagues under the Sea.
#We however see some outlying misclassified documents from other books, incorrectly classified as originating from the topic
#(book) 'Great Expectations'.

####Gaining a better idea of the miss-classifications####

chapter_classifications <- chapters_gamma %>% #We are looking at the top gammas per-topic per-document. 
  group_by(title, chapter) %>%                #That is, we are looking at the documents that belong to a topic with the highest
  top_n(1, gamma) %>%                         #probability ($\gamma$)
  ungroup()

chapter_classifications #We can see from here that chapter 23 from 'Great Expectations' (topic 4) was computed to belong to
                        #belong to 'Pride and Prejudice' (topic 1) with probability 0.547.

#Determing which documents (chapters) were most often misclassified.
#Verstaan nog nie 100% hoe die gedoen word nie - lees weer mooi.
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

#We see that only two of the chapters from 'Great Expectations' (topic 4) were misclassified! The rest of the documents were
#all correctly classified.

####___________________________________________________________________________________________________________________________####

####The concept of by-word assignment ####

#It is often of interest to see which words in each document were assigned to which topic. This can be explored by the 'augment'
#function, as illustrated in the code below:

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

#Visualisin the LDA model performance in the form of a confusion (or matching) matrix. We can see from this matrix that 
#some words (term) that came from Great Expectations were incorrectly classified as coming from Pride and Prejudice and 
#The War od the Worlds. This confirms our previous findings when considering the model misclassifications.

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Book words were assigned to",
    y = "Book words came from",
    fill = "% of assignments"
  )

#Here we are considering which words were misclassified. 

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

#The below code shows us which words were most frequently misclassified. 
wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

#The word love is very common to the book 'Pride and Prejudice'. When the word love appeared in Great Expectations, it was 
#incorrectly classified as having originated from the topic of 'Pride and Prejudice'.
#That is, the word love has a high beta for the topic of 'Pride and Prejudice'. Chapters in 'Great Expectations' that contain
#many words which are more common (have higher betas) for 'Pride and Prejudice', may have a higher gamma value for 'Pride and
#Prejudice' than for the correct 'Great Expectations'.

#Certain terms span multiple documents, regardless of their topic. While the word love appears the most in 'Pride and Prejudice',
#it is by no means unique to 'Pride and Prejudice'.

#_______________________________________________________________________________________________________________________________#
####Applying LDA from the Mallet package####

# create a vector with one string per chapter
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)
