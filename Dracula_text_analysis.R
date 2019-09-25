
# DRACULA - text analysis and prep for tableau iron viz -------------------

#The gutenbergr package provides access to the public domain works 
##from the Project Gutenberg collection.

library(gutenbergr)
library(dplyr)
library(tidytext)
library(stringr)

gutenberg_works()

dracula <- gutenberg_download(345)
dracula

#--TIDYING TEXT

# dracula_tidy  <- dracula %>%
#   unnest_tokens(word, text) %>%
#   anti_join(stop_words)
# 
# dracula_tidy
# 
# dracula_tidy %>%
#   count(word, sort = TRUE)
# 
# library(ggplot2)
# 
# dracula_tidy %>%
#   count(word, sort = TRUE) %>%
#   filter(n > 100) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()


#----SENTIMENT----

#add line number and chapter, and unnest to one word per row
dracula_full <- dracula %>%
  mutate(linenumber = row_number(),
        chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)


#remove stop words from main text (excludes contents pages - first 27 chapters)
dracula_tidy  <- dracula_full %>%
  filter(chapter>27) %>%
  anti_join(stop_words)

#correct chapter number
dracula_tidy  <- dracula_tidy %>% 
  mutate('chapter' = chapter - 27L) 

#check word count
dracula_tidy %>%
  count(word, sort=TRUE) %>%
filter(word == 'death')

#change column names for sentiment dictionaries
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
nrc

bing <- bing %>% 
  rename(sentiment_bing = sentiment)
 
afinn <- afinn  %>% 
  rename(score_afinn = score)

nrc <- nrc %>% 
  rename(sentiment_nrc = sentiment)

#join in sentiments - just bing and afinn
dracula_sentiment <- dracula_tidy %>%
  left_join(bing) %>%
  left_join(afinn) 

#check word count
dracula_sentiment  %>%
count(word, sort=TRUE) %>%
  filter(word == 'death')


summary(dracula_sentiment)




# TESING ADDING CHAPTER TITLES ------------------------------------------------------------------

#check chapter titles - think how best to add these back in to main data as another field

#filter only chapter titles from the contents page
dracula_chapter_titles <- dracula_full  %>%
  filter( chapter !=0 &
            chapter < 28 &
            word != 'chapter' &
            word !=  'page')  %>%
  group_by(chapter)

#concatenate words back into chapter title
dracula_chapter_titles <- dracula_chapter_titles %>%
  summarise_all(paste, collapse = " ")

#remove unneeded columnes and rename
dracula_chapter_titles <- dracula_chapter_titles %>%
  select(chapter,word) %>%
  rename(chapter_title = word)


##regez here to remove numbers and roman numerals

#replace numbers with "" - want to parse though?
dracula_chapter_titles$chapter_title <-
gsub('[[:digit:]]+','',dracula_chapter_titles$chapter_title)  


#str_replace_all(dracula_chapter_titles$chapter_title,'\\d','')


#just have narrator name as new column

dracula_chapter_titles <- dracula_chapter_titles %>%
  mutate(narrator = case_when(
      grepl("jonathan",chapter_title) ~ 'Jonathan Harker' ,
      grepl("lucy and mina",chapter_title)  ~ 'Lucy & Mina',
      grepl("mina murray",chapter_title)  ~ 'Mina Murray/Harker',
      grepl("mina harker",chapter_title)  ~ 'Mina Murray/Harker',
      grepl("seward",chapter_title)  ~ 'Dr. Seward',
      grepl("lucy westenra",chapter_title)  ~ 'Lucy Westenra',
      grepl("dailygraph",chapter_title)  ~ 'Dailygraph cutting'
    )
  )
# ADD CHAPTER SUMMARIES -------------------------------------------------------

#filter just chapter numbers
dracula_chapter_titles$chapter


chapter_summary <- 
  c("Harker arrives in Transylvania to meet Dracula and assist him as a lawyer.",
    "Harker realizes he is Dracula's prisoner.",
    "Harker realizes he is Dracula's prisoner.",
    "Harker escapes from Dracula's castle.",
    "Lucy accepts Holmwood's proposal. Dejected after Lucy rejects him, Seward focuses on diagnosing Renfield.",
    "Mina and Lucy vacation in Whitby.",
    "Dracula arrives in Whitby and begins his attacks on Lucy.",
    "Chapter 8",
    "Chapter 9",
    "Back in London, Van Helsing and Seward treat Lucy's illness, but because of mistakes, she grows worse.",
    "Chapter 11",
    "Mina brings Harker, shaken and fragile, home to England.",
    "Lucy succumbs to Dracula's curse.",
    "Chapter 14",
    "Chapter 15",
    "Van Helsing and the young men destroy Un-Dead Lucy.",
    "Mina begins to organize notes at the asylum.",
    "Chapter 18",
    "Dracula begins to attack Mina.",
    "Renfield warns the others, who are busy locating Dracula's lairs.",
    "Dracula kills Renfield and flees England.",
    "Chapter 22",
    "Mina resists the curse and helps the men plot the pursuit. Van Helsing leads the team to Castle Dracula to kill the vampires.",
    "Chapter 24",
    "Chapter 25",
    "Chapter 26",
    "The men destroy Dracula and release Mina from the curse."
    )


#create chapter numbers as vector
chapter <- as.vector(dracula_chapter_titles$chapter)

#bind chapter number and summary together, then convert to tibble
chapter_summary_table <- cbind(chapter,chapter_summary)
chapter_summary_table <- as_tibble(chapter_summary_table)

#change chapter to integer (from chr), so can join to chapter titles
chapter_summary_table <- chapter_summary_table %>%
  mutate(chapter = as.integer(chapter))


dracula_chapter_titles <- dracula_chapter_titles%>% 
  left_join(chapter_summary_table)


-----####

#join chapter narrator back to dataset - by chapter number

dracula_sentiment  <-dracula_sentiment %>%
  left_join(dracula_chapter_titles)

dracula_sentiment %>% count(narrator)




# OUTPUT ------------------------------------------------------------------
  
#write to csv
write.csv(dracula_sentiment,file = "/Users/Jonathon.Lines/Desktop/dracula_text.csv")








