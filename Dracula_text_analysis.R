# DRACULA - text analysis and prep for tableau iron viz -------------------

#The gutenbergr package provides access to the public domain works 
##from the Project Gutenberg collection.

library(gutenbergr)
library(dplyr)
library(tidytext)
library(stringr)
library(textdata)

gutenberg_works()

dracula <- gutenberg_download(345)
dracula

#----SENTIMENT----

#add line number and chapter, and unnest to one word per row
dracula_full <- dracula %>%
  mutate(linenumber = row_number(),
        chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#keep table with whole line per row
dracula_full_lines <- dracula  %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                 ignore_case = TRUE))))  %>%
  select(linenumber,text)

summary(dracula_full_lines)

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

# ADDING CHAPTER TITLES ------------------------------------------------------------------

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


#replace numbers with "" 
dracula_chapter_titles$chapter_title <-
gsub('[[:digit:]]+','',dracula_chapter_titles$chapter_title)  

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

#create chatper summaries
chapter_summary <- 
  c("Jonathan Harker arrives in Transylvania to meet Dracula and assist him as a lawyer.",
    "Jonathan realizes he is Dracula's prisoner.",
    "Jonathan is visited by three beautiful women with red lips and sharp teeth, but Dracula intervenes.",
    "Jonathan escapes from Dracula's castle.",
    "Lucy accepts Holmwood's proposal. Dejected after Lucy rejects him, Seward focuses on diagnosing Renfield.",
    "Mina and Lucy vacation in Whitby.",
    "Dracula arrives in Whitby.",
    "Mina witnesses Dracula attack Lucy.",
    "Mina writes to Lucy from Budapest, where Jonathan Harker is ill with a brain fever.",
    "Back in London, Van Helsing and Seward treat Lucy's illness, but because of mistakes, she grows worse.",
    "Van Helsing and Dr. Seward return to Lucy and find her close to death.",
    "Mina brings Jonathan, shaken and fragile, home to England.",
    "Lucy succumbs to Dracula's curse.",
    "Mina transcribes Jonathan's diary, and gives it to Van Helsing.",
    "Seward and Van Helsing visit a wounded child with bite marks on their neck. Lucy is now Un-Dead.",
    "Van Helsing and the young men destroy Un-Dead Lucy.",
    "Mina begins to organize notes at the asylum.",
    "Mina visit Renfield. Van Helsing shares the legend of nosferatu, or Un-Dead.",
    "Dracula begins to attack Mina.",
    "Renfield warns the others, who are busy locating Dracula's lairs.",
    "Dracula kills Renfield and flees England.",
    "Jonathan recounts the end of Renfield's story.",
    "Mina resists the curse and helps the men plot the pursuit.",
    "Van Helsing discovers Dracula has boarded a ship to Varna, a Russian port.",
    "Mina asks the group to pledge they will destroy her if she turns into a vampire.",
    "The group travel to Dracula's castle.",
    "The men destroy Dracula and release Mina from the curse."
    )


#change chapter numbers as vector
chapter <- as.vector(dracula_chapter_titles$chapter)

#bind chapter number and summary together, then convert to tibble
chapter_summary_table <- cbind(chapter,chapter_summary)
chapter_summary_table <- as_tibble(chapter_summary_table)

#change chapter to integer (from chr), so can join to chapter titles
chapter_summary_table <- chapter_summary_table %>%
  mutate(chapter = as.integer(chapter))

dracula_chapter_titles <- dracula_chapter_titles%>% 
  left_join(chapter_summary_table)


#join chapter narrator back to dataset - by chapter number

dracula_final  <-dracula_sentiment %>%
  left_join(dracula_chapter_titles) %>%
  left_join(dracula_full_lines)

dracula_final %>% count(narrator)
dracula_final %>% count(chapter_summary)

summary(dracula_final)


# OUTPUT ------------------------------------------------------------------
  
#write to csv
write.csv(dracula_final,file = "xxxx")









