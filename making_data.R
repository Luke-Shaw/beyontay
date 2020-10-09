################################################################################
# How the files stored in /data were made
################################################################################

# packages
library(tidyverse)
library(hunspell)

# #TidyTuesday data used 
beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')
taytay_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/taylor_swift_lyrics.csv')

# Making the Taylor Swift lyrics one lyric line per row (like the Beyonce ones)
clean_taytay <- taytay_lyrics %>% 
  separate_rows(Lyrics, sep = "\n") %>% 
  select(line = Lyrics, song_name = Title, artist_name = Artist) %>% 
  group_by(song_name) %>% 
  mutate(song_line = row_number())

# Put the data sets into one
line_per_row <- beyonce_lyrics %>% 
  select(artist_name, song_name, line, song_line) %>% 
  bind_rows(clean_taytay)

# let's go even further, one word per line! Maybe this isn't sensible? 
word_per_row <- line_per_row %>% 
  separate_rows(line, sep = " ") %>% 
  filter(line != "") %>%
  rename(word = line) %>% 
  group_by(song_name, song_line) %>% 
  mutate(word_in_line = row_number()) %>% 
  ungroup()

# only the words from the end of each line (because we want to rhyme the lines)
line_ending_words <- word_per_row %>% 
  filter(word != "") %>% 
  group_by(artist_name, song_name, song_line) %>% 
  slice_tail(n = 1)

# The apostrophe is the only non-alphanumeric character we do want to 
# keep, as I've != Ive but we want to get rid of commas, brackets, etc.
# Hence the column word_no_punct_but_apostrophe
line_ending_words <- line_ending_words %>% 
  mutate(word_no_punct_but_apostrophe = str_replace_all(word, "[^[:alnum:]|']", "")) 

to_transcribe <- unique(line_ending_words$word_no_punct_but_apostrophe)

# when using https://www.phonetizer.com/ui# it turns out the 1744 element caused an error
# Line below commented out to avoid the big output. The "::" between words is to separate 
# them effectively.
# paste0(to_transcribe[-1744], collapse = " :: ")

# tried using datapasta but the copy clipboard didn't work into R, so copied and pasted into
# LibreOffice Writer then saved as a .txt file with UTF-8 encoding
ipa_output <- readLines(con = here::here("data/IPA_last_words.txt"), encoding = "UTF-8")

# Make a tibble with the lookup. Manually found that 1744 was a problem as it's 'reír' that caused
# an IPA problem, and 74 is blank so didn't get translated. Separated by "()" as  that's what 
# phonetizer allocated as a space between words
word_to_ipa_tbl <- tibble(
  word = to_transcribe[-c(1744,74)],
  ipa = unlist(strsplit(ipa_output, split=" \\(\\) "))
)
# join onto our line enders
line_ending_words <- line_ending_words %>% 
  left_join(word_to_ipa_tbl, by = "word")

# The 2 datasets used in creating BeyonTay verses are the following, which are created
# in this script (except for the phonetic alphabet part which had some manual steps explained above)
write_csv(line_ending_words, here::here("data/line_ending_words.csv"))
write_csv(line_per_row, here::here("data/line_per_row.csv"))
