################################################################################
# Functions and Packages for creating a BeyonTay verse
################################################################################
# packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ggtext")) install.packages("ggtext")

get_rhyming_lyrics <- function(ipa_to_rhyme, 
                               line_ending_words, 
                               line_per_row){
  # Find 2 lyrics, one Beyonce one Taylor, that rhyme. ipa_to_rhyme is a word in
  # international phonetic alphabet that exists within line_ending_words.
  # Rhyming is done by guessing that if the last 3 IPA characters match then it's
  # a valid rhyme, else the whole word if either word is less than three IPA 
  # characters
  
  # first work out the final (up to) 3 characters of ipa_to_rhyme
  n = nchar(ipa_to_rhyme)
  last_chars_of_ipa_to_rhyme = ifelse(n - 2 > 0,
                                      substr(ipa_to_rhyme, n-2, n),
                                      ipa_to_rhyme)
  m = nchar(last_chars_of_ipa_to_rhyme)
  
  rhymes <- line_ending_words %>%
    # was doing case_when for last_chars but substr default behaviour is nice
    # so isn't an issue ie substr("a",-1,1) = "a"
    mutate(last_chars = substr(ipa, nchar(ipa)-m+1,nchar(ipa))) %>%
    filter(last_chars == last_chars_of_ipa_to_rhyme)
  
  unique_rhymes <- rhymes %>% group_by(word) %>% summarise(.groups="drop")
  
  rhyming_lyrics <- line_per_row %>% 
    right_join(rhymes,
               by = c("artist_name", "song_name","song_line")) 
  
  return(rhyming_lyrics)
}

get_random_lyric_pair <- function(line_ending_words,
                                  line_per_row,
                                  keep_going = FALSE){
  # get a random lyric pair one from Beyonce one from Taylor. Method is 
  # to randomly pick a lyric, and see if can get a rhyme from other artist
  # using the fn get_rhyming_lyrics. If not, either print an error or 
  # try again, depending on the logical variable keep_going
  
  # choose the first line
  row <- round(runif(1,1,nrow(line_ending_words)))
  chosen_line <- line_ending_words[row,]
  chosen_lyric <- line_per_row %>% filter(artist_name == chosen_line$artist_name, 
                                          song_name == chosen_line$song_name,
                                          song_line == chosen_line$song_line)
  
  rhyming_lyrics <- get_rhyming_lyrics(ipa_to_rhyme = chosen_line$ipa,
                                       line_ending_words,
                                       line_per_row)
  
  # filter out where words sound the same and by same artist
  rhyming_options <- rhyming_lyrics %>% 
    filter(artist_name != chosen_line$artist_name) %>%
    filter(ipa != chosen_line$ipa)
  
  if(nrow(rhyming_options)==0){
    if(keep_going){
      get_random_lyric_pair(line_ending_words, line_per_row, keep_going)
    } else { print(paste0("couldn't find rhyming pair for '",chosen_line$word, "' in ",
                          chosen_line$artist_name,"'s lyric line: ",
                          chosen_lyric$line)) 
    }
  } else {
    row2 <- round(runif(1,1,nrow(rhyming_options)))
    chosen_rhyme_line <- rhyming_options[row2,]
    lyric_pair <- chosen_lyric %>% 
      bind_rows(tibble(artist_name = chosen_rhyme_line$artist_name,
                       song_name = chosen_rhyme_line$song_name,
                       song_line = chosen_rhyme_line$song_line,
                       line = chosen_rhyme_line$line))
    
    return(lyric_pair)
  }
}

get_verse <- function(seed = "ignore",
                      line_limit = 40){
  # get 2 BeyonTay rhyming couplets. 
  # line_limit is the max nchar per line, default 40 for easy plotting 
  if(seed!="ignore"){set.seed(seed)}
  
  # use while to check within line_limit
  first_couplet <- get_random_lyric_pair(line_ending_words, 
                                         line_per_row, 
                                         keep_going = TRUE)
  while(max(nchar(first_couplet$line)) > line_limit){
    first_couplet <- get_random_lyric_pair(line_ending_words, 
                                           line_per_row, 
                                           keep_going = TRUE)
  }
  
  secnd_couplet <- get_random_lyric_pair(line_ending_words,
                                         line_per_row, 
                                         keep_going = TRUE)
  while(max(nchar(secnd_couplet$line)) > line_limit){
    secnd_couplet <- get_random_lyric_pair(line_ending_words, 
                                           line_per_row, 
                                           keep_going = TRUE)
  }
  
  verse <- bind_rows(first_couplet, secnd_couplet)
  
  return(verse)
}

plot_verse <- function(verse){
  # ggplot for a verse output
  the_plot <- ggplot(verse, aes(x=c(1,2,3), y=c(4,3,2,1), colour = artist_name)) +
    geom_text(aes(x=2, label=line,hjust=0,fontface=2), size = 5) +
    scale_color_manual(labels = c("Beyoncé","Taylor Swift"), 
                       values = c("#1b9E77","#7570B3")) +
    geom_label(aes(x=1,
                   y=-2,
                   hjust=0, 
                   label=str_wrap(
                     paste0("Songs: ",
                            paste0(song_name,collapse=", ")),
                     width = 65)),
               size = 4,
               col=1) +
    lims(x=c(1,6),y=c(-3,7)) +
    theme_void() +
    labs(title = paste0("A ", 
                        "<b style='color:#1b9E77'>Beyon</b>", 
                        "<b style='color:#7570B3'>Tay</b>",
                        " Verse"),
         subtitle = paste0(
           " Randomly pairing ",
           "<b style='color:#1b9E77'>Beyoncé </b>", 
           " and",
           "<b style='color:#7570B3'> Taylor Swift</b>",
           " lyrics that rhyme"),
         colour="",
         size="",
         caption=paste0(
           "#TidyTuesday by @lukefshaw \n",
           "Code: https://github.com/Luke-Shaw/beyontay ")) +
    theme(plot.title = element_markdown(lineheight = 1.1),
          plot.subtitle = element_markdown(lineheight = 1.1),
          legend.position = "none",
          plot.background = element_rect(
            fill = "white",
            colour = "black",
            size = 1  ))
  return(the_plot)
}

save_verse_plot <- function(verse_plot = last_plot(), 
                            filename = "./verse_plot.png"){
  # save the verse plot in dimensions that work for the way the 
  # ggplot has been built
  ggsave(filename = filename,
         plot = verse_plot,
         width = 15, height = 10, units = "cm")
  
  print(paste0("plot saved at: ", filename))
  
}
