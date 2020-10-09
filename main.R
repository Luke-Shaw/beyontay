################################################################################
# Main script for creating a BeyonTay verse
################################################################################

# functions and data ------------------------------------------------------
source("./functions.R")
line_ending_words <- read_csv(here::here("data/line_ending_words.csv"),col_types = cols())
line_per_row <- read_csv(here::here("data/line_per_row.csv"),col_types = cols())

# Making a BeyonTay verse -------------------------------------------------

# Can use the variable seed to set a new random number determining which lyrics 
# to pull, or can delve into the code in /functions.R to see the raw data
verse_plot(get_verse(seed = 1))

# Some personal favourites
verse_plot(get_verse(seed = 6))
verse_plot(get_verse(seed = 10))
verse_plot(get_verse(seed = 12))
verse_plot(get_verse(seed = 14))
verse_plot(get_verse(seed = 18))
verse_plot(get_verse(seed = 1729))

set.seed(1)
verse_plot(get_verse())
