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
plot_verse(get_verse(seed = 1))

# Some examples. Note doesn't always work perfectly (see blog)
plot_verse(get_verse(seed = 8))
plot_verse(get_verse(seed = 19))
plot_verse(get_verse(seed = 22))
plot_verse(get_verse(seed = 25))
plot_verse(get_verse(seed = 39))

# Or can randomly go forth without setting a seed (though may be harder to 
# recover a good lyric!)
plot_verse(get_verse())
