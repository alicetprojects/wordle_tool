# WORDLE_TOOL - A tool to analyse the probability of each letter of the alphabet
# in each position of the 5 spaces in the word guessing game WORDLE 
# WORDLE can be found at https://www.powerlanguage.co.uk/wordle/ 


# with thanks to an anonymous friend on Twitter

# libraries needed

library(tidyverse)
library(readxl)

# import 5-letter words table (pre-processed data in .xlsx format)
# *** CITATION: https://www-cs-faculty.stanford.edu/~knuth/sgb-words.txt

words5letter <- read_xlsx(path = "knuth_5letter_words.xlsx", col_names = FALSE)

# convert to strings + clean up

words_string <- toString(words5letter$...1)
words_string

words_paste <- paste(words_string, collapse = '')

words_paste <- str_remove_all(words_paste, pattern = "[:punct:]")
words_paste <- str_remove_all(words_paste, pattern = "[:space:]")
words_paste


# create a new tibble with 1 letter per entry from the previous tibble



# function to group letters individually

lettergroup1 <- function(string) {
  t <- as.character(string)
  n <- 1
  padded <- c(sapply(seq(1, nchar(t), by = n), function(x) substr(t, x, x + n - 1)))
  return(padded)
  padded
}

words_split <- lettergroup1(words_paste)
words_split


split_tibble <- tibble(letter = words_split)

# count total letters

count_letters <- nrow(split_tibble)
count_letters



# function to count each letter's frequency

lettercount <- function(df, letter) {
  p <- df$letter == letter
  p <- tibble(p)
  p$p[FALSE] <- 1
  sumletters <- sum(p$p)
  return(sumletters)
  sumletters
}



# measuring the frequency of each letter for each place in the five blank spots

seq_vec1 <- seq(from = 1, to = 28785, by = 5)

seq_vec2 <- seq(from = 2, to = 28785, by = 5)

seq_vec3 <- seq(from = 3, to = 28785, by = 5)

seq_vec4 <- seq(from = 4, to = 28785, by = 5)

seq_vec5 <- seq(from = 5, to = 28785, by = 5)


split_tibble$place <- seq(from = 1, to = 28785, by = 1)


first_letters <- tibble(letter = split_tibble$letter[seq_vec1])

second_letters <- tibble(letter = split_tibble$letter[seq_vec2])

third_letters <- tibble(letter = split_tibble$letter[seq_vec3])

fourth_letters <- tibble(letter = split_tibble$letter[seq_vec4])

fifth_letters <- tibble(letter = split_tibble$letter[seq_vec5])



alphaseq <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
              "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
              "u", "v", "w", "x", "y", "z")

alphaseq <- tibble(letter = alphaseq)

alphaseq$num <- c(1:26)

num_letters <- c("0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
                 "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0", "0",
                 "0", "0")

num_letters <- tibble(num_letters = num_letters)

applyalpha <- function(df) {
  
  df <- df
  alphaseq <- alphaseq
  n <- 1
  num_letters = num_letters
  finalcounts <- tibble(num_letters = num_letters)
  for(n in alphaseq$num[1:26]) {
    finalcounts$num_letters[n] <- lettercount(df, alphaseq$letter[n])
    n <- n + 1
  }
  return(finalcounts)
  finalcounts
}


finalc1 <- applyalpha(first_letters)
finalc2 <- applyalpha(second_letters)
finalc3 <- applyalpha(third_letters)
finalc4 <- applyalpha(fourth_letters)
finalc5 <- applyalpha(fifth_letters)

# not exactly the format I hoped for, but the first row of each tibble above
# contains the values for each letter in alphabetical order. Let's resolve
# the formatting issues

str(finalc1)

alphaseqvec <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j",
              "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
              "u", "v", "w", "x", "y", "z")

finalc1 <- slice_head(finalc1)

colnames(finalc1$num_letters)[1:26] <- alphaseqvec[1:26]

finalc2 <- slice_head(finalc2)

colnames(finalc2$num_letters)[1:26] <- alphaseqvec[1:26]

finalc3 <- slice_head(finalc3)

colnames(finalc3$num_letters)[1:26] <- alphaseqvec[1:26]

finalc4 <- slice_head(finalc4)

colnames(finalc4$num_letters)[1:26] <- alphaseqvec[1:26]

finalc5 <- slice_head(finalc5)

colnames(finalc5$num_letters)[1:26] <- alphaseqvec[1:26]

# clean up the format issue, part 2

finalc1vec <- as.numeric(finalc1$num_letters[1:26])
finalc2vec <- as.numeric(finalc2$num_letters[1:26])
finalc3vec <- as.numeric(finalc3$num_letters[1:26])
finalc4vec <- as.numeric(finalc4$num_letters[1:26])
finalc5vec <- as.numeric(finalc5$num_letters[1:26])

# create new tibbles with letters and counts in order

finalfirst <- tibble(letter = alphaseqvec, count = finalc1vec)

finalsecond <- tibble(letter = alphaseqvec, count = finalc2vec)

finalthird <- tibble(letter = alphaseqvec, count = finalc3vec)

finalfourth <- tibble(letter = alphaseqvec, count = finalc4vec)

finalfifth <- tibble(letter = alphaseqvec, count = finalc5vec)

# calculate the frequency of each letter in each of the five positions

finalfirst$freq <- finalfirst$count / sum(finalfirst$count)
finalsecond$freq <- finalsecond$count / sum(finalsecond$count)
finalthird$freq <- finalthird$count / sum(finalthird$count)
finalfourth$freq <- finalfourth$count / sum(finalfourth$count)
finalfifth$freq <- finalfifth$count / sum(finalfifth$count)

# visualize results by letter and count in scatterplots

ggplot(data = finalfirst, aes(x = letter, y = count)) +
  geom_point()

ggplot(data = finalsecond, aes(x = letter, y = count)) +
  geom_point()

ggplot(data = finalthird, aes(x = letter, y = count)) +
  geom_point()

ggplot(data = finalfourth, aes(x = letter, y = count)) +
  geom_point()

ggplot(data = finalfifth, aes(x = letter, y = count)) +
  geom_point()





