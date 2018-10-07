library(testthat)
x <- c(1, 2, 3, 4, 5)
y <- c(1, 2, 3, 4, NA)
z <- c(TRUE, FALSE, TRUE)
w <- letters[1:5]
source("functions.R")
context("Test for range value") stat
test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5) 
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x), 1)
  expect_type(stat_range(x), 'double')
})
library(testthat)
test_file("tests.R")
y <- c(1, 2, 3, 4, NA)
context("Test for range value") 

test_that("range works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(stat_range(y), NA)
  expect_length(stat_range(y), 1)
  expect_type(stat_range(y), 'double')
})

context("Test for range value") 

test_that("range works as expected", {
  z <- c(TRUE,FALSE,FALSE)
  
  expect_equal(stat_range(z), 1)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'integer')
})

head(USArrests)
states <- rownames(USArrests)
head(states)
nchar(states)
tolower(states)
toupper(states)
casefold(states)
num_chars<-nchar(states)

char_freqs<-data.frame(states=states,number=num_chars)
char_freqs
state_freq <- char_freqs$number
state_name <- as.character(char_freqs$states)
names(state_freq) <- state_name
barplot(state_freq)
paste0("Alabama = 7" "Alaska = 6" "Arizona = 7" "Arkansas = 8" "California = 10")      
