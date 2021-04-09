# Script to debug a remote installation.

library(testthat)
library(dplyr, quietly = T)
context("Federalist Papers")

test_data <- bookworm(host = "benschmidt.org", method = "return_tsv", database = "federalist", counttype = c("WordCount", "TextCount"), groups = c("author"))
test_that(
  "Hamilton has a lot of words",
  expect_gt({
    test_data %>% filter(author == "HAMILTON") %>% select(WordCount) %>% unlist()
  }, 125000)
)

test_data <- bookworm(host = "benschmidt.org", method = "return_tsv", database = "federalist", counttype = c("WordCount", "TextCount"), groups = c("author"), word = c("upon"))

test_that(
  "Hamilton uses 'upon' 374 times",
  expect_equivalent({
    test_data %>% filter(author == "HAMILTON") %>% select(WordCount) %>% unlist()
  }, 374)
)
