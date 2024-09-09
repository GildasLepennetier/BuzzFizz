#FizzBuzz

fizzbuzz_3_5 <- function(x = NA) {
  if (is.na(x)) {return(NA)}
  by_3 = FALSE ; by_3_txt = "Fizz"
  by_5 = FALSE ; by_5_txt = "Buzz"
  if (x %% 3 == 0) {by_3 = TRUE}
  if (x %% 5 == 0) {by_5 = TRUE}
  if (by_3 & by_5) {return(paste0(by_3_txt, by_5_txt))}
  if (by_3) {return(by_3_txt)}
  if (by_5) {return(by_5_txt)}
  return(x)
}


fizzbuzz_any <- function(x = NA, modulos = c(3, 5), repacements = c("Fizz", "Buzz")) {
	require(tidyverse)
	if (is.na(x)) {return(NA)}
	if (length(modulos) != length(repacements)) {
		stop("modulos and repacements must have the same length")
	}
	tmp <- tibble(modulos = modulos, repacements = repacements, x = x) %>%
		filter(x %% modulos == 0) %>%
		pull(repacements) %>%
		paste0(collapse = "")
	if (tmp == "") {return(x)} else {return(tmp)}
}

# Usage examples:

# nb_tests <- 1000
# system.time({test0 <- sapply(1:nb_tests, fizzbuzz_3_5)}) # 0.304
# system.time({	test1 <- sapply(1:nb_tests, fizzbuzz_any, modulos = c(3, 5), repacements = c("Fizz", "Buzz"))}) # 200
# all.equal(test0, test1)
# test2 <- sapply(1:nb_tests, fizzbuzz_any, modulos = c(3, 5, 7), repacements = c("Fizz", "Buzz", "Fuzz"))
# test3 <- sapply(1:nb_tests, fizzbuzz_any, modulos = c(3, 5, 7, 11), repacements = c("Fizz", "Buzz", "Fuzz", "Bizz"))


