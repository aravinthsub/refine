install.packages("dplyr")
install.packages("tidyr")
install.packages("stringdist")
install.packages("magrittr")
library(dplyr)
library(stringdist)
library(tidyr)
library(magrittr)

summary(refine)
refine <- read.csv("refine.csv")

refine$company <- tolower(refine$company)

companyNames <- c("philips", "akzo", "van houten", "unilever")

for (i in 1:nrow(refine)) {
  position <- amatch(refine$company[i], companyNames, maxDist=5, nomatch=0)
  refine$company[i] = companyNames[position]
}

refine <- refine %>%
  separate(Product.code...number, c("product_code", "product_number"), "-" )

product_codes <- c("p", "v", "x", "q")
product_codes_position <- match(refine$product_code, product_codes, nomatch= 0)

product_categories <- c("Smartphone", "TV", "Laptop", "Tablet")

product <- product_categories[product_codes_position]


refine <- refine %>%
  mutate(product)

refine <- refine %>%
  mutate(full_address = paste(address, city, country, sep=", "))

refine <- refine %>%
  mutate(value = 1,
         company = paste0("company_", company)) %>%
  spread(company, value, fill = 0)

refine <- refine %>%
  mutate(value = 1,
         product = paste0("product_", product)) %>%
  spread(product, value, fill = 0)