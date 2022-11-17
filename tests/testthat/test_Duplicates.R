library(polmineR)
testthat::context("Duplicates")

test_that(
  "Duplicates",
  {
    if ("NADIRASZ" %in% corpus()$corpus){
      
      D <- Duplicates$new(
        corpus = "NADIRASZ",
        char_regex = "[a-zA-ZäöüÄÖÜ]",
        p_attribute = "word",
        s_attribute = "article_date",
        sample = 50L,
        n = 1L,
        threshold = 0.9
      )

      article_bundle <- corpus("NADIRASZ") |>
        subset(article_date == "2000-01-01") |>
        split(s_attribute = "article_id")

      D$detect(x = article_bundle, mc = 3L, progress = FALSE)
      
      testthat::expect_equal(
        D$duplicates[["name"]], c("A9743732", "A9743756"),
        D$duplicates[["duplicate_name"]], c("A9743755", "A9743733"),
      )
    }
  }
)


test_that(
  "Duplicates - same day",
  {
    if ("NADIRASZ" %in% corpus()$corpus){

      D <- Duplicates$new(
        corpus = "NADIRASZ",
        char_regex = "[a-zA-ZäöüÄÖÜ]",
        p_attribute = "word",
        s_attribute = "article_date",
        sample = 50L,
        n = 0L,
        threshold = 0.9
      )
      
      dates <- c(
        "2000-01-01",
        "2000-01-03",
        "2000-01-04",
        "2000-01-05",
        "2000-01-07",
        "2000-01-08"
      )

      article_bundle <- corpus("NADIRASZ") |>
        subset(article_date %in% dates) |>
        split(s_attribute = "article_id")

      D$detect(x = article_bundle, mc = 3L, progress = FALSE, how = "algebra")
    }
  }
)
