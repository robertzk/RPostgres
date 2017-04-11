context("dbGetInfo")

test_that("double disconnect throws error", {
  con <- dbConnect(Postgres())
  info <- dbGetInfo(con)
  expect_true(setequal(names(info), c("dbname", "host", "port", "user", "pass", "protocol_version", "server_version")))
})
