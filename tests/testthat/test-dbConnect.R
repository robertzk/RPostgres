context("Connection")

test_that("double disconnect throws error", {
  con <- dbConnect(Postgres())
  expect_true(dbDisconnect(con))
  expect_error(dbDisconnect(con), "not valid")
})

test_that("querying closed connection throws error", {
  db <- dbConnect(Postgres())
  dbDisconnect(db)
  expect_error(dbSendQuery(db, "select * from foo"), "not valid")
})

test_that("warn if previous result set is invalidated", {
  con <- dbConnect(Postgres())
  rs1 <- dbSendQuery(con, "SELECT 1 + 1")

  expect_warning(rs2 <- dbSendQuery(con, "SELECT 1 + 1"), "Cancelling previous query")
  expect_false(dbIsValid(rs1))

  dbClearResult(rs2)
})

test_that("no warning if previous result set is closed", {
  con <- dbConnect(Postgres())
  rs1 <- dbSendQuery(con, "SELECT 1 + 1")
  dbClearResult(rs1)

  rs2 <- dbSendQuery(con, "SELECT 1 + 1")
  dbClearResult(rs2)
})

test_that("warning if close connection with open results", {
  con <- dbConnect(Postgres())
  rs1 <- dbSendQuery(con, "SELECT 1 + 1")

  expect_warning(dbDisconnect(con), "still in use")
})
