context("dbWriteTable")
library(DBI)

describe("Writing to the database", {
  db_test_that("writing to a database table is successful", {
    dbWriteTable(test_con, "beaver2", beaver2)
    expect_equal(dbReadTable(test_con, "beaver2"), beaver2)
  })

  db_test_that("writing to a database table with character features is successful", {
    iris2 <- transform(iris, Species = as.character(Species))
    dbWriteTable(test_con, "iris", iris2)
    expect_equal(dbReadTable(test_con, "iris"), iris2)
  })
})

describe("Appending to the database", {
  db_test_that("append to a database table is successful", {
    dbWriteTable(test_con, "beaver2", beaver2)
    dbWriteTable(test_con, "beaver2", beaver2, append = TRUE)
    expect_equal(dbReadTable(test_con, "beaver2"), rbind(beaver2, beaver2))
  })

  db_test_that("append to a database table with character features is successful", {
    iris2 <- transform(iris, Species = as.character(Species))
    dbWriteTable(test_con, "iris", iris2)
    dbWriteTable(test_con, "iris", iris2, append = TRUE)
    expect_equal(dbReadTable(test_con, "iris"), rbind(iris2, iris2))
  })
})

describe("Usage of the field.types argument", {
  db_test_that("New table creation respects the field.types argument", {
    iris2       <- transform(iris, Petal.Width = as.integer(Petal.Width),
                             Species = as.character(Species))
    field.types <- c("real", "double precision", "numeric", "bigint", "text")

    dbWriteTable(test_con, "iris", iris2, field.types = field.types)
    expect_equal(dbReadTable(test_con, "iris"), iris2)

    # http://stackoverflow.com/questions/2146705/select-datatype-of-the-field-in-postgres
    types <- DBI::dbGetQuery(test_con,
      paste("select column_name, data_type from information_schema.columns ",
            "where table_name = 'iris'"))
    expected <- data.frame(column_name = colnames(iris2),
                           data_type = field.types, stringsAsFactors = FALSE)
    types    <- without_rownames(types[order(types$column_name), ])
    expected <- without_rownames(expected[order(expected$column_name), ])

    expect_equal(types, expected)
  })

  db_test_that("Appending still works when using the field.types argument", {
    iris2       <- transform(iris, Petal.Width = as.integer(Petal.Width),
                             Species = as.character(Species))
    field.types <- c("real", "double precision", "numeric", "bigint", "text")

    dbWriteTable(test_con, "iris", iris2, field.types = field.types)
    dbWriteTable(test_con, "iris", iris2, field.types = field.types, append = TRUE)
    expect_equal(dbReadTable(test_con, "iris"), rbind(iris2, iris2))

    # http://stackoverflow.com/questions/2146705/select-datatype-of-the-field-in-postgres
    types <- DBI::dbGetQuery(test_con,
      paste("select column_name, data_type from information_schema.columns ",
            "where table_name = 'iris'"))
    expected <- data.frame(column_name = colnames(iris2),
                           data_type = field.types, stringsAsFactors = FALSE)
    types    <- without_rownames(types[order(types$column_name), ])
    expected <- without_rownames(expected[order(expected$column_name), ])

    expect_equal(types, expected)
  })
})
