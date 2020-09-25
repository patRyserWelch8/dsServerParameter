source('options/options_definitions.R')

context("assignSharingSettingsDS::expt::correct_outcome")
test_that("exists list",
{
   if (exists("settings",where = 1))
   {
     rm("settings", pos=1)
   }
   expect_equal(exists("settings", where = 1), FALSE)
   assignSharingSettingsDS()
   expect_equal(exists("settings", where = 1), TRUE)
})

test_that("correct fields",
{
   list.fields <- c("name.struct", "masking", "concealing", "received", "encrypted", "decrypted",
                    "data", "index_x", "index_y", "no_columns", "no_rows", "min_rows","max_rows",
                    "min_columns", "max_columns", "min_value")
   settings <- get("settings", pos=1)
   expect_equal(all(list.fields %in% names(settings)), TRUE)
})

test_that("with options",
{
   set.default.options.not.restrictive()
   assignSharingSettingsDS()
   settings <- get("settings", pos=1)
   expect_equal(settings$name.struct, getOption("param.name.struct"))
   expect_equal(settings$sharing.allowed, TRUE)
   
   set.default.options.restrictive()
   assignSharingSettingsDS()
   settings <- get("settings", pos=1)
   expect_equal(settings$name.struct, getOption("param.name.struct"))
   expect_equal(settings$sharing.allowed, FALSE)
})

test_that("with options incorrect",
{
   set.default.options.incorrect.struct()
   assignSharingSettingsDS()
   settings <- get("settings", pos=1)
   expect_equal(settings$name.struct, "sharing")
   expect_equal(settings$sharing.allowed, FALSE)
   
   set.default.options.incorrect.allowed()
   assignSharingSettingsDS()
   settings <- get("settings", pos=1)
   expect_equal(settings$name.struct, "sharing")
   expect_equal(settings$sharing.allowed, FALSE)
})
