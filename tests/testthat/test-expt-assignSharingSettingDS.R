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
   test_that(all(list.fields %in% names(settings)), TRUE)
})
