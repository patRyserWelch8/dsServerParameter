    
.test.data.structure <- function(data)
{
    
    expected.list <- c("header","payload", "property.a", "property.b","property.c","property.d")
    #structure
    expect_equal(is.list(data),TRUE)
    expect_equal(all(expected.list %in% names(data), TRUE), TRUE)
    expect_equal(is.character(data$header),TRUE)
    expect_equal(is.character(data$payload),TRUE)
    expect_equal(is.numeric(data$property.a),TRUE)
    expect_equal(is.numeric(data$property.b),TRUE)
    expect_equal(is.numeric(data$property.c),TRUE)
    expect_equal(is.numeric(data$property.d),TRUE)
    
    
    data.list    <- strsplit(data$payload,";")
    data.vector  <- unlist(data.list)
    expect_equal(length(data.vector) >= (11 * 13),TRUE)
    
}
