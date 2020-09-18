# These function are required to test locally the use of options. Those
# are used 
set.default.options.not.restrictive <- function()
{
  options(param.name.struct = "sharing_testing")
  options(param.sharing.allowed = 1)
}

set.default.options.restrictive <- function()
{
  options(param.name.struct = "sharing_testing")
  options(param.sharing.allowed = 0)
}

set.default.options.incorrect.struct <- function()
{
  options(param.name.struct = "")
  options(param.sharing.allowed = 0)
}

set.default.options.numerical.struct <- function()
{
  options(param.name.struct = 1)
  options(param.sharing.allowed = 0)
}

set.default.options.incorrect.allowed <- function()
{
  options(param.name.struct = "sharing")
  options(param.sharing.allowed = 0.5)
}
