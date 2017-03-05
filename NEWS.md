# What's changed

## Internal

* Code linting using [formatR](https://yihui.name/formatr/) and reorganization into multiple files
* Moved 'Depends:' packages to 'Imports:'
* Remove dependency on 'RCurl' in favor of 'httr'. Use 'httptest' to allow test running without hitting the Census API.

## New features

* Allow setting API key in `options` (and thus in .Rprofile) under 'census.api.key'. Add helper function `api.key.load` to wrap usage of "extdata/key.rda" throughout and to handle the dual key locations. Removed "file" argument to `api.key.install` because none of the places that loaded the key looked anywhere other than "extdata/key.rda".
* If the Census API query fails, the error message that the API returns is now printed in the error message instead of a generic "no data was found". Helpful for debugging when queries don't work as expected.

## Fixes

* Fix querying of 1990 census, which fails if "NAME" is specified as a variable; it uses "ANPSADPI" instead
* Performance improvements to parsing results in `acs.fetch`
