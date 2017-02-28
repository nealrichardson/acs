# What's changed

* Code linting using [formatR](https://yihui.name/formatr/) and reorganization into multiple files
* Allow setting API key in `options` (and thus in .Rprofile) under 'census.api.key'. Add helper function `api.key.load` to wrap usage of "extdata/key.rda" throughout and to handle the dual key locations. Removed "file" argument to `api.key.install` because none of the places that loaded the key looked anywhere other than "extdata/key.rda".
