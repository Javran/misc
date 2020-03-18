{-
  Module for caching downloaded resources locally.
 -}
module Cache where

{-
  TODO: all of below.

  For a given baseOutputPath, cache will be:

  {baseOutputPath}/{version}/{resourcePath}.

    e.g.
    /kcs2/img/common/common_itemicons.json?version=4.5.3.0

    becomes:
    {baseOutputPath}/4.5.3.0/kcs2/img/common/common_itemicons.json

    if this resource is a bundle of some other resources
    (in this case, json+png describes that bundle)

    extracted resources will be put in a directory with '.extract' as suffix. e.g.:

    {baseOutputPath}/4.5.3.0/kcs2/img/common/common_itemicons.extract/

    - Note: In case when a missing resource is given but without version number, we'll try to
      download /kcs2/version.json from the same server and determine version number from there.

    - Note: urls under /kcs2/img seems to follow a pattern and use spritesmith most of the time,
      we can perhaps parse that.
 -}

