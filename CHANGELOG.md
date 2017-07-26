# Changelog

## 2017-7-26
 * Add support for SVG tags -- this BREAKS any substitutions using the same name as an SVG tag
 * Add larceny ("l") namespace for ambiguous tags
 * Add start of a conversion script to help find SVG-shadowed substitutions. It doesn't actually work as-is -- you need to make some internal Larceny modules accessible and change the template directory inside the script. Not sure how/if to address this for now, because there are only a couple of people who are ever going to use this script.

## 2017-7-21
 * Fix bug in template processing.

## 7/20/2017
 * Change so that by default, no longer throws errors when no Fill for a Blank.
 * Add `fallbackSub` so users can specify behavior when a Fill is missing.
 * Remove `MissingBlanks` exception.

## Changelog bankruptcy

## 2/8/2017

* `textFill` now escapes HTML entities, `rawTextFill` added
