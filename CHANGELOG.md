# Changelog

## 2017-7-26
 * Support SVG tags -- this breaks any substitutions using the same name as an SVG tag
 * Add larceny ("l") namespace for ambiguous tags

## 2017-7-21
 * Fix bug in template processing.

## 7/20/2017
 * Change so that by default, no longer throws errors when no Fill for a Blank.
 * Add `fallbackSub` so users can specify behavior when a Fill is missing.
 * Remove `MissingBlanks` exception.

## Changelog bankruptcy

## 2/8/2017

* `textFill` now escapes HTML entities, `rawTextFill` added
