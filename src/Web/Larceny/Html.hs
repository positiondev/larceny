{-# LANGUAGE OverloadedStrings #-}

module Web.Larceny.Html where

import qualified Data.HashSet as HS
import           Data.Text    (Text)

-- retrieved from: https://www.w3.org/TR/html-markup/elements-by-function.html
html5Nodes :: HS.HashSet Text
html5Nodes = HS.fromList [

-- 5.1. The root element

    "html", -- root element

-- 5.2. Document metadata

    "head", -- document metadata container
    "title", -- document title
    "base", -- base URL
    "link", -- inter-document relationship metadata
    "meta", -- metadata CHANGED
    "style", -- style (presentation) information

-- 5.3. Scripting

    "script", -- embedded script
    "noscript", -- fallback content for script

-- 5.4. Sections

    "body", -- document body
    "section", -- section NEW
    "nav", -- group of navigational links NEW
    "article", -- article NEW
    "aside", -- tangential content NEW
    "h1", -- heading
    "h2", -- heading
    "h3", -- heading
    "h4", -- heading
    "h5", -- heading
    "h6", -- heading
    "hgroup", -- heading group NEW
    "header", -- header NEW
    "footer", -- footer NEW
    "main", -- main VERY NEW (wasn't in the spec linked to above)
    "address", -- contact information

-- 5.5. Grouping content

    "p", -- paragraph
    "hr", -- thematic break CHANGED
    "br", -- line break
    "pre", -- preformatted text
    "blockquote", -- block quotation
    "ol", -- ordered list
    "ul", -- unordered list
    "li", -- list item
    "dl", -- description list
    "dt", -- term or name
    "dd", -- description or value
    "figure", -- figure with optional caption NEW
    "figcaption", -- figure caption NEW
    "div", -- generic flow container

-- 5.6. Text-level semantics

    "a", -- hyperlink CHANGED
    "em", -- emphatic stress
    "strong", -- strong importance
    "small", -- small print CHANGED
    "s", -- struck text CHANGED
    "cite", -- cited title of a work CHANGED
    "q", -- quoted text
    "dfn", -- defining instance
    "abbr", -- abbreviation
    "time", -- date and/or time NEW
    "code", -- code fragment
    "var", -- variable or placeholder text
    "samp", -- (sample) output
    "kbd", -- user input
    "sub", -- subscript
    "sup", -- superscript
    "i", -- offset text conventionally styled in italic CHANGED
    "b", -- offset text conventionally styled in bold CHANGED
    "u", -- offset text conventionally styled with an underline CHANGED
    "mark", -- marked (highlighted) text NEW
    "ruby", -- ruby annotation NEW
    "rt", -- ruby text NEW
    "rp", -- ruby parenthesis NEW
    "bdi", -- BiDi isolate NEW
    "bdo", -- BiDi override
    "span", -- generic span

-- 5.7. Edits

    "ins", -- inserted text
    "del", -- deleted text

-- 5.8. Embedded content

    "img", -- image
    "iframe", -- nested browsing context (inline frame)
    "embed", -- integration point for plugins NEW
    "object", -- generic external content
    "param", -- initialization parameters for plugins
    "video", -- video NEW
    "audio", -- audio stream NEW
    "source", -- media source NEW
    "track", -- supplementary media track NEW
    "canvas", -- canvas for dynamic graphics NEW
    "map", -- image-map definition
    "area", -- image-map hyperlink

-- 5.9. Tables

    "table", -- table
    "caption", -- table title
    "colgroup", -- table column group
    "col", -- table column
    "tbody", -- table row group
    "thead", -- table heading group
    "tfoot", -- table footer row group
    "tr", -- table row
    "td", -- table cell
    "th", -- table header cell

-- 5.10. Forms

    "form", -- user-submittable form
    "fieldset", -- set of related form controls
    "legend", -- title or explanatory caption
    "label", -- caption for a form control
    "input", -- input control CHANGED
    "button", -- button
    "select", -- option-selection form control
    "datalist", -- predefined options for other controls NEW
    "optgroup", -- group of options
    "option", -- option
    "textarea", -- text input area
    "keygen", -- key-pair generator/input control NEW
    "output", -- result of a calculation in a form NEW
    "progress", -- progress indicator NEW
    "meter", -- scalar gauge NEW

-- 5.11. Interactive elements

    "details", -- control for additional on-demand information NEW
    "summary", -- summary, caption, or legend for a details control NEW
    "command", -- command NEW
    "menu"] -- list of commands CHANGED
