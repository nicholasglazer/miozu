-- Author Glazer Nicholas <glazer.nicholas@gmail.com>
-- Read more about colors at https://miozu.com/colors
module Miozu
  ( miozu00
  , miozu01
  , miozu02
  , miozu03
  , miozu04
  , miozu05
  , miozu06
  , miozu07
  , yellow
  , cyan
  , orange
  , green
  , peach
  , magenta
  , blue
  , red
  ) where

-- Miozu color palette
miozu00 :: [Char]
miozu00 = "#232733" -- Darkest Background
miozu01 :: [Char]
miozu01 = "#2C3040" -- Lighter Background (Used for status bars, line number and folding marks)
miozu02 :: [Char]
miozu02 = "#3E4359" -- Selection Background
miozu03 :: [Char]
miozu03 = "#565E78" -- Comments, Invisibles, Line Highlighting
miozu04 :: [Char]
miozu04 = "#737E99" -- Dark Foreground (Used for status bars)
miozu05 :: [Char]
miozu05 = "#D0D2DB" -- Default Foreground, Caret, Delimiters, Operators
miozu06 :: [Char]
miozu06 = "#F3F4F7" -- Light Foreground (Not often used)
miozu07 :: [Char]
miozu07 = "#FAFDFB" -- Light Background (Not often used)
yellow  :: [Char]
yellow  = "#E8D176" -- Variables, XML Tags, Markup Link Text, Markup Lists, Diff Deleted
cyan    :: [Char]
cyan    = "#40FFE2" -- Integers, Boolean, Constants, XML Attributes, Markup Link Url
orange  :: [Char]
orange  = "#FF9837" -- Classes, Markup Bold, Search Text Background
green   :: [Char]
green   = "#6DD672" -- Strings, Inherited Class, Markup Code, Diff Inserted
peach   :: [Char]
peach   = "#FF9982" -- Support, Regular Expressions, Escape Characters, Markup Quotes
magenta :: [Char]
magenta = "#C974E6" -- Functions, Methods, Attribute IDs, Headings
blue    :: [Char]
blue    = "#83D2FC" -- Keywords, Storage, Selector, Markup Italic, Diff Changed
red     :: [Char]
red     = "#EB3137" -- Deprecated, Opening/Closing Embedded Language Tags, e.g.
