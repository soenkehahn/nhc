
module Utils where


import Control.Arrow


-- | Normalizes lines as the nix lines literals do.
--
-- >>> normalizeLines " \n \n  foo\n\n  bar\n    baz\n \n"
-- "foo\n\nbar\n  baz\n"
normalizeLines :: String -> String
normalizeLines =
    lines >>>
    -- convert whitespace lines to empty lines
    map (\ line -> if all (== ' ') line then "" else line) >>>
    -- strip empty lines at start and end
    dropWhile null >>> reverse >>> dropWhile null >>> reverse >>>
    -- strip indentation
    stripIndentation >>>
    unlines
  where
    stripIndentation :: [String] -> [String]
    stripIndentation ls =
        let indent = minimum $
                map (length . takeWhile (== ' ')) $
                filter (not . null) ls
        in map (drop indent) ls
