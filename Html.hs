module Html (
  cleanupHtml
) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Tree
import System.IO.Unsafe(unsafePerformIO)
import Data.Char(isSpace)

-- removes unsafe/annoying elements from an HTML String
cleanupHtml :: String -> String
cleanupHtml = renderTags . removeBrs . flattenTree . cleanupHtml' . tagTree . parseTags

removeBrs (o@(TagOpen "br" _):c@(TagClose "br"):(TagOpen "br" _):(TagClose "br"):tags) = removeBrs (o:c:tags)
removeBrs [TagOpen "br" _, TagClose "br"] = []
removeBrs (t:tags) = t:removeBrs tags
removeBrs [] = []

cleanupHtml' :: [TagTree String] -> [TagTree String]
cleanupHtml' t = transformTree cleanup t
  where cleanup (TagBranch "iframe" _ _) = []
        cleanup (TagBranch "img" _ _) = []
        cleanup (TagBranch tag _ children) | tag /= "br" && onlyBlank children = []
        cleanup (TagBranch tag attrs children) = [TagBranch tag (filterAttrs attrs) children]
        cleanup (TagLeaf leaf) = map TagLeaf (cleanupLeaf leaf)

        cleanupLeaf (TagOpen "img" _) = []
        cleanupLeaf (TagOpen tag attrs) =  [TagOpen tag (filterAttrs attrs)]
        cleanupLeaf tag = [tag]

        filterAttrs = filter (whitelistedAttr . fst)

        whitelistedAttr "href" = True
        whitelistedAttr _ = False

        onlyBlank = all isBlank
       
        isBlank (TagLeaf (TagOpen "br" _)) = True
        isBlank (TagLeaf (TagText txt)) = all isSpace txt
        isBlank _ = False

