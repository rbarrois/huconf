
import Data.Maybe as DM
import System.Environment as SE
import System.IO as SIO


-- Grammar:
--
-- expr = [block]
--      | If(cond) expr Endif
--      | If(cond) expr Else expr Endif



type IfCond = String

data FileTree = LeafContent [BlockLine] | IfNode IfCond [FileTree] [FileTree]
        deriving (Eq, Show, Read)


extractIfBlocks :: [LexedLine] -> ([(LexedLine, [FileTree])], [LexedLine])
extractIfBlocks lines = let
        (trees, hd:rems) = parseLines lines
    in case hd of
        LineEndif -> ([(hd, trees)], rems)
        LineElse -> let (parsed, rems2) = extractIfBlocks rems
                    in ((hd, trees):parsed, rems2)
        _ -> error "Unexpected token"
        

makeIfNode :: IfCond -> [(LexedLine, [FileTree])] -> FileTree
makeIfNode cond [] = IfNode cond [] []
makeIfNode cond [(LineElse, mainBlock), (LineEndif, elseBlock)] = IfNode cond mainBlock elseBlock
makeIfNode cond [(LineEndif, mainBlock)] = IfNode cond mainBlock []


parseLines :: [LexedLine] -> ([FileTree], [LexedLine])
parseLines [] = ([], [])
parseLines ((Block lines):xs) = ([LeafContent lines], xs)
parseLines ((LineIf cond):xs) = let (blocks, rems) = extractIfBlocks xs in ([makeIfNode cond blocks], rems)


fullParse :: [LexedLine] -> [FileTree]
fullParse [] = []
fullParse lines = let (trees, rems) = parseLines lines in trees ++ (fullParse rems)



-- data DocTree = LeafComment String | LeafText String | IfNode [(String, [DocTree])] [DocTree]
--     deriving (Eq, Show, Read)
-- 
-- 
-- eatDoc :: [LexedLine] -> (DocTree, [LexedLine])
-- eatDoc ((LineComment comment):xs) = (LeafComment comment, xs)
-- eatDoc ((LineText line):xs) = (LeafLine line, xs)
-- eatDoc ((LineIf line):xs)


data BaseLexedLine = BaseLineIf String | BaseLineElse | BaseLineEndif | BaseLineComment String | BaseLineText String
    deriving (Eq, Show, Read)

data BlockLine = BlockComment String | BlockText String
    deriving (Eq, Show, Read)

data LexedLine = LineIf String | LineElse | LineEndif | Block [BlockLine] | EndFile
    deriving (Eq, Show, Read)

_baseLexLine :: String -> BaseLexedLine
_baseLexLine ('#':'#':rem) = BaseLineComment rem
_baseLexLine ('#':'@':'i':'f':' ':cond) = BaseLineIf cond
_baseLexLine "#@else" = BaseLineElse
_baseLexLine "#@endif" = BaseLineEndif
_baseLexLine ('#':'@':cmd) = error $ "Invalid command " ++ cmd ++ "."
_baseLexLine line = BaseLineText line


_lexMergeFolder :: BaseLexedLine -> [LexedLine] -> [LexedLine]
_lexMergeFolder (BaseLineComment s) ((Block x):xs) = ((Block ((BlockComment s):x)):xs)
_lexMergeFolder (BaseLineText s) ((Block x):xs) = ((Block ((BlockText s):x)):xs)
_lexMergeFolder (BaseLineComment s) lines = ((Block [BlockComment s]):lines)
_lexMergeFolder (BaseLineText s) lines = ((Block [BlockText s]):lines)
_lexMergeFolder (BaseLineIf x) lines = (LineIf x):lines
_lexMergeFolder BaseLineElse lines = LineElse:lines
_lexMergeFolder BaseLineEndif lines = LineEndif:lines


lexLines :: [String] -> [LexedLine]
lexLines lines = foldr _lexMergeFolder [] (map _baseLexLine lines)


-- data ParserMode = ParserOut | ParserIf | ParserElse
--     deriving (Eq, Show, Read, Enum)
-- 
-- type ParserState = (ParserMode, Bool)
-- 
-- 
-- parseLine :: [String] -> [ParserState] -> LexedLine -> ([ParserState], Maybe String)
-- parseLine cats stateStack (LineComment _)  = (stateStack, Nothing)
-- parseLine cats stateStack (LineIf cond) | cond `elem` cats = (((ParserIf, True):stateStack), Nothing)
-- parseLine cats stateStack (LineIf _) = (((ParserIf, False):stateStack), Nothing)
-- parseLine cats ((ParserIf, x):stateStack) LineElse = (((ParserElse, not x):stateStack), Nothing)
-- parseLine cats _ LineElse = error "Invalid #@else outside stack"
-- parseLine cats ((ParserIf, _):stateStack) LineEndif = (stateStack, Nothing)
-- parseLine cats ((ParserElse, _):stateStack) LineEndif = (stateStack, Nothing)
-- parseLine cats _ LineEndif = error "Invalid #@endif outside control block"
-- parseLine cats ((mode, display):modeStack) (LineText line) | display = (((mode, display):modeStack), Just line)
-- parseLine cats ((mode, display):modeStack) (LineText _) = (((mode, display):modeStack), Nothing)
-- parseLine cats _ line = error $ "Invalid line " ++ (show line) ++ "."
-- 
-- 
-- parseLines :: [String] -> [ParserState] -> [LexedLine] -> [Maybe String]
-- parseLines cats stateStack [] = []
-- parseLines cats stateStack (line:lines) = let (newStack, convLine) = parseLine cats stateStack line in (convLine:parseLines cats newStack lines)
-- 
-- 
-- huconfParse :: [String] -> [String] -> [String]
-- huconfParse cats xs = DM.catMaybes $ parseLines cats [(ParserOut, True)] (map lexLine xs)
-- 

evalContent :: BlockLine -> Maybe String
evalContent (BlockComment _) = Nothing
evalContent (BlockText l) = Just l

evalBlock :: [String] -> FileTree -> [Maybe String]
evalBlock cats (LeafContent lines) = map evalContent lines
evalBlock cats (IfNode cond mainBlock elseBlock) | cond `elem` cats = concatMap (evalBlock cats) mainBlock
                                                 | otherwise = concatMap (evalBlock cats) elseBlock

huconfEval :: [String] -> [FileTree] -> [String]
huconfEval cats lines = DM.catMaybes $ concatMap (evalBlock cats) lines

huconfParse :: [String] -> [FileTree]
huconfParse = fullParse . lexLines

main = do
    (filename:categories) <- SE.getArgs
    handle <- SIO.openFile filename SIO.ReadMode
    contents <- hGetContents handle
    let fileLines = lines contents
    mapM_ putStrLn $ huconfEval categories $ huconfParse fileLines

