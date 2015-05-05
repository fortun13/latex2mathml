module Latex2MathML.Parser.Main (parse) where

import Latex2MathML.Utils.Definitions
import Latex2MathML.Utils.Functions (throwError)

parse :: [Token] -> Either String [ASTModel]
parse lst = (parse' lst End) >>= (\x -> return $ fst x)

parse' :: [Token] -> Token -> Either String ([ASTModel],[Token])
parse' [] _ = return ([],[])
parse' (h:t) stop
    | h == stop = return ([],t)
parse' ((MyNum x):t) stop = simpleBind t MN stop x
parse' ((MyStr string):t) stop = parse' t stop >>= (\x -> return (parseString string ++ fst x,snd x))
parse' ((Command name):t) stop = iterateUsing parseCommand t name stop
parse' (Sup:t) stop = iterateUsing readSup t "" stop
parse' (Sub:t) stop = iterateUsing readSub t "" stop
parse' ((Operator o) : t) stop = simpleBind t ASTOperator stop o
parse' lst _ = throwError $ "Fatal error at parsing before: " ++ (show $ take 10 lst)

simpleBind :: [Token] -> (String -> ASTModel) -> Token -> String -> Either String ([ASTModel],[Token])
simpleBind lst type' stop value = parse' lst stop >>= (\x -> return ((type' value) : fst x,snd x))

iterateUsing :: (String -> [Token] -> Either String (ASTModel,[Token])) -> [Token] -> String -> Token -> Either String ([ASTModel],[Token])
iterateUsing function lst value stop = do
    tmp <- function value lst
    tmp2 <- parse' (snd tmp) stop
    return (fst tmp : fst tmp2, snd tmp2)

parseString :: String -> [ASTModel]
parseString [] = []
parseString (h:t) = [Variable h] ++ parseString t

parseCommand :: String -> [Token] -> Either String (ASTModel,[Token])
parseCommand [] _ = throwError "Empty Command name"
parseCommand name lst@(h:t)
    | name == "begin" = readComplexCommand lst
    -- dropping 2 because it should be MyStr CommandName (i.e. array) and BodyEnd
    | name == "end" = return (ComplexEnd,drop 2 t)
    | h == BodyBegin = do
        body <- readCommandBody lst
        return (InlineCommand name [] (fst body),snd body)
    | h == Operator "[" = do
        par <- readParameters t
        body <- readCommandBody $ snd par
        return (InlineCommand name (fst par) (fst body),snd body)
    | otherwise = return (BodylessCommand name,lst)

readParameters :: [Token] -> Either String ([ASTModel],[Token])
readParameters lst = parse' lst (Operator "]")

readCommandBody :: [Token] -> Either String ([[ASTModel]],[Token])
readCommandBody (BodyBegin:t) = do
    tmp <- parse' t BodyEnd
    tmp2 <- readCommandBody $ snd tmp
    return (fst tmp : fst tmp2,snd tmp2)
readCommandBody lst = return ([],lst)

readComplexCommand :: [Token] -> Either String (ASTModel,[Token])
readComplexCommand [] = return (Empty,[])
readComplexCommand (BodyBegin : (MyStr n) : BodyEnd : t) = do
    parameters <- readComplexParameters t
    body <- parse' (snd parameters) (Command "end")
    return (ComplexCommand n (fst parameters) (fst body),drop 3 (snd body))
readComplexCommand lst = throwError $ "Error at parsing complex command before: " ++ (show $ take 10 lst)

readComplexParameters :: [Token] -> Either String ([ASTModel],[Token])
readComplexParameters (Operator "[" : t) = complexParametersHelper t (Operator "]")
readComplexParameters (BodyBegin : t) = complexParametersHelper t BodyEnd
readComplexParameters lst = return ([],lst)

complexParametersHelper :: [Token] -> Token -> Either String ([ASTModel],[Token])
complexParametersHelper lst stop = do
    tmp <- parse' lst stop
    tmp2 <- readComplexParameters $ snd tmp
    return (fst tmp ++ fst tmp2,snd tmp2)

readSup :: String -> [Token] -> Either String (ASTModel,[Token])
readSup _ lst = readSupOrSub lst ASTSup

readSub :: String -> [Token] -> Either String (ASTModel,[Token])
readSub _ lst = readSupOrSub lst ASTSub

readSupOrSub :: [Token] -> ([ASTModel] -> ASTModel) -> Either String (ASTModel,[Token])
readSupOrSub (BodyBegin:t) type' = parse' t BodyEnd >>= (\x -> return (type' $ fst x,snd x))
readSupOrSub (Command name : t) type' = parseCommand name t >>= (\x -> return (type' $ [fst x],snd x))
readSupOrSub (MyStr (h:tl) : t) type' = parse' [MyStr [h]] BodyEnd >>= (\x -> return (type' $ fst x,MyStr tl : t))
readSupOrSub (MyNum (h:tl) : t) type' = parse' [MyNum [h]] BodyEnd >>= (\x -> return (type' $ fst x,MyNum tl : t))
readSupOrSub (h:t) type' = parse' [h] BodyEnd >>= (\x -> return (type' $ fst x,t))
readSupOrSub lst type' = throwError $ "Error at parsing " ++ (show $ type' []) ++ " before" ++ (show $ take 10 lst)