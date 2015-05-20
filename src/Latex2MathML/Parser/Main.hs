module Latex2MathML.Parser.Main (parse) where

import Latex2MathML.Utils.Definitions
--import Latex2MathML.Utils.Functions (throwError)
import Data.Map ((!),member)
import Control.Monad.Error.Class
import Control.Monad.Trans.Either

parse :: [Token] -> EitherT String IO [ASTModel]
parse lst = (hoistEither $ parse' lst End) >>= (return . fst)
--parse lst = do
--    tmp <- liftIO lst
--    hoistEither $ (parse' tmp End) >>= (return . fst)
--parse lst = (parse' lst End) >>= (return . fst)

parse' :: [Token] -> Token -> Either String ([ASTModel],[Token])
parse' [] _ = return ([],[])
parse' (h:t) stop
    | h == stop = return ([Empty],t)
parse' (MyNum x : t) stop = simpleBind t MN stop x
parse' (MyStr string : t) stop = parse' t stop >>= (\x -> return (parseString string ++ fst x,snd x))
parse' (Command name : t) stop = iterateUsing parseCommand t name stop
parse' (Sup:t) stop = iterateUsing readSup t "" stop
parse' (Sub:t) stop = iterateUsing readSub t "" stop
parse' (Operator o : t) stop = simpleBind t ASTOperator stop o
parse' (BodyBegin : t) stop = parse' t stop
parse' (BodyEnd : t) stop = parse' t stop
parse' lst _ = throwError $ "Parser: Fatal error at parsing before: " ++ (show $ take 10 lst)

simpleBind :: [Token] -> (String -> ASTModel) -> Token -> String -> Either String ([ASTModel],[Token])
simpleBind lst type' stop value = parse' lst stop >>= (\x -> return (type' value : fst x,snd x))

iterateUsing :: (String -> [Token] -> Either String (ASTModel,[Token])) -> [Token] -> String -> Token -> Either String ([ASTModel],[Token])
iterateUsing function lst value stop = do
    tmp <- function value lst
    tmp2 <- parse' (snd tmp) stop
    return (fst tmp : fst tmp2, snd tmp2)
--    if (fst tmp2) /= [] && last (fst tmp2) == Empty
--        then return (fst tmp : (init $ fst tmp2), snd tmp2)
--        else return (fst tmp : fst tmp2, snd tmp2)

parseString :: String -> [ASTModel]
parseString = foldr (\x -> (++) [Variable x]) []

parseCommand :: String -> [Token] -> Either String (ASTModel,[Token])
parseCommand [] _ = throwError "Parser: Empty Command name"
parseCommand name [] = return (BodylessCommand name,[])
parseCommand name lst@(h:t)
    | name == "begin" = readComplexCommand lst
    -- dropping 2 because it should be MyStr CommandName (i.e. array) and BodyEnd
    | name == "end" = return (ComplexEnd,drop 2 t)
    | h == BodyBegin = do
        body <- readCommandBody lst
        if member name commandsArity && length (fst body) < commandsArity ! name
            then throwError $ "Parser: Not enough bodies for command: " ++ name
            else return (InlineCommand name [] (fst body),snd body)
    | h == Operator "[" =
        if name == "sqrt"
        then do
            par <- readParameters t
            if last (fst par) == Empty
                then
                    if head (snd par) == BodyBegin
                        then do
                            body <- readCommandBody $ snd par
                            return (InlineCommand name (init $ fst par) (fst body),snd body)
                        else do
                            tmp <- readInlineWithoutBody (take (commandsArity ! name) (snd par)) name (init $ fst par) []
                            return (fst tmp,snd tmp ++ drop (commandsArity ! name) (snd par))
                else throwError $ "Parser: Parameters for command: " ++ name ++ " not closed"
        else do
            tmp <- readInlineWithoutBody (take (commandsArity ! name) lst) name [] []
            return (fst tmp, snd tmp ++ drop (commandsArity ! name) lst)
    | Data.Map.member name commandsArity = do
        tmp <- readInlineWithoutBody (take (commandsArity ! name) lst) name [] []
        return (fst tmp,snd tmp ++ drop (commandsArity ! name) lst)
--        let tmp = take (commandsArity ! name) t
    | otherwise = return (BodylessCommand name,lst)

readParameters :: [Token] -> Either String ([ASTModel],[Token])
readParameters lst = parse' lst (Operator "]")

readCommandBody :: [Token] -> Either String ([[ASTModel]],[Token])
readCommandBody (BodyBegin:t) = do
    tmp <- parse' t BodyEnd
    if last (fst tmp) == Empty
        then do
            tmp2 <- readCommandBody $ snd tmp
            return ((init $ fst tmp) : fst tmp2,snd tmp2)
        else
            throwError "Parser: Not closed curly bracket"
readCommandBody lst = return ([],lst)

readComplexCommand :: [Token] -> Either String (ASTModel,[Token])
readComplexCommand (BodyBegin : MyStr n : BodyEnd : t) = do
    parameters <- readComplexParameters t
    body <- parse' (snd parameters) (Command "end")
    if last (fst body) == Empty
        then
        -- drop 3 because - BodyBegin (Command name (for example array)) and BodyEnd
        if (take 3 $ snd body) /= [BodyBegin,MyStr n,BodyEnd]
            then throwError $ "Parser: Bad closing environment for: " ++ n
            else return (ComplexCommand n (fst parameters) (fst body),drop 3 (snd body))
        else throwError "Parser: Error at parsing complex command"
readComplexCommand lst = throwError $ "Parser: Error at parsing complex command before: " ++ (show $ take 10 lst)

readComplexParameters :: [Token] -> Either String ([ASTModel],[Token])
readComplexParameters (Operator "[" : t) = complexParametersHelper t (Operator "]")
readComplexParameters (BodyBegin : t) = complexParametersHelper t BodyEnd
readComplexParameters lst = return ([],lst)

complexParametersHelper :: [Token] -> Token -> Either String ([ASTModel],[Token])
complexParametersHelper lst stop = do
    tmp <- parse' lst stop
    if last (fst tmp) == Empty
        then do
            tmp2 <- readComplexParameters $ snd tmp
            return ((init $ fst tmp) ++ fst tmp2,snd tmp2)
        else
            throwError "Parser: Parameters for complex command not enclosed"

readInlineWithoutBody :: [Token] -> String -> [ASTModel] -> [[ASTModel]] -> Either String (ASTModel,[Token])
readInlineWithoutBody [] name _ _ = throwError $ "Parser: Error at parsing command: " ++ name ++ "; Probably not enough bodies"
readInlineWithoutBody (MyNum val : t) name par lst = general val t name par lst (\y -> MN [y]) MyNum
readInlineWithoutBody (Operator val : t) name par lst = general val t name par lst (\y -> ASTOperator [y]) Operator
readInlineWithoutBody (MyStr val : t) name par lst = general val t name par lst Variable MyStr
readInlineWithoutBody (Command val : t) name par lst = do
    tmp <- parseCommand val t
    if fst tmp == BodylessCommand val
        then
        if 1 == (commandsArity ! name - length lst)
            then return (InlineCommand val par (lst ++ [[fst tmp]]),t)
            else readInlineWithoutBody t name par (lst ++ [[fst tmp]])
        else throwError $ "Parser: Cannot parse complicated command without braces: " ++ name

general :: String -> [Token] -> String -> [ASTModel] -> [[ASTModel]] -> (Char -> ASTModel) -> (String -> Token) -> Either String (ASTModel,[Token])
general tokenValue remainingTokens name par lst fun type'
    | length tokenValue > neededLength =
        let
            tmp = take neededLength tokenValue
        in
            return (InlineCommand name par (lst ++ makeOneElementBodies tmp fun),type' (drop neededLength tokenValue) : remainingTokens)
    | length tokenValue == neededLength = return (InlineCommand name par (lst ++ makeOneElementBodies tokenValue fun),remainingTokens)
    | otherwise = readInlineWithoutBody remainingTokens name par (lst ++ makeOneElementBodies tokenValue fun)
    where neededLength = commandsArity ! name - length lst

makeOneElementBodies :: String -> (Char -> ASTModel)  -> [[ASTModel]]
makeOneElementBodies ([h]) fun = [[fun h]]
makeOneElementBodies (h:t) fun = [fun h] : makeOneElementBodies t fun

readSup :: String -> [Token] -> Either String (ASTModel,[Token])
readSup _ lst = readSupOrSub lst ASTSup

readSub :: String -> [Token] -> Either String (ASTModel,[Token])
readSub _ lst = readSupOrSub lst ASTSub

readSupOrSub :: [Token] -> ([ASTModel] -> ASTModel) -> Either String (ASTModel,[Token])
readSupOrSub (BodyBegin:t) type' = do
    tmp <- parse' t BodyEnd
    if snd tmp == []
        then
            if last (fst tmp) == Empty
                then return (type' $ init $ fst tmp,snd tmp)
                else throwError $ "Parser: Body for subscript or superscript is not closed"
        else return (type' $ fst tmp,snd tmp)
--    if BodyEnd `elem` t
--        then
--            parse' t BodyEnd >>= (\x -> return (type' $ fst x,snd x))
--        else throwError $ "Parser: Body for subscript or superscript is not closed"

readSupOrSub (Command name : t) type' = parseCommand name t >>= (\x -> return (type' [fst x],snd x))
readSupOrSub (MyStr (h:[]) : t) type' = parse' [MyStr [h]] End >>= (\x -> return (type' $ fst x,t))
readSupOrSub (MyNum (h:[]) : t) type' = parse' [MyNum [h]] End >>= (\x -> return (type' $ fst x,t))
readSupOrSub (MyStr (h:tl) : t) type' = parse' [MyStr [h]] End >>= (\x -> return (type' $ fst x,MyStr tl : t))
readSupOrSub (MyNum (h:tl) : t) type' = parse' [MyNum [h]] End >>= (\x -> return (type' $ fst x,MyNum tl : t))
readSupOrSub (h:t) type' = parse' [h] End >>= (\x -> return (type' $ fst x,t))
readSupOrSub lst type' = throwError $ "Parser: Error at parsing " ++ show (type' []) ++ " before" ++ show (take 10 lst)