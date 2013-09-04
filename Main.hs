import System.IO
import Data.List.Split
import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.String
import System.IO.Unsafe 

main = do
	putStrLn "++++++ Lexer 0.001 ++++++"
	putStrLn "Abriendo Archivo"
	analize
	putStr "Exito"

lexemes = do
	ls<- unsafePerformIO . readFile $ "codigo.c"
	return ls	

analize = guardar (listaTuplas (splitOn " " lexemes))
		
listaTuplas ls = [(lexeme) ++ (" , ") ++ (asignar lexeme) ++ (" \n") | lexeme<-ls]
	
guardar ls= do
	writeFile "lexemes.csv" (foldr (++) " " ls)
	 
asignar :: String -> String	
asignar lexeme 
	| en_lista lexeme ["+","-","*","/","%"] = "OPERATOR"
	| en_lista lexeme ["int", "float", "char", "double", "long"] = "PRIMITIVE_TYPE"
	| en_lista lexeme ["&"] = "MEMORY_OPERATOR"
	| en_lista lexeme ["="] = "ASSIGNAMENT"
	| en_lista lexeme ["=="] = "EQUAL_OPERATOR"
	| en_lista lexeme ["++" , "--"] = "INCREMENT-DECREMENT_OPERATOR"
	| en_lista lexeme ["&&" , "||", "!="] = "LOGICAL_OPERATOR"
	| en_lista lexeme ["<", "<=", ">", ">="] = "RELATIONAL_OPERATOR"
	| en_lista lexeme [",", ";", ":", "(", ")", "{", "}"] = "PUNCTUATION"
	| en_lista lexeme ["auto", "break", "case", "const", "continue", "default", "do", "else", "enum" ] = "RESERVED_WORD"
	| en_lista lexeme ["extern", "for", "goto", "if", "register", "return", "signed", "sizeof"] = "RESERVED_WORD"
	| en_lista lexeme ["static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"] = "RESERVED_WORD"
	| lexeme =~ "\\`[-+]?[0-9]*\\`" :: Bool = "INTEGER"
	| lexeme =~ "\\`[-+]?[0-9]*.?[0-9]*\\`" :: Bool = "REAL"
	| lexeme =~ "\\`[A-Za-z]*\\`" :: Bool = "IDENTIFIER"
	| lexeme =~ "\"\\.|[^\"\\]*\"" :: Bool = "STRING"
	| otherwise = "UNKNOWN_TOKEN"

en_lista :: String -> [String] -> Bool
en_lista _ [] = False
en_lista lexeme (x:xs) = do
	if map toLower lexeme == x then
		True
	else
		en_lista lexeme xs