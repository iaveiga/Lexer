import System.IO
import Data.List.Split
import Data.Char
import Text.Regex.Posix

main = do
	putStr "S"
	
asignar :: String -> String	
asignar lexeme 
	| en_lista lexeme ["+","-","*","/","%","^"] = "OPERATOR"
	| en_lista lexeme ["auto", "break", "case", "const", "continue", "default", "do", "else", "enum" ] = "RESERVED_WORD"
	| en_lista lexeme ["extern", "for", "goto", "if", "register", "return", "signed", "sizeof"] = "RESERVED_WORD"
	| en_lista lexeme ["static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"] = "RESERVED_WORD"
	| en_lista lexeme ["int", "float", "char", "double", "long"] = "PRIMITIVE_TYPE"
	| otherwise = "ERROR"

--Si un elemento (String) está en una lista de String.		
en_lista :: String -> [String] -> Bool
en_lista _ [] = False
en_lista lexeme (x:xs) = do
	if map toLower lexeme == x then
		True
	else
		en_lista lexeme xs