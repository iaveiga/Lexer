import System.IO
import Data.List.Split
import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.String

main = do
	putStrLn "++++++ Lexer 0.001 ++++++"
	putStrLn "Abriendo Archivo"
	dato <- readFile "C:/codigo.c"
	ls_lexemes <- splitOn " " dato
	guardar (listaTuplas ls_lexemes)
	putStr "Exito"

	
	
--listaTuplas::[String] -> [(String,String)]
listaTuplas ls = [(lexeme) ++ (" , ") ++ (asignar lexeme) ++ (" \n") | lexeme<-ls]
	
guardar ls= do
	writeFile "archivo.csv" (foldr (++) " " ls)
	 
asignar :: String -> String	
asignar lexeme 
	| en_lista lexeme ["+","-","*","/","%","^"] = "OPERATOR"
	| en_lista lexeme [";"] = "END_STATEMENT"
	| en_lista lexeme ["&"] = "MEMORY_OPERATOR"
	| en_lista lexeme [",", ";", ":", "(", ")", "{", "}"] = "PUNCTUATION"
	| en_lista lexeme ["auto", "break", "case", "const", "continue", "default", "do", "else", "enum" ] = "RESERVED_WORD"
	| en_lista lexeme ["extern", "for", "goto", "if", "register", "return", "signed", "sizeof"] = "RESERVED_WORD"
	| en_lista lexeme ["static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"] = "RESERVED_WORD"
	| en_lista lexeme ["int", "float", "char", "double", "long"] = "PRIMITIVE_TYPE"
	| en_lista lexeme ["EOF", "NULL", "BUFSIZ"] = "MACROS"
	| en_lista lexeme ["remove", "rename", "tmpfile", "tmpnam", "fclose", "fflush", "fopen", "freopen", "setbuf", "setvbuf", "fprintf"] = "FUNTIONS"
	| en_lista lexeme ["scanf", "snprintf", "sprintf", "sscanf", "vfprintf", "vfscanf", "fscanf", "printf", "vprintf", "vscanf"] = "FUNCTIONS"
	| en_lista lexeme ["vsnprintf", "vsprintf", "vsscanf", "fgetc", "fgets", "fputc", "fputs", "getc", "getchar", "gets", "putc", "putchar", "puts"] = "FUNCTIONS"
	| en_lista lexeme ["ungetc", "fread", "fwrite", "fgetpos", "fseek", "fsetpos", "ftell", "rewind", "clearerr", "feof", "ferror", "perror"] = "FUNCTIONS"
	| en_lista lexeme ["time", "rand", "srand", "strcmp","strcat","strcpy","strlen","isalpha", "isalnum", "isdigit", "islower", "isupper"] = "FUNCTIONS"
	| en_lista lexeme ["tolower","toupper", "sizeof", "malloc"] = "FUNCTIONS"
	| lexeme =~ "\\`[-+]?[0-9]*\\'" :: Bool = "INTEGER"
	| lexeme =~ "\\`[-+]?[0-9]*.?[0-9]*\\`" :: Bool = "REAL"
	| lexeme =~ "\"(\\.|[^\"\\])*\"" :: Bool = "STRING"
	| lexeme =~ "\\`[0-9]*\\`" :: Bool = "REAL"
	| lexeme =~ "\\`[A-Za-z0-9]*\\'" :: Bool = "IDENTIFIER"
	| otherwise = "UNKNOWN_TOKEN"

--Si un elemento (String) está en una lista de String.		
en_lista :: String -> [String] -> Bool
en_lista _ [] = False
en_lista lexeme (x:xs) = do
	if map toLower lexeme == x then
		True
	else
		en_lista lexeme xs