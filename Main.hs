import System.IO
import Data.List.Split
import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.String

main = do
	putStr "S"


listaTuplas::[String] -> [(String,String)]
listaTuplas ls = [(lexeme,asignar lexeme) | lexeme<-ls]
	
asignar :: String -> String	
asignar lexeme 
	| en_lista lexeme ["+","-","*","/","%","^"] = "OPERATOR"
	| en_lista lexeme ["auto", "break", "case", "const", "continue", "default", "do", "else", "enum" ] = "RESERVED_WORD"
	| en_lista lexeme ["extern", "for", "goto", "if", "register", "return", "signed", "sizeof"] = "RESERVED_WORD"
	| en_lista lexeme ["static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"] = "RESERVED_WORD"
	| en_lista lexeme ["int", "float", "char", "double", "long"] = "PRIMITIVE_TYPE"
	| en_lista lexeme ["EOF", "NULL", "BUFSIZ"] = "MACROS"
	| en_lista lexeme ["remove", "rename", "tmpfile", "tmpnam", "fclose", "fflush", "fopen", "freopen", "setbuf", "setvbuf", "fprintf", "fscanf", "printf", "scanf", "snprintf", "sprintf", "sscanf", "vfprintf", "vfscanf", "vprintf", "vscanf", "vsnprintf", "vsprintf", "vsscanf", "fgetc", "fgets", "fputc", "fputs", "getc", "getchar", "gets", "putc", "putchar", "puts", "ungetc", "fread", "fwrite", "fgetpos", "fseek", "fsetpos", "ftell", "rewind", "clearerr", "feof", "ferror", "perror"] = "FUNTIONS"
	| lexeme =~ "\\`[A-Zaz0-9]*\\'" :: Bool = "IDENTIFIER"
	| lexeme =~ "\\`[+-]?[0-9]*\\`" :: Bool = "INTEGER"
	| lexeme =~ "\\`[+-]?[0.0-9.0]*\\`" :: Bool = "INTEGER"
	| lexeme =~ "\\`[0-9]*\\`" :: Bool = "REAL"
	| otherwise = "ERROR"

--Si un elemento (String) está en una lista de String.		
en_lista :: String -> [String] -> Bool
en_lista _ [] = False
en_lista lexeme (x:xs) = do
	if map toLower lexeme == x then
		True
	else
		en_lista lexeme xs