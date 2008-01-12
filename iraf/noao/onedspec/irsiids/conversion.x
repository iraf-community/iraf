define	MAX_CHARS	256


# ASCII_TO_EBCDIC --  Vector procedure to convert ASCII characters to EBCDIC
# characters using the lookup table atoe.

procedure ascii_to_ebcdic (inbuffer, outbuffer, nchars)

char	inbuffer[ARB]
short	outbuffer[ARB], atoe[MAX_CHARS]
int	l, nchars

data (atoe[l], l = 1, 8)      / 0b,   1b,   2b,   3b, '7' , '-' , '.' , '/'  / 
data (atoe[l], l = 9, 16)     /26b,   5b, '%' ,  13b,  14b,  15b,  16b,  17b /
data (atoe[l], l = 17, 24)    /20b,  21b,  22b,  23b, '<' , '=' , '2' , '&'  / 
data (atoe[l], l = 25, 32)    /30b,  31b, '?' , '\'',  34b,  35b,  36b,  37b / 
data (atoe[l], l = 33, 40)    /'@' , 'O' , 177b, '{' , '[' , 'l' , 'P' , '}' / 
data (atoe[l], l = 41, 48)    /'M' , ']' , '\\' , 'N' , 'k' , '`' , 'K' , 'a'/ 
data (atoe[l], l = 49, 56)    /360b, 361b, 362b, 363b, 364b, 365b, 366b, 367b/ 
data (atoe[l], l = 57, 64)    /370b, 371b, 'z' , '^' , 'L' , '~' , 'n' , 'o' / 
data (atoe[l], l = 65, 72)    /'|' , 301b, 302b, 303b, 304b, 305b, 306b, 307b/ 
data (atoe[l], l = 73, 80)    /310b, 311b, 321b, 322b, 323b, 324b, 325b, 326b/ 
data (atoe[l], l = 81, 88)    /327b, 330b, 331b, 342b, 343b, 344b, 345b, 346b/ 
data (atoe[l], l = 89, 96)    /347b, 350b, 351b, 'J' , 340b, 'Z' , '_' , 'm' / 
data (atoe[l], l = 97, 104)   /'y' , 201b, 202b, 203b, 204b, 205b, 206b, 207b/ 
data (atoe[l], l = 105, 112)  /210b, 211b, 221b, 222b, 223b, 224b, 225b, 226b/ 
data (atoe[l], l = 113, 120)  /227b, 230b, 231b, 242b, 243b, 244b, 245b, 246b/ 
data (atoe[l], l = 121, 128)  /247b, 250b, 251b, 300b, 'j' , 320b, 241b,   7b/ 
data (atoe[l], l = 129, 136)  /' ' , '!' , '"' , '#' , '$' ,  25b,   6b,  27b/ 
data (atoe[l], l = 137, 144)  /'(' , ')' , '*' , '+' , ',' ,  11b,  12b,  33b/ 
data (atoe[l], l = 145, 152)  /'0' , '1' ,  32b, '3' , '4' , '5' , '6' ,  10b/ 
data (atoe[l], l = 153, 160)  /'8' , '9' , ':' , ';' ,   4b,  24b, '>' , 341b/ 
data (atoe[l], l = 161, 168)  /'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' / 
data (atoe[l], l = 169, 176)  /'I' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' / 
data (atoe[l], l = 177, 184)  /'X' , 'Y' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' / 
data (atoe[l], l = 185, 192)  /'h' , 'i' , 'p' , 'q' , 'r' , 's' , 't' , 'u' / 
data (atoe[l], l = 193, 200)  /'v' , 'w' , 'x' , 200b, 212b, 213b, 214b, 215b/ 
data (atoe[l], l = 201, 208)  /216b, 217b, 220b, 232b, 233b, 234b, 235b, 236b/ 
data (atoe[l], l = 209, 216)  /237b, 240b, 252b, 253b, 254b, 255b, 256b, 257b/ 
data (atoe[l], l = 217, 224)  /260b, 261b, 262b, 263b, 264b, 265b, 266b, 267b/ 
data (atoe[l], l = 225, 232)  /270b, 271b, 272b, 273b, 274b, 275b, 276b, 277b/ 
data (atoe[l], l = 233, 240)  /312b, 313b, 314b, 315b, 316b, 317b, 332b, 333b/ 
data (atoe[l], l = 241, 248)  /334b, 335b, 336b, 337b, 352b, 353b, 354b, 355b/ 
data (atoe[l], l = 249, 256)  /356b, 357b, 372b, 373b, 374b, 375b, 376b, 377b/

begin
	call alutcs (inbuffer, outbuffer, nchars, atoe)
end

# EBCDIC_TO_ASCII -- Vector procedure to convert EBCDIC characters to ASCII
# characters.

procedure ebcdic_to_ascii (inbuffer, outbuffer, nchars)

char	outbuffer[ARB]
short	inbuffer[ARB], etoa[MAX_CHARS]
int	l, nchars

data (etoa[l], l = 1, 8)      / 0b,   1b,   2b,   3b, 234b,  11b, 206b, 177b / 
data (etoa[l], l = 9, 16)     /227b, 215b, 216b,  13b,  14b,  15b,  16b,  17b/ 
data (etoa[l], l = 17, 24)    /20b,  21b,  22b,  23b, 235b, 205b,  10b, 207b / 
data (etoa[l], l = 25, 32)    /30b,  31b, 222b, 217b,  34b,  35b,  36b,  37b / 
data (etoa[l], l = 33, 40)    /200b, 201b, 202b, 203b, 204b,  12b,  27b,  33b/ 
data (etoa[l], l = 41, 48)    /210b, 211b, 212b, 213b, 214b,   5b,   6b,   7b/ 
data (etoa[l], l = 49, 56)    /220b, 221b,  26b, 223b, 224b, 225b, 226b,   4b/ 
data (etoa[l], l = 57, 64)    /230b, 231b, 232b, 233b,  24b,  25b, 236b,  32b/ 
data (etoa[l], l = 65, 72)    /' ' , 240b, 241b, 242b, 243b, 244b, 245b, 246b/ 
data (etoa[l], l = 73, 80)    /247b, 250b, '[' , '.' , '<' , '(' , '+' , '!' / 
data (etoa[l], l = 81, 88)    /'&' , 251b, 252b, 253b, 254b, 255b, 256b, 257b/ 
data (etoa[l], l = 89, 96)    /260b, 261b, ']' , '$' , '*' , ')' , ';' , '^' / 
data (etoa[l], l = 97, 104)   /'-' , '/' , 262b, 263b, 264b, 265b, 266b, 267b/ 
data (etoa[l], l = 105, 112)  /270b, 271b, '|' , ',' , '%' , '_' , '>' , '?' / 
data (etoa[l], l = 113, 120)  /272b, 273b, 274b, 275b, 276b, 277b, 300b, 301b/ 
data (etoa[l], l = 121, 128)  /302b, '`' , ':' , '#' , '@' , '\'' , '=' , '"'/ 
data (etoa[l], l = 129, 136)  /303b, 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' / 
data (etoa[l], l = 137, 144)  /'h' , 'i' , 304b, 305b, 306b, 307b, 310b, 311b/ 
data (etoa[l], l = 145, 152)  /312b, 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' / 
data (etoa[l], l = 153, 160)  /'q' , 'r' , 313b, 314b, 315b, 316b, 317b, 320b/ 
data (etoa[l], l = 161, 168)  /321b, '~' , 's' , 't' , 'u' , 'v' , 'w' , 'x' / 
data (etoa[l], l = 169, 176)  /'y' , 'z' , 322b, 323b, 324b, 325b, 326b, 327b/ 
data (etoa[l], l = 177, 184)  /330b, 331b, 332b, 333b, 334b, 335b, 336b, 337b/ 
data (etoa[l], l = 185, 192)  /340b, 341b, 342b, 343b, 344b, 345b, 346b, 347b/ 
data (etoa[l], l = 193, 200)  /'{' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' / 
data (etoa[l], l = 201, 208)  /'H' , 'I' , 350b, 351b, 352b, 353b, 354b, 355b/ 
data (etoa[l], l = 209, 216)  /'}' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' / 
data (etoa[l], l = 217, 224)  /'Q' , 'R' , 356b, 357b, 360b, 361b, 362b, 363b/ 
data (etoa[l], l = 225, 232)  /'\\', 237b, 'S' , 'T' , 'U' , 'V' , 'W' , 'X' / 
data (etoa[l], l = 233, 240)  /'Y' , 'Z' , 364b, 365b, 366b, 367b, 370b, 371b/ 
data (etoa[l], l = 241, 248)  /'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' / 
data (etoa[l], l = 249, 256)  /'8' , '9' , 372b, 373b, 374b, 375b, 376b, 377b/

begin
	call alutsc (inbuffer, outbuffer, nchars, etoa)
end

# IBM_TO_ASCII -- Vector procedure for converting IBM characters to ASCII
# characters.

procedure ibm_to_ascii (inbuffer, outbuffer, nchars)

char	outbuffer[ARB]
short	inbuffer[ARB], ibmtoa[MAX_CHARS]
int	l, nchars

data (ibmtoa[l], l = 1, 8)     /0b,   1b,   2b,   3b, 234b,  11b, 206b, 177b  /
data (ibmtoa[l], l = 9, 16)    /1227b, 215b, 216b,  13b,  14b,  15b,  16b, 17b/ 
data (ibmtoa[l], l = 17, 24)   /20b,  21b,  22b,  23b, 235b, 205b,  10b, 207b / 
data (ibmtoa[l], l = 25, 32)   /30b,  31b, 222b, 217b,  34b,  35b,  36b,  37b / 
data (ibmtoa[l], l = 33, 40)   /200b, 201b, 202b, 203b, 204b,  12b,  27b,  33b/ 
data (ibmtoa[l], l = 41, 48)   /210b, 211b, 212b, 213b, 214b,   5b,   6b,   7b/ 
data (ibmtoa[l], l = 49, 56)   /220b, 221b,  26b, 223b, 224b, 225b, 226b,   4b/ 
data (ibmtoa[l], l = 57, 64)   /230b, 231b, 232b, 233b,  24b,  25b, 236b,  32b/ 
data (ibmtoa[l], l = 65, 72)   /' ' , 240b, 241b, 242b, 243b, 244b, 245b, 246b/ 
data (ibmtoa[l], l = 73, 80)   /247b, 250b,   0b, '.' , '<' , '(' , '+' , '|' / 
data (ibmtoa[l], l = 81, 88)   /'&' , 251b, 252b, 253b, 254b, 255b, 256b, 257b/ 
data (ibmtoa[l], l = 89, 96)   /260b, 261b, '!' , '$' , '*' , ')' , ';' , '^' / 
data (ibmtoa[l], l = 97, 104)  /'-' , '/' , 262b, 263b, 264b, 265b, 266b, 267b/ 
data (ibmtoa[l], l = 105,112)  /270b, 271b,   0b, ',' , '%' , '_' , '>' , '?' / 
data (ibmtoa[l], l = 113, 120) /272b, 273b, 274b, 275b, 276b, 277b, 300b, 301b/ 
data (ibmtoa[l], l = 121, 128) /302b, '`' , ':' , '#' , '@' , '\'' , '=' , '"'/ 
data (ibmtoa[l], l = 129, 136) /303b, 'a' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' / 
data (ibmtoa[l], l = 137, 144) /'h' , 'i' , 304b, 305b, 306b, 307b, 310b, 311b/ 
data (ibmtoa[l], l = 145, 152) /312b, 'j' , 'k' , 'l' , 'm' , 'n' , 'o' , 'p' / 
data (ibmtoa[l], l = 153, 160) /'q' , 'r' , 313b, 314b, 315b, 316b, 317b, 320b/ 
data (ibmtoa[l], l = 161, 168) /321b, '~' , 's' , 't' , 'u' , 'v' , 'w' , 'x' / 
data (ibmtoa[l], l = 169, 176) /'y' , 'z' , 322b, 323b, 324b, 325b, 326b, 327b/ 
data (ibmtoa[l], l = 177, 184) /330b, 331b, 332b, 333b, 334b, 335b, 336b, 337b/ 
data (ibmtoa[l], l = 185, 192) /340b, 341b, 342b, 343b, 344b, 345b, 346b, 347b/ 
data (ibmtoa[l], l = 193, 200) /'{' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' / 
data (ibmtoa[l], l = 201, 208) /'H' , 'I' , 350b, 351b, 352b, 353b, 354b, 355b/ 
data (ibmtoa[l], l = 209, 216) /'}' , 'J' , 'K' , 'L' , 'M' , 'N' , 'O' , 'P' / 
data (ibmtoa[l], l = 217, 224) /'Q' , 'R' , 356b, 357b, 360b, 361b, 362b, 363b/ 
data (ibmtoa[l], l = 225, 232) /'\\', 237b, 'S' , 'T' , 'U' , 'V' , 'W' , 'X' / 
data (ibmtoa[l], l = 233, 240) /'Y' , 'Z' , 364b, 365b, 366b, 367b, 370b, 371b/ 
data (ibmtoa[l], l = 241, 248) /'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' / 
data (ibmtoa[l], l = 249, 256) /'8' , '9' , 372b, 373b, 374b, 375b, 376b, 377b /

begin
	call alutsc (inbuffer, outbuffer, nchars, ibmtoa)
end

# Vector procedure to convert ASCII characters to ibm characters

procedure ascii_to_ibm (inbuffer, outbuffer, nchars)

char	inbuffer[ARB]
short	outbuffer[ARB], atoibm[MAX_CHARS]
int	l, nchars

data (atoibm[l], l = 1, 8)     /0b,   1b,   2b,   3b, '7' , '-' , '.' , '/'   / 
data (atoibm[l], l = 9, 16)    /26b,   5b, '%' ,  13b,  14b,  15b,  16b,  17b / 
data (atoibm[l], l = 17, 24)   /20b,  21b,  22b,  23b, '<' , '=' , '2' , '&'  / 
data (atoibm[l], l = 25, 32)   /30b,  31b, '?' , '\'',  34b,  35b,  36b,  37b / 
data (atoibm[l], l = 33, 40)   /'@' , 'Z' , 177b, '{' , '[' , 'l' , 'P' , '}' / 
data (atoibm[l], l = 41, 48)   /'M' , ']' , '\\', 'N' , 'k' , '`' , 'K' , 'a' / 
data (atoibm[l], l = 49, 56)   /360b, 361b, 362b, 363b, 364b, 365b, 366b, 367b/ 
data (atoibm[l], l = 57, 64)   /370b, 371b, 'z' , '^' , 'L' , '~' , 'n' , 'o' / 
data (atoibm[l], l = 65, 72)   /'|' , 301b, 302b, 303b, 304b, 305b, 306b, 307b/ 
data (atoibm[l], l = 73, 80)   /310b, 311b, 321b, 322b, 323b, 324b, 325b, 326b/ 
data (atoibm[l], l = 81, 88)   /327b, 330b, 331b, 342b, 343b, 344b, 345b, 346b/ 
data (atoibm[l], l = 89, 96)   /347b, 350b, 351b, 255b, 340b, 275b, '_' , 'm' / 
data (atoibm[l], l = 97, 104)  /'y' , 201b, 202b, 203b, 204b, 205b, 206b, 207b/ 
data (atoibm[l], l = 105, 112) /210b, 211b, 221b, 222b, 223b, 224b, 225b, 226b/ 
data (atoibm[l], l = 113, 120) /227b, 230b, 231b, 242b, 243b, 244b, 245b, 246b/ 
data (atoibm[l], l = 121, 128) /247b, 250b, 251b, 300b, 'O' , 320b, 241b,   7b/ 
data (atoibm[l], l = 129, 136) /' ' , '!' , '"' , '#' , '$' ,  25b,   6b,  27b/ 
data (atoibm[l], l = 137, 144) /'(' , ')' , '*' , '+' , ',' ,  11b,  12b,  33b/ 
data (atoibm[l], l = 145, 152) /'0' , '1' ,  32b, '3' , '4' , '5' , '6' ,  10b/ 
data (atoibm[l], l = 153, 160) /'8' , '9' , ':' , ';' ,   4b,  24b, '>' , 341b/ 
data (atoibm[l], l = 161, 168) /'A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'G' , 'H' / 
data (atoibm[l], l = 169, 176) /'I' , 'Q' , 'R' , 'S' , 'T' , 'U' , 'V' , 'W' / 
data (atoibm[l], l = 177, 184) /'X' , 'Y' , 'b' , 'c' , 'd' , 'e' , 'f' , 'g' / 
data (atoibm[l], l = 185, 192) /'h' , 'i' , 'p' , 'q' , 'r' , 's' , 't' , 'u' / 
data (atoibm[l], l = 193, 200) /'v' , 'w' , 'x' , 200b, 212b, 213b, 214b, 215b/ 
data (atoibm[l], l = 201, 208) /216b, 217b, 220b, 232b, 233b, 234b, 235b, 236b/ 
data (atoibm[l], l = 209, 216) /237b, 240b, 252b, 253b, 254b, 255b, 256b, 257b/ 
data (atoibm[l], l = 217, 224) /260b, 261b, 262b, 263b, 264b, 265b, 266b, 267b/ 
data (atoibm[l], l = 225, 232) /270b, 271b, 272b, 273b, 274b, 275b, 276b, 277b/ 
data (atoibm[l], l = 233, 240) /312b, 313b, 314b, 315b, 316b, 317b, 332b, 333b/ 
data (atoibm[l], l = 241, 248) /334b, 335b, 336b, 337b, 352b, 353b, 354b, 355b/ 
data (atoibm[l], l = 249, 256) /356b, 357b, 372b, 373b, 374b, 375b, 376b, 377b/

begin
	call alutcs (inbuffer, outbuffer, nchars, atoibm)
end

# ALUTSC -- Vector operator to map one set of characters to another using a
# lookup table.

procedure alutsc (a, b, nchars, lut)

char	b[nchars]
int	nchars, i
short	a[nchars], lut[ARB]

begin
	do i = 1, nchars, 1
	    b[i] = lut[a[i] + 1]
end

# ALUTCS -- Vector operator to map one set of characters to another using
# a lookup table.

procedure alutcs (a, b, nchars, lut)

char	a[nchars]
int	nchars, i
short	b[nchars], lut[ARB]

begin
	do i = nchars, 1, -1
	    b[i] = lut[a[i] + 1]
end
