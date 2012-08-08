procedure bases (i)

int     i		{prompt="Integer for base conversion"}

string	nbyte = "0"	{prompt="Number of bytes of precision", enum="0|1|2|4"}
bool	verbose = yes	{prompt="Print labels for the columns?"}

begin
        int     ii, ui, nibble[8], nnibble, nn, ndigits, index
	bool	is_negative, is_byte, is_short, is_ascii, is_ubyte, is_ushort

        ii = i

	is_negative	= (ii < 0)

	is_ascii	= (ii <= 07fx && ! is_negative)
	is_ubyte	= (ii < 100x && ii >= -100x)
	is_ushort	= (ii < 10000x && ii >= -10000x)

	is_byte		= (abs(ii) <= 07fx)
	is_short	= (abs(ii) <= 07fffx)

	if (nbyte == "0") {
	    if (is_ubyte || is_byte)
		nnibble = 2
	    else if (is_ushort || is_short)
		nnibble = 4
	    else
		nnibble = 8
	} else if (nbyte == "1") {
	    nnibble = 2
	} else if (nbyte == "2") {
	    nnibble = 4
	} else if (nbyte == "4") {
	    nnibble = 8
	}

	# explicitly convert to 2's complement for the bytes or shorts
	ui = ii
#	if (is_negative && nnibble != 8) {
	if (is_negative) {
	    ndigits = 4*nnibble - 1
	    ui = 2**ndigits + 2**ndigits - abs(ii)
	}

	nn = ui
	for (index=nnibble; index>=1; index-=1) {
	    nibble[index] = max (0x, min ( 0ffx, mod(nn,10x)))
	    nn = nn / 10x
	}

	if (verbose) {
	    if (nnibble == 2)
		printf (" dec  hex  oct  ")
	    else if (nnibble == 4)
		printf ("  dec    hex    octal  ")
	    else
		printf ("      dec       hex        octal     ")

	    for (index=1; index<=(nnibble/2); index+=1)
		printf (" 7654 3210")

	    if (is_negative)
		printf (" unsigned")
	    else if (is_ascii)
		printf ("  ascii")

	    printf ("\n")
	}

	if (nnibble == 2)
	    printf ("%4d  %02xx  %03ob ", ii, ui, ui)
	else if (nnibble == 4)
	    printf ("%6d  %04xx  %06ob ", ii, ui, ui)
	else
	    printf ("%11d  %08xx  %011ob ", ii, ui, ui)

	for (index=1; index<=nnibble; index+=1)
	    printf (" %04r2", nibble[index])

	if (is_negative)
	    printf ("  %d", ui)
	else if (is_ascii)
	    printf ("  %3c", ii)

	printf ("\n")
end
