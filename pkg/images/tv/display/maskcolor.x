include	<ctotok.h>

define	COLORS	"|black|white|red|green|blue|yellow|cyan|magenta|"
define	DEFCOLOR	 203


# MASKCOLOR_MAP -- Create the mask colormap object.

pointer procedure maskcolor_map (colorstring)

char	colorstring		#I Color specification string
pointer	colors			#O Mask colormap object

int	i, j, ip, ncolors, token, lasttoken, maskval1, maskval2, color, offset
int	strdic(), ctoi()
pointer	sp, str

int	coltrans[8]
data	coltrans/202,203,204,205,206,207,208,209/

define	err_	10

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate memory for the colormap object.
	call malloc (colors, 4*10, TY_INT)

	# Initialize
	ncolors = 1
	maskval1 = INDEFI
	maskval2 = INDEFI
	color = DEFCOLOR
	offset = NO

	Memi[colors] = ncolors
	Memi[colors+2] = color
	Memi[colors+3] = offset

	# Parse the color specification.
	token = 0
	call sscan (colorstring)
	repeat {
	    lasttoken = token
	    call gargtok (token, Memc[str], SZ_LINE)
	    switch (token) {
	    case TOK_IDENTIFIER:
		call strlwr (Memc[str])
		i = strdic (Memc[str], Memc[str], SZ_LINE, COLORS)
		if (i == 0)
		    goto err_
		color = coltrans[i]
	    case TOK_NUMBER:
		if (lasttoken == TOK_NUMBER) {
		    if (Memc[str] != '-')
			goto err_
		    ip = 2
		    if (ctoi (Memc[str], ip, maskval2) == 0)
			goto err_
		} else {
		    if (Memc[str] == '+') {
			offset = YES
			ip = 2
		    } else if (Memc[str] == '-') {
			offset = YES
			ip = 1
		    } else
			ip = 1
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		    maskval2 = color
		}
	    case TOK_OPERATOR:
		if (Memc[str] != '=' || lasttoken != TOK_NUMBER)
		    goto err_
		maskval1 = min (color, maskval2)
		maskval2 = max (color, maskval2)

		if (Memc[str+1] == '+') {
		    call gargtok (token, Memc[str+2], SZ_LINE)
		    offset = YES
		    ip = 3
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		} else if (Memc[str+1] == '-') {
		    call gargtok (token, Memc[str+2], SZ_LINE)
		    offset = YES
		    ip = 2
		    if (ctoi (Memc[str], ip, color) == 0)
			goto err_
		}
	    case TOK_PUNCTUATION, TOK_EOS:
		if (Memc[str] != ',' && Memc[str] != EOS)
		    goto err_
		if (!IS_INDEFI(maskval1)) {
		    do i = 2, ncolors {
			j = 4 * i - 4
			if (Memi[colors+j] == maskval1 &&
			    Memi[colors+j+1] == maskval2)
			    break
		    }
		    if (i > ncolors) {
			if (mod (ncolors, 10) == 0)
			    call realloc (colors, 4*(ncolors+10), TY_INT)
			ncolors = ncolors + 1
		    }
		    j = 4 * i - 4
		    Memi[colors+j] = maskval1
		    Memi[colors+j+1] = maskval2
		    Memi[colors+j+2] = color
		    Memi[colors+j+3] = offset
		} else {
		    Memi[colors+2] = color
		    Memi[colors+3] = offset
		}
		if (token == TOK_EOS)
		    break
		maskval1 = INDEFI
		maskval2 = INDEFI
		offset = NO
	    default:
		goto err_
	    }
	}

	Memi[colors] = ncolors
	call sfree (sp)
	return (colors)

err_
	call mfree (colors, TY_INT)
	call sfree (sp)
	call error (1, "Error in color specifications")
end


# MASKCOLOR_FREE -- Free the mask color object.

procedure maskcolor_free (colors)

pointer	colors			#I Mask colormap object

begin
	call mfree (colors, TY_INT)
end


# MASKCOLOR -- Return a color for a mask value.

int procedure maskcolor (colors, maskval)

pointer	colors			#I Mask colormap object
int	maskval			#I Mask value
int	color			#O Color value

int	i, j, offset

begin
	color = Memi[colors+2]
	offset = Memi[colors+3]
	do i = 2, Memi[colors] {
	    j = 4 * i - 4
	    if (maskval >= Memi[colors+j] && maskval <= Memi[colors+j+1]) {
		color = Memi[colors+j+2]
		offset = Memi[colors+j+3]
		break
	    }
	}

	if (offset == YES)
	    color = maskval + color
	return (max (0, color))
end
