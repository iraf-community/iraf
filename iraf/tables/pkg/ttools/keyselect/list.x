include "keyselect.h"

#* HISTORY *
#* B.Simon	12-Mar-92	Original

# BRK_LIST -- Retrieve a string from the list

int procedure brk_list (list, ic, sep, str, maxch)

char	list[ARB]	# i: list of items
int	ic		# u: index into list
char	sep		# i: character separating strings in the list
char	str[ARB]	# o: output string
int	maxch		# i: maximum length of output string
#--
int	jc

begin
	# Copy characters into output string until separation character
	# or end of list is found

	for (jc = 1; jc <= maxch; jc = jc + 1) {
	    str[jc] = list[ic]
	    ic = ic + 1

	    if (str[jc] == sep) {
		break
	    } else if (str[jc] == EOS) {
		ic = ic - 1	# back up to EOS character
		break
	    }
	}

	str[jc] = EOS
	return (jc-1)
end

# CNT_LIST -- Count the number of items in a list

int procedure cnt_list (list)

char	list[ARB]	# i: list of items
#--
int	ic, count

begin
	# Number of items is number of separation characters plus one

	count = 1
	for (ic = 1; list[ic] != EOS; ic = ic + 1) {
	    if (list[ic] == SEP_CHAR)
		count = count + 1
	}

	return (count)
end

# FMT_LIST -- Format a list into canonical form

procedure fmt_list (list)

char	list[ARB]	# u: list of keyword names
#--
bool	tween
int	ic, jc

begin
	jc = 1
	tween = true

	# Eliminate consecutive separation characters between list items

	for (ic = 1; list[ic] != EOS; ic = ic + 1) {
	    if (IS_SEP(list[ic])) {
		if (! tween) {
		    tween = true
		    list[jc] = SEP_CHAR
		    jc = jc + 1
		}

	    } else {
		tween = false
		if (jc < ic)
		    list[jc] = list[ic]
		jc = jc + 1
	    }
	}

	# Eliminate trailing separation character

	if (! tween || jc == 1) {
	    list[jc] = EOS
	} else {
	    list[jc-1] = EOS
	}

end

# RD_LIST -- Read values from a file into a list

procedure rd_list (fname, list, maxch)

char	fname[ARB]	# i: file containing list
char	list[ARB]	# o: output list
int	maxch		# i: maximum length of list
#--
int	fd, ic, nc

int	open(), getline()

begin
	# Concatenate contents of the file into a single long string
	# while preserving the newlines between them

	fd = open (fname, READ_ONLY, TEXT_FILE)

	for (ic = 1; ic < maxch; ic = ic + nc) {
	    nc = getline (fd, list[ic])
	    if (nc <= 0)
		break
	}

	list[ic] = EOS
	call close (fd)

end

# SEP_LIST -- Separate list into keywords and table column names

procedure sep_list (list, keywords, columns, maxch)

char	list[ARB]	# i: combined list of columns and keywords
char	keywords[ARB]	# o: list of header keyword names
char	columns[ARB]	# o: list of table column names
int	maxch		# i: declared length of output strings
#--
char	eq, sep, cat
int	ic, jc, kc, mc, nc
pointer	sp, word, key, col

data	eq	/ ASSIGN_CHAR /
data	sep	/ SEP_CHAR /
data	cat	/ CONCAT_CHAR /

string	nolist  "List of header keywords is empty. No table created."

int	stridx(), gstrcpy(), brk_list()

begin
	call smark(sp)
	call salloc (word, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (col, SZ_LINE, TY_CHAR)

	ic = 1
	jc = 1
	kc = 1

	# Extract the next item from the combined list of columns and keywords

	while (brk_list (list, ic, sep, Memc[word], SZ_LINE) > 0) {

	    # Break the item into the column and keyword names
	    # If both are not given in the item assume they are the same

	    nc = stridx (eq, Memc[word])
	    if (nc > 0) {
		Memc[word+nc-1] = EOS
		call strcpy (Memc[word], Memc[col], SZ_LINE)
		call strcpy (Memc[word+nc], Memc[key], SZ_LINE)

	    } else {
		call strcpy (Memc[word], Memc[col], SZ_LINE)
		call strcpy (Memc[word], Memc[key], SZ_LINE)

		# Translate keyword names into their default column names
		# and substitute underscores for the concatenation char

		if (Memc[col] == '$') {
		    call name_keyword (Memc[col], Memc[col], SZ_LINE)

		} else {
		    repeat {
			mc = stridx (cat, Memc[col])
			if (mc == 0)
			    break

			Memc[col+mc-1] = '_'
		    }
		}
	    }

	    # Append  keyword and column name to output string

	    jc = jc + gstrcpy (Memc[key], keywords[jc], maxch-jc)
	    keywords[jc] = SEP_CHAR
	    jc = jc + 1

	    kc = kc + gstrcpy (Memc[col], columns[kc], maxch-kc)
	    columns[kc] = SEP_CHAR
	    kc = kc + 1
	}

	# Exit with error if either list is empty

	if (jc == 1 || kc == 1)
	    call error (1, nolist)

	# Eliminate trailing separation character

	keywords[jc-1] = EOS
	columns[kc-1] = EOS

	call sfree(sp)
end
