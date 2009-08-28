# DP_APHEADER -- Copy the text database column headers to another file.
# Consider placing this simple routine in the pttables library at some point.

procedure dp_apheader (in, out)

int	in		# input file descriptor
int	out		# output file descriptor

size_t	sz_val
pointer	sp, line
long	l_val
int	getline()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)

	while (getline (in, Memc[line]) != EOF) {
	    if (Memc[line] != '#')
		break
	    if (Memc[line+1] == 'N')
		break
	    call putline (out, Memc[line])
	}

	l_val = BOF
	call seek (in, l_val)

	call sfree (sp)
end


# DP_APBANNER -- Copy the text database keyword definitions to another file.
# Consider placing this simple routine in the pttables library at some point.


procedure dp_apbanner (in, out)

int	in		# input file descriptor
int	out		# output file descriptor

size_t	sz_val
pointer	sp, line
long	l_val
int	getline()

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (line, sz_val, TY_CHAR)

	while (getline (in, Memc[line]) != EOF) {
	    if (Memc[line] != '#')
		break
	    if (Memc[line+1] == 'K')
		next
	    call putline (out, Memc[line])
	}
	l_val = BOF
	call seek (in, l_val)

	call sfree (sp)
end
