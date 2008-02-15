# ME_SETEXPR -- Set the pixel mask region to the appropriate number.

procedure me_setexpr (expr, pmim, pregno, pregval, verbose)

char	expr[ARB]		#I the region expression 
pointer	pmim			#I the pixelmask image descriptor
int	pregno			#I the current region number
int	pregval			#I the current region value
bool	verbose			#I print status messages ?

pointer	sp, chregval
int	nchars, stat
int	itoc(), me_rgmask()

begin
	call smark (sp)
	call salloc (chregval, SZ_FNAME, TY_CHAR)
	nchars = itoc (pregval, Memc[chregval], SZ_FNAME)
	if (nchars <= 0) {
	    if (verbose) {
		call printf ("    Region value %d cannot be encoded\n")
		    call pargi (pregval)
	    }
	} else {
	    stat = me_rgmask (expr, Memc[chregval], "p", pmim)
	    if (stat == ERR) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    }
	}

	call sfree (sp)
end

