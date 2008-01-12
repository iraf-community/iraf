define	SZ_KEY		8
define	SZ_VALUE	19
define	FMT_TAB		"%-20s"

task xxampl = t_xxampl

# T_XXAMPL -- Example program showing how to use the gflib functions

procedure t_xxampl ()

#--
pointer	input		# image name
pointer	keywords	# comma separated list of keywords

int	ic, igroup, ngroup
real	datamin, datamax
pointer	sp, im, key, value

int	word_fetch(), gf_gcount(), gf_map()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (keywords, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_KEY, TY_CHAR)
	call salloc (value, SZ_VALUE, TY_CHAR)

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("keywords", Memc[keywords], SZ_FNAME)

	# Transparently open an image of any type
	im = gf_map (Memc[input], READ_ONLY, NULL)

	# Get the number of groups in the image
	ngroup = gf_gcount (im)

	do igroup = 1, ngroup {
	    if (igroup > 1) {
		# Open each group after the first
		call gf_opengr (im, igroup, datamin, datamax, NULL)

	    } else {
		# Print a title first time through 
		ic = 1
		while (word_fetch (Memc[keywords], ic, Memc[key], SZ_KEY) > 0){
		    call printf (FMT_TAB)
		    call pargstr (Memc[key])
		}

		call printf ("\n\n")
	    }

	    # Print keyword values
	    ic = 1
	    while (word_fetch (Memc[keywords], ic, Memc[key], SZ_KEY) > 0) {
		iferr {
		    call imgstr (im, Memc[key], Memc[value], SZ_VALUE)
		} then {
		    Memc[value] = EOS
		}

		call printf (FMT_TAB)
		call pargstr (Memc[value])
	    }

	    call printf ("\n")
	}

	call sfree (sp)
end
