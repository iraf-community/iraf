# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imx.h"

define	DEBUG		FALSE
define	SZ_PREFIX	2


pointer procedure imx_preproc (template)

char	template[ARB]				#i input template string

pointer	exp, pre, out, op, list
char	file[SZ_PATHNAME]
char 	mods[SZ_LINE], fname[SZ_LINE]
int	i, j, osize, len, llen, nmods

define	output 	 {Memc[out+op]=$1;op=op+1}
define	outstr 	 {len=strlen($1);for(j=1;j<=len;j=j+1)output($1[j])}
define	outcomma {if(($1))output(',')}

pointer	imx_fnexpand (), imx_preproc_list()
pointer	fntopnb()
int	fntlenb(), fntgfnb(), strlen(), strsearch(), strncmp(), imx_split()

begin
	# First Pass: Do any filename expansion in the template, maintaining 
	# the '@' prefix and any modifiers.  The result is a comma-delimited 
	# list we process later to expand further.

	exp = imx_fnexpand (template)

	# Second Pass: Process the matched list to expand the '@' files and 
	# modifiers into a simple comma-delimited list the FNT interface 
	# will process.

	pre = imx_preproc_list (Memc[exp])

	# Third Pass: Handle concatenation in the filenames.
	if ((strncmp (Memc[pre],"http://",7) == 0) ||
	    (strncmp (Memc[pre],"file://",7) == 0)) {
	    osize = strlen (Memc[pre])
	    call calloc (out, osize, TY_CHAR)
	    call strcpy (Memc[pre], Memc[out], osize)

	} else if (strsearch(Memc[pre],"//") > 0 && 
	    strsearch(Memc[pre],".fits") > 0) {

	    # FIXME -- Need to handle the case of concatenation with 
	    # a MEF file.  Problem is, expanding the MEF requires we
	    # recursively call ourselves to expand the image so we
	    # need to do some restructuring.  For example,
	    #
	    #  foo // @mef.fits  ->  foomef.fits[1],foomef.fits[2], ....
	    #  @mef.fits // foo  ->  meffoo.fits[1],meffoo.fits[2], ....

	    call error (0, "Image expansion/concatenation not yet supported.")


	} else if (strsearch (Memc[pre], "//") > 0) {

            nmods = imx_split (Memc[pre], fname, mods, SZ_LINE)
	    list = fntopnb (fname, YES)
	    llen = fntlenb (list)

	    osize = strlen (Memc[pre])
	    call calloc (out, osize * 2, TY_CHAR)

	    op = 0
            for (i=0; i < llen; i=i+1) {
                call aclrc (file, SZ_PATHNAME)
                if (fntgfnb (list, file, SZ_PATHNAME) == EOF) 
                    break

		if ((op + strlen (file) + strlen (mods) + 3) >= osize) {
		    osize = osize + SZ_LINE
		    call realloc (out, osize, TY_CHAR)
		}

		# FIXME ???
		#outcomma (i > 0); output ('@'); outstr(file) ; outstr(mods)
		outcomma (i > 0); outstr(file) ; outstr(mods)
            }
	    output ('\0')
	    call fntclsb (list)

	} else {
	    osize = strlen (Memc[pre])
	    call calloc (out, osize, TY_CHAR)
	    call strcpy (Memc[pre], Memc[out], osize)
	}

	if (DEBUG) {
	    call eprintf ("pre exp = '%s'\n") ; call pargstr (Memc[exp])
	    call eprintf ("pre pre = '%s'\n") ; call pargstr (Memc[pre])
	    call eprintf ("pre out = '%s'\n") ; call pargstr (Memc[out])
	}

	call mfree (exp, TY_CHAR)		# clean up
	call mfree (pre, TY_CHAR)

	return (out)
end


# IMX_FNEXPAND -- Do any filename expansion in the template, maintaining the 
# '@' prefix and any modifiers.  The result is a comma-delimited list we 
# process later to expand further.

pointer procedure imx_fnexpand (template)

char	template[ARB]				#i input template string

pointer	elem, ep, op, out, listp, sz_out, op_start, op_end
int	i, j, ip, in, len, llen, nelem, fi, fo
char    prefix[SZ_PREFIX], fname[SZ_PATHNAME], mods[SZ_LINE]
char    left[SZ_PATHNAME], right[SZ_PATHNAME]
char    file[SZ_PATHNAME], cfname[SZ_PATHNAME], osfn[SZ_PATHNAME]

define	output {Memc[op]=$1;op=op+1}
define	outstr {len=strlen($1);for(j=1;j<=len;j=j+1)output($1[j])}

int	fntopnb(), fntgfnb(), fntlenb(), strlen(), stridxs(), strsearch()
int	imx_get_element(), strncmp(), stridx()

begin
	# Allocate an intial string buffer.
	call calloc (out, SZ_FNT, TY_CHAR)
	call calloc (elem, SZ_FNT, TY_CHAR)

	in = 1
	nelem = 0
	op = out
	op_start = out
	op_end = out + SZ_FNT - 1
	sz_out = SZ_FNT

	while (imx_get_element (template, in, Memc[elem], SZ_FNT) != EOS) {

	    ep = elem
	    nelem = nelem + 1
	    outcomma(nelem > 1)

	    call aclrc (prefix, SZ_PREFIX)
	    call aclrc (fname,  SZ_PATHNAME)
	    call aclrc (mods,   SZ_LINE)

	    # Gather any prefix '@' symbols.
	    if (Memc[elem] == '@') {
	        for (i=1; Memc[ep] == '@'; i=i+1) {
		    prefix[i] = Memc[ep]
		    ep = ep + 1
	        }
	    } else {
		ip = stridx ('@', Memc[elem])
		if (ip > 1) {
		  call strcpy (Memc[elem], prefix, ip-1)
		  ep = elem + ip - 1
		  prefix[ip] = EOS
 		  call strcat ("//", prefix[ip], SZ_PREFIX)
		}
	    }
	
	    # Get the filename component up to the EOS or the modifiers.
	    for (i=1; Memc[ep] != '[' && Memc[ep] != EOS; i=i+1) {
		fname[i] = Memc[ep]
		ep = ep + 1
	    }

            if (strncmp ("http://", fname, 7) == 0) {
        	call fmapfn ("cache$", osfn, SZ_PATHNAME)
        	call strupk (osfn, osfn, SZ_PATHNAME)

                #call fcadd (osfn, fname, "fits", cfname, SZ_PATHNAME)
                call fcadd (osfn, fname, "", cfname, SZ_PATHNAME)

                call strcpy (cfname, fname, SZ_PATHNAME)

            } else if (strncmp ("file://", fname, 7) == 0) {
		fi = 8
                if (strncmp ("file:///localhost", fname, 17) == 0)
		    fi = 18
                else if (strncmp ("file://localhost", fname, 16) == 0)
		    fi = 17

		for (fo=1; fname[fi] != EOS; fi=fi+1) {
		    if (fname[fi] == '/' && fname[fi+1] == '/')
			fi = fi + 1
		    cfname[fo] = fname[fi]
		    fo = fo + 1
		}
                call strcpy (cfname, fname, SZ_PATHNAME)
            }
	
	    # Get the modifier strings.
	    for (i=1; Memc[ep] != EOS ; i=i+1) {
		mods[i] = Memc[ep]
		ep = ep + 1
	    }
	
		
	    if (DEBUG) {
		call eprintf ("fnexp: '%s' --> '%s' '%s' '%s'\n")
		    call pargstr (Memc[elem]); call pargstr (prefix);
		    call pargstr (fname);    call pargstr (mods)
	    }

	    # Expand wildcards if needed.
	    if (stridxs("*?", fname) > 0) {

		# FIXME - Need to do concatenation here ...??
	        if (strsearch (fname, "//") > 0) {
		    call aclrc (left, SZ_PATHNAME)
		    call aclrc (right, SZ_PATHNAME)

		    # Gather the left and right side of a concatenation with
		    # wildcards.  Expand the side with the wildcard but
		    # maintain the concatenation so we keep the previous
		    # behavior in how these processed.
		    for (ip=1; fname[ip] != '/'; ip=ip+1)
		 	left[ip] = fname[ip]
		    ip = ip + 2
		    for (i=1; fname[ip] != EOS; ip=ip+1) {
		 	right[i] = fname[ip]
			i = i + 1
		    }

	    	    if (stridxs("*?", left) > 0) {
			listp = fntopnb (left, YES)
			llen = fntlenb (listp)
	 		for (i=0; i < llen; i=i+1) {
		    	    call aclrc (file, SZ_PATHNAME)
		    	    if (fntgfnb (listp, file, SZ_PATHNAME) == EOF) 
				break
		    	    outcomma (i > 0)
		    	    outstr(prefix)
		    	    outstr(file) ; outstr("//") ; outstr(right)
			}
			call fntclsb (listp)
	    	    } else {
			listp = fntopnb (right, YES)
			llen = fntlenb (listp)
	 		for (i=0; i < llen; i=i+1) {
		    	    call aclrc (file, SZ_PATHNAME)
		    	    if (fntgfnb (listp, file, SZ_PATHNAME) == EOF) 
				break
		    	    outcomma (i > 0)
		    	    outstr(prefix)
		    	    outstr(left) ; outstr("//") ; outstr(file)
			}
			call fntclsb (listp)
		    }
		    next

		} else {
		    listp = fntopnb (fname, YES)
		    llen = fntlenb (listp)
	 	    for (i=0; i < llen; i=i+1) {
		        call aclrc (file, SZ_PATHNAME)
		        if (fntgfnb (listp, file, SZ_PATHNAME) == EOF) 
		    	    break
		        outcomma ( i > 0)
		        outstr(prefix) ; outstr(file) ; outstr(mods)


	        	# Reallocate the output string if needed.
	        	if ((op_end - op) < SZ_FNAME || op >= op_end) {
		    	    sz_out = sz_out + SZ_FNT
		    	    len = (op - out - 1)

		    	    call calloc (op_start, sz_out, TY_CHAR)
			    call amovc (Memc[out], Memc[op_start], len)
			    for (op=op_start; Memc[op] != EOS; )
				op = op + 1

		    	    op_end = op_start + sz_out
			    call mfree (out, TY_CHAR)
		    	    out = op_start
	        	}
		    }
		    call fntclsb (listp)
		}


	    } else {
		outstr(prefix) ; outstr(fname) ; outstr(mods)
	    }

	    call aclrc (Memc[elem], SZ_FNT)
	}
	output ('\0')

	call mfree (elem, TY_CHAR)
	return (out)
end


# IMX_PREPROC_LIST -- Process the expanded filename string to open any
# @files and produce final expression strings.

pointer procedure imx_preproc_list (template)

char	template[ARB]			#i template string

pointer	tp, ip, op, itp, listp, elem
int	i, lp, in, len, tend, tlen, plen, llen
int	nchars, atat, nelem, in_filter
char    ch, file[SZ_LINE], expr[SZ_LINE], fname[SZ_PATHNAME]
char    ikparams[SZ_LINE], sec[SZ_LINE], dirname[SZ_PATHNAME]

define	output {Memc[op]=$1;op=op+1}

int	fntopnb(), fntgfnb(), fntlenb(), strlen(), stridxs(), strsearch()
int	access(), imx_get_element(), imx_breakout(), isdirectory()

begin
	# Allocate an intial string buffer.
	tlen = strlen (template)
	plen = max(strlen(template)*2, SZ_FNT)
	call calloc (tp, plen, TY_CHAR)
	call calloc (itp, tlen + 1, TY_CHAR)
	call calloc (elem, SZ_FNT, TY_CHAR)

	in = 1
	op = tp
	nelem = 0
	while (imx_get_element (template, in, Memc[elem], SZ_FNT) != EOS) {

	    # Break out the filename and expression.
	    nchars = imx_breakout (Memc[elem], NO, file, expr, 
			    sec, ikparams, SZ_LINE)

	    nelem = nelem + 1
	    outcomma (nelem > 1)

	    atat = NO
	    call aclrc (Memc[itp], tlen+1)

	    if (stridxs("[]", Memc[elem]) > 0 && expr[1] != EOS) {
		if (Memc[elem] == '@' || strsearch (Memc[elem], "//") > 0)
	            call sprintf (Memc[itp], tlen+1, "%s")
		else
	            call sprintf (Memc[itp], tlen+1, "@%s")
		        call pargstr (Memc[elem])

	    } else if (strsearch (Memc[elem], "][") > 0) {
	        call sprintf (Memc[itp], tlen+1, "@%s")
	    	    call pargstr (Memc[elem])

	    } else {
		# Simple filename or @file, just copy it out if it exists.
		if (Memc[elem] == '@') {
		    if (Memc[elem+1] != '@' && access (Memc[elem+1],0,0) == NO)
			if (strsearch (Memc[elem], "//") == 0)
		            next
		    if (Memc[elem+1] == '@') {
			lp = 1
			atat = YES
	        	call sprintf (Memc[itp], tlen+1, "%s")
	    	    	    call pargstr (Memc[elem])
		    } else {
			lp = 0
		        for (; Memc[elem+lp] != EOS; lp=lp+1)
		            output (Memc[elem+lp])
		        next
		    }
		} else {
		    lp = 0
		    for (; Memc[elem+lp] != EOS; lp=lp+1)
		            output (Memc[elem+lp])
		}
	    }

	    ip = itp
   	    tend = itp + strlen (Memc[itp]) - 1
	    ch = Memc[ip]

	    if (ch == '@') {				# @file

	        if (Memc[ip+1] == '@') {   		# @@file
		    atat = YES
		    ip = ip + 1
		}

		if (atat == NO) {
		    # No metachars, copy item entirely to output string.
		    in_filter = NO
		    while (Memc[ip] != EOS && ip <= tend) {
			if (Memc[ip] == '[') in_filter = YES
			if (Memc[ip] == ']') in_filter = NO
			if (Memc[ip] == ',' && in_filter == NO) {
			    output (Memc[ip])
			    ip = ip + 1
			    break
			}
			output (Memc[ip])
			ip = ip + 1
		    }
		    next
		}

		if (atat == YES) {
		    if (isdirectory (file[3], dirname, SZ_PATHNAME) > 0) {
			len = strlen (file)
			if (file[len] != '$')
			    call strcat ("/", file, SZ_FNAME)
			call strcat ("*.fits", file, SZ_FNAME)
		        listp = fntopnb (file[3], YES)
		    } else 
		        listp = fntopnb (file[2], YES)
		} else 
		    listp = fntopnb (file, YES)

		llen = fntlenb (listp)
	 	for (i=0; i < llen; i=i+1) {
		    call aclrc (fname, SZ_PATHNAME)
		    if (fntgfnb (listp, fname, SZ_PATHNAME) == EOF) 
			break

		    if (atat == YES)
		        output ('@')
		    for (lp=1; fname[lp] != EOS; lp=lp+1)
			output (fname[lp])
		    if (expr[1] != EOS) {	# append extension info
		        output ('[')
		        for (lp=1; expr[lp] != EOS; lp=lp+1)
			    output (expr[lp])
			if (ikparams[1] != EOS) {
		            output (',')
		            for (lp=1; ikparams[lp] != EOS; lp=lp+1)
			        output (ikparams[lp])
			}
		        output (']')
		    }
		    if (sec[1] != EOS) {	# append any section notation
		        output ('[')
		        for (lp=1; sec[lp] != EOS; lp=lp+1)
			    output (sec[lp])
		        output (']')
		    }

		    outcomma (i < (llen-1))
		}
		call fntclsb (listp)
		ip = ip + nchars + 1

	        if (Memc[ip+1] == ',')
		    break
	    } # else 
	      #	 call strcpy (Memc[elem], Memc[op], SZ_FNT)
	}

	call mfree (itp, TY_CHAR)
	call mfree (elem, TY_CHAR)

	return (tp)
end


# IMX_GET_ELEMENT -- Get the next element of a list template.

int procedure imx_get_element (template, ip, elem, maxch)

char	template[ARB]				#i input template string
int	ip					#u template index
char	elem[ARB]				#o output string buffer
int	maxch					#i max size of output element

int 	op, level, done
char	ch

begin
	op = 1
	done = 0
	level = 0

	if (template[ip] == EOS)
	    return (EOS)
	if (template[ip] == ',')
	    ip = ip + 1

	call aclrc (elem, maxch)
	while (template[ip] != EOS) {
	    ch = template[ip]

	    if (ch == EOS || (ch == ',' && level == 0)) {
		done = 1
	    } else if (ch == '[')
		level = level + 1
	    else if (ch == ']')
		level = level - 1

	    if (done == 1) {
		return (ip + 1)
	    } else 
	 	elem[op] = ch

	    ip = ip + 1
	    op = op + 1
	}

	return (ip)
end


# IMX_SPLIT -- Split a list element into the coarse filename and modifiers

int procedure imx_split (in, fname, mods, maxch)

char	in[ARB]					#i input template string
char	fname[ARB]				#o filename
char	mods[ARB]				#o modifier strings
int	maxch					#i max size of output string

int	i, j, nmods

begin
	# Allocate an intial string buffer.
	nmods = 0
	call aclrc (mods, maxch)
	call aclrc (fname, maxch)


	# Gather any prefix '@' symbols.
	for (i=1; in[i] != '[' && in[i] != EOS && i < maxch; i=i+1)
	    fname[i] = in[i]
	
	    # Get the filename component up to the EOS or the modifiers.
	if (in[i] == '[') {
	    for (j=1; in[i] != EOS && i < maxch && j < maxch; i=i+1) {
		mods[j] = in[i]
		j = j + 1
		if (in[i] == '[')
		    nmods = nmods + 1
	    }
	}
	
	return (nmods)
end
