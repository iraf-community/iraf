include <ctype.h>
include <plset.h>

define	DEF_MAXNCOLS	1000
define	DEF_MAXNLINES	4096
define	MAX_NRANGES	100


# RFT_FLIST -- Decode a list of files and associated extensions into a pixel
# list.

pointer procedure rft_flist (file_list, first_file, last_file, nfiles) 

char	file_list[ARB]		# the input file list string
int	first_file		# the first file in the list
int	last_file		# the last file in the list
int	nfiles			# the number of files in the list

int	i, j, maxncols, maxnlines, nrfiles, rp, rbegin, rend, rstep
int	last_ext, ebegin, eend, estep, ep, nefiles
pointer	sp, extensions, str, axes, pl

bool	pl_linenotempty()
int	rft_gfranges()
pointer	pl_create()

begin
	# Allocate some working space.
	call smark (sp)
	call salloc (extensions, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (axes, 2, TY_INT)

	# Initialize the file list.
	pl = NULL
	maxncols = DEF_MAXNCOLS
	maxnlines = DEF_MAXNLINES

	repeat {

	    # Initialize the file counter parameters.
	    first_file = INDEFI
	    last_file = INDEFI
	    nfiles = 0
	    rp = 1

	    # Open the file list.
	    Memi[axes] = maxncols
	    Memi[axes+1] = maxnlines
	    pl = pl_create (2, Memi[axes], 1)

	    # Decode the file list.
	    nrfiles = rft_gfranges (file_list, rp, YES, maxnlines, rbegin,
	        rend, rstep, NO, Memc[extensions])
	    while (nrfiles > 0) {

		# Check the file number limits and terminate the loop if
		# the current list size is exceeded.
		if (IS_INDEFI(first_file))
		    first_file = rbegin
		else
		    first_file = min (rbegin, first_file)
		if (IS_INDEFI(last_file))
		    last_file = rend
		else
		    last_file = max (last_file, rend)
		if (last_file > maxnlines)
		    break

		# Initialize the extensions list decoding.
		ep = 1
		last_ext = INDEFI

		# Decode the associated extensions files. If the extensions
		# list is empty
		nefiles = rft_gfranges (Memc[extensions], ep, YES, maxncols,
		    ebegin, eend, estep, YES, Memc[str])
		while (nefiles > 0) {

		    # Check the  extensions number limits and quit if they
		    # are exceeded.
		    if (IS_INDEFI(last_ext))
		        last_ext = eend
		    else
		        last_ext = max (last_ext, eend)
		    if (last_ext > maxncols)
		        break

		    # Set the appropriate elements in the list.
		    if (rstep == 1) {
			if (estep == 1)
		    	    call pl_box (pl, ebegin, rbegin, eend,
			        rend, PIX_SET + PIX_VALUE(1))
			else {
			    do i = ebegin, eend, estep
			        call pl_box (pl, i, rbegin, i, rend, 
			            PIX_SET + PIX_VALUE(1))
			}
		    } else {
		        do i = rbegin, rend, rstep {
			    do j = ebegin, eend, estep
		                call pl_point (pl, j, i, PIX_SET +
			            PIX_VALUE(1))
			}
		    }

		    nefiles = rft_gfranges (Memc[extensions], ep, NO, maxncols,
		        ebegin, eend, estep, YES, Memc[str])
		}

		# Break if an extensions list decode error occurs.
		if (nefiles == 0)
		    break

	        nrfiles = rft_gfranges (file_list, rp, NO, maxnlines, rbegin,
		    rend, rstep, NO, Memc[extensions])
	    }

	    # Break if a file or extensions list decode error ocurred.
	    if (nrfiles == 0 || nefiles == 0)
		break

	    # If the file or extensions list is larger than the current maximum
	    # then free the list increase the default space and repeat the
	    # procedure.

	    if (!IS_INDEFI(last_file)) {
	        if (last_file > maxnlines) {
		    if (pl != NULL)
		        call pl_close (pl)
		    pl = NULL
		    maxnlines = maxnlines + DEF_MAXNLINES
	        } else {
		    do i = first_file, last_file {
		        Memi[axes] = 1
		        Memi[axes+1] = i
		        if (pl_linenotempty (pl, Memi[axes]))
			    nfiles = nfiles + 1
		    }
		}
	    }

	    if (!IS_INDEFI(last_ext)) {
		if (last_ext > maxncols) {
		    if (pl != NULL)
		        call pl_close (pl)
		    pl = NULL
		    maxncols = maxncols + DEF_MAXNCOLS
		}
	    }

	} until (pl != NULL)

	# Free space.
	call sfree (sp)

	# If the file list is empty or a decode error occurred return
	# a NULL list, otherwise return the pointer to the list.

	if (nfiles <= 0 || nrfiles == 0 || nefiles == 0) {
	    nfiles = 0
	    first_file = INDEFI
	    last_file = INDEFI
	    call pl_close (pl)
	    return (NULL)
	} else
	    return (pl)
end


# RFT_GFRANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Each range may have an
# associated extensions lists delimited by square brackets. Return as output
# each range in sequence. Range limits must be positive nonnegative integers.
# EOF is returned if the  end of the file_list is reached.  0 is returned if a
# conversion error takes place. Otherwise the number of elements in the
# range is returned.

int procedure rft_gfranges (range_string, ip, firstr, rmax, rbegin, rend,
	rstep, zeroindex, extensions)

char    range_string[ARB]       # range string to be decoded
int	ip			# the range string pointer
int	firstr			# first range to be returned
int	rmax			# the maximum file number
int	rbegin			# the begining of the range			
int	rend			# the end of the range
int	rstep			# the range step size
int	zeroindex		# allow zero indexing ?
char	extensions[ARB]		# the output extensions string

int	ep, itemp
int	ctoi()

begin
	# Initialize.
	if (zeroindex == YES) {
	    rbegin = 0
            rend = rmax - 1
	} else {
	    rbegin = 1
            rend = rmax
	}
        rstep = 1
	extensions[1] = EOS

	# Return default range if the range string is NULL.
	if (range_string[ip] == EOS) {
	    rbegin = 1
	    rend = rmax
	    if (firstr == YES)
	        return (rend)
	    else
	        return (EOF)
	}

        # Skip delimiters
        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
            ip = ip + 1

        # Get first limit.
        # Must be a number, '-', 'x', *, or EOS.
        if (range_string[ip] == EOS) {                      # end of list
	    rbegin = 1
	    rend = rmax
	    if (firstr == YES)
	        return (rend)
	    else
		return (EOF)
	} else if (range_string[ip] == '*') {
	    ;
        } else if (range_string[ip] == '-') {
            ;
        } else if (range_string[ip] == 'x') {
            ;
        } else if (IS_DIGIT(range_string[ip])) {              # ,n..
            if (ctoi (range_string, ip, rbegin) == 0)
                return (0)
	    else if (zeroindex == NO) {
	        if (rbegin <= 0)
		    return (0)
	    } else {
	        if (rbegin < 0)
		    return (0)
	    }
        } else
            return (0)

	# Extract extensions file list.
	if (range_string[ip] == '[') {
	    ip = ip + 1
	    ep = 1
	    while (range_string[ip] != EOS) {
		if (range_string[ip] == ']') {
		    ip = ip + 1
		    break
		}
		extensions[ep] = range_string[ip]
		ip = ip + 1
		ep = ep + 1
	    }
	    extensions[ep] = EOS
	    if (range_string[ip-1] != ']')
		return (0)
	}

        # Skip delimiters
        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
            ip = ip + 1

        # Get last limit
        # Must be '-', 'x', or '*' otherwise last = first.
	if (range_string[ip] == '*')
	    ;
        else if (range_string[ip] == 'x')
            ;
        else if (range_string[ip] == '-') {
            ip = ip + 1
            while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
                ip = ip + 1
            if (range_string[ip] == EOS)
                ;
            else if (IS_DIGIT(range_string[ip])) {
                if (ctoi (range_string, ip, rend) == 0)
                    return (0)
		else if (zeroindex == NO) {
		    if (rend <= 0)
		        return (0)
		} else {
		    if (rend < 0)
		        return (0)
		}
            } else if (range_string[ip] == 'x')
                    ;
            else
                return (0)
        } else
                rend = rbegin

	# Skip extensions files for now.
	if (range_string[ip] == '[') {
	    ip = ip + 1
	    ep = 1
	    while (range_string[ip] != EOS) {
		if (range_string[ip] == ']') {
		    ip = ip + 1
		    break
		}
		extensions[ep] = range_string[ip]
		ip = ip + 1
		ep = ep + 1
	    }
	    extensions[ep] = EOS
	    if (range_string[ip-1] != ']')
	        return (0)
	}

        # Skip delimiters
        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
            ip = ip + 1

        # Get step.
        # Must be 'x' or assume default step.
        if (range_string[ip] == '*')
	    ip = ip + 1
        else if (range_string[ip] == 'x') {
            ip = ip + 1
            while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
                ip = ip + 1
            if (range_string[ip] == EOS)
                ;
            else if (IS_DIGIT(range_string[ip])) {
                if (ctoi (range_string, ip, rstep) == 0)
                    ;
	        else if (rstep <= 0)
		    return (0)
            } else if (range_string[ip] == '-')
                ;
            else
                return (0)
        }

	# Skip extensions files for now.
	if (range_string[ip] == '[') {
	    ip = ip + 1
	    ep = 1
	    while (range_string[ip] != EOS) {
		if (range_string[ip] == ']') {
		    ip = ip + 1
		    break
		}
		extensions[ep] = range_string[ip]
		ip = ip + 1
		ep = ep + 1
	    }
	    extensions[ep] = EOS
	    if (range_string[ip-1] != ']')
	        return (0)
	}

        # Output the range triple.
	if (rend < rbegin) {
	    itemp = rbegin
	    rbegin = rend
	    rend = itemp
	}
	if (zeroindex == YES) {
	    rbegin = rbegin + 1
	    rend = rend + 1
	}
        return (abs (rend - rbegin) / rstep + 1 )
end


