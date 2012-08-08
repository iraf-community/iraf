# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <imhdr.h>
include "iki.h"

# IKIEXTN.X -- Image extension handling.  This package is used to map image
# file extensions to image types and vice versa.
#
#	       iki_extninit (imtype, def_imtype, imextn, def_imextn)
#    status = iki_validextn (kernel, extn)
#      status = iki_getextn (kernel, index, extn, maxch)
#        value = iki_getpar (param)
#
# iki_extninit initializes the image extension handling package.  This parses
# the lists of extensions and patterns and builds an internal descriptor which
# will be used by the other routines for extension handling.  iki_validextn
# tests whether a given extension is valid for a particular image kernel
# (type of image).  iki_getextn is used with an index argument to get a list
# of the extensions for a particular image type.  iki_getpar queries the value
# of IKI global parameters.

define	SZ_IMTYPE	128
define	SZ_IMEXTN	1024


# IKI_EXTNINIT -- Initialize the image extension handling package.  This is
# typically done once when IKI is first initialized.  Changes to the image
# typing environment made subsequently have no effect unless the package is
# reinitialized.

int procedure iki_extninit (env_imtype, def_imtype, env_imextn, def_imextn)

char	env_imtype[ARB]			#I imtype environment variable
char	def_imtype[ARB]			#I default imtype value string
char	env_imextn[ARB]			#I imextn environment variable
char	def_imextn[ARB]			#I default imextn value string

int	kset[MAX_KERNELS]
pointer	sp, ip, ip_save, imtype, imextn, strval
int	op, delim, status, nchars, i, j, kernel
int	envfind(), iki_getfield(), gstrcpy(), iki_validextn()
bool	streq(), envgetb()

include	"iki.com"

begin
	call smark (sp)
	call salloc (imtype, SZ_IMTYPE, TY_CHAR)
	call salloc (imextn, SZ_IMEXTN, TY_CHAR)
	call salloc (strval, SZ_FNAME, TY_CHAR)

	status = OK

	# Get the imtype string.  The value of the env_imtype variable is used
	# if the variable is found, otherwise the default is used.

	Memc[imtype] = EOS
	if (env_imtype[1] != EOS)
	    if (envfind (env_imtype, Memc[imtype], SZ_IMTYPE) <= 0)
		Memc[imtype] = EOS
	if (Memc[imtype] == EOS)
	    call strcpy (def_imtype, Memc[imtype], SZ_IMTYPE)

	# Get the imextn string.  The value of the env_imextn variable is used
	# if the variable is found, otherwise the default is used.

	Memc[imextn] = EOS
	if (env_imextn[1] != EOS)
	    if (envfind (env_imextn, Memc[imextn], SZ_IMEXTN) <= 0)
		Memc[imextn] = EOS
	if (Memc[imextn] == EOS)
	    call strcpy (def_imextn, Memc[imextn], SZ_IMEXTN)

	# Process imextn.  This specifies the set of valid extensions for
	# each image type.  This must be done before processing imtype below,
	# since iki_validextn can be used when processing imtype.  The imextn
	# string is of the form "<kernel>:<extn>[,<extn>...] ..." where 
	# <kernel> is the IKI kernel name (k_kname) and <extn> is a regular
	# expression to be used to test for a matching file extension.
	# For example, imextn = "oif:imh stf:hhh,??h fits:,fits,fit".

	k_nextn = 0
	k_sbufused = 0
	call aclri (kset, MAX_KERNELS)

	# Process the user extension string first followed by the builtin
	# defaults.  Anything given in the user string takes precedence
	# while anything omitted uses the builtin defaults instead (if there
	# is no user imextn this processes the default string twice).

	do i = 1, 2 {
	    if (i > 1)
		call strcpy (def_imextn, Memc[imextn], SZ_IMEXTN)

	    ip = imextn
	    while (Memc[ip] != EOS && IS_WHITE(Memc[ip]))
		ip = ip + 1

	    repeat {
		# Get the kernel name.
		if (iki_getfield (ip, Memc[strval], SZ_FNAME, delim) <= 0)
		    break
		call strlwr (Memc[strval])
		if (delim != ':') {
		    status = ERR
		    break
		}

		# Lookup kernel.
		kernel = 0
		do j = 1, k_nkernels {
		    if (streq (Memc[strval], k_kname[1,j])) {
			kernel = j
			break
		    }
		}
		if (kernel <= 0) {
		    status = ERR
		    break
		}

		# Process the list of extension patterns.
		op = k_sbufused + 1
		ip_save = ip

		while (iki_getfield (ip, Memc[strval], SZ_FNAME, delim) > 0) {
		    # call strlwr (Memc[strval])

		    # Skip it if we already have something for this kernel.
		    if (kset[kernel] == 0) {
			# Get a new extension descriptor.
			if (k_nextn >= MAX_IMEXTN) {
			    status = ERR
			    break
			} else
			    k_nextn = k_nextn + 1

			# Save the kernel index associated with this extension.
			k_kernel[k_nextn] = kernel

			# Save the extension string.
			k_extn[k_nextn] = op
			nchars = gstrcpy(Memc[strval],k_sbuf[op],SZ_IKISBUF-op)
			op = op + nchars + 1

			# Save the strmatch pattern for the extension.
			k_pattern[k_nextn] = op
			k_sbuf[op] = '^';  op = op + 1
			nchars = gstrcpy(Memc[strval],k_sbuf[op],SZ_IKISBUF-op)
			op = op + nchars + 1
		    }

		    ip_save = ip
		    if (delim != ',')
			break
		}

		kset[kernel] = 1
		k_sbufused = op - 1

	    } until (Memc[ip] == EOS)
	}

	# Process imtype.  This sets the default image type for new images.
	# For example, imtype = "oif,inherit" would create OIF (.imh) images
	# by default, inheriting the old image type if a newcopy image is
	# being written.

	k_defimtype = 1
	k_inherit = NO
	ip = imtype
	kernel = 0

	while (iki_getfield (ip, Memc[strval], SZ_FNAME, delim) > 0) {
	    call strlwr (Memc[strval])

	    # Check for the inherit/noinherit keywords.
	    if (streq (Memc[strval], "inherit")) {
		k_inherit = YES
		next
	    }
	    if (streq (Memc[strval], "noinherit")) {
		k_inherit = NO
		next
	    }

	    # Scan the kernels to see if we have a kernel name.
	    if (kernel <= 0)
		do i = 1, k_nkernels
		    if (streq (Memc[strval], k_kname[1,i])) {
			kernel = i
			break
		    }

	    # Check for a valid imagefile extension.
	    if (kernel <= 0)
		kernel = iki_validextn (0, Memc[strval])
	}

	if (kernel <= 0)
	    status = ERR
	else
	    k_defimtype = kernel

	if (envgetb ("ikidebug"))
	    call iki_debug ("IKI debug:", STDERR, 0)

	call sfree (sp)
	return (status)
end


# IKI_VALIDEXTN -- Determine if the given imagefile extension is valid for
# the given image kernel (image type).  If kernel=0 the extensions for all
# kernels are examined.  If a valid match is found the kernel index is
# returned as the function value, otherwise 0 is returned.

int procedure iki_validextn (kernel, extn)

int	kernel				#I kernel index, zero for all kernels
char	extn[ARB]			#I extension to be tested

int	i, ip
int	strmatch()
include	"iki.com"

begin
	do i = 1, k_nextn
	    if (kernel == 0 || k_kernel[i] == kernel) {
		ip = strmatch (extn, k_sbuf[k_pattern[i]])
		if (ip > 0 && extn[ip] == EOS)
		    return (k_kernel[i])
	    }

	return (0)
end


# IKI_GETEXTN -- Get an entry from the list of valid extensions (actually
# extension patterns) for the given image kernel.  If kernel=0 all entries
# are returned.  The kernel index for the output extension is returned as
# the function value.  ERR is returned if the requested extension does not
# exist.

int procedure iki_getextn (kernel, index, extn, maxch)

int	kernel				#I kernel index, zero for all kernels
int	index				#I extension number (1 indexed)
char	extn[ARB]			#O extension
int	maxch				#I max chars out

int	i, n
include	"iki.com"

begin
	n = 0
	do i = 1, k_nextn
	    if (kernel == 0 || k_kernel[i] == kernel) {
		n = n + 1
		if (index == n) {
		    call strcpy (k_sbuf[k_extn[i]], extn, maxch)
		    return (k_kernel[i])
		}
	    }

	return (ERR)
end


# IKI_GETPAR -- Return the value of an IKI global parameter (integer valued).

int procedure iki_getpar (param)

char	param[ARB]			#I parameter name

bool	streq()
include	"iki.com"

begin
	if (streq (param, "inherit"))
	    return (k_inherit)
	else if (streq (param, "defimtype"))
	    return (k_defimtype)
end


# IKI_GETFIELD -- Get the next field from a punctuation or whitespace
# delimited list.  The length of the field is returned as the function value.
# EOF is returned at the end of the list.  Zero can be returned if a field
# is zero length, e.g. in "foo,,foo" the second field is zero length.

int procedure iki_getfield (ip, outstr, maxch, delim)

pointer	ip				#U string pointer
char	outstr[ARB]			#O receives string
int	maxch				#I max chars out
int	delim				#O delimiter char

int	op, ch

begin
	# Skip any leading whitespace.
	while (Memc[ip] != EOS && IS_WHITE(Memc[ip]))
	    ip = ip + 1

	# Check for end of list.
	if (Memc[ip] == EOS || Memc[ip] == '\n')
	    return (EOF)

	op = 1
	for (ch=Memc[ip];  ch != EOS && !IS_WHITE(ch);  ch=Memc[ip]) {
	    if (ch == ',' || ch == ':' || ch == ';')
		break
	    else {
		outstr[op] = ch
		op = op + 1
	    }
	    ip = ip + 1
	}

	delim = ch
	if (delim != EOS)
	    ip = ip + 1

	outstr[op] = EOS
	return (op - 1)
end


# IKI_DEBUG -- Print debug information on the IKI internal data structures.

procedure iki_debug (str, fd, flags)

char	str[ARB]			#I title string
int	fd				#I output file
int	flags				#I (not used)

int	i
include	"iki.com"

begin
	# Print global variables.
	call fprintf (fd, "%s nkernels=%d sbufused=%d deftype=%d ")
	    call pargstr (str)
	    call pargi (k_nkernels)
	    call pargi (k_sbufused)
	    call pargi (k_defimtype)
	call fprintf (fd, "inherit=%d nextn=%d\n")
	    call pargi (k_inherit)
	    call pargi (k_nextn)

	# List the installed kernels.
	call fprintf (fd, "installed kernels ")
	do i = 1, k_nkernels {
	    call fprintf (fd, "%s=%d ")
		call pargstr (k_kname[1,i])
		call pargi (i)
	}
	call fprintf (fd, "\n")
	
	# Print the extension table.
	do i = 1, k_nextn {
	    call fprintf (fd, "%6s %d (%s) %s\n")
		call pargstr (k_sbuf[k_extn[i]])
		call pargi (k_kernel[i])
		call pargstr (k_kname[1,k_kernel[i]])
		call pargstr (k_sbuf[k_pattern[i]])
	}

	call flush (fd)
end
