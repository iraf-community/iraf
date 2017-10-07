# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<syserr.h>
include	<chars.h>
include	<config.h>
include	<knet.h>
include	<fio.h>

.help vfntrans
.nf ___________________________________________________________________________
VFNTRANS -- Procedures for translating VFN's to OSFN's and back again in
memory.  These procedures are called by the VFNMAP procedures, but do not
access the VFN database.  This package contains most of the knowledge of the
characteristics of OS filenames.  The characteristics of OS filenames (max
length, extensions, etc.) are defined in <config.h>.

	    vfn_translate (vfn, osdir, lenosdir, root, lenroot, extn, lenextn)
	  vfn_expand_ldir (vfn, outstr, maxch)
	       vfn_encode (vfn, ip, root, lenroot, extn, lenextn)
      nchars = vfn_decode (osfn, ip, vfn, maxch)
        vfn_map_extension (iraf_extn, os_extn, maxch)
      vfn_unmap_extension (iraf_extn, os_extn, maxch)
    	      vfn_squeeze (root, outstr, maxch)
 y/n = vfn_is_hidden_file (fname)

The main vfn to osfn translation routine is VFN_TRANSLATE.  VFN_EXPAND_LDIR
performs recursive logical directory expansion.  The encode and decode routines
perform escape sequence encoding and its inverse.  Substitution of OS filename
extensions for IRAF extensions, and vice versa, is performed by the map
extension procedures.
.endhelp ______________________________________________________________________

# Size limiting definitions.

define	MAX_PUSHBACK	16	# determines length of pushback stack
define	SZ_PBBUF	255	# size of pushback buffer
define	MAX_EXTENSIONS	20	# max filename extension pairs in EXTN_MAP
define	MAX_RESERVEXTN	20	# max reserved filename extensions
define	SZ_EXTNMAP	64	# storage for iraf/os extn pairs


# VFN_TRANSLATE -- Translate a VFN into the OSDIR, ROOT, and EXTN fields.
# If a logical directory prefix is given it will be recursively expanded.
# Any number of subdirectories may be given; each is folded into the OSDIR.
# The ROOT field is escape sequence encoded but not squeezed.  The EXTN
# field is encoded and selected IRAF extensions are mapped into OS
# extensions.

procedure vfn_translate (rawvfn, osdir, lenosdir, root, lenroot, extn, lenextn)

char	rawvfn[ARB]		# input virtual filename
char	osdir[SZ_OSDIR]		# OS directory prefix		(output)
int	lenosdir		# length of the osdir string	(output)
char	root[SZ_VFNFN]		# OS root filename		(output)
int	lenroot			# length of the root string	(output)
char	extn[SZ_VFNFN]		# OS filename extension		(output)
int	lenextn			# length of the extn string	(output)

pointer	sp, ip, vfn, fname, sqroot
int	gstrcpy(), nowhite()
errchk	syserr

begin
	call smark (sp)
	call salloc (vfn, SZ_PATHNAME, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (sqroot, MAX_ROOTLEN, TY_CHAR)

	# Strip any whitespace at either end of the filename.
	if (nowhite (rawvfn, Memc[vfn], SZ_PATHNAME) == 0)
	    call syserr (SYS_FNOFNAME)

	# If the VFN begins with a legal OS directory prefix it is assumed
	# to be an OSFN and no mapping is performed.  Do not bother to break
	# the filename into osdir, root, extn.

	call zfxdir (Memc[vfn], osdir, SZ_OSDIR, lenosdir)

	if (lenosdir > 0) {
	    # VFN is actually an OSFN.  Return the OSFN as the osdir filename
	    # to avoid the 32 char restriction.
	    root[1] = EOS
	    lenroot = 0
	    extn[1] = EOS
	    lenextn = 0

	    lenosdir = gstrcpy (Memc[vfn], osdir, SZ_OSDIR)

	    call sfree (sp)
	    return
	}

	# The VFN is really a VFN.  Check for a logical directory prefix and
	# recursively expand it into an OS directory prefix if found.  A VFN of
	# the form "ldir$xxx" is converted to "osdir // xxx".  Additional
	# subdirectories may be introduced in the expansion.

	call vfn_expand_ldir (Memc[vfn], Memc[fname], SZ_PATHNAME)
	call zfxdir (Memc[fname], osdir, SZ_OSDIR, lenosdir)
	ip = fname + lenosdir

	# Translate the VFN.  Each pass through the loop extracts and processes
	# the next field of the VFN.  A field may be a subdirectory delimited
	# by a /, a root filename, or an extension.  Subdirectory names are
	# encoded and squeezed but not looked up in the mapping table, hence
	# long directory names must be unique within a directory.

	repeat {
	    call vfn_encode (Memc, ip, root, lenroot, extn, lenextn)
	    if (Memc[ip] == '/' ||
		(root[1] == '.' && lenroot == 1) ||
		(root[1] == '.' && root[2] == '.' && lenroot == 2)) {

		if (lenroot > MAX_ROOTLEN) {
		    call vfn_squeeze (root, Memc[sqroot], MAX_ROOTLEN)
		    call zfsubd (osdir, SZ_OSDIR, Memc[sqroot], lenosdir)
		} else
		    call zfsubd (osdir, SZ_OSDIR, root, lenosdir)
		root[1] = EOS
		lenroot = 0
		if (Memc[ip] == '/')
		    ip = ip + 1
	    }
	} until (Memc[ip] == EOS)

	# Map IRAF extension into OS extension.
	if (lenextn > 0)
	    call vfn_map_extension (extn, extn, SZ_VFNFN)
	call sfree (sp)
end


# VFN_EXPAND_LDIR -- Copy the input VFN to the output string.  As the copy is
# being performed we scan for the ldir delimiter.  If it is encountered, we
# lookup the ldir in the environment and push the value back into the input,
# resuming scanning on the new input.  Logical directories are thus expanded
# recursively, i.e., one ldir may reference another in its definition.  If an
# ldir references itself storage will overflow and an error message is printed.

procedure vfn_expand_ldir (vfn, outstr, maxch)

char	vfn[ARB]	# VFN possibly containing an ldir prefix
char	outstr[maxch]	# output string
int	maxch

char	ch
pointer	pbbuf					# pushback buffer
pointer	pb_stack[MAX_PUSHBACK]			# pushback stack
int	n, op, op_node, op_env, pbsp, in
pointer	nextch, ip, sp

int	envfind(), gstrcpy(), ki_localnode()
define	input {$1=Memc[ip];ip=ip+1}
define	output {outstr[op]=$1;op=op+1}
errchk	syserrs, envfind

begin
	call smark (sp)
	call salloc (pbbuf, SZ_PBBUF, TY_CHAR)

	# Discard leading whitespace and copy the VFN into the input buffer.
	for (in=1;  IS_WHITE (vfn[in]);  in=in+1)
	    ;

	nextch  = pbbuf + gstrcpy (vfn[in], Memc[pbbuf], SZ_PBBUF) + 1
	ip      = pbbuf
	op      = 1	
	op_node = 1
	pbsp    = 1

	# Copy characters successively from the input buffer to the output
	# buffer (outstr).  Expand logical names recursively (by rescanning
	# the translation string returned by ENVGETS).

	repeat {
	    input (ch)
	    if (IS_ALNUM (ch)) {			# --- regular chars
		output (ch)

	    } else if (ch == FNNODE_CHAR) {
		# If CH is the node name delimiter, either the named node
		# is the local node passed as a prefix to any ldir strings,
		# or the node was referenced IN one of the logical directory
		# replacement strings.

		if (pbsp == 1) {
		    # If no logical directory definitions have been pushed
		    # back, the named node must be the local node.

		    output (ch)
		    op_node = op

		} else {
		    outstr[op] = EOS

		    if (ki_localnode (outstr) == YES) {
			# Same as above; ignore local node prefix.
			output (ch)
			op_node = op

		    } else {
			# A remote node has been referenced during logical
			# directory expansion.  Filename translation must take
			# place on the remote node, so exit immediately,
			# returning the new filename "node!vfn".  This will
			# repeat when filename translation resumes on the
			# remote node, but over there the named node will be
			# the local node and the if(yes) branch will be taken.

			output (ch)
			op = op + gstrcpy (vfn[in], outstr[op], maxch-op+1)
			break
		    }
		}

	    } else if (ch == FNLDIR_CHAR) {
		# If CH is the logical name delimiter, look up the logical name
		# in the environment table.  If found, push definition back into
		# pbbuf, clear the output buffer, and scan pushed back string.
		# If not found, pass the string on as a file name.  Delete the
		# '$' character, so that OS directory specs like "osdir$"
		# are passed on correctly.

		output (EOS)
		n = envfind (outstr[op_node], Memc[nextch],
		    pbbuf + SZ_PBBUF - nextch)

		if (n >= 0) {				# push back defn
		    pb_stack[pbsp] = ip			# save ip on stk
		    pbsp = pbsp + 1
		    ip = nextch				# set ip to new input
		    nextch = nextch + n + 1
		    # Check for recursion (stack overflow).
		    if (pbsp > MAX_PUSHBACK || nextch-pbbuf >= SZ_PBBUF)
			call syserrs (SYS_FZMAPRECUR, Memc[pbbuf])
		    op = op_node			# discard logical name
		} else
		    op = op - 1				# cancel EOS, delete $

	    } else if (ch == '(') {
		# Environment substitution.  Mark the position of the left
		# paren for later replacement.

		op_env = op
		output (ch)

	    } else if (ch == ')') {
		# Complete an environment substitution.

		outstr[op_env] = EOS
		n = gstrcpy (outstr[op_node], Memc[nextch],
		    pbbuf + SZ_PBBUF - nextch)

		pb_stack[pbsp] = ip			# save ip on stk
		pbsp = pbsp + 1
		ip = nextch				# set ip to new input
		nextch = nextch + n

		# Check for recursion (stack overflow).
		if (pbsp > MAX_PUSHBACK || nextch-pbbuf >= SZ_PBBUF)
		    call syserrs (SYS_FZMAPRECUR, Memc[pbbuf])

		# Get the envvar value string; use null string if not defined.
		output (EOS)
		n = envfind (outstr[op_env+1], Memc[nextch],
		    pbbuf + SZ_PBBUF - nextch)
		if (n <= 0) {
		    Memc[nextch] = EOS
		    n = 0
		}

		nextch = nextch + n + 1
		op = op_node

	    } else if (ch == EOS) {
		# EOS may mean either the end of a pushed back string, or the
		# end of the VFN string.  Pop old ip off stack, continue until
		# the pushback stack is empty (end of VFN).

		pbsp = pbsp - 1				# pop old ip off stk
		if (pbsp == 0)				# --- all done
		    break
		ip = pb_stack[pbsp]

	    } else if (ch == ESCAPE) {
		# Escaped characters are passed straight to the output.
		# Preserve the escape unless the escaped character is a
		# metacharacter recognized by this procedure (i.e., $ or @).

		input (ch)
		if (ch == EOS)
		    ip = ip - 1
		else if (ch == FNLDIR_CHAR || ch == FNNODE_CHAR)
		    output (ch)
		else {
		    output ('\\')
		    output (ch)
		}

	    } else if (ch > BLANK) {
		# Whitespace and control chars are deleted.
		output (ch)
	    }

	    if (op > maxch)				# check for overflow
		call syserrs (SYS_FZMAPOVFL, vfn)
	}

	output (EOS)
	call sfree (sp)
end


# VFN_ENCODE -- Extract and encode the next field of the input VFN.  The subdir
# delimiter / or EOS will delimit the scan; the input pointer must be set to
# the first character to be scanned upon input and is left pointing at the
# delimiter character upon output.  If the field is a subdir no extension will
# be returned.  If an extension is encountered but its length exceeds that
# permitted by the OS or if another "." delimited field is encountered then
# the "." is encoded and the extension is included in the root.  If the OS is
# case insensitive the output string will be all lower case.

procedure vfn_encode (vfn, ip, root, lenroot, extn, lenextn)

char	vfn[ARB]		# virtual filename to be scanned
int	ip			# offset of first char to be scanned (in/out)
char	root[SZ_VFNFN]		# receives the encoded root filename
int	lenroot			# nchars in root
char	extn[SZ_VFNFN]		# receives the encoded filename extn
int	lenextn			# nchars in extn

int	out, i
char	ch, nextch
bool	uc_mode, processing_extension, escape_extension, subdir
pointer	sp, field, op

int	gstrcpy()
define	putch {Memc[op]=$1;op=op+1}
define	notextn_ 91

begin
	call smark (sp)
	call salloc (field, SZ_FNAME, TY_CHAR)

	# Skip leading whitespace and control chars.
	while (vfn[ip] > 0 && vfn[ip] <= BLANK)
	    ip = ip + 1

	# Do something sensible if the null string is input.
	if (vfn[ip] == EOS) {
	    root[1] = EOS
	    extn[1] = EOS
	    lenroot = 0
	    lenextn = 0
	    call sfree (sp)
	    return
	}

	out = 1
	op = field
	uc_mode = false
	processing_extension = false
	escape_extension = false

	# If the first char is legal in an OSFN but is not a letter and only
	# letters are permitted as the first char, then a no-op sequence (shift
	# to lower) is output first.  

	if (LEADING_ALPHA_ONLY)
	    if (vfn[ip] != '.' && !IS_ALPHA(vfn[ip]))
		call vvfn_escape (SHIFT_TO_LOWER, root, out, SZ_VFNFN)

	# The main loop.  Examine each input character and output zero or
	# more characters depending on the input character class and on what
	# the host system permits.  Most characters are expected to be lower
	# case alphas or digits, hence what we do after these first couple of
	# IF's should not affect efficiency much.  NOTE: set the escape char
	# to NUL to turn off escapes on a machine that allows any char in a
	# a filename.

	ch = vfn[ip]
	do i = 1, ARB {
	    if (ch == EOS) {
		break
	    } else if (IS_LOWER(ch)) {
		if (uc_mode) {
		    putch (VFN_ESCAPE_CHAR)
		    putch (SHIFT_TO_LOWER)
		    uc_mode = false
		}
		putch (ch)

	    } else if (IS_DIGIT(ch)) {
		putch (ch)
		
	    } else if (ch == '_') {
		if (UNDERSCORE_PERMITTED)
		    putch (ch)
		else {
		    putch (VFN_ESCAPE_CHAR)
		    putch (UNDERSCORE_CODE)
		}

	    } else if (IS_UPPER(ch)) {
		if (!CASE_INSENSITIVE)
		    putch (ch)
		else if (uc_mode)
		    putch (TO_LOWER(ch))
		else {
		    # Determine whether to do a case shift or just escape
		    # a single character.  The crossover point is at 2 chars.
		    # If the next character is also upper case we shift up.

		    putch (VFN_ESCAPE_CHAR)
		    if (IS_UPPER (vfn[ip+1])) {
			uc_mode = true
			putch (SHIFT_TO_UPPER)
		    } else
			putch (SHIFT_NEXTCHAR)
		    putch (TO_LOWER(ch))
		}

	    } else if (ch == '.') {
		# Determine whether the "." marks an extension or is part of
		# one of the two special subdirectories "." and "..".

		subdir = false
		if (op == field && !processing_extension) {
		    nextch = vfn[ip+1]
		    if (nextch == '/' || nextch == EOS) {
			subdir = true
		    } else if (nextch == '.' &&
		    	      (vfn[ip+2] == '/' || vfn[ip+2] == EOS)) {
			subdir = true
			putch (ch)
			ip = ip + 1
		    }
		}
		    
		if (subdir) {
		    putch (ch)

		} else if (processing_extension) {
		    # We are already processing an extension and have just
		    # encountered another one (e.g., "file.x.old").  Escape
		    # the dot and include it and the old extension in the
		    # root.  Start a new extension.

		    putch (EOS)
		    if (PERIOD_PERMITTED) {
			root[out] = '.'
			out = out + 1
		    } else
			call vvfn_escape (PERIOD_CODE, root, out, SZ_VFNFN)
		    out = out + gstrcpy (Memc[field], root[out], SZ_VFNFN-out+1)
		    op = field
		    escape_extension = false
		
		} else {
		    # This is the first extension to be encountered.  Move the
		    # contents of the field buffer to root and clear the buffer.

		    putch (EOS)
		    out = out + gstrcpy (Memc[field], root[out], SZ_VFNFN-out+1)
		    op = field
		    processing_extension = true
		    escape_extension = false
		}

	    } else if (ch == '\\' && vfn[ip+1] != EOS) {
		# Escaped characters are passed unconditionally, stripping the
		# escape character itself unless used to escape the first char
		# of an extension, in which case the \ must be retained in the
		# output extension to defeat extension mapping.

		if (processing_extension && op == field)
		    escape_extension = true
		ip = ip + 1
		putch (vfn[ip])

	    } else if (ch == '/') {
		# End of subdirectory name terminates scan.  Extensions are
		# not recognized within subdirectory names; include as part
		# of root name.

		if (processing_extension)
		    goto notextn_
		else
		    break

	    } else if (ch <= BLANK) {
		# Strip whitespace and control chars.
	    } else {
		# Unknown characters are not mapped; user's discretion.
		putch (ch)
	    }

	    # If we are processing an extension and the max length of an OS
	    # extension has been exceeded, the extension is considered part of
	    # the root and we are no longer processing an extension.

	    if (processing_extension)
		if (op - field > MAX_EXTNLEN) {
notextn_	    if (PERIOD_PERMITTED) {
			root[out] = '.'
			out = out + 1
		    } else
			call vvfn_escape (PERIOD_CODE, root, out, SZ_VFNFN)
		    processing_extension = false
		    escape_extension = false
		    if (ch == '/')
			break
		}

	    ip = ip + 1
	    ch = vfn[ip]
	}

	putch (EOS)
	if (processing_extension) {
	    # Move extension to the extn output buffer.  Add the escape
	    # character if the extension was escaped.
	    if (escape_extension) {
		extn[1] = '\\'
		lenextn = gstrcpy (Memc[field], extn[2], SZ_VFNFN-1) + 1
	    } else
		lenextn = gstrcpy (Memc[field], extn, SZ_VFNFN)

	    # If extn is the null string then root ended in a period; include
	    # the period in the root.
	    if (lenextn == 0)
		if (PERIOD_PERMITTED) {
		    root[out] = '.'
		    out = out + 1
		} else
		    call vvfn_escape (PERIOD_CODE, root, out, SZ_VFNFN)

	    root[out] = EOS
	    lenroot = out - 1

	} else {
	    extn[1] = EOS
	    lenextn = 0
	    lenroot = out + gstrcpy (Memc[field], root[out], SZ_VFNFN-out+1) - 1
	}

	call sfree (sp)
end


# VVFN_ESCAPE -- Deposit a character in the output buffer preceded by the
# escape character.  Ensure that the output buffer does not overflow.

procedure vvfn_escape (ch, outbuf, op, maxch)

int	ch		# character to be output (passed as an INT)
char	outbuf[maxch]	# output buffer
int	op		# output pointer (in/out)
int	maxch

begin
	outbuf[op] = VFN_ESCAPE_CHAR
	op = min (maxch, op + 1)
	outbuf[op] = ch
	op = min (maxch, op + 1)
end


# VFN_DECODE -- Decode an escape sequence encoded field of a filename.  This is
# easier than encoding because we have fewer decisions to make.
#
# NOTE: set the escape char to NUL on a machine which allows any character in a
# filename.  Since such a character will never be encountered in filenames this
# effectively turns off decoding and will prevent misinterpretation of OS
# filenames not written by IRAF.

int procedure vfn_decode (osfn, ip, outstr, maxch)

char	osfn[ARB]	# escape sequence encoded filename
int	ip		# input pointer (in/out)
char	outstr[maxch]	# output string
int	maxch

int	ch, op
bool	convert_to_upper
define	putback_ 91

begin
	convert_to_upper = false

	# Optimization: most filenames start with a simple sequence of letters.
	# Dispense with these as a special case, and fall into the more general
	# code when some other character is encountered.

	do op = 1, maxch {
	    ch = osfn[ip+op-1]
	    if (ch != VFN_ESCAPE_CHAR && IS_LOWER (ch))
		outstr[op] = ch
	    else {
		ip = ip + op - 1
		break
	    }
	}

	for (ch=osfn[ip];  ch != EOS && op <= maxch;  ch=osfn[ip]) {
	    # Process escapes.

	    if (ch == VFN_ESCAPE_CHAR && osfn[ip+1] != EOS) {
		ip = ip + 1
		ch = osfn[ip]

		switch (ch) {
		case SHIFT_NEXTCHAR:
		    if (IS_LOWER (osfn[ip+1])) {
			ip = ip + 1
			outstr[op] = TO_UPPER (osfn[ip])
			op = op + 1
		    } else
			goto putback_

		case SHIFT_TO_LOWER, SHIFT_TO_UPPER:
		    if (IS_LOWER (osfn[ip+1]))
			convert_to_upper = (ch == SHIFT_TO_UPPER)
		    else
			goto putback_

		case UNDERSCORE_CODE:
		    outstr[op] = '_'
		    op = op + 1

		case PERIOD_CODE:
		    outstr[op] = '.'
		    op = op + 1

		default:
		    # Not a recognized escape.  Output the escape char
		    # and put the next char back into the input.
putback_
		    outstr[op] = VFN_ESCAPE_CHAR
		    op = op + 1
		    ip = ip - 1
		}
	    
	    } else if (IS_LOWER (ch)) {
		if (convert_to_upper)
		    outstr[op] = TO_UPPER(ch)
		else
		    outstr[op] = ch
		op = op + 1

	    } else if (ch == EXTN_DELIMITER) {
		break

	    } else if (ch == FNLDIR_CHAR) {
		outstr[op] = '\\'
		op = op + 1
		outstr[op] = ch
		op = op + 1

	    } else {
		outstr[op] = ch
		op = op + 1
	    }

	    ip = ip + 1
	}

	outstr[op] = EOS
	return (op - 1)
end


# VFN_MAP_EXTENSION -- Map an IRAF filename extension to a host OS filename
# extension.  Unrecognized extensions are merely copied.  The set of
# extensions to be mapped is defined in <config.h>

procedure vfn_map_extension (iraf_extn, os_extn, maxch)

char	iraf_extn[ARB]		# IRAF filename extension
char	os_extn[maxch]		# OS filename extension
int	maxch

bool	first_time
char	first_char
int	extn
bool	streq()
data	first_time /true/

int	nextn			# number of extensions in map
short	iraf[MAX_EXTENSIONS]	# indices of IRAF extensions
short	os[MAX_EXTENSIONS]	# indices of OS extensions
char	map[SZ_EXTNMAP]		# iraf to os mappings, e.g. "|iraf,os|.."
common	/vfnxtn/ nextn, iraf, os, map

begin
	# Init the iraf and os index arrays and count the number of extension
	# in the map.

	if (first_time) {
	    call vvfn_init_extnmap (map, iraf, os, nextn, MAX_EXTENSIONS)
	    first_time = false
	}

	first_char = iraf_extn[1]

	# Escaped extensions, i.e., "root.\extn", are passed on unmodified but
	# with the escape character deleted.

	if (first_char == '\\') {
	    call strcpy (iraf_extn[2], os_extn, maxch)
	    return
	}
	    
	# Search map for the IRAF extension and of found return OS extension,
	# else return IRAF extension.

	if (first_char != EOS)
	    for (extn=1;  extn <= nextn;  extn=extn+1)
		if (map[iraf[extn]] == first_char)
		    if (streq (iraf_extn, map[iraf[extn]])) {
			call strcpy (map[os[extn]], os_extn, maxch)
			return
		    }

	call strcpy (iraf_extn, os_extn, maxch)
end


# VFN_UNMAP_EXTENSION -- Convert OS extension to IRAF extension by table lookup
# in the MAP array of extn pairs.

procedure vfn_unmap_extension (os_extn, iraf_extn, maxch)

char	os_extn[maxch]		# OS filename extension
char	iraf_extn[ARB]		# IRAF filename extension
int	maxch

int	extn
char	first_char
bool	first_time
bool	streq()
data	first_time /true/

int	nextn			# number of extensions in map
short	iraf[MAX_EXTENSIONS]	# indices of IRAF extensions
short	os[MAX_EXTENSIONS]	# indices of OS extensions
char	map[SZ_EXTNMAP]		# iraf to os mappings, e.g. "|iraf,os|.."
common	/vfnxtn/ nextn, iraf, os, map

begin
	# Init the iraf and os index arrays and count the number of extension
	# in the map.

	if (first_time) {
	    call vvfn_init_extnmap (map, iraf, os, nextn, MAX_EXTENSIONS)
	    first_time = false
	}

	# Search map for the OS extension and if found return IRAF extension.
	# If the OS extension matches an IRAF extension then escape the first
	# char of the extension to avoid interpretation as an IRAF extension.

	first_char = os_extn[1]
	if (first_char != EOS)
	    for (extn=1;  extn <= nextn;  extn=extn+1) {
		if (map[os[extn]] == first_char) {
		    if (streq (os_extn, map[os[extn]])) {
			call strcpy (map[iraf[extn]], iraf_extn, maxch)
			return
		    }
		}
		if (map[iraf[extn]] == first_char) {
		    if (streq (os_extn, map[iraf[extn]])) {
			iraf_extn[1] = '\\'
			call strcpy (map[iraf[extn]], iraf_extn[2], maxch-1)
			return
		    }
		}
	    }

	call strcpy (os_extn, iraf_extn, maxch)
end


# VVFN_INIT_EXTNMAP -- Scan the map and initialize the indices of the
# extension strings the first time we are called.  Replace the field
# delimiters with EOS to ease string comparisons.  The format of the map
# string is "Airaf,osA...", where A is any field delimiter character and
# a comma must be given between fields.  Embedded whitespace is not
# permitted.  For example:  map = "|a,olb|e,exe|"

procedure vvfn_init_extnmap (map, iraf, os, nextn, max_extn)

char	map[ARB]		# set of extns to be mapped
short	iraf[max_extn]		# indices of IRAF extensions
short	os[max_extn]		# indices of OS extensions
int	nextn			# number of extensions in map
int	max_extn

int	ip
char	delim
bool	first_time
data	first_time /true/

begin
	if (!first_time)
	    return

	call strcpy (EXTN_MAP, map, SZ_EXTNMAP)
	nextn = 0
	delim = map[1]
	if (delim == EOS)
	    return

	for (ip=2;  map[ip] != EOS && nextn < max_extn;  ip=ip+1) {
	    nextn = nextn + 1

	    iraf[nextn] = ip
	    while (map[ip] != ',' && map[ip] != EOS)
		ip = ip + 1
	    map[ip] = EOS
	    ip = ip + 1

	    os[nextn] = ip
	    while (map[ip] != delim && map[ip] != EOS)
		ip = ip + 1
	    map[ip] = EOS
	}

	if (nextn > max_extn)
	    call fatal (1, "fio$vfntrans.x: too many extensions")

	first_time = false
end


# VFN_SQUEEZE -- Squeeze the root filename (or any string) to fit into the
# output string.  Squeezing preserves the first N-1 and final characters of
# the input string, e.g., if N=6 "concatenate" is squeezed to "concae".

procedure vfn_squeeze (root, outstr, maxch)

char	root[ARB]		# input string to be squeezed
char	outstr[maxch]		# output, squeezed string
int	maxch			# length of squeezed string

int	ip, op

begin
	# Omit leading whitespace.
	for (ip=1;  IS_WHITE (root[ip]);  ip=ip+1)
	    ;

	# Squeeze root to outstr.
	for (op=0;  root[ip] != EOS;  ip=ip+1) {
	    op = min (maxch, op + 1)
	    outstr[op] = root[ip]
	}
	outstr[op+1] = EOS
end


# VFN_IS_HIDDEN_FILE -- Determine if the named file is a hidden file.
# Hidden files are files with reserved extensions.  The set of reserved
# extensions is given by a list of the form "|.ex1|.ext|...|".

int procedure vfn_is_hidden_file (fname)

char	fname[ARB]		# unpacked filename
char	ch
short	extn[MAX_RESERVEXTN]
bool	first_time
int	nextn, first_char, off, i
bool	streq()
int	strldx()
string	reserved RESERVED_EXTNS
data	first_time /true/

begin
	if (first_time) {
	    call vvfn_init_reserved_extns (reserved, extn,MAX_RESERVEXTN, nextn)
	    first_time = false
	}

	if (nextn > 0) {
	    ch = EXTN_DELIMITER
	    off = strldx (ch, fname) + 1
	    first_char = fname[off]

	    if (off > 0 && first_char != EOS)
		do i = 1, nextn
		    if (reserved[extn[i]] == first_char)
			if (streq (reserved[extn[i]], fname[off]))
			    return (YES)
	}
	
	return (NO)
end


# VVFN_INIT_RESERVED_EXTNS -- Inde the list of reserved extensions.  Overwrite
# the delimiter character with EOS, set the indices of the extension strings
# in the EXTN array, and count the number of extensions.  The format of the
# reserved extension array is "|str1|str2|str3|...|strN|", where the first
# char is taken to be the delimiter character.

procedure vvfn_init_reserved_extns (ex, extn, max_extn, nextn)

char	ex[ARB]			# list of reserved extensions
short	extn[max_extn]		# indices of substrings
int	max_extn		# max extensions
int	nextn			# number of extensions (output)

char	delim
int	ip

begin
	nextn = 0
	delim = ex[1]
	if (delim == EOS)
	    return
	ip = 2

	while (ex[ip] != EOS && nextn < max_extn) {
	    nextn = nextn + 1
	    extn[nextn] = ip

	    while (ex[ip] != delim && ex[ip] != EOS)
		ip = ip + 1

	    if (ex[ip] == delim) {
		ex[ip] = EOS
		ip = ip + 1
	    }
	}
end
