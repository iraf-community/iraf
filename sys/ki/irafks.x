# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<finfo.h>
include	<fset.h>
include	<fio.h>
include	<clset.h>
include	<knet.h>
include	"ki.h"

# KSERVER.X -- The IRAF kernel server, used to serve up kernel functions on
# a remote host.

define	DEBUG_FILE	"/tmp/ks.out"	# MACHDEP
#define	DEBUG_FILE	"iraftmp:ks.out"
define	DEBUG		NO
#define	DEBUG		YES

define	DEF_LENIOBUF	32768		# reallocated if too small
define	SZ_TXBUF	1024		# handy text buffer
define	LEN_BFDRIVER	8		# actually 6, but 8 is a power of 2
define	LEN_TXDRIVER	8		# no. entry points in tx driver
define	MAX_BFDRIVERS	10		# max binary file devices
define	MAX_TXDRIVERS	2		# max text file devices

define	LEN_BFDD	(LEN_BFDRIVER*MAX_BFDRIVERS)
define	LEN_TXDD	(LEN_TXDRIVER*MAX_TXDRIVERS)

define	ZOPNTX		txdd[($1)]		# text files
define	ZCLSTX		txdd[($1)+1]
define	ZGETTX		txdd[($1)+2]
define	ZPUTTX		txdd[($1)+3]
define	ZFLSTX		txdd[($1)+4]
define	ZSEKTX		txdd[($1)+5]
define	ZNOTTX		txdd[($1)+6]
define	ZSTTTX		txdd[($1)+7]

define	ZOPNBF		bfdd[($1)]		# binary files
define	ZCLSBF		bfdd[($1)+1]
define	ZARDBF		bfdd[($1)+2]
define	ZAWRBF		bfdd[($1)+3]
define	ZAWTBF		bfdd[($1)+4]
define	ZSTTBF		bfdd[($1)+5]


# IRAFKS -- The main entry point of the iraf kernel server task.  The kernel
# server program is not CL callable but is instead spawned by the OS to listen
# listen on a socket.  The ONENTRY procedure gains control from the IRAF main
# at process startup, before the in task interpreter is entered.  The t_irafks
# procedure is never actually called by the interpreter as the TASK statement 
# suggests.  The purpose of the task statement is to give us an IRAF main.

task	irafks = t_irafks
procedure t_irafks()
begin
end


# ONENTRY -- The real task executed by the irafks.e executable.

int procedure onentry (prtype, bkgfile, cmd)

int	prtype			#I process type flag (not used)
char	bkgfile[ARB]		#I bkgfilename if detached process (not used)
char	cmd[ARB]		#I optional command (used to flag irafks type) 

bool	error_restart
int	debuginit, chan, junk
char	osfn[SZ_PATHNAME], debugfile[SZ_PATHNAME]

int	getpid(), open()
data	error_restart /false/
data	debuginit /DEBUG/

int	debug, spy
common	/dbgcom/ debug, spy

begin
	# Debug messages can be enabled either by compiling with DEBUG=YES
	# or by running the kernel server with "-d debugfile" on the command
	# line (a bit of a trick using the detached process syntax).

	if (prtype == PR_DETACHED) {
	    debug = YES
	    call strcpy (bkgfile, debugfile, SZ_PATHNAME)
	} else {
	    debug = debuginit
	    call strcpy (DEBUG_FILE, debugfile, SZ_PATHNAME)
	}

	# If an error occurs and we go through error restart, deadlock will
	# probably occur as the client may be awaiting a status packet while
	# upon restart we will be awaiting a command packet.  Hence if restart
	# occurs, shut the kernel server down.

	if (error_restart) {
	    call zclsks (chan, junk)
	    return (PR_EXIT)
	} else
	    error_restart = true

	# Open the network connection to the host process.
	call strpak (cmd, osfn, SZ_PATHNAME)
	call zopnks (osfn, READ_WRITE, chan)

	# Open the debug file if so indicated.  The value of the debug flag
	# should be patched before execution with a debugger if debug output
	# is desired.

	if (debug == YES) {
	    spy = open (debugfile, APPEND, TEXT_FILE)
	    call fseti (spy, F_FLUSHNL, YES)

	    call fprintf (spy, "[%d] -------------------------\n")
		call pargi (getpid())
	    call fprintf (spy, "server channel = %d\n")
		call pargi (chan)
	}

	# Redirect the standard input and output of the kernel server task
	# to the null file to prevent deadlock if the task unexpectedly
	# reads or writes the standard input or output.

	call fredir (STDIN,  "dev$null", READ_ONLY,  TEXT_FILE)
	call fredir (STDOUT, "dev$null", WRITE_ONLY, TEXT_FILE)
	call fredir (STDERR, "dev$null", WRITE_ONLY, TEXT_FILE)

	# Serve up the kernel until EOF is seen on the input stream.
	if (chan != ERR)
	    call kserver (chan, chan, DEF_LENIOBUF)

	# Exit w/o running interpreter.
	call zclsks (chan, junk)
	return (PR_EXIT)
end


# KSERVER -- Kernel server interpreter.  This procedure is called from the
# kernel server program to interpret and execute (serve up) kernel instructions
# issued by a remote host.  Execution terminates when EOF is seen on the input
# channel.  All i/o is all low level since the level of function implemented
# here is that of the iraf kernel.  This code executes in a private subprocess
# hence we need not worry about memory usage or reentrancy.
#
# NOTE -- Avoid passing packet data to subprocedures, since the kernel
# procedures called directly or indirectly by this code may themselves use
# the local KII packet data structure.

procedure kserver (in, out, buflen)

int	in		# input channel, a binary stream
int	out		# output channel, a binary stream
int	buflen		# iobuf size or 0

pointer	iobuf, op, top
long	fi[LEN_FINFO]
char	curdir[SZ_PATHNAME]
int	len_iobuf, status, i, nchars, opcode, subcode, arg1, arg2, arg3
int	bfdd[LEN_BFDD], txdd[LEN_TXDD]

char	txbuf[SZ_TXBUF], queue[SZ_FNAME]
char	osfn1[SZ_PATHNAME], osfn2[SZ_PATHNAME], temp[SZ_PATHNAME]
char	o_str[SZ_LINE], s_str[SZ_LINE]
int	ks_receive(), ks_send(), strlen(), envscan()
int	diropen(), gstrcpy(), getline()
include	"kii.com"
errchk	ks_error
define	reply_ 91

int	debug, spy
common	/dbgcom/ debug, spy

begin
	if (debug == YES) {
	    call fprintf (spy, "start kernel server, in=%d, out=%d\n")
		call pargi (in)
		call pargi (out)
	}

	# Allocate a buffer for read and write requests on a channel.  We always
	# transfer data immediately, before accepting another request, hence
	# the same buffer may be reused for all requests.

	if (buflen > 0)
	    len_iobuf = buflen / SZB_CHAR
	else
	    len_iobuf = DEF_LENIOBUF / SZB_CHAR
	call malloc (iobuf, len_iobuf, TY_CHAR)

	if (debug == YES) {
	    call fprintf (spy, "kernel server, len_iobuf=%d\n")
		call pargi (len_iobuf)
	}

	# Load the device drivers.
	call ks_loadbf (bfdd)
	call ks_loadtx (txdd)

	# Initialize our record of the current working directory.
	curdir[1] = EOS

	# Enter the main interpreter loop of the kernel server, reading and
	# processing kernel requests from the host until EOF is seen.  The
	# host is completely in control.  Kernel requests are passed to the
	# local kernel (or to yet another kernel server) unchanged except
	# for filenames, which must be mapped in the context of the local
	# machine.

	while (ks_receive (in) > 0) {
	    opcode  = p_opcode
	    subcode = p_subcode
	    arg1    = p_arg[1]
	    arg2    = p_arg[2]
	    arg3    = p_arg[3]

	    p_sbuflen = 0

	    if (debug == YES) {
		call  ks_op2str (opcode, subcode, o_str, s_str)
		if (opcode < KI_ZFIOBF) {
		    call fprintf (spy, "opcode=%s, arg[] =")
		        call pargstr (o_str)
		} else {
		    call fprintf (spy, "opcode=%s), subcode=%s, arg[] =")
		        call pargstr (o_str)
		        call pargstr (s_str)
		}
		do i = 1, 10 {
		    call fprintf (spy, " %d")
		    call pargi (p_arg[i])
		}
		call fprintf (spy, "\n")
	    }

	    switch (opcode) {
	    case KI_ENVINIT:
		# Called shortly after process startup to pass the environment
		# list to the kernel server (req'd for filename mapping).
		# May also be called after process startup to add entries to
		# the environment list.  Note that the kernel server process,
		# since it is an IRAF process, will have read the zzsetenv.def
		# file to define the standard variables during process startup,
		# but the client must transmit its environment list anyhow to
		# set the values of any newly defined variables.

		# Get the packed environment list string.  If this is small
		# it is sent in the packet string buffer, otherwise the data
		# follows in a separate record.

		nchars = arg1
		if (nchars <= SZ_SBUF)
		    status = envscan (p_sbuf)
		else {
		    if (len_iobuf < nchars) {
			call realloc (iobuf, nchars, TY_CHAR)
			len_iobuf = nchars
		    }

		    call zardks (in, Memc[iobuf], nchars, long(0))
		    call zawtks (in, status)
		    if (status != nchars)
			break

		    # Unpack it and process it into the symbol table.
		    call strupk (Memc[iobuf], Memc[iobuf], nchars)
		    status = envscan (Memc[iobuf])
		}

		if (debug == YES) {
		    call fprintf (spy, "%d environment entries scanned\n")
			call pargi (status)
		}

		# Do not send a status packet back for single variable updates.
		if (nchars < SZ_SBUF)
		    next
		else if (status < 0)
		    break

		# The environment variables HOST, IRAF, and TMP may differ
		# from node to node.  The ZGTENV primitive in the local
		# kernel will pick up the local values of these variables
		# from the HSI global include file <iraf.h>.  This happens
		# automatically if the variables are not defined in the
		# environment list.  The simplest way to keep them out of
		# the environment list is to exclude them from the list when
		# it is composed by ki_openks() on the client node, so we
		# do not have to do anything here.

	    case KI_SETROOT:
		# Called to set the pathname of the root iraf directory on
		# the local node.  This need not be the same as on the client
		# node hence we must override the definition of "iraf" in the
		# environment list.  We are passed the OS pathname of the
		# server process (i.e., this process) which is assumed to be
		# resident in iraf$lib on the current node.  The pathname of
		# iraf$ is therefore obtained by a call to ZFXDIR followed by
		# a call to ZFSUBD with subdirectory "..".

		# NOTE -- This function is obsoleted by the above code which
		# sets the values of HOST, IRAF, and TMP from <iraf.h>.  Leave
		# it in for a while nonetheless, just in case it is called.

		call strcpy (p_sbuf[arg1], osfn1, SZ_PATHNAME)
		call zfxdir (osfn1, osfn2, SZ_PATHNAME, nchars)
		call zfsubd (osfn2, SZ_PATHNAME, "..", nchars)
		call strcpy ("set iraf=", osfn1, SZ_PATHNAME)
		call strcat (osfn2, osfn1, SZ_PATHNAME)

		status = envscan (osfn1)

		if (debug == YES) {
		    call fprintf (spy, "%s\n")
			call pargstr (osfn1)
		}

		if (status < 1)
		    break

	    case KI_FMAPFN:
		# Map a filename in the context of the server node.
		call strcpy (p_sbuf[arg1], osfn1, SZ_PATHNAME)
		iferr (call ks_fmapfn (osfn1, temp, SZ_PATHNAME))
		    status = ERR
		else {
		    call strupk (temp, p_sbuf, SZ_SBUF)
		    p_sbuflen = strlen (p_sbuf)
		    status = p_sbuflen
		}

	    case KI_ZFACSS:
		# Test file accessibility and/or type.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfacss (osfn1, arg2, arg3, status)

	    case KI_ZFALOC:
		# Preallocate space for a file.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfaloc (osfn1, arg2, status)

	    case KI_ZFCHDR:
		# Change the default directory.

		if (debug == YES) {
		    call fprintf (spy, "change directory to `%s'\n")
			call pargstr (p_sbuf[arg1])
		}

		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfchdr (osfn1, status)

		# Save the logical name of the new default directory, but only
		# if the zfchdr request is successful.

		if (status != ERR)
		    call strcpy (temp, curdir, SZ_PATHNAME)

	    case KI_ZFDELE:
		# Delete a file.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfdele (osfn1, status)

	    case KI_ZFGCWD:
		# Get the name of the current default directory.  Return the
		# unmapped name rather than the mapped OSFN because the client
		# deals with unmapped pathnames when the file resides on a
		# remote node.

		if (curdir[1] == EOS) {
		    call zfgcwd (osfn1, SZ_PATHNAME, status)
		    call strupk (osfn1, p_sbuf, SZ_SBUF)
		} else
		    status = gstrcpy (curdir, p_sbuf, SZ_SBUF)

		p_arg[2] = 1
		p_sbuflen = strlen (p_sbuf)

	    case KI_ZFINFO:
		# Get directory info for a file.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfinfo (osfn1, fi, status)

		if (status != ERR) {
		    # Return the integer part of the FI structure in args 2+.
		    do i = 1, min(MAX_ARGS-1, LEN_FINFO)
			p_arg[i+1] = fi[i]

		    # Return the owner string in the string buffer.
		    call strupk (FI_OWNER(fi), p_sbuf, SZ_SBUF)
		    p_sbuflen = strlen (p_sbuf)
		}

	    case KI_ZFMKCP:
		# Make a null length copy of a file.
		iferr {
		    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn1, SZ_PATHNAME)
		    call strcpy (p_sbuf[arg2], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn2, SZ_PATHNAME)
		} then {
		    status = ERR
		} else
		    call zfmkcp (osfn1, osfn2, status)

	    case KI_ZFMKDR:
		# Make a new directory.
		iferr {
		    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn1, SZ_PATHNAME)
		} then {
		    status = ERR
		} else
		    call zfmkdr (osfn1, status)

	    case KI_ZFPROT:
		# Set or query file protection.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else
		    call zfprot (osfn1, arg2, status)

	    case KI_ZFRNAM:
		# Rename a file.
		iferr {
		    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn1, SZ_PATHNAME)
		    call strcpy (p_sbuf[arg2], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn2, SZ_PATHNAME)
		} then {
		    status = ERR
		} else
		    call zfrnam (osfn1, osfn2, status)

	    case KI_ZFRMDR:
		# Remove a directory.
		iferr {
		    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn1, SZ_PATHNAME)
		} then {
		    status = ERR
		} else
		    call zfrmdr (osfn1, status)

	    case KI_ZDVALL:
		# Allocate or deallocate a device.
		if (debug == YES) {
		    call fprintf (spy, "allocate `%s' flag=%d\n")
			call pargstr (p_sbuf[arg1])
			call pargi (arg2)
		}
		call strpak (p_sbuf[arg1], temp, SZ_PATHNAME)
		call zdvall (temp, arg2, status)

	    case KI_ZDVOWN:
		# Query device allocation.
		call strpak (p_sbuf[arg1], osfn1, SZ_PATHNAME)
		call zdvown (osfn1, temp, SZ_PATHNAME, status)
		call strupk (temp, p_sbuf, SZ_SBUF)
		p_sbuflen = strlen (p_sbuf)

            case KI_ZFUTIM:
                # Update thje file modify time.
                call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
                if (debug == YES) {
                    call fprintf (spy, "utime `%s' atime=%d mtime=%d\n")
                        call pargstr (temp)
                        call pargi (arg2)
                        call pargi (arg3)
                }
                iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
                    status = ERR
                else
                    call zfutim (osfn1, arg2, arg3, status)

	    case KI_ZOPDIR:
		# Open a directory for reading.  Since we must perform the
		# inverse mapping we use the high level DIROPEN package
		# rather than call the kernel directly.

		if (debug == YES) {
		    call fprintf (spy, "open directory `%s', mode %d\n")
			call pargstr (p_sbuf[arg1])
			call pargi (arg2)
		}

		call strcpy (p_sbuf[arg1], osfn1, SZ_PATHNAME)
		iferr (status = diropen (osfn1, arg2))
		    status = ERR

		if (debug == YES) {
		    call fprintf (spy, "diropen returns %d\n")
			call pargi (status)
		}

	    case KI_ZCLDIR:
		# Close a directory file.

		iferr (call close (arg1))
		    status = ERR
		else
		    status = OK

	    case KI_ZGFDIR:
		# Get the next filename from a directory.  To reduce traffic
		# on the net we return many filenames at once.  The reverse
		# mapping must be performed locally, returning VFN's to the
		# client process.

		top = iobuf + min (len_iobuf, arg2)
		op  = iobuf

		# Fill the output buffer.  Set argument 2 to 1 if EOF is seen
		# on the directory.

		arg2 = 1
		iferr {
		    while (getline (arg1, Memc[op]) != EOF) {
			op = op + strlen (Memc[op])
			if (op + SZ_FNAME >= top) {
			    arg2 = 0
			    break
			}
		    }
		} then {
		    status = ERR
		    p_arg[2] = arg2
		    goto reply_
		}

		# If the data is small enough return it in the string buffer,
		# else return it as a second record.

		nchars = op - iobuf
		if (nchars <= SZ_SBUF) {
		    call amovc (Memc[iobuf], p_sbuf, nchars)
		    p_sbuflen = nchars
		    status = nchars
		    # goto reply_

		} else {
		    p_arg[1] = nchars
		    if (ks_send (out, opcode, subcode) == ERR)
			return

		    call chrpak (Memc[iobuf], 1, Memc[iobuf], 1, nchars)
		    call zawrks (out, Memc[iobuf], nchars, long(0))
		    call zawtks (out, status)
		    if (status <= 0)
			return
		    
		    next
		}

	    case KI_ZOSCMD:
		# Issue a command to the local host command interpreter.
		# Spool the output in a file and return the name of the
		# file to the client so that it can recover the output via
		# the text file i/o interface.

		call strpak (p_sbuf[arg1], txbuf, SZ_TXBUF)
		call strpak ("", osfn1, SZ_PATHNAME)
		call mktemp ("tmp$zos", temp, SZ_PATHNAME)
		call ks_fmapfn (temp, osfn2, SZ_PATHNAME)

		call zoscmd (txbuf, osfn1, osfn2, osfn2, status)

		call strupk (osfn2, p_sbuf, SZ_SBUF)
		p_sbuflen = strlen (p_sbuf)

	    case KI_ZOPDPR:
		# Open a detached process (submit bkg job).
		iferr {
		    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn1, SZ_PATHNAME)
		    call strcpy (p_sbuf[arg2], temp, SZ_PATHNAME)
		    call ks_fmapfn (temp, osfn2, SZ_PATHNAME)
		    call strcpy (p_sbuf[arg3], queue, SZ_FNAME)
		} then {
		    status = ERR
		} else
		    call zopdpr (osfn1, osfn2, queue, status)

	    case KI_ZCLDPR:
		# Close a detached process.
		call zcldpr (arg1, arg2, status)

	    case KI_ZOPCPR:
		# Open a connected subprocess.
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn1, SZ_PATHNAME))
		    status = ERR
		else {
		    call zopcpr (osfn1, arg2, arg3, status)
		    p_arg[2] = arg2
		    p_arg[3] = arg3
		}

	    case KI_ZCLCPR:
		# Close a connected subprocess.
		call zclcpr (arg1, status)

	    case KI_ZINTPR:
		# Interrupt a connected subprocess.
		call zintpr (arg1, arg2, status)

	    case KI_ZFIOBF,KI_ZFIOLP,KI_ZFIOPL,KI_ZFIOPR,KI_ZFIOSF,KI_ZFIOGD:
		# Binary file drivers.
		iferr (call ks_zfiobf (in, out, iobuf, len_iobuf, bfdd))
		    break
		else
		    next

	    case KI_ZFIOTX, KI_ZFIOTY:
		# Text file drivers.
		iferr (call ks_zfiotx (in, out, iobuf, len_iobuf, txdd))
		    break
		else
		    next

	    case KI_ZFIOMT:
		# Magtape driver.
		iferr (call ks_zfiomt (in, out, iobuf, len_iobuf))
		    break
		else
		    next

	    default:
		# If we receive an illegal opcode on the channel shut the
		# server down immediately, as communications have almost
		# certainly been irrecoverably corrupted.  We should probably
		# go one step further and compute a checksum on the packet
		# header.

		call ks_error (opcode, "illegal opcode on channel")
	    }

	    # Transmit response packet back to host.  Shutdown if there is
	    # an i/o error on the socket.
reply_
	    if (debug == YES) {
		call fprintf (spy, "status = %d\n")
		    call pargi (status)
		call flush (spy)
	    }

	    p_arg[1] = status
	    if (ks_send (out, opcode, subcode) == ERR)
		break
	}

	call mfree (iobuf, TY_CHAR)

	if (debug == YES) {
	    call fprintf (spy, "kernel server, normal exit\n")
	    call flush (spy)
	}
end


# KS_ZFIOBF -- I/O to the class of binary file devices.  The i/o request is
# passed in the KII common (unpacked packet from the host via the network).

procedure ks_zfiobf (in, out, iobuf, len_iobuf, bfdd)

int	in, out			# input and output channels to host
pointer	iobuf			# scratch i/o buffer
int	len_iobuf		# current length of buffer
int	bfdd[ARB]		# loaded device drivers

long	lval, ks_maxbufsize
int	dd, status, nchars, arg1, arg2, arg3
char	osfn[SZ_PATHNAME], temp[SZ_PATHNAME]
errchk	realloc
int	ks_send()
include	"kii.com"
define	fatal_ 91

int	debug, spy
common	/dbgcom/ debug, spy

begin
	# Determine the table offset of the device driver in the table of all
	# loaded binary file device drivers.  The device driver opcodes are
	# assigned sequentially and KI_ZFIOBF is always first.

	dd = (p_opcode - KI_ZFIOBF) * LEN_BFDRIVER + 1

	# Make sure the iobuffer is large enough.  If a large enough buffer
	# cannot be allocated something is very wrong and the server shuts
	# down.

	if (p_subcode == BF_ARD || p_subcode == BF_AWR) {
	    nchars = (p_arg[2] + SZB_CHAR-1) / SZB_CHAR
	    if (len_iobuf < nchars) {
		call realloc (iobuf, nchars, TY_CHAR)
		len_iobuf = nchars
	    }
	}

	arg1 = p_arg[1]
	arg2 = p_arg[2]
	arg3 = p_arg[3]

	switch (p_subcode) {
	case BF_OPN:
	    # Open a binary file.

	    if (debug == YES) {
		call fprintf (spy, "open binary file `%s', mode %d\n")
		    call pargstr (p_sbuf[arg1])
		    call pargi (arg2)
	    }

	    # Do not map the filename strings of special devices, since the
	    # syntax of such strings may bear no resemblance to that of an
	    # ordinary filename.

	    status = OK
	    if (p_opcode == KI_ZFIOBF) {
		call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
		iferr (call ks_fmapfn (temp, osfn, SZ_PATHNAME))
		    status = ERR
	    } else
		call strpak (p_sbuf[arg1], osfn, SZ_PATHNAME)

	    if (status != ERR)
		call zcall3 (ZOPNBF(dd), osfn, arg2, status)

	case BF_CLS:
	    # Close a binary file.

	    if (debug == YES) {
		call fprintf (spy, "close %d\n")
		    call pargi (arg1)
	    }

	    call zcall2 (ZCLSBF(dd), arg1, status)

	case BF_ARD:
	    # Read from a binary file.  The read must be performed in one
	    # operation to preserve the record size.  Overlapped i/o is
	    # provided by the dual process nature of the read; the actual
	    # device read is not performed asynchronously since we must
	    # complete each request before processing the next one.

	    if (debug == YES) {
		call fprintf (spy, "aread (%d, %d, %d)\n")
		    call pargi (arg1)
		    call pargi (arg2)
		    call pargi (arg3)
	    }

	    # Read the data.
	    call zcall4 (ZARDBF(dd), arg1, Memc[iobuf], arg2, arg3)
	    call zcall2 (ZAWTBF(dd), arg1, status)

	    # Send the ZAWT packet to the host followed by the data block.
	    # The next operation performed by the host on the channel MUST
	    # be completion of the i/o transfer, but the host can go off and
	    # do other things before completing the transfer.

	    p_arg[1] = status
	    if (ks_send (out, p_opcode, BF_AWT) == ERR)
		goto fatal_
	    if (status > 0) {
		call zawrks (out, Memc[iobuf], status, 0)
		call zawtks (out, status)
		if (status <= 0)
		    goto fatal_
	    }

	    if (debug == YES) {
		call fprintf (spy, "status %d\n")
		    call pargi (status)
	    }

	    return

	case BF_AWR:
	    # Write to a binary file.  For maximum performance the write
	    # operation is half duplex, i.e., the ZAWT operation is ignored
	    # for writes to a binary file over the network.  If a write
	    # error occurs when writing to the physical device we shutdown
	    # the entire kernel process, causing a FIO write error in the
	    # host if the next kernel server operation is another write to
	    # the same channel.  This may cause ficticious i/o errors on
	    # other channels as well, but the performance gain is worth it.

	    if (debug == YES) {
		call fprintf (spy, "awrite (%d, %d, %d)\n")
		    call pargi (arg1)
		    call pargi (arg2)
		    call pargi (arg3)
	    }

	    # Read the data from the host.
	    call zardks (in, Memc[iobuf], arg2, 0)
	    call zawtks (in, status)
	    if (debug == YES) {
		call fprintf (spy, "read net status %d\n")
		    call pargi (status)
	    }
	    if (status != arg2)
		goto fatal_

	    # Write the data to the output device.
	    # TODO - delay the call to zawtbf to overlap i/o even further.

	    call zcall4 (ZAWRBF(dd), arg1, Memc[iobuf], arg2, arg3)
	    call zcall2 (ZAWTBF(dd), arg1, status)
	    if (debug == YES) {
		call fprintf (spy, "write device status %d\n")
		    call pargi (status)
	    }
	    if (status != arg2)
		goto fatal_

	    return

	case BF_AWT:
	    # Wait for i/o on the device channel.  Not implemented as a
	    # discreen kernel server operation.

	    status = ERR

	case BF_STT:
	    # Get channel status.
	    call zcall3 (ZSTTBF(dd), arg1, arg2, lval)

	    # The max transfer size for a binary device is limited by the
	    # network interface as well as the device.

	    if (arg2 == FSTT_MAXBUFSIZE || arg2 == FSTT_OPTBUFSIZE) {
		call zsttks (out, FSTT_MAXBUFSIZE, ks_maxbufsize)
		if (lval == 0)
		    lval = ks_maxbufsize
		else if (ks_maxbufsize > 0)
		    lval = min (ks_maxbufsize, lval)
	    }
	    status = lval

	default:
	    status = ERR
	}

	# Return a status packet to the host if the operation was not a read
	# or a write.

	if (debug == YES) {
	    call fprintf (spy, "status %d\n")
		call pargi (status)
	}

	p_arg[1] = status
	if (ks_send (out, p_opcode, p_subcode) != ERR)
	    return

fatal_
	call ks_error (1, "kernel server binary file i/o error")
end


# KS_ZFIOTX -- I/O to the class of text file devices.  The i/o request is
# passed in the KII common (unpacked packet from the host via the network).
# Text file i/o is buffered by the KI, reading and writing an integral
# number of full lines of text in each transfer.

procedure ks_zfiotx (in, out, iobuf, len_iobuf, txdd)

int	in, out			# input and output channels to host
pointer	iobuf			# scratch i/o buffer (not used)
int	len_iobuf		# current length of buffer (not used)
int	txdd[ARB]		# loaded device drivers

long	lval, reclen
bool	buffer_full
pointer	rp, nextrec
char	osfn[SZ_PATHNAME], temp[SZ_PATHNAME]
int	dd, status, maxch, nchars, arg1, arg2, arg3

int	ks_send()
long	ki_decode()
include	"kii.com"
define	fatal_ 91

int	debug, spy
common	/dbgcom/ debug, spy

begin
	# Determine the table offset of the device driver in the table of all
	# loaded text file device drivers.  The device driver opcodes are
	# assigned sequentially and KI_ZFIOTX is always first.

	dd = (p_opcode - KI_ZFIOTX) * LEN_TXDRIVER + 1

	arg1 = p_arg[1]
	arg2 = p_arg[2]
	arg3 = p_arg[3]

	switch (p_subcode) {
	case TX_OPN:
	    # Open a text file.

	    if (debug == YES) {
		call fprintf (spy, "open text file `%s', mode %d\n")
		    call pargstr (p_sbuf[arg1])
		    call pargi (arg2)
	    }

	    call strcpy (p_sbuf[arg1], temp, SZ_PATHNAME)
	    iferr (call ks_fmapfn (temp, osfn, SZ_PATHNAME))
		status = ERR
	    else
		call zcall3 (ZOPNTX(dd), osfn, arg2, status)

	case TX_CLS:
	    # Close a binary file.

	    if (debug == YES) {
		call fprintf (spy, "close text file %d\n")
		    call pargi (arg1)
	    }

	    call zcall2 (ZCLSTX(dd), arg1, status)

	case TX_GET:
	    # Read from a text file.  If the device is an ordinary text file
	    # (device TX) read as many lines of maximum size SZ_LINE as will
	    # fit in the output buffer.  If the device is a terminal return
	    # a single line in each call.  Each line is returned as a record
	    # with the record length and seek offset of the line included
	    # in the record header (buffering complicates the ZNOTTX function).

	    if (debug == YES) {
		call fprintf (spy, "gettx %d\n")
		    call pargi (arg1)
	    }

	    rp = iobuf

	    repeat {
		maxch = min (arg2, SZ_LINE)
		call zcall2 (ZNOTTX(dd), arg1, lval)
		call zcall4 (ZGETTX(dd), arg1, Memc[R_DATA(rp)],
		    maxch, status)

		if (status >= 0) {
		    reclen = R_GETRECLEN (status)
		    call ki_encode (reclen, R_RECLEN(rp), NCHARS_INT)
		    call ki_encode (lval,   R_SEKOFF(rp), NCHARS_LONG)
		    
		    rp = rp + reclen
		}

		nextrec = rp + R_GETRECLEN (SZ_LINE)
		buffer_full = (nextrec - iobuf > arg2)

	    } until (p_opcode != KI_ZFIOTX || status <= 0 || buffer_full)

	    # If the data record is small enough to fit in the packet string
	    # buffer, return it in the packet, else return it as a second
	    # record following the packet.

	    if (status == ERR)
		nchars = ERR
	    else if (status == EOF && rp == iobuf)
		nchars = EOF
	    else {
		nchars = rp - iobuf
		if (nchars <= SZ_SBUF) {
		    call amovc (Memc[iobuf], p_sbuf, nchars)
		    p_sbuflen = nchars
		}
	    }

	    p_arg[1] = nchars
	    if (ks_send (out, p_opcode, p_subcode) == ERR)
		goto fatal_

	    if (nchars > SZ_SBUF) {
		call chrpak (Memc[iobuf], 1, Memc[iobuf], 1, nchars)
		call zawrks (out, Memc[iobuf], nchars, long(0))
		call zawtks (out, status)
		if (status != nchars)
		    goto fatal_
	    }

	    return

	case TX_PUT:
	    # Put a block of data to a text file.  If the block is larger than
	    # the packet string buffer it is passed as a second record following
	    # the packet.

	    nchars = arg2
	    if (nchars <= SZ_SBUF)
		call zcall4 (ZPUTTX(dd), arg1, p_sbuf, nchars, status)
	    else {
		call zardks (in, Memc[iobuf], nchars, long(0))
		call zawtks (in, status)
		if (status != nchars)
		    goto fatal_

		call chrupk (Memc[iobuf], 1, Memc[iobuf], 1, nchars)
		call zcall4 (ZPUTTX(dd), arg1, Memc[iobuf], nchars, status)
	    }

	    # If an error occurs writing to a text file close the kernel
	    # server itself down, rather than handshaking on each packet.

	    if (status == ERR)
		goto fatal_

	    return

	case TX_FLS:
	    # Flush text file output.
	    call zcall2 (ZFLSTX(dd), arg1, status)

	case TX_SEK:
	    # Seek on a text file.
	    lval = ki_decode (p_sbuf, NCHARS_LONG)
	    call zcall3 (ZSEKTX(dd), arg1, lval, status)

	case TX_NOT:
	    # Note the file position of a text file.  The seek offset is
	    # returned encoded as a char sequence in the string buffer to
	    # avoid problems with integer precision.

	    call zcall2 (ZNOTTX(dd), arg1, lval)
	    call ki_encode (lval, p_sbuf, NCHARS_LONG)
	    p_sbuflen = NCHARS_LONG
	    status = lval

	case TX_STT:
	    # Get channel status.

	    call zcall3 (ZSTTTX(dd), arg1, arg2, lval)
	    call ki_encode (lval, p_sbuf, NCHARS_LONG)
	    p_sbuflen = NCHARS_LONG
	    status = lval

	default:
	    status = ERR
	}

	# Return a status/data packet to the host if the subcode did not
	# specify the get or put function.

	if (debug == YES) {
	    call fprintf (spy, "status %d\n")
	    call pargi (status)
	}

	p_arg[1] = status
	if (ks_send (out, p_opcode, p_subcode) != ERR)
	    return

fatal_
	call ks_error (1, "kernel server text file i/o error")
end


# KS_ZFIOMT -- I/O to the magtape device.  The i/o request is passed in
# the KII common (unpacked packet from the host via the network).

procedure ks_zfiomt (in, out, iobuf, len_iobuf)

int	in, out			# input and output channels to host
pointer	iobuf			# scratch i/o buffer
int	len_iobuf		# current length of buffer

long	lval
int	status, nchars, mode, dc_off, dc_len
int	newfile, arg[MAX_ARGS]
char	drive[SZ_FNAME]
errchk	realloc
include	"kii.com"
int	ks_send()
define	fatal_ 91

int	debug, spy
common	/dbgcom/ debug, spy

begin
	call amovi (p_arg, arg, MAX_ARGS)

	# Make sure the iobuffer is large enough.  If a large enough buffer
	# cannot be allocated something is very wrong and the server shuts
	# down.

	if (p_subcode == MT_RD || p_subcode == MT_WR) {
	    nchars = (arg[2] + SZB_CHAR-1) / SZB_CHAR
	    if (len_iobuf < nchars) {
		call realloc (iobuf, nchars, TY_CHAR)
		len_iobuf = nchars
	    }
	}

	switch (p_subcode) {
	case MT_OP:
	    # Open a magtape device.

	    mode = arg[2]
	    dc_off = arg[3]
	    dc_len = arg[4]
	    newfile = arg[5]

	    # Get the device name string.
	    call strpak (p_sbuf[arg[1]], drive, SZ_PATHNAME)

	    # Get the devcap string.
	    if (dc_len > 0 && dc_off == 0) {
		call zardks (in, Memc[iobuf], dc_len+1, 0)
		call zawtks (in, status)
		if (status != dc_len + 1)
		    goto fatal_
	    } else
		call strpak (p_sbuf[dc_off], Memc[iobuf], len_iobuf)

	    if (debug == YES) {
		call fprintf (spy,
		    "open magtape device `%s', mode=%d, file=%d, devcap=`%s'\n")
		    call pargstr (p_sbuf[arg[1]])
		    call pargi (mode)
		    call pargi (newfile)

		    call strupk (Memc[iobuf], Memc[iobuf], len_iobuf)
		    call pargstr (Memc[iobuf])
		    call strpak (Memc[iobuf], Memc[iobuf], len_iobuf)
	    }

	    call zzopmt (drive, mode, Memc[iobuf], arg[6], newfile, status)
	    p_arg[2] = newfile

	case MT_CL:
	    # Close a binary file.
	    call zzclmt (arg[1], p_arg[2], status)

	case MT_RD:
	    # Read from a magtape file.  The read must be performed in one
	    # operation to preserve the record size.  Overlapped i/o is
	    # provided by the dual process nature of the read; the actual
	    # device read is not performed asynchronously since we must
	    # complete each request before processing the next one.

	    # Read the data.
	    call zzrdmt (arg[1], Memc[iobuf], arg[2], arg[3])
	    call zzwtmt (arg[1], p_arg[2], status)

	    # Send the ZAWT packet to the host followed by the data block.
	    # The next operation performed by the host on the channel MUST
	    # be completion of the i/o transfer, but the host can go off and
	    # do other things before completing the transfer.

	    p_arg[1] = status
	    if (ks_send (out, p_opcode, MT_WT) == ERR)
		goto fatal_
	    if (status > 0) {
		call zawrks (out, Memc[iobuf], status, 0)
		call zawtks (out, status)
		if (status <= 0)
		    goto fatal_
	    }

	    return

	case MT_WR:
	    # Write to a magtape file.  For maximum performance the write
	    # operation is half duplex, i.e., the ZAWT operation is ignored
	    # for writes to a binary file over the network.  If a write
	    # error occurs when writing to the physical device we shutdown
	    # the entire kernel process, causing a FIO write error in the
	    # host if the next kernel server operation is another write to
	    # the same channel.  This may cause ficticious i/o errors on
	    # other channels as well, but the performance gain is worth it.

	    # Read the data from the host.
	    call zardks (in, Memc[iobuf], arg[2], 0)
	    call zawtks (in, status)
	    if (status != arg[2])
		goto fatal_

	    # Write the data to the output device.
	    # TODO - delay the call to zawtbf to overlap i/o even further.

	    call zzwrmt (arg[1], Memc[iobuf], arg[2], arg[3])
	    call zzwtmt (arg[1], p_arg[2], status)
	    if (status != arg[2])
		goto fatal_

	    return

	case MT_WT:
	    # Wait for i/o on the device channel.  Not implemented as a
	    # discreet kernel server operation; for a read the wait status
	    # is returned with the data, and for a write we assume the normal
	    # status and break the connection if the assumption is false.

	    status = ERR

	case MT_ST:
	    # Get device status.
	    call zzstmt (arg[1], arg[2], lval)
	    status = lval

	case MT_RW:
	    # Rewind a drive.
	    dc_off = p_arg[2]
	    dc_len = p_arg[3]

	    # Get the device name string.
	    call strpak (p_sbuf[arg[1]], drive, SZ_PATHNAME)

	    # Get the devcap string.
	    if (dc_len > 0 && dc_off == 0) {
		call zardks (in, Memc[iobuf], dc_len+1, 0)
		call zawtks (in, status)
		if (status != dc_len + 1)
		    goto fatal_
	    } else
		call strpak (p_sbuf[dc_off], Memc[iobuf], len_iobuf)

	    call zzrwmt (drive, Memc[iobuf], status)

	default:
	    status = ERR
	}

	# Return a status packet to the host if the operation was not a read
	# or a write.

	if (debug == YES) {
	    call fprintf (spy, "status %d\n")
	    call pargi (status)
	}

	p_arg[1] = status
	if (ks_send (out, p_opcode, p_subcode) != ERR)
	    return

fatal_
	call ks_error (1, "kernel server magtape i/o error")
end


# KS_ERROR -- Spool error message if debug is enabled, then call error
# to kill kernel server.

procedure ks_error (errcode, errmsg)

int	errcode
char	errmsg[ARB]

int	debug, spy
common	/dbgcom/ debug, spy

begin
	if (debug == YES) {
	    call fprintf (spy, "ERROR (%d, `%s')\n")
		call pargi   (errcode)
		call pargstr (errmsg)
	}

	call error (errcode, errmsg)
end


# KS_LOADBF -- Load the binary file drivers.  The order in which the driver
# entry points are loaded must agree with the defines at the head of this file.

procedure ks_loadbf (bfdd)

int	bfdd[ARB]			# device table
int	off, locpr()
extern	zopnbf(), zclsbf(), zardbf(), zawrbf(), zawtbf(), zsttbf()
extern	zopnlp(), zclslp(), zardlp(), zawrlp(), zawtlp(), zsttlp()
extern	zopnpl(), zclspl(), zardpl(), zawrpl(), zawtpl(), zsttpl()
extern			    zardpr(), zawrpr(), zawtpr(), zsttpr()
extern	zopnsf(), zclssf(), zardsf(), zawrsf(), zawtsf(), zsttsf()
extern	zopngd(), zclsgd(), zardgd(), zawrgd(), zawtgd(), zsttgd()

begin
	off = (KI_ZFIOBF - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = locpr (zopnbf)
	bfdd[off+2] = locpr (zclsbf)
	bfdd[off+3] = locpr (zardbf)
	bfdd[off+4] = locpr (zawrbf)
	bfdd[off+5] = locpr (zawtbf)
	bfdd[off+6] = locpr (zsttbf)

	off = (KI_ZFIOLP - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = locpr (zopnlp)
	bfdd[off+2] = locpr (zclslp)
	bfdd[off+3] = locpr (zardlp)
	bfdd[off+4] = locpr (zawrlp)
	bfdd[off+5] = locpr (zawtlp)
	bfdd[off+6] = locpr (zsttlp)

	off = (KI_ZFIOPL - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = locpr (zopnpl)
	bfdd[off+2] = locpr (zclspl)
	bfdd[off+3] = locpr (zardpl)
	bfdd[off+4] = locpr (zawrpl)
	bfdd[off+5] = locpr (zawtpl)
	bfdd[off+6] = locpr (zsttpl)

	off = (KI_ZFIOPR - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = 0
	bfdd[off+2] = 0
	bfdd[off+3] = locpr (zardpr)
	bfdd[off+4] = locpr (zawrpr)
	bfdd[off+5] = locpr (zawtpr)
	bfdd[off+6] = locpr (zsttpr)

	off = (KI_ZFIOSF - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = locpr (zopnsf)
	bfdd[off+2] = locpr (zclssf)
	bfdd[off+3] = locpr (zardsf)
	bfdd[off+4] = locpr (zawrsf)
	bfdd[off+5] = locpr (zawtsf)
	bfdd[off+6] = locpr (zsttsf)

	off = (KI_ZFIOGD - KI_ZFIOBF) * LEN_BFDRIVER
	bfdd[off+1] = locpr (zopngd)
	bfdd[off+2] = locpr (zclsgd)
	bfdd[off+3] = locpr (zardgd)
	bfdd[off+4] = locpr (zawrgd)
	bfdd[off+5] = locpr (zawtgd)
	bfdd[off+6] = locpr (zsttgd)
end


# KS_LOADTX -- Load the text file drivers.  The order in which the driver
# entry points are loaded must agree with the defines at the head of this file.

procedure ks_loadtx (txdd)

int	txdd[ARB]			# device table
int	off, locpr()
extern	zopntx(), zclstx(), zgettx(), zputtx(), zflstx(), zsektx(), znottx(),
	zstttx()
extern	zopnty(), zclsty(), zgetty(), zputty(), zflsty(), zsekty(), znotty(),
	zsttty()

begin
	off = (KI_ZFIOTX - KI_ZFIOTX) * LEN_TXDRIVER
	txdd[off+1] = locpr (zopntx)
	txdd[off+2] = locpr (zclstx)
	txdd[off+3] = locpr (zgettx)
	txdd[off+4] = locpr (zputtx)
	txdd[off+5] = locpr (zflstx)
	txdd[off+6] = locpr (zsektx)
	txdd[off+7] = locpr (znottx)
	txdd[off+8] = locpr (zstttx)

	off = (KI_ZFIOTY - KI_ZFIOTX) * LEN_TXDRIVER
	txdd[off+1] = locpr (zopnty)
	txdd[off+2] = locpr (zclsty)
	txdd[off+3] = locpr (zgetty)
	txdd[off+4] = locpr (zputty)
	txdd[off+5] = locpr (zflsty)
	txdd[off+6] = locpr (zsekty)
	txdd[off+7] = locpr (znotty)
	txdd[off+8] = locpr (zsttty)
end


# KS_SEND -- Encode the packet in the kii common in a machine independent form
# and send it over the network.

int procedure ks_send (server, opcode, subcode)

int	server			# channel to host
int	opcode			# function opcode
int	subcode			# function subcode (for drivers)

int	status
include	"kii.com"

int	debug, spy
common	/dbgcom/ debug, spy

begin
	p_opcode  = opcode
	p_subcode = subcode

	# Encode the packet in machine independent form, i.e., LEN_INTFIELDS
	# 32 bit MII integers followed by p_sbuflen chars, one char per byte.

	call miipak32 (FIRSTINTFIELD, p_packet, LEN_INTFIELDS, TY_INT)
	call chrpak (p_sbuf, 1, p_packet, LEN_INTFIELDS * 4 + 1,
	    min (SZ_SBUF, p_sbuflen + 1))

	# Transmit the packet.
	call zawrks (server, p_packet, SZB_PACKET, long(0))
	call zawtks (server, status)

	if (debug == YES) {
	    call fprintf (spy, "ks_send: status=%d\n");call pargi (status)
	}

	return (status)
end


# KS_RECEIVE -- Read a machine independent KII packet from the network
# interface and decode it into the internal, machine dependent form in the
# kii common.  This procedure differs from the procedure KI_RECEIVE in
# that it does not verify the opcode and subcode of the received packet.

int procedure ks_receive (server)

int	server			# os channel to server process
int	status
include	"kii.com"

int	debug, spy
common	/dbgcom/ debug, spy

begin
	# Read the packet.
	# [DEBUG]: call zzrdks (server, p_packet, SZB_PACKET, long(0))

	call zardks (server, p_packet, SZB_PACKET, long(0))
	call zawtks (server, status)

	if (debug == YES && status <= 0) {
	    call fprintf (spy, "ERROR: ks_receive: status=%d\n")
		call pargi (status)
	}

	if (status <= 0)
	    return (status)

	# The encoded packet consists of LEN_INTFIELDS 32 bit MII integers
	# followed by p_sbuflen chars, one char per byte.

	call miiupk32 (p_packet, FIRSTINTFIELD, LEN_INTFIELDS, TY_INT)
	call chrupk (p_packet, LEN_INTFIELDS * 4 + 1, p_sbuf, 1,
	    min (SZ_SBUF, p_sbuflen + 1))

	if (debug == YES) {
	    call fprintf (spy, "ks_receive: status=%d\n") ; call pargi (status)
	}

	return (status)
end


# KS_FMAPFN -- Equivalent functionality of FMAPFN but with debug output.

procedure ks_fmapfn (vfn, osfn, maxch)

char	vfn[ARB]		# filename to be mapped
char	osfn[maxch]		# packed output OS filename
int	maxch			# max chars out

char	upk_osfn[SZ_FNAME]
errchk	fmapfn
int	debug, spy
common	/dbgcom/ debug, spy

begin
	call fmapfn (vfn, osfn, maxch)

	if (debug == YES) {
	    call strupk (osfn, upk_osfn, SZ_FNAME)

	    call fprintf (spy, "`%s' -> `%s'\n")
		call pargstr (vfn)
		call pargstr (upk_osfn)
	}
end


procedure ks_op2str (opcode, subcode, o_str, s_str)

int	opcode			#i opcode
int	subcode			#i opcode
char	o_str[ARB]		#o string containing opcode instruction
char	s_str[ARB]		#o string containing subcode instruction

begin
	switch (opcode) {
	case KI_ENVINIT: call strcpy ("KI_ENVINIT", o_str, SZ_LINE) 
	case KI_SETROOT: call strcpy ("KI_SETROOT", o_str, SZ_LINE) 
	case KI_ZOSCMD:  call strcpy ("KI_OSCMD",   o_str, SZ_LINE) 
	case KI_FMAPFN:  call strcpy ("KI_FMAPFN",  o_str, SZ_LINE) 

	case KI_ZFACSS:  call strcpy ("KI_ZFACSS", o_str, SZ_LINE)  
	case KI_ZFALOC:	 call strcpy ("KI_ZFALOC", o_str, SZ_LINE)  
	case KI_ZFCHDR:  call strcpy ("KI_ZFCHDR", o_str, SZ_LINE)  
	case KI_ZFDELE:  call strcpy ("KI_ZFDELE", o_str, SZ_LINE)  
	case KI_ZFINFO:  call strcpy ("KI_ZFINFO", o_str, SZ_LINE)  
	case KI_ZFGCWD:  call strcpy ("KI_ZFGCWD", o_str, SZ_LINE)  
	case KI_ZFMKCP:  call strcpy ("KI_ZFMKCP", o_str, SZ_LINE)  
	case KI_ZFMKDR:  call strcpy ("KI_ZFMKDR", o_str, SZ_LINE)  
	case KI_ZFPATH:  call strcpy ("KI_ZFPATH", o_str, SZ_LINE)  
	case KI_ZFPROT:  call strcpy ("KI_ZFPROT", o_str, SZ_LINE)  
	case KI_ZFRNAM:  call strcpy ("KI_ZFRNAM", o_str, SZ_LINE)  
	case KI_ZFRMDR:  call strcpy ("KI_ZFRMDR", o_str, SZ_LINE)  
	case KI_ZFSUBD:  call strcpy ("KI_ZFSUBD", o_str, SZ_LINE)  
	case KI_ZDVALL:  call strcpy ("KI_ZDVALL", o_str, SZ_LINE)  
	case KI_ZDVOWN:  call strcpy ("KI_ZDVOWN", o_str, SZ_LINE)  
	case KI_ZFUTIM:  call strcpy ("KI_ZFUTIM", o_str, SZ_LINE)  

	case KI_ZOPDIR:  call strcpy ("KI_ZOPDIR", o_str, SZ_LINE)  
	case KI_ZCLDIR:  call strcpy ("KI_ZCLDIR", o_str, SZ_LINE)  
	case KI_ZGFDIR:  call strcpy ("KI_ZGFDIR", o_str, SZ_LINE)  
 
	case KI_ZOPDPR:  call strcpy ("KI_ZOPDPR", o_str, SZ_LINE)  
	case KI_ZCLDPR:  call strcpy ("KI_ZCLDPR", o_str, SZ_LINE)  
	case KI_ZOPCPR:  call strcpy ("KI_ZOPCPR", o_str, SZ_LINE)  
	case KI_ZCLCPR:  call strcpy ("KI_ZCLCPR", o_str, SZ_LINE)  
	case KI_ZINTPR:  call strcpy ("KI_ZINTPR", o_str, SZ_LINE)  

	# Device driver opcodes.
	case KI_ZFIOBF:  call strcpy ("KI_ZFIOBF", o_str, SZ_LINE)  
	case KI_ZFIOLP:  call strcpy ("KI_ZFIOLP", o_str, SZ_LINE)  
	case KI_ZFIOPL:  call strcpy ("KI_ZFIOPL", o_str, SZ_LINE)  
	case KI_ZFIOPR:  call strcpy ("KI_ZFIOPR", o_str, SZ_LINE)  
	case KI_ZFIOSF:  call strcpy ("KI_ZFIOSF", o_str, SZ_LINE)  
	case KI_ZFIOGD:  call strcpy ("KI_ZFIOGD", o_str, SZ_LINE)  

	case KI_ZFIOTX:  call strcpy ("KI_ZFIOTX", o_str, SZ_LINE)  
	case KI_ZFIOTY:  call strcpy ("KI_ZFIOTY", o_str, SZ_LINE)  

	case KI_ZFIOMT:  call strcpy ("KI_ZFIOMT", o_str, SZ_LINE)  

	default: 	 call strcpy ("", o_str, SZ_LINE)
	}


	# Now convert the subcode if needed.
	call aclrc (s_str, SZ_LINE)
	if (opcode >= KI_ZFIOBF && opcode <= KI_ZFIOGD) {
	    switch (subcode) {
	    case BF_OPN:  call strcpy ("BF_OPN", s_str, SZ_LINE) 
	    case BF_CLS:  call strcpy ("BF_CLS", s_str, SZ_LINE) 
	    case BF_ARD:  call strcpy ("BF_ARD", s_str, SZ_LINE) 
	    case BF_AWR:  call strcpy ("BF_AWR", s_str, SZ_LINE) 
	    case BF_AWT:  call strcpy ("BF_AWT", s_str, SZ_LINE) 
	    case BF_STT:  call strcpy ("BF_STT", s_str, SZ_LINE) 
	    default: 	  call strcpy ("", s_str, SZ_LINE)
	    }
	
	} else if (opcode >= KI_ZFIOTX || opcode <= KI_ZFIOTY) {
	    switch (subcode) {
	    case TX_OPN:  call strcpy ("TX_OPN", s_str, SZ_LINE) 
	    case TX_CLS:  call strcpy ("TX_CLS", s_str, SZ_LINE) 
	    case TX_GET:  call strcpy ("TX_GET", s_str, SZ_LINE) 
	    case TX_PUT:  call strcpy ("TX_PUT", s_str, SZ_LINE) 
	    case TX_FLS:  call strcpy ("TX_FLS", s_str, SZ_LINE) 
	    case TX_SEK:  call strcpy ("TX_SEK", s_str, SZ_LINE) 
	    case TX_NOT:  call strcpy ("TX_NOT", s_str, SZ_LINE) 
	    case TX_STT:  call strcpy ("TX_STT", s_str, SZ_LINE) 
	    default: 	  call strcpy ("", s_str, SZ_LINE)
	    }

	} else if (opcode >= KI_ZFIOMT) {
	    switch (subcode) {
	    case MT_OP:   call strcpy ("MT_OP", s_str, SZ_LINE) 
	    case MT_CL:   call strcpy ("MT_CL", s_str, SZ_LINE) 
	    case MT_RD:   call strcpy ("MT_RD", s_str, SZ_LINE) 
	    case MT_WR:   call strcpy ("MT_WR", s_str, SZ_LINE) 
	    case MT_WT:   call strcpy ("MT_WT", s_str, SZ_LINE) 
	    case MT_ST:   call strcpy ("MT_ST", s_str, SZ_LINE) 
	    case MT_RW:   call strcpy ("MT_RW", s_str, SZ_LINE) 
	    default: 	  call strcpy ("", s_str, SZ_LINE)
	    }
	}

end
