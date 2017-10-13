# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include	<config.h>


define  L_SENTINAL      20030125
define  U_SENTINAL      20040922


# MEM_INIT -- Initialize the MEMIO interface for the task.

procedure mem_init (task)

char	task[ARB]			# task name

int	mgtenv()

include "nmemio.com"

begin
	# Initialize I/O buffers for stdout/stderr.  We do this here to
	# create the file buffers without counting these in the memory 
	# usage stats.
#	call fmkbfs (STDOUT)
#	call fmkbfs (STDERR)

	# Allocate the garbage collection buffer.
	mcollect  = mgtenv ("MEMIO_COLLECT")
	call mgc_init()

	# Initialize the sentinal values.
	lsentinal   = L_SENTINAL
	usentinal   = U_SENTINAL

	mwatch    = mgtenv ("MEMIO_WATCH")
	mreport   = mgtenv ("MEMIO_REPORT")
	mclear    = mgtenv ("MEMIO_CLEAR")
	mdebug    = mgtenv ("MEMIO_DEBUG")

	max_alloc = 0
	mem_used  = 0
	leaked	  = 0
	nleaked	  = 0
	nalloc	  = 0
	nfree	  = 0
	bmax      = 0

	in_task	  = 1
end


# MGTENV -- Get an environment variable for MEMIO control.

int procedure mgtenv (varname)

char	varname[ARB]			# env variable to find

int	ival, ip, status, junk
char	key[SZ_LINE], value[SZ_LINE]

int	ctoi()

begin
	ip = 1				# init
        ival = 0
	call aclrc (key, SZ_LINE)
	call aclrc (value, SZ_LINE)

        call strpak (varname, key, SZ_LINE)
        call zgtenv (key, value, SZ_LINE, status)
        call strupk (value, value, SZ_LINE)

	if (status == 0)		# variable defined w/out value
	    ival = 1
	else if (status > 0)		# get environment variable value
	    junk = ctoi (value, ip, ival)

	return (ival)
end


# MEM_PTYPE -- Print a pointer type.  Used in error messages.

procedure mem_ptype (dtype)

int     dtype

begin
	switch (dtype) {
	case TY_BOOL:       call pargstr ("TY_BOOL")
	case TY_CHAR:       call pargstr ("TY_CHAR")
	case TY_SHORT:      call pargstr ("TY_SHORT")
	case TY_INT:        call pargstr ("TY_INT")
	case TY_LONG:       call pargstr ("TY_LONG")
	case TY_REAL:       call pargstr ("TY_REAL")
	case TY_DOUBLE:     call pargstr ("TY_DOUBLE")
	case TY_COMPLEX:    call pargstr ("TY_COMPLEX")
	case TY_STRUCT:     call pargstr ("TY_STRUCT")
	case TY_POINTER:    call pargstr ("TY_POINTER")
	default:    	    call pargstr ("unknown")
	}
end


# MIO_INIT -- Initialize the MEMIO interface for the task.

procedure mio_init ()

include "nmemio.com"

begin
	mgc       = NULL
	mcollect  = 0
	mwatch    = 0
	mreport   = 0
	mclear    = 0
	mdebug    = 0

	max_alloc = 0
	mem_used  = 0
	leaked	  = 0
	nleaked	  = 0
	nalloc	  = 0
	nfree	  = 0

	in_task	  = 0
end
