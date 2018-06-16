# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>

# PROPDPR -- Open a detached process.  A detached process runs independently of
# and asynchronous with the parent, with no direct communications (i.e., like
# the IPC channels of connected subprocesses).  The bkgfile is prepared by the
# parent and read by the detached process, telling the detached process what
# to do.  There are no restrictions on the format or contents of the bkgfile
# other than those placed by the implementor of the two processes (either a
# text or binary file may be used).  Deletion of the bkgfile is however assumed
# to signify that the bkg process has terminated.

int procedure propdpr (process, bkgfile, bkgmsg)

char	process[ARB]		# vfn of executable process file
char	bkgfile[ARB]		# vfn of background file
char	bkgmsg[ARB]		# control string for kernel

int	jobcode, pr
pointer	sp, process_osfn, bkgfile_osfn, pk_bkgmsg
errchk	fmapfn, syserrs, malloc
include	"prd.com"
data	first_time /true/

begin
	call smark (sp)
	call salloc (process_osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (bkgfile_osfn, SZ_PATHNAME, TY_CHAR)
	call salloc (pk_bkgmsg,    SZ_LINE,     TY_CHAR)

	# First time initialization of the job table.
	if (first_time) {
	    do pr = 1, MAX_BKGJOBS
		pr_jobcode[pr] = NULL
	    first_time = false
	}

	# Get job slot.
	for (pr=1;  pr <= MAX_BKGJOBS;  pr=pr+1)
	    if (pr_jobcode[pr] == NULL)
		break
	if (pr > MAX_BKGJOBS)
	    call syserrs (SYS_PRBKGOVFL, process)

	# Map file names.
	call fmapfn (process, Memc[process_osfn], SZ_PATHNAME)
	call fmapfn (bkgfile, Memc[bkgfile_osfn], SZ_PATHNAME)
	call strpak (bkgmsg,  Memc[pk_bkgmsg],    SZ_LINE)

	# Spawn or enqueue detached process.
	call zopdpr (Memc[process_osfn], Memc[bkgfile_osfn], Memc[pk_bkgmsg],
	    jobcode)
	if (jobcode == ERR)
	    call syserrs (SYS_PRBKGOPEN, process)

	# Set up bkg job descriptor.
	pr_jobcode[pr] = jobcode
	pr_active[pr] = YES
	call malloc (pr_bkgfile[pr], SZ_FNAME, TY_CHAR)
	call strcpy (bkgfile, Memc[pr_bkgfile[pr]], SZ_FNAME)

	call sfree (sp)
	return (pr)
end
