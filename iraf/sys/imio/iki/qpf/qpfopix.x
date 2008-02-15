# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	"qpf.h"

# QPF_OPIX -- Open the "pixel storage file", i.e., open the special QPF/QPOE
# virtual file driver, which samples the QPOE event list in real time to
# produce image "pixels", where each pixel contains a count of the number of
# photons mapping to that point in the output image matrix.

procedure qpf_opix (im, status)

pointer	im			#I image descriptor
int	status			#O return status

pointer	sp, fname, qpf
extern	qpfzop(), qpfzrd(), qpfzwr(), qpfzwt(), qpfzst(), qpfzcl()
int	fopnbf()

begin
	status = OK
	if (IM_PFD(im) != NULL)
	    return

	# Verify that the QPIO open succeeded at open time; if not, the file
	# may not have an event list (which is legal, but not for pixel i/o).

	qpf = IM_KDES(im)
	if (QPF_IO(qpf) == NULL) {
	    status = ERR
	    return
	}

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Encode the QPF descriptor as a pseudo-filename to pass the descriptor
	# through fopnbf to the QPF virtual binary file driver.

	call sprintf (Memc[fname], SZ_FNAME, "QPF%d")
	    call pargi (IM_KDES(im))

	# Open a file descriptor for the dummy QPOE file driver, used to access
	# the event list as a virtual pixel array (sampled at runtime).

	iferr (IM_PFD(im) = fopnbf (Memc[fname], READ_ONLY,
	    qpfzop, qpfzrd, qpfzwr, qpfzwt, qpfzst, qpfzcl)) {

	    IM_PFD(im) = NULL
	    status = ERR
	}

	call sfree (sp)
end
