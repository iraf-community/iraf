include <fset.h>
include <imio.h>
include	<imhdr.h>
include "gi.h"

# GI_OPENGR --  Procedure to skip onto the next group of a geis image
#	       already open. The group parameter block information
#	       is in memory for the previous group and it will be
#	       necessary only to write the gpb to the image (if the
#	       access mode is other than READ_ONLY) pixel file.
#	       If access is NEW_IMAGE we need to skip only after 
#	       the gpb has been written back to the file.
# NZ Nov 1994: Change the argument in st_wgpb for READ_WRITE acmode.
#              Now they have IM_MIN and IM_MAX rather than datamin,datamax.
# BS Aug 1996: Update the image size to fix a nasty image write bug
# BS Jun 1997: Replace IM_KERNEL check with call to gi_geis
# BS Jul 1997: Add new procedure that bypasses geis file check
# PEH July 2007:  In gi_newgrp, add a call to imsetbuf if the pixel file
#              is open.  This was done because under some circumstances
#              (e.g. looping from the last group to the first) a call to
#              imgl1r returned incorrect values.
	    
procedure gi_opengr (im, gn, datamin, datamax, imt)

pointer	im		# i: image descriptor
int	gn		# i: group number to skip to
real	datamin		# u: image minimun value
real	datamax		# u: image maximum value
pointer imt		# i: image template descriptor (NEW_COPY only)
#--
bool	gi_geis()
errchk	gi_newgrp

begin
	# Return if image is not geis file
	if (! gi_geis (im))
	   call error (13, "Image is not geis type")

	call gi_newgrp (im, gn, datamin, datamax, imt)
end

# GI_NEWGRP -- Bypass geis check for new groups

procedure gi_newgrp (im, gn, datamin, datamax, imt)

pointer	im		# i: image descriptor
int	gn		# i: group number to skip to
real	datamin		# u: image minimun value
real	datamax		# u: image maximum value
pointer imt		# i: image template descriptor (NEW_COPY only)
#--
int	stf, fd, compress, blklen, pixoff
int	save1[IM_MAXDIM], save2[IM_MAXDIM]

int	sizeof()
long	fstatl()
errchk	gi_copyua

begin
	blklen = 1
	compress = YES
	stf = IM_KDES(im)

	pixoff = (gn-1)*STF_SZGROUP(stf) + 1

	if (gn > STF_GCOUNT(stf))
	   call error (13, "Group number requested > No of groups in image")

	switch (IM_ACMODE(im)) {
	case READ_WRITE, WRITE_ONLY, APPEND:
	    # Flush any data from buffer
	    call imflush (im)

	    # Update the group parameter block. Write the gpb of the
	    # current open group back to the data file.
	    #
	    call stf_wgpb (im, STF_GROUP(stf), IM_MIN(im), IM_MAX(im))

	    # Rgpb is independent of imoff
	    #
	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax

	    if (IM_SECTUSED(im) == YES) {
               call amovl (IM_SVLEN(im,1), save1, IM_MAXDIM)
               call amovl (IM_PHYSLEN(im,1), save2, IM_MAXDIM)
	       call imioff (im, pixoff, compress, blklen)
               call amovl (save1, IM_SVLEN(im,1), IM_MAXDIM)
               call amovl (save2, IM_PHYSLEN(im,1), IM_MAXDIM)
	    } else 
	       call imioff (im, pixoff, compress, blklen)
	case READ_ONLY:
	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax
	
	    if (IM_SECTUSED(im) == YES) {
               call amovl (IM_SVLEN(im,1), save1, IM_MAXDIM)
               call amovl (IM_PHYSLEN(im,1), save2, IM_MAXDIM)
	       call imioff (im, pixoff, compress, blklen)
               call amovl (save1, IM_SVLEN(im,1), IM_MAXDIM)
               call amovl (save2, IM_PHYSLEN(im,1), IM_MAXDIM)
	    } else {
	       call imioff (im, pixoff, compress, blklen)
	    }

	case NEW_IMAGE:
	    # Flush any data from buffer
	    call imflush (im)
	 
	    # Update the group parameter block. Write the gpb of the
	    # current open group back to the data file. We don't
	    # need to read gpb for the 'gn' group since we are
	    # creating those values.
	    call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)

	    call imioff (im, pixoff, compress, blklen)
	case NEW_COPY:
	    #
	    if (STF_NEWIMAGE(stf) == NO)
	       call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    else {
 	       # We need to copy the values of the gpb from the
	       # input image (imt descriptor) to the output image
	       # before jumping to the next group. Stfwgp copies
	       # the gpb values from the userarea to the pixel file

	       if (STF_PFD(stf) != NULL) {
	          call imflush (im)
	          call stf_wgpb (im, STF_GROUP(stf), datamin, datamax)
	       }

	       call gi_copyua (imt, im)

	    }
	    if (IM_SECTUSED(im) == YES) {
               call amovl (IM_SVLEN(im,1), save1, IM_MAXDIM)
               call amovl (IM_PHYSLEN(im,1), save2, IM_MAXDIM)
	       call imioff (im, pixoff, compress, blklen)
               call amovl (save1, IM_SVLEN(im,1), IM_MAXDIM)
               call amovl (save2, IM_PHYSLEN(im,1), IM_MAXDIM)
	    } else {
	       call imioff (im, pixoff, compress, blklen)
	    }
	default:
	    # open an existing group within an existing image
	    call stf_rgpb (im, gn, READ_ONLY, datamin, datamax)
	    IM_MIN(im) = datamin
	    IM_MAX(im) = datamax

	    if (IM_SECTUSED(im) == YES) {
               call amovl (IM_SVLEN(im,1), save1, IM_MAXDIM)
               call amovl (IM_PHYSLEN(im,1), save2, IM_MAXDIM)
	       call imioff (im, pixoff, compress, blklen)
               call amovl (save1, IM_SVLEN(im,1), IM_MAXDIM)
               call amovl (save2, IM_PHYSLEN(im,1), IM_MAXDIM)
	    } else{
	       call imioff (im, pixoff, compress, blklen)
	    }
	}

	# update the length of the image (BPS 08.07.96)
	if (IM_PFD(im) == NULL) {
	    IM_FILESIZE(im) = 0
	} else {
	    fd = IM_PFD(im)
	    IM_FILESIZE(im) = fstatl (fd, F_FILESIZE)
	    call imsetbuf (fd, im)
	}

	if (mod(pixoff, sizeof(IM_PIXTYPE(im))) != 1)
	   IM_FAST(im) = NO

	# Update to the new group
	STF_GROUP(stf) = gn

end

include	<error.h>
include	<mach.h>

procedure gi_copyua (imt, im)

pointer imt, im

long	offset
pointer	sp, stf, lbuf, pp
int	pfd, pn, sz_param, i

bool	bval, imgetb()
# changed to short and long for short integers in gpb
short	sval, imgets()
long	lval, imgetl()
#
real	rval, imgetr()
double	dval, imgetd()
errchk	open, seek
int	imaccf()

string	badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

#	stf = IM_KDES(im)
	stf = IM_KDES(imt)   # (june 1992) imt is the input descriptor. 
	pfd = STF_PFD(stf)

	# Not all images have group parameter blocks.
	if (STF_PSIZE(stf) == 0) {
	    call sfree (sp)
	    return
	}

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.

	offset = 0
	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	    pp = STF_PDES(stf,pn)

	    # Fetch the value of the parameter from IMIO and write it into
	    # the GPB binary data structure.

	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		iferr (bval = imgetb (imt, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    bval = false
		}
		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call imputb (im, P_PTYPE(pp), bval)
	        }

	    case TY_SHORT:
		iferr (sval = imgets (imt, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    sval = 0
		}
		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call imputs (im, P_PTYPE(pp), sval)
	        }

	    case TY_LONG:
		iferr (lval = imgetl (imt, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    lval = 0
		}
		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call imputl (im, P_PTYPE(pp), lval)
	        }

	    case TY_REAL:
		iferr (rval = imgetr (imt, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    rval = 0.0
		}
		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call imputr (im, P_PTYPE(pp), rval)
	        }

	    case TY_DOUBLE:
		iferr (dval = imgetd (imt, P_PTYPE(pp))) {
		    call erract (EA_WARN)
		    dval = 0.0d0
		}
		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call imputd (im, P_PTYPE(pp), dval)
	        }

	    case TY_CHAR:
		# Blank fill the string buffer.
		do i = 1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		# Fetch the string value of the parameter.
		iferr (call imgstr (imt, P_PTYPE(pp), Memc[lbuf], SZ_LINE))
		    call erract (EA_WARN)

		# Replace the EOS delimiter by a blank.
		# i = strlen (Memc[lbuf])
		# Memc[lbuf+i] = ' '

		# Transfer the value to an existing parameter only.
	        if (imaccf (im, P_PTYPE(pp)) == YES) {
		   call impstr (im, P_PTYPE(pp), Memc[lbuf])
	        }

	    default:
		call error (1, badtype)
	    }
	    call flush (STDOUT)

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}

	call sfree (sp)
end
