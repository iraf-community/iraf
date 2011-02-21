include	<error.h>
include	<imio.h>
include	<imhdr.h>
include	<ctype.h>

define	IS_FITS		(IS_DIGIT($1)||IS_UPPER($1)||($1=='-')||($1=='_'))

# T_HFIX -- Fix image headers
#
# Fix image headers using a user supplied command.
# This task is a prototype which directly accesses the user header block
# and uses CLCMDW.

procedure t_hfix ()

int	images			# List of images to be fixed
pointer	cmd			# Fix command
bool	update			# Update image header

int	mode, reclen
pointer	sp, image, efile, ecmd, eline
pointer	im, ua, fd, hd, ip, jp, kp

int	imtopenp(), imtgetim(), stridxs(), open(), stropen()
int	getline(), gstrcpy()
bool	clgetb()
pointer	immap()
errchk	open, clcmdw

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cmd,SZ_LINE, TY_CHAR)
	call salloc (efile, SZ_FNAME, TY_CHAR)
	call salloc (ecmd, SZ_LINE, TY_CHAR)
	call salloc (eline, SZ_LINE, TY_CHAR)

	# Get task parameters and set update mode
	images = imtopenp ("images")
	call clgstr ("command", Memc[cmd], SZ_LINE)
	update = clgetb ("update")
	if (update)
	    mode = READ_WRITE
	else
	    mode = READ_ONLY

	# Fix the image headers.
	while (imtgetim (images, Memc[image], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[image], mode, NULL)) {
		call erract (EA_WARN)
		next
	    }

	    # Determine header blocking
	    ua = IM_USERAREA(im)
	    reclen = stridxs ("\n", Memc[ua]) - 1
	    if (IM_UABLOCKED(im) < 0) {
		if (reclen == 80)
		    IM_UABLOCKED(im) = YES
		else
		    IM_UABLOCKED(im) = NO
	    }

	    # Create a temporary file and copy the user area into it
	    call mktemp ("tmp", Memc[efile], SZ_FNAME)
	    fd = open (Memc[efile], NEW_FILE, TEXT_FILE)
	    hd = stropen (Memc[ua], ARB, READ_ONLY)
	    call fcopyo (hd, fd)
	    call close (fd)
	    call close (hd)

	    # Expand the user command
	    jp = ecmd
	    for (ip=cmd; Memc[ip]!=EOS; ip=ip+1) {
		if (Memc[ip] == '$') {
		    if (Memc[ip+1] == 'i') {
			for (kp=image; Memc[kp]!=EOS; kp=kp+1) {
			    Memc[jp] = Memc[kp]
			    jp = jp + 1
			}
			ip = ip + 5
		    } else {
			for (kp=efile; Memc[kp]!=EOS; kp=kp+1) {
			    Memc[jp] = Memc[kp]
			    jp = jp + 1
			}
			ip = ip + 5
		    }
		} else {
		    Memc[jp] = Memc[ip]
		    jp = jp + 1
		}
	    }
	    Memc[jp] = EOS

	    iferr {
		# Fix the header with the user command
		call clcmdw (Memc[ecmd])

		if (update) {
		    # Copy the fixed header back into the user area, reblocking
		    # where necessary.  Skip non-FITS lines.

		    kp = ua
		    fd = open (Memc[efile], READ_ONLY, TEXT_FILE)
		    while (getline (fd, Memc[eline]) != EOF) {
			for (ip=eline; IS_WHITE(Memc[ip]); ip=ip+1)
			    ;
			for (jp=ip; IS_FITS(Memc[jp]); jp=jp+1)
			    ;
			for (; jp<ip+8 && Memc[jp]==' '; jp=jp+1)
			    ;
			if (jp<ip+8 && Memc[jp] != EOS && Memc[jp] != '\n')
			    next
			if (Memc[jp] == '=' && Memc[jp+1] != ' ')
			    next
			for (; jp<ip+80 && Memc[jp]!=EOS && Memc[jp]!='\n';
			    jp=jp+1)
			    ;
			if (IM_UABLOCKED(im) == YES)
			    for (; jp<ip+reclen; jp=jp+1)
				Memc[jp] = ' '
			Memc[jp] = '\n'
			Memc[jp+1] = EOS
			kp = kp + gstrcpy(Memc[ip], Memc[kp], SZ_LINE)
		    }
		    Memc[kp] = EOS

		    call close (fd)
		}
		call delete (Memc[efile])
	    } then
		call erract (EA_WARN)

	    call imunmap (im)
	}

	call imtclose (images)
	call sfree (sp)
end
