include <error.h>
include <diropen.h>
include <ctype.h>
include	<finfo.h>
include <imhdr.h>
include <imio.h>
include <gset.h>
include <gim.h>

define	SZ_BIGBUF	16384
define	USER_AREA	Memc[($1+IMU-1)*SZ_STRUCT + 1]
define	SAMPLE_SIZE	600
define	NCOLORS		200
define	MAX_INTENSITY	255
define	SWATH		32


# IMBROWSE -- Image browser GUI demo task.

procedure t_imbrowse()

pointer	gp
real	x, y
int	wcs, key, ip, op
char	directory[SZ_PATHNAME]
char	buf[SZ_LINE], cmd[SZ_FNAME], args[SZ_LINE]
char	device[SZ_FNAME], uifname[SZ_PATHNAME], template[SZ_LINE]
char	section[SZ_LINE], strval[SZ_LINE], curdir[SZ_PATHNAME]

bool	streq()
pointer	gopenui()
int	clgcur(), clgeti()
int	imb_isdirectory()

begin
	# Get the start directory.
	if (clgeti ("$nargs") > 0) {
	    call clgstr ("directory", directory, SZ_PATHNAME)
	    if (imb_isdirectory (directory, curdir, SZ_PATHNAME) > 0)
		call strcpy (curdir, directory, SZ_PATHNAME)
	    else
		directory[1] = EOS
	} else
	    directory[1] = EOS

	call clgstr ("device", device, SZ_FNAME)
	call clgstr ("uifname", uifname, SZ_FNAME)
	call clgstr ("template", template, SZ_LINE)
	call clgstr ("section", section, SZ_LINE)

	gp = gopenui (device, NEW_FILE, uifname, STDGRAPH)
	call gmsg (gp, "template", template)
	call gmsg (gp, "section", section)

	call fpathname (directory, curdir, SZ_PATHNAME)
	call imb_setdir (gp, curdir, ".", template)

	while (clgcur ("coords", x, y, wcs, key, strval, SZ_LINE) != EOF) {
	    switch (key) {
	    case 'q', 'Q':
		break

	    case ':':
		for (ip=1;  IS_WHITE(strval[ip]);  ip=ip+1)
		    ;
		op = 1
		while (strval[ip] != EOS && !IS_WHITE(strval[ip])) {
		    cmd[op] = strval[ip]
		    op = op + 1
		    ip = ip + 1
		}
		cmd[op] = EOS
		for (  ;  IS_WHITE(strval[ip]);  ip=ip+1)
		    ;
		call strcpy (strval[ip], args, SZ_LINE)

		if (streq (cmd, "chdir")) {
		    call imb_setdir (gp, curdir, args, template)

		} else if (streq (cmd, "template")) {
		    call strcpy (args, template, SZ_LINE)
		    call imb_setdir (gp, curdir, ".", template)

		} else if (streq (cmd, "section")) {
		    call strcpy (args, section, SZ_LINE)

		} else if (streq (cmd, "header")) {
		    call imb_pheader (gp, curdir, args, section)

		} else if (streq (cmd, "display")) {
		    call imb_display (gp, curdir, args, section)

		} else {
		    call sprintf (buf, SZ_LINE, "unrecognized command: `%s'\n")
			call pargstr (strval)
		    call gmsg (gp, "errormsg", buf)
		}

	    default:
		call sprintf (buf, SZ_LINE,
		    "unrecognized cursor command: key=%c strval=`%s'\n")
		    call pargi (key)
		    call pargstr (strval)
		call gmsg (gp, "errormsg", buf)
	    }
	}

	call gclose (gp)
end


# IMB_SETDIR -- Set the current directory.

procedure imb_setdir (gp, curdir, newdir, template)

pointer	gp			#I graphics descriptor
char	curdir[ARB]		#I current directory
char	newdir[ARB]		#I new directory or subdirectory
char	template[ARB]		#I filename template

pointer	s_op, f_op, pt
int	fd, errcode, nchars
pointer	sp, lbuf, fname, subdirs, files, dirpath, ftemp

pointer	pt_compile()
int	errget(), gstrcpy(), imb_isdirectory(), imb_issubdir()
int	diropen(), nowhite(), getline(), access(), pt_match()
errchk	fchdir, fpathname, gmsg, diropen, getline, access, pt_compile
define	error_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (ftemp, SZ_PATHNAME, TY_CHAR)
	call salloc (subdirs, SZ_COMMAND, TY_CHAR)
	call salloc (files, SZ_BIGBUF, TY_CHAR)
	call salloc (dirpath, SZ_PATHNAME, TY_CHAR)

	s_op = subdirs
	f_op = files

	# We should check for buffer overflow and realloc if it occurs, but
	# we omit this at present in this demo program.

	iferr {
	    # Get pathname of new directory.
	    if (imb_issubdir (curdir, newdir) == YES) {
		call strcpy (curdir, Memc[fname], SZ_PATHNAME)
		call zfsubd (Memc[fname], SZ_PATHNAME, newdir, nchars)
	    } else if (imb_isdirectory(newdir,Memc[dirpath],SZ_PATHNAME) > 0) {
		call fpathname (Memc[dirpath], Memc[fname], SZ_PATHNAME)
	    } else
		call fpathname (newdir, Memc[fname], SZ_PATHNAME)

	    # Read the directory and construct a list of subdirectories and
	    # a list of files matching the given template.

	    fd = diropen (Memc[fname], PASS_HIDDEN_FILES)
	    call strcpy (Memc[fname], curdir, SZ_PATHNAME)
	    call gmsg (gp, "directory", Memc[fname])
	    pt = pt_compile (template)

	    s_op = s_op + gstrcpy ("/\n", Memc[s_op], ARB)
	    while (getline (fd, Memc[lbuf]) != EOF) {
		if (nowhite (Memc[lbuf], Memc[ftemp], SZ_PATHNAME) <= 0)
		    next
		call imb_mkfname (Memc[ftemp], curdir, Memc[fname], SZ_PATHNAME)
		if (access (Memc[fname], 0, DIRECTORY_FILE) == YES)
		    s_op = s_op + gstrcpy (Memc[lbuf], Memc[s_op], ARB)
		if (pt_match (pt, Memc[fname]) == YES)
		    f_op = f_op + gstrcpy (Memc[lbuf], Memc[f_op], ARB)
	    }

	    call pt_free (pt)
	    call close (fd)
	} then
	    goto error_

	Memc[s_op] = EOS
	Memc[f_op] = EOS

	call gmsg (gp, "subdirs", Memc[subdirs])
	call gmsg (gp, "files", Memc[files])

	call sfree (sp)
	return
error_
	errcode = errget (Memc[lbuf], SZ_LINE)
	call gmsg (gp, "errormsg", Memc[lbuf])
	call sfree (sp)
end


# IMB_PHEADER -- Print an image header.

procedure imb_pheader (gp, curdir, image, section)

pointer	gp			#I graphics descriptor
char	curdir[ARB]		#I directory
char	image[ARB]		#I image name
char	section[ARB]		#I image section

int	in, min_lenuserarea
pointer	sp, lbuf, hbuf, ip, op, im, fname
int	stropen(), getline(), gstrcpy()
pointer	immap()

begin
        call smark (sp)
        call salloc (lbuf, SZ_LINE, TY_CHAR)
        call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call imb_mkfname (image, curdir, Memc[fname], SZ_PATHNAME)
	iferr (im = immap (Memc[fname], READ_ONLY, 0)) {
	    call sprintf (Memc[lbuf], SZ_LINE, "cannot open image `%s'")
		call pargstr (image)
	    call gmsg (gp, "errormsg", Memc[lbuf])
	    call sfree (sp)
	    return
	}

	call salloc (hbuf, IM_HDRLEN(im), TY_CHAR)
	op = hbuf

        # Open user area in header.
        min_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
        in = stropen (USER_AREA(im), min_lenuserarea, READ_ONLY)

        # Copy header records to the output, stripping any trailing
        # whitespace and clipping at the right margin.

        while (getline (in, Memc[lbuf]) != EOF) {
            for (ip=lbuf;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
                ;
            while (ip > lbuf && Memc[ip-1] == ' ')
                ip = ip - 1
            Memc[ip] = '\n'
            Memc[ip+1] = EOS

	    op = op + gstrcpy (Memc[lbuf], Memc[op], ARB)
        }

	call gmsg (gp, "image_title", IM_TITLE(im))
	call gmsg (gp, "header", Memc[hbuf])

        call close (in)
	call imunmap (im)
        call sfree (sp)
end


# IMB_DISPLAY -- Display an image.

procedure imb_display (gp, curdir, image, section)

pointer	gp			#I graphics descriptor
char	curdir[ARB]		#I directory
char	image[ARB]		#I image name
char	section[ARB]		#I image section

int	ncols, nrows, i, v
real	contrast, z1, z2, dz1, dz2
int	r[NCOLORS], g[NCOLORS], b[NCOLORS]
int	nsample_lines, len_stdline, j1, j2, npix
pointer	sp, im, fname, lbuf, in, out, pkras

int	clgeti()
real	clgetr()
pointer	immap(), imgs2r()
errchk	gseti, gswind, clgeti, clgetr, zscale, malloc, imgs2r
errchk	gim_createraster, gim_setmapping, gim_writecolormap, gim_writepixels

begin
        call smark (sp)
        call salloc (lbuf, SZ_LINE, TY_CHAR)
        call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call imb_mkfname (image, curdir, Memc[fname], SZ_PATHNAME)
	call strcat (section, Memc[fname], SZ_PATHNAME)

	iferr (im = immap (Memc[fname], READ_ONLY, 0)) {
	    call sprintf (Memc[lbuf], SZ_LINE, "cannot open image `%s%s'")
		call pargstr (image)
		call pargstr (section)
	    call gmsg (gp, "errormsg", Memc[lbuf])
	    call sfree (sp)
	    return
	}

	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)

	call gmsg (gp, "image_title", IM_TITLE(im))

	# Create raster to hold image.
	call gim_createraster (gp, 1, 0, ncols, nrows, 8)

	# Associate a WCS with raster 1.
	call gseti (gp, G_RASTER, 1)
	call gswind (gp, 0.5, real(ncols) + 0.5, 0.5, real(nrows) + 0.5)

	# Set the primary raster to screen mapping.
	call gim_setmapping (gp, 1, 0,
	    1,  CT_PIXEL, 0.0, 0.0, real(ncols), real(nrows),
	    0,  CT_NDC,   0.0, 0.0, 1.0, 1.0)

	# Write colormap.
	do i = 1, NCOLORS {
	    v = MAX_INTENSITY * (real((i - 1)) / (NCOLORS - 1))
	    r[i] = v;  g[i] = v;  b[i] = v
	}
	call gim_writecolormap (gp, 0, LAST_COLOR+1, NCOLORS, r, g, b)
	call gim_writecolormap (gp, 1, LAST_COLOR+1, NCOLORS, r, g, b)

	contrast = clgetr ("contrast")
	nsample_lines = clgeti ("nsample_lines")
	len_stdline = SAMPLE_SIZE / nsample_lines
	call zscale (im, z1, z2, contrast, SAMPLE_SIZE, len_stdline)

	dz1 = LAST_COLOR + 1
	dz2 = dz1 + NCOLORS - 1
	call malloc (out, ncols * SWATH, TY_REAL)
	call malloc (pkras, ncols * SWATH, TY_CHAR)
	
	do j1 = 1, nrows, SWATH {
	    j2 = min (j1 + SWATH - 1, nrows)
	    npix = (j2 - j1 + 1) * ncols
	    in = imgs2r (im, 1, ncols, j1, j2)
	    call amapr (Memr[in], Memr[out], npix, z1, z2, dz1, dz2)
	    call achtrb (Memr[out], Memc[pkras], npix)
	    call gim_writepixels (gp, 1,
		Memc[pkras], 8, 0, j1-1, ncols, j2-j1+1)
	}

	call mfree (out, TY_REAL)
	call mfree (pkras, TY_CHAR)
	call imunmap (im)
        call sfree (sp)
end


# IMB_MKFNAME -- Construct a filename given a directory name and the file
# name.

procedure imb_mkfname (file, directory, fname, maxch)

char	file[ARB]		#I input filename
char	directory[ARB]		#I directory file resides in
char	fname[ARB]		#O path to file
int	maxch			#I max chars out

begin
	call fdirname (directory, fname, maxch)
	call strcat (file, fname, maxch)
end


# IMB_ISDIRECTORY -- Test whether the named file is a directory.  Check first 
# to see if it is a subdirectory of the current directory; otherwise look in
# the environment to see if it is a logical directory.  If VFN is a directory,
# return the OS pathname of the directory in pathname, and the number of
# chars in the pathname as the function value.  Otherwise return 0.

int procedure imb_isdirectory (vfn, pathname, maxch)

char	vfn[ARB]		# name to be tested
char	pathname[ARB]		# receives path of directory
int	maxch			# max chars out

bool	isdir
pointer	sp, fname, op
int	ip, fd, nchars, ch
long	file_info[LEN_FINFO]
int	finfo(), diropen(), gstrcpy(), strlen()
bool	streq()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Copy the VFN string, minus any whitespace on either end.
	op = fname
	for (ip=1;  vfn[ip] != EOS;  ip=ip+1) {
	    ch = vfn[ip]
	    if (!IS_WHITE (ch)) {
		Memc[op] = ch
		op = op + 1
	    }
	}
	Memc[op] = EOS

	isdir = false
	if (streq (vfn, ".") || streq (vfn, "..")) {
	    isdir = true

	} else if (finfo (Memc[fname], file_info) != ERR) {
	    isdir = (FI_TYPE(file_info) == FI_DIRECTORY)

	    if (isdir) {
		call fdirname (Memc[fname], pathname, maxch)
		nchars = strlen (pathname)
	    }

	} else {
	    # If we get here, either VFN is a logical directory (with the
	    # $ omitted), or it is the name of a new file.

	    Memc[op] = '$'
	    Memc[op+1] = EOS
	    ifnoerr (fd = diropen (Memc[fname], 0)) {
		call close (fd)
		isdir = true
	    }

	    nchars = gstrcpy (Memc[fname], pathname, maxch)
	}

	call sfree (sp)
	if (isdir)
	    return (nchars)
	else {
	    pathname[1] = EOS
	    return (0)
	}
end


# IMB_ISSUBDIR -- Test whether the named file is a subdirectory of the 
# current directory.

int procedure imb_issubdir (curdir, newdir)

char	curdir[ARB]		# current directory
char	newdir[ARB]		# subdir name to be tested

bool	subdir
pointer	sp, fname
int	root, extn, nchars
long	file_info[LEN_FINFO]
int	finfo(), btoi()
bool	streq()

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	subdir = false
	if (streq (newdir, ".") || streq (newdir, "..")) {
	    subdir = true

	} else {
	    call zfnbrk (newdir, root, extn)
	    if (root == 1) {
		call strcpy (curdir, Memc[fname], SZ_PATHNAME)
		call zfsubd (Memc[fname], SZ_PATHNAME, newdir, nchars)
		if (finfo (Memc[fname], file_info) != ERR)
		    subdir = (FI_TYPE(file_info) == FI_DIRECTORY)
	    }
	}

	call sfree (sp)
	return (btoi (subdir))
end


# Pattern template matching utility.
# --------------------------------------
define	MAX_PATTERNS	64
define	SZ_PATBUF	SZ_LINE

define	LEN_PATDES	(10 + MAX_PATTERNS * SZ_PATBUF)
define	PT_NPATTERNS	Memi[$1]
define	PT_PATBUF	Memi[$1+10+(($2)-1)*SZ_PATBUF]

# PT_COMPILE -- Compile a pattern template into the pattern descriptor.
# A pattern template is a comma delimited list of patterns, e.g., "*.x,*.y".

pointer procedure pt_compile (template)

char	template[ARB]			#I pattern template

pointer	sp, pattern, pt, op
int	junk, npatterns, ip, pch, ch
int	patmake()
errchk	calloc

begin
	call smark (sp)
	call salloc (pattern, SZ_LINE, TY_CHAR)

	call calloc (pt, LEN_PATDES, TY_STRUCT)
	npatterns = 0

	for (ip=1;  template[ip] == ',' || IS_WHITE (template[ip]);  ip=ip+1)
	    ;

	while (template[ip] != EOS) {
	    # Get the next pattern.
	    op = pattern
	    Memc[op] = '^';  op = op + 1

	    pch = 0
	    ch = template[ip]

	    while (ch != EOS && ch != ',' && !IS_WHITE(ch)) {
		if (ch == '*' && pch != ']') {
		    Memc[op] = '?'
		    op = op + 1
		}
		Memc[op] = ch
		op = op + 1
		ip = ip + 1
		pch = ch
		ch = template[ip]
	    }

	    Memc[op] = '$';  op = op + 1
	    Memc[op] = EOS

	    # Encode the pattern.
	    npatterns = npatterns + 1
	    junk = patmake (Memc[pattern], PT_PATBUF(pt,npatterns), SZ_PATBUF)

	    while (template[ip] == ',' || IS_WHITE (template[ip]))
		ip = ip + 1
	}

	PT_NPATTERNS(pt) = npatterns
	call sfree (sp)
	return (pt)
end


# PT_MATCH -- Test a string to see if it matches one of the patterns in the
# compiled pattern template.

int procedure pt_match (pt, str)

pointer	pt			#I pattern template descriptor
char	str[ARB]		#I string to be matched against template

int	i
int	patmatch()

begin
	for (i=1;  i <= PT_NPATTERNS(pt);  i=i+1)
	    if (patmatch (str, PT_PATBUF(pt,i)) > 0)
		return (YES)

	return (NO)
end


# PT_FREE -- Free a pattern template descriptor.

procedure pt_free (pt)

pointer	pt			#I pattern template descriptor

begin
	call mfree (pt, TY_STRUCT)
end
