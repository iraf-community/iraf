# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"oif.h"

# IMFDIR -- Routines for setting or retrieving the "imdir" (pixel file storage
# directory) for IMFORT.
#
#	      im[sg]dir (dir)		# set/get imdir - F77 versions
#		imsdirx (dir)		# set imdir - SPP version
#	  nch = imgdirx (dir, maxch)	# get imdir - SPP version
#
# By default, pixel files are stored in the same directory as the header file,
# using a HDR$ pathname in the image header.  If the user wishes they can
# explicitly set the directory into which all further pixel files will be
# placed, until another call to the set-imdir routine.


# IMSDIR -- Set the value of `imdir' for imfort.

procedure imsdir (dir)

%	character*(*) dir

char	imdir[SZ_PATHNAME]
common	/imdcom/ imdir

begin
	call imdinit()
	call f77upk (dir, imdir, SZ_PATHNAME)
end


# IMGDIR -- Get the value of `imdir' for imfort.

procedure imgdir (dir)

%	character*(*) dir

char	imdir[SZ_PATHNAME]
common	/imdcom/ imdir

begin
	call imdinit()
	call f77pak (imdir, dir, len(dir))
end


# IMSDIRX -- Set the value of `imdir' for imfort, SPP version.

procedure imsdirx (dir)

char	dir[ARB]		#I new value of imdir

char	imdir[SZ_PATHNAME]
common	/imdcom/ imdir

begin
	call imdinit()
	call strcpy (dir, imdir, SZ_PATHNAME)
end


# IMGDIRX -- Get the value of `imdir' for imfort, SPP version.

int procedure imgdirx (dir, maxch)

char	dir[maxch]		#O receives value of imdir
int	maxch

int	gstrcpy()
char	imdir[SZ_PATHNAME]
common	/imdcom/ imdir

begin
	call imdinit()
	return (gstrcpy (imdir, dir, maxch))
end


# IMDINIT -- Runtime initialization of the imdir common.

procedure imdinit()

int	status
char	envvar[5]
bool	first_time
data	first_time /true/

char	imdir[SZ_PATHNAME]
common	/imdcom/ imdir

begin
	if (first_time) {
	    # Check the host environment for the default IMDIR.
	    call strpak ("imdir", envvar, 5)
	    call zgtenv (envvar, imdir, SZ_PATHNAME, status)
	    if (status < 0) {
		call strpak ("IMDIR", envvar, 5)
		call zgtenv (envvar, imdir, SZ_PATHNAME, status)
	    }

	    # Use the builtin default HDR$ if not defined in host enviroment.
	    if (status < 0)
		call strcpy (HDR, imdir, SZ_PATHNAME)
	    else
		call strupk (imdir, imdir, SZ_PATHNAME)

	    first_time = false
	}
end
