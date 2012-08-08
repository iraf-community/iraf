include <imhdr.h>
include "cif.h"

#---------------------------------------------------------------------------
.help cif Apr94 source
.ih
NAME
cif -- Coordinated Input File object
.ih
DESCRIPTION
The Coordinated Input File (CIF) object manages multiple input/output
files that are keyed on a single input file list.  This is useful for
tasks whose input data may come from several different types of input
files.  These input files are coordinated, or linked with, some
primary input file.  This object also handles the creation of output
files, again linked in some way with the primary input file list.

An explanation of the problem for which this interface was developed
should help demonstrate the point of the CIF object.  For GHRS, the
calibrated output products includes a flux file, indicated by a file
extension of C1H, and a wavelength vector file, indicated by a file
extension of C0H.  The number of groups in each file is the same, with
each group of the wavelength file corresponding to each group of the
flux file.  The primary input file list would be the flux files and
the one of the secondary input files would be the rootname of the flux
file but with the C0H extension.  The CIF object ensures that the
files are opened "in lock step".  As output, there probably is only
one output list, each file with the same number of groups as the
primary input file, but with a new extension, some addition to the
rootname, or placed in another directory with the same root/extension.

The following sections discuss the public and private
interfaces.
.ih
PUBLIC INTERFACE
The public interface to CIF consists of three subroutines and a number
of variables.  The subroutines are:

.nf
	cif_alloc
	cif_next
	cif_free
.fi

Detailed description of the subroutines are:

.ls pointer = cif_alloc (n_in, n_out)
This routine creates the CIF object.  It requires the number of input
files and output files that will be coordinated with the primary input
file.  Note, the primary input file is NOT counted as one of the
inputs.  The CIF variables are initialized to default or undefined
values.
.ls n_in (int)
Number of input files that will be coordinated.  0 if no input files
are required.
.le
.ls n_out (int)
Number of output files that will be coordinated.  0 if no output files
are requierd.
.le
.ls RETURNS (pointer)
Pointer to the CIF object.  This pointer will be used in other
subroutine calls and accessing the CIF object variables.
.le
.le
.ls boolean = cif_next (o, type)
This is the main loop call for the CIF object.  The CIF variables are
populated with the next set of file names.  This can either be the
next groups of files, or a new set of files depending on the type
specified.  If the value of cif_next is TRUE, then there are more
files.  If FALSE, the end of the list has been reached, i.e. there are
no more primary files to be had.  On return, the CIF variables *_file
and *_status are set.  See the description below of the CIF
variables for more information.  
.ls o (pointer)
The pointer to a CIF object created with cif_alloc.
.le
.ls type (int)
What "type" to get next.  Possible values are
.nf
	CIF_FILE  - Get next primary file and associated
	            in/out files.
	CIF_GROUP - Get next group
.fi
.le
.ls RETURNS (bool)
TRUE if there is another set of file names available.  FALSE if no
more primary files are found.
.le
.le
.ls cif_free (o)
Deallocates the CIF object.
.ls o (pointer)
The pointer to the CIF object to destroy.  Value will be NULL on
return.
.le
.le

The CIF variables can be found in the include file 'cif.h' under the
public definition section.  Below is the current list followed by a
detailed explanation.

.nf
	# CIF structure variables: Primary file
	CIF_p_file_list(o)
	CIF_p_file(o)
	CIF_p_ext(o)
	CIF_p_status(o)
	CIF_p_nloop(o)
	
	# CIF structure variables: Input Files
	CIF_in_file_list(o,i)
	CIF_in_file(o,i)
	CIF_in_ext(o,i)
	CIF_in_status(o,i)
	CIF_in_nloop(o,i)

	# CIF structure variables: Output Files.
	CIF_out_file_list(o,i)
	CIF_out_file(o,i)
	CIF_out_ext(o,i)
	CIF_out_status(o,i)
.fi

For all of the above variables, 'o' is the pointer to the CIF object
and 'i' is the particular input/output file list to access, since
there can be multiple input/output files.  The definition of each
variable is as follows:
.ls CIF_p_file_list, CIF_in_file_list, CIF_out_file_list (char[CIF_SZ_FNAME])
These variables contain the initial file lists for the primary, input,
and output files.  The lists can contain wildcards, substitution, and
"@file" specifications.  Anything that is consistent with the IRAF
'imtopen' IMIO call.  These variables have to be set after the call to
'cif_alloc' but before calling 'cif_next'.

The output list is used a bit differently from the input lists.  If
the next file name from an output list is a directory, the output file
name will be the same as the current primary file, but with this
directory.  If the output list is empty, and the last name retrieved
from the output list is not a directory, then the output filename will
be the same as the current primary file, depending on the value of
CIF_out_ext, see below.  If the output list is empty and the last file
from the output list was a directory, that directory will be used for
all subsequent primary files.
.le
.ls CIF_p_ext, CIF_in_ext, CIF_out_ext (char[CIF_SZ_FNAME])
These variables contain the default file name extension to use for
each file.  The default extensions do not have to be specified, i.e.
the value of these variables is an empty string.  In that case, the
actions described below for each variable does not occur.  The
symantics of the _EXT variable is slightly different for each type of
file:
.ls CIF_p_ext
If the next primary file, as retrieved from the primary file list,
does not exist, replace the original extension with this extension.
If the file still does not exist, the call to cif_next will error.
.le
.ls CIF_in_ext
This variable is used in two different ways.  If the input file list
is empty, then this extension is placed on the current primary file
and the file is looked for.  If the input list is not empty but the
next input file does not exist, this extension replaces the original
extension and existance is checked again.
.le
.ls CIF_out_ext
If specified, this extension is always placed on the output file name.
.le
.le
.ls CIF_p_nloop, CIF_in_nloop (int)
These variable specify how many calls to 'cif_next' must occur before
the next group/file is retrieved from the particular primary or input
list.  The default value is 1, or every time 'cif_next' is called,
retrieve the next group/file.  This is useful when a single group in
either the primary or input file corresponds to a number of groups in
another input file (or primary file).  

The output files don't have a counter, since the output file names are
created based on the primary file.
.le
.ls CIF_p_file, CIF_in_file, CIF_out_file (char[CIF_SZ_FNAME])
On return from 'cif_next', these variables contain the next set of
file names.  The validity of the name contained in each variable is
determined by the corresponding *_STATUS variable, see below.  If any
of the files are images, a group specification is appended.
.le
.ls CIF_p_status, CIF_in_status, CIF_out_status (int)
The status or validity of the names contained in the *_FILE variables
after a call to 'cif_next'.  The possible values are:
.ls CIF_OK
The name represents another file, different from the name returned by
a previous call to 'cif_next'.  For the primary and input files, the
file exists.  For output files, the file does not exist.
.le
.ls CIF_NONE
For input files, there is no more files in that particular input list.
The value of CIF_in_file is invalid, i.e. contains "garbage".  The
primary file and output files will never have this status value.  This
is because, when there are no more primary files, 'cif_next' returns
FALSE and the output files are created based on the primary files.
.le
.ls CIF_SAME
For primary and input values, this value indicates that the file name
is the same as that returned from a previous call to 'cif_next'.  A
file name will not change only because the value of *_NLOOP and the
current call to 'cif_next' implied to not change the file name.

For output files, this status indicates that the output file name is
the same as the current primary file.  This can occur if the output
file list is empty and either no default extension was specified, or
the default extension happens to be the same as the current primary
file.  Note, this indicates that the character string of the output
file matches the character string of the current primary file.  There
is no check whether the created files would actually be the same.
For example, if the current primary file is "root.ext", and the output
file is "./root.ext", the status would be CIF_OK, not CIF_SAME.  See
CIF_EXISTS. 
.le
.ls CIF_EXISTS
For output files only, this indicates that the file exists.
.le
.le
The one constant in use is CIF_SZ_FNAME, size of all character
variables used by CIF.
.ih
PRIVATE INTERFACE
Note: There won't be much discussion here.  Remember: "Use the force,
read the source".

The CIF object consists of two structures.  The CIF file object, which
maintains the individual files, and the CIF structure which
manages the number of files.

.ls Subroutines
The subroutines are as follows:
.ls pointer = cif_alloc_file_obj()
Create the CIF file object.  This contains the information specific to
an individual file list.
.ls RETURNS (pointer)
Returns a pointer to a CIF FILE object.
.le
.le
.ls cif_free_file_obj (o)
Destroy a CIF FILE object.
.ls o (pointer)
The FILE object to destroy.  On return, the value will be NULL.
.le
.le
.ls bool = cif_next_primary (o)
Get the next primary file and all input/output files.  Besides the
routines in the public interface, this is the only one that deals with
the CIF structure.  This routine, regardless of the group counts and
whether there are any groups left, retrieve the next files from the
primary, input, and output lists and populates the CIF public
variables appropriately.
.ls o (pointer)
A pointer to the CIF object.
.le
.ls RETURNS (boolean)
TRUE if there is another set of files.  FALSE if there are no more
primary files.
.le
.le
.ls cif_base2name (o, p)
Get the next file name for the specified FILE object, using
information from the primary FILE object.
.ls o (pointer)
The FILE object to get the next name for.  This should only be a
primary FILE object or an input FILE object.  Output FILE objects use
'cif_out'.
.le
.ls p (pointer)
The FILE object for the primary file list.
.le
.le
.ls int = cif_file_type (fname)
Determine the type of file specified.  If the file does not exist, the
routine generates an error.
.ls fname (char[ARB])
The name of the file to determine the type of.
.le
.ls RETURNS (int)
A file type id.  See 'cif.h' under the Private definitions for a list
of file types.
.le
.le
.ls bool = cif_next_group (o, loop)
Get the file name representing the next group of the specified file.
.ls o
The primary or input FILE object to get the next group of.
.le
.ls loop (int)
The number of times 'cif_next' has been called.  Used to decide
whether another group should be returned or not.
.le
.ls RETURNS (bool)
TRUE if there is another group, even if the group has not changed.
FALSE if there are no more groups in the current image.
.le
.le
.ls cif_out (o, p)
Find the next output file name based on the primary file.
.ls o (pointer)
The output FILE object to get the name for.
.le
.ls p (pointer)
The primary FILE object.
.le
.le
.le
.endhelp
#---------------------------------------------------------------------------
pointer	procedure cif_alloc (n_in, n_out)

int	n_in			# I:  Number of secondary input files.
int	n_out			# I:  Number of output files.

# Declarations.
pointer	cif_alloc_file_obj()	# Alloce a CIF FILE object.
pointer	o			# The CIF object.
int	i			# Generic.

errchk	cif_alloc_file_obj, malloc

begin
	# Allocate the CIF object.
	call malloc (o, CIF_SZ, TY_STRUCT)
	
	# Allocate the CIF FILE object for the primary file.
	CIF_p(o) = cif_alloc_file_obj()

	# Allocate FILE objects for each input file.
	CIF_n_in(o) = n_in
	call malloc (CIF_in_ptr(o), CIF_n_in(o), TY_POINTER)
	do i = 1, CIF_n_in(o) {
	    CIF_in(o,i) = cif_alloc_file_obj ()
	}
	
	# Allocate FILE objects for each output file.
	CIF_n_out(o) = n_out
	call malloc (CIF_out_ptr(o), CIF_n_out(o), TY_POINTER)
	do i = 1, CIF_n_out(o) {
	    CIF_out(o,i) = cif_alloc_file_obj ()
	}

	# Initialize the loop count
	CIF_loop(o) = 0
	
	# That's all folks.
        return (o)
end
#---------------------------------------------------------------------------
# End of cif_alloc
#---------------------------------------------------------------------------
procedure cif_free (o)

pointer	o			# IO: CIF object, NULL on return.

# Declarations.
int	i			# generic.

errchk	cif_free_file_obj, mfree

begin
	# Free FILE objects for each output file.
	do i = 1, CIF_n_out(o)
	   call cif_free_file_obj (CIF_out(o,i))
	call mfree (CIF_out_ptr(o), TY_POINTER)
	
	# Free FILE objects for each input file.
	do i = 1, CIF_n_in(o)
	   call cif_free_file_obj (CIF_in(o,i))
	call mfree (CIF_in_ptr(o), TY_POINTER)

	# Free the primary FILE object.
	call cif_free_file_obj (CIF_p(o))
	
	# Remove the object.
	call mfree (o, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of cif_free
#---------------------------------------------------------------------------
pointer procedure cif_alloc_file_obj ()

# Declarations.
pointer	o			# The CIF FILE object.

errchk	malloc

begin
	# Get memory.
	call malloc (o, CIF_SZ_FILE, TY_STRUCT)
	call malloc (CIF_cbuf(o), CIF_SZ_FILE_CBUF, TY_CHAR)

	# Setup initial values.
	call strcpy ("", CIF_file_list(o), CIF_SZ_FNAME)
	call strcpy ("", CIF_file(o), CIF_SZ_FNAME)
	CIF_list(o) = NULL
	CIF_group(o) = NULL
	call strcpy ("", CIF_ext(o), CIF_SZ_FNAME)
	CIF_status(o) = CIF_NONE
	CIF_nloop(o) = 1
	CIF_cg(o) = INDEFI
	call strcpy ("", CIF_base(o), CIF_SZ_FNAME)
	CIF_type(o) = INDEFI

	# That's all folks.
	return (o)
end
#---------------------------------------------------------------------------
# End of cif_alloc_file_obj
#---------------------------------------------------------------------------
procedure cif_free_file_obj (o)

pointer	o			# IO:  CIF FILE object, NULL on return.

# Declarations.
errchk	imtclose, mfree, tp_close

begin
	# Close other opened objects.
	if (CIF_list(o) != NULL)
	    call imtclose (CIF_list(o))
	if (CIF_group(o) != NULL)
	    call tp_close (CIF_group(o))

	# That's all folks.
	call mfree (CIF_cbuf(o), TY_CHAR)
	call mfree (o, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of cif_free_file_obj
#---------------------------------------------------------------------------
bool procedure cif_next (o, type)

pointer	o			# I:  The CIF object.
int	type			# I:  Get a group or file.

# Declarations
bool	another			# True if another set of files are available.
bool	bx			# Generic.
bool	cif_next_group()	# Get next group.
bool	cif_next_primary()	# Get next primary files.
int	i			# Generic.
int	imtlen()		# Length of a file list.
pointer	imtopen()		# Open an file list.

errchk	imtlen, imtopen

begin
	# Increment the loop count.
	CIF_loop(o) = CIF_loop(o) + 1
	
	# If the lists have not been opened, do it now.
	if (CIF_list(CIF_p(o)) == NULL) {
	    CIF_list(CIF_p(o)) = imtopen (CIF_file_list(CIF_p(o)))
	    if (imtlen (CIF_list(CIF_p(O))) <= 0)
		call error (1, "cif: no input files specified")
	    do i = 1, CIF_n_in(o)
		CIF_list(CIF_in(o,i)) = imtopen (CIF_file_list(CIF_in(o,i)))
	    do i = 1, CIF_n_out(o)
		CIF_list(CIF_out(o,i)) = imtopen (CIF_file_list(CIF_out(o,i)))
	    another = cif_next_primary (o)
	}

	# Else, if type is FILE, just get next set of files.
	else if (type == CIF_NEXT_FILE)
	    another = cif_next_primary (o)

	# Else, loop through groups.
	else {
	    if (cif_next_group (CIF_p(o), CIF_loop(o))) {

		# Loop through all the inputs.
		do i = 1, CIF_n_in(o)
		    bx = cif_next_group (CIF_in(o,i), CIF_loop(o))

		# Loop through all the outputs.
		do i = 1, CIF_n_out(o)
		    call cif_out (CIF_out(o,i), CIF_p(o))

		# There is another file.
		another = true
	    }

	    # Else, get the next set of files.
	    else
		another = cif_next_primary (o)
	}

        # That's all folks.
        return (another)
end
#---------------------------------------------------------------------------
# End of cif_next
#---------------------------------------------------------------------------
bool procedure cif_next_primary (o)

pointer	o			# I:  The CIF object.

# Declarations.
bool	another			# True if another set of files is available.
int	i			# Generic.
int	imtgetim()		# Get next file from file list.
char	sx[SZ_LINE]		# Generic string.

errchk	imtgetim

begin
	# Open next primary image.  If there are no more, then
	# that's all.
	if (imtgetim (CIF_list(CIF_p(o)), CIF_base(CIF_p(o)),
		      CIF_SZ_FNAME) != EOF) {
	    call cif_base2name (CIF_p(o), CIF_p(o))
	    if (CIF_status(CIF_p(o)) == CIF_NONE) {
		call sprintf (sx, SZ_LINE, "cif: no primary file %s")
		call pargstr (CIF_base(CIF_p(o)))
		call error (1, sx)
	    }
	    
	    # Open the next set of input files.
	    do i = 1,CIF_n_in(o) {
		if (imtgetim (CIF_list(CIF_in(o,i)), CIF_base(CIF_in(o,i)),
			      CIF_SZ_FNAME) == EOF)
		    call strcpy ("", CIF_base(CIF_in(o,i)), CIF_SZ_FNAME)
		call cif_base2name (CIF_in(o,i), CIF_p(o))
	    }
	    
	    # Open the next set of output files.
	    do i = 1, CIF_n_out(o) {
		CIF_status(CIF_out(o,i)) = CIF_OK
		if (imtgetim (CIF_list(CIF_out(o,i)), CIF_base(CIF_out(o,i)),
			      CIF_SZ_FNAME) == EOF) {
		    if (IS_INDEFI(CIF_type(CIF_out(o,i))))
			CIF_type(o,i) = CIF_GENERIC
		    else
			CIF_status(CIF_out(o,i)) = CIF_SAME
		}

		call cif_out (CIF_out(o,i), CIF_p(o))
	    }

	    # Indicate that another set of files are available.
	    another = true
	    
	} else
	    another = false

	# That's all folks.
	return (another)
end
#---------------------------------------------------------------------------
# End of cif_next_primary
#---------------------------------------------------------------------------
procedure cif_base2name (o, p)

pointer	o			# I:  CIF FILE Object to find name for.
pointer	p			# I:  CIF FILE Object of primary file.

# Declarations
bool    bx			# Generic.
int	cif_file_type()		# Determine file type of file.
int	i			# Generic.
int	strlen()		# Get length of string.
bool	tp_fetch()		# Get next group.
pointer	tp_open()		# Open a group list.

errchk	tp_close, tp_fetch, tp_open

begin
	# If there is a group list open, close it.
	if (CIF_group(o) != NULL)
	    call tp_close (CIF_group(o))
	
	# Determine file type.  If there is an error, try with
	# the default extension.  If that doesn't exist, try default
	# extension of the primary name.
	CIF_status(o) = CIF_OK
	if (strlen(CIF_base(o)) <= 0) {
	    call change_ext (CIF_base(p), CIF_ext(o), CIF_file(o),
			     CIF_SZ_FNAME)
	    iferr (CIF_type(o) = cif_file_type (CIF_file(o)))
		CIF_status(o) = CIF_NONE
	} else {
	    call strcpy (CIF_base(o), CIF_file(o), CIF_SZ_FNAME)
	    iferr (CIF_type(o) = cif_file_type (CIF_file(o))) {
		call change_ext (CIF_file(o), CIF_ext(o), CIF_file(o),
				 CIF_SZ_FNAME)
		iferr (CIF_type(o) = cif_file_type (CIF_file(o)))
		    CIF_status(o) = CIF_NONE
	    }
	}

	# Make the new name the base.
	if (CIF_status(o) == CIF_OK) {
	    call strcpy (CIF_file(o), CIF_base(o), CIF_SZ_FNAME)
	    
	    # If the file is an image, open the group list.
	    if (CIF_type(o) == CIF_IMAGE) {
		CIF_group(o) = tp_open (CIF_file(o), 0, i)
		bx = tp_fetch (CIF_group(o), CIF_file(o))
	    }
	    CIF_cg(o) = 1
	}
end
#---------------------------------------------------------------------------
# End of cif_base2name
#---------------------------------------------------------------------------
int procedure cif_file_type (fname)

char	fname[ARB]		# I:  The file to determine type of.

# Declarations.
int	access()		# Get file access.
pointer	immap()			# Open an image.
pointer	px			# Generic.
int	strlen()		# Get length of string.
int	type			# Type of file.

errchk	access, imunmap

begin
	if (strlen (fname) <= 0)
	    call error (1, "cif: Unknown type")
	else ifnoerr (px = immap (fname, READ_ONLY, NULL)) {
	    type = CIF_IMAGE
	    call imunmap (px)
	} else if (access (fname, 0, 0) == YES)
	    type = CIF_GENERIC
	else
	    call error (1, "cif: Unknown type")

	return (type)
end
#---------------------------------------------------------------------------
# End of cif_file_type
#---------------------------------------------------------------------------
bool procedure cif_next_group (o, loop)

pointer	o			# I:  The CIF FILE object.
int	loop			# I:  Current loop count.

# Declarations
bool	tp_fetch()		# Get next group.

errchk	tp_fetch

begin
	# Is this file a type to have groups?
	if (CIF_type(o) == CIF_IMAGE) {
	    
	    # Is this loop one to change on?
	    if (mod (loop-1, CIF_nloop(o)) == 0) {

		# Get the next group.
		if (tp_fetch (CIF_group(o), CIF_file(o))) {
		    CIF_status(o) = CIF_OK
		    CIF_cg(o) = CIF_cg(o) + 1
		}

		# Else, no more data.
		else
		    CIF_status(o) = CIF_NONE
	    }

	    # Nope, keep it the same.
	    else
		CIF_status(o) = CIF_SAME
	}

	# Else, nope, no groups here.
	else
	    CIF_status(o) = CIF_NONE

	# Return true if a file exists.
	return (CIF_status(o) != CIF_NONE)
end
#---------------------------------------------------------------------------
# End of cif_next_group
#---------------------------------------------------------------------------
procedure cif_out (o, p)

pointer	o			# I:  CIF FILE Object to get output name.
pointer	p			# I:  Primary CIF FILE Object to get info.

# Declarations
int	access()		# Is file accessable?
int	cl_index, cl_size	# Cluster info.
char	dir[CIF_SZ_FNAME]	# Directory of the file name.
char	ext[CIF_SZ_FNAME]	# Extension of the file name.
int	i			# Generic.
int	isdirectory()		# Is a file a directory?
char	ksection[CIF_SZ_FNAME]	# Ksection of the file name.
char	root[CIF_SZ_FNAME]	# Root of the file name.
char	section[CIF_SZ_FNAME]	# Section of the file name.
bool	streq()			# Are strings equal?
int	strlen()		# Get length of string.
char	sx[1]			# Generic.

errchk	access, fbuild, fparse, isdirectory

begin
	# If a new input, determine what it is.
	if (CIF_status(o) != CIF_SAME) {
	    if (strlen (CIF_base(o)) <= 0)
		CIF_type(o) = CIF_SAME_ROOT
	    else if (isdirectory (CIF_base(o), root, SZ_PATHNAME) > 0)
		CIF_type(o) = CIF_DIRECTORY
	    else
		CIF_type(o) = CIF_GENERIC
	}

	# Create the new file name.
	call fparse (CIF_file(p), dir, CIF_SZ_FNAME, root, CIF_SZ_FNAME,
		     ext, CIF_SZ_FNAME, cl_index, cl_size, section,
		     CIF_SZ_FNAME, ksection, CIF_SZ_FNAME)
	switch (CIF_type(o)) {
	case CIF_DIRECTORY:
	    call strcpy (CIF_base(o), dir, CIF_SZ_FNAME)

	case CIF_GENERIC:
            call fparse (CIF_base(o), dir, CIF_SZ_FNAME, root, CIF_SZ_FNAME,
                         ext, CIF_SZ_FNAME, i, i, sx, 1, sx, 1)
	}

	# If a different extension is supplied, use it.
	if (strlen (CIF_ext(o)) > 0) {
	    call strcpy (".", ext, CIF_SZ_FNAME)
	    call strcat (CIF_ext(o), ext, CIF_SZ_FNAME)
	}
	
	# Build the new file name.
	call fbuild (dir, root, ext, cl_index, cl_size, section, ksection,
		     CIF_file(o), CIF_SZ_FNAME)

	# Set status if the name is the same as the primary file.
	if (streq (CIF_file(p), CIF_file(o)))
	    CIF_status(o) = CIF_SAME
	else {
	    if (access (CIF_file(o), 0, 0) == YES)
		CIF_status(o) = CIF_EXISTS
	    else
		CIF_status(o) = CIF_OK
	}
end
#---------------------------------------------------------------------------
# End of cif_out
#---------------------------------------------------------------------------
procedure cif_test()

pointer	cif, cif_alloc()
bool	cif_next()
int	clgeti(), i

begin
	cif = cif_alloc (2, 1)
	
	call clgstr ("primary", CIF_p_file_list(cif), CIF_SZ_FNAME)
	call clgstr ("p_ext", CIF_p_ext(cif), CIF_SZ_FNAME)
	CIF_p_nloop(cif) = clgeti ("p_loop")
	call clgstr ("in1", CIF_in_file_list(cif,1), CIF_SZ_FNAME)
	call clgstr ("in1_ext", CIF_in_ext(cif,1), CIF_SZ_FNAME)
	CIF_in_nloop(cif,1) = clgeti ("in1_loop")
	call clgstr ("in2", CIF_in_file_list(cif,2), CIF_SZ_FNAME)
	call clgstr ("in2_ext", CIF_in_ext(cif,2), CIF_SZ_FNAME)
	CIF_in_nloop(cif,2) = clgeti ("in2_loop")
	call clgstr ("out1", CIF_out_file_list(cif,1), CIF_SZ_FNAME)
	call clgstr ("out1_ext", CIF_out_ext(cif,1), CIF_SZ_FNAME)
	
	while (cif_next (cif, CIF_NEXT_GROUP)) {
	    call printf ("Primary file == '%s'")
	    call pargstr (CIF_p_file(cif))
	    if (CIF_p_status(cif) == CIF_SAME)
		call printf (" (same as previous)")
	    call printf ("\n")

	    do i = 1, 2 {
		switch (CIF_in_status(cif,i)) {
		case CIF_OK:
		    call printf ("	Input %d is '%s'\n")
		    call pargi (i)
		    call pargstr (CIF_in_file(cif,i))
		case CIF_NONE:
		    call printf ("	No files for input %d\n")
		    call pargi (i)
		case CIF_SAME:
		    call printf ("	Input %d is '%s' (same as previous)\n")
		    call pargi (i)
		    call pargstr (CIF_in_file(cif,i))
		}	
	    }

	    call printf ("	Output file is '%s'")
	    call pargstr (CIF_out_file(cif,1))
	    if (CIF_out_status(cif,1) == CIF_EXISTS)
		call printf (" (file exists)")
	    else if (CIF_out_status(cif,1) == CIF_SAME)
		call printf (" (same as input)")
	    call printf ("\n")
	}

	call cif_free (cif)
end
#---------------------------------------------------------------------------
# End of cif_test
#---------------------------------------------------------------------------
