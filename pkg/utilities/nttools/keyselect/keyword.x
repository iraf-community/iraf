include <imio.h>
include <imhdr.h>
include "keyselect.h"

#* HISTORY *
#* B.Simon	12-Mar-92	Original

# GET_KEYWORD -- Get the keyword from the image header

procedure get_keyword (im, name, dtype, value, maxch)

pointer	im		# i: image descriptor
char	name[ARB]	# i: keyword name
int	dtype		# o: keyword data type
char	value[ARB]	# o: keyword value
int	maxch		# i: maximum length of value string
#--
include "keyselect.com"

string	badname "Warning: header keyword %s not found in %s\n"

int	imgftype(), gf_gfind()

begin
	# Any name beginning with a $ is a special keyword

	if (name[1] == '$') {
	    call spec_keyword (im, name, dtype, value, maxch)

	} else {
	    # Get the data type of the header keyword
	    # If the keyword is not found set the data type to
	    # zero to indicate this and return

	    iferr {
		dtype = imgftype (im, name)
	    } then {
		call eprintf (badname)
		call pargstr (name)
		call pargstr (IM_HDRFILE(im))

		dtype = 0
		value[1] = EOS
		return
	    }

	    if (dtype == TY_SHORT || dtype == TY_LONG) 
		dtype = TY_INT
	    if (dtype == TY_CHAR)
		dtype = - maxch

	    # Read header keyword from image. This procedure sets hasgroup 
	    # to true if asked to retrieve a group parameter

	    call imgstr (im, name, value, maxch)
	    if (dtype == TY_BOOL) {
		if (value[1] == 'T') {
		    call strcpy ("yes", value, maxch)
		} else {
		    call strcpy ("no", value, maxch)
		}
	    }

	    if (gf_gfind (im, name) > 0)
		hasgroup = true
	}

end

# NAME_KEYWORD -- Retrieve the default column name for a special keyword

procedure name_keyword (name, colname, maxch)

char	name[ARB]	# i: keyword name
char	colname[ARB]	# o: default column name
int	maxch		# i: maximum length of column name
#--
int	idx, junk
pointer	sp, errmsg

string	special   "group,dir,ext,hdr,pix,root"
string	defaults  "group,directory,extension,header_file,data_file,rootname"
string	badname   "Name for special keyword not recognized (%s)"

int	word_match(), word_find()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	if (name[1] != '$') {
	    call strcpy (name, colname, maxch)
	    return
	}

	# Get the index of special keyword name in the list
	# The find the corresponding name in the list of defaults

	idx = word_match (name[2], special)
	if (idx == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (name)
	    call error (1, Memc[errmsg])
	} else {
	    junk = word_find (idx, defaults, colname, maxch)
	}

	call sfree (sp)
end

# SPEC_KEYWORD -- Get the value of a special keyword

procedure spec_keyword (im, name, dtype, value, maxch)

pointer	im		# i: image descriptor
char	name[ARB]	# i: keyword name
int	dtype		# o: keyword data type
char	value[ARB]	# o: keyword value
int	maxch		# i: maximum length of value string
#--
include "keyselect.com"

int	match, ival, junk
pointer	sp, image, ldir, root, errmsg, hdr, ext

string	int_special "group"
string	str_special "dir,ext,hdr,pix,root"

string	badname    "Name for special keyword not recognized (%s)"
string	badimgext  "Image extension not recognized (%s)"

bool	streq()
int	word_match(), fnldir(), fnroot(), itoc()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Search lists for special keyword

	match = - word_match (name[2], int_special)
	if (match == 0)
	    match = word_match (name[2], str_special)

	# Data type is determined from which list it is on

	if (match < 0) {
	    dtype = TY_INT
	} else if (match > 0) {
	    dtype = - maxch
	} else {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (name)
	    call error (1, Memc[errmsg])
	}

	# Break image name into its component parts

	if (match > 0) {
	    call imgcluster (IM_HDRFILE(im), Memc[image], SZ_FNAME)

	    hdr = image + fnldir (Memc[image], Memc[ldir], SZ_FNAME)
	    ext = hdr + 1 + fnroot (Memc[hdr], Memc[root], SZ_FNAME)
	}


	# Get value of special keyword

	switch (match) {
	case -1:
	    # group number $group
	    hasgroup = true
	    ival = max (1, IM_CLINDEX(im))
	    junk = itoc (ival, value, maxch)
	case 0:
	    # (not used)
	    ;
	case 1:
	    # directory name $dir
	    call strcpy (Memc[ldir], value, maxch)
	case 2:
	    # extension $ext
	    call strcpy (Memc[ext], value, maxch)
	case 3:
	    # header file name $hdr
	    call strcpy (Memc[hdr], value, maxch)
	case 4:
	    # pixel file name $pix
	    if (Memc[ext+2] != 'h' || Memc[ext+3] != EOS) {
		call sprintf (Memc[errmsg], SZ_LINE, badimgext)
		call pargstr (Memc[hdr])
		call error (1, Memc[errmsg])
	    }

	    call strcpy (Memc[root], value, maxch)
	    if (streq (Memc[ext], "imh")) {
		call strcat (".pix", value, maxch)
	    } else {
		Memc[ext+2] = 'd'
		call strcat (".", value, maxch)
		call strcat (Memc[ext], value, maxch)
	    }
	case 5:
	    # root name $root
	    call strcpy (Memc[root], value, maxch)
	}

	call sfree (sp)
end

# TYPE_KEYWORD -- Retrieve the type of a special keyword

int procedure type_keyword (name)

char	name[ARB]	# i: special keyword name
#--
int	dtype
pointer	sp, errmsg

string	int_special "group"
string	str_special "dir,ext,hdr,pix,root"
string	badname     "Name for special keyword not recognized (%s)"

int	word_match()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	if (name[1] != '$') {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (name)
	    call error (1, Memc[errmsg])

	} else if (word_match (name[2], int_special) > 0) {
	    dtype = TY_INT

	} else if (word_match (name[2], str_special) > 0) {
	    dtype = TY_CHAR

	} else {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (name)
	    call error (1, Memc[errmsg])
	}

	call sfree (sp)
	return (dtype)
end

