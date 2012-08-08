include	<imio.h>

define	NFIELD		4
define	FIELD		Memc[($1)+($2-1)*(SZ_FNAME+1)]

# TP_PARSE -- Parse an image name into its component parts
#
# Parse an image name to obtain the root name, the image section, the 
# group index, and the group count. 
#
# B.Simon	28-Feb-89	Original
# B.Simon	02-Jun-89	imparse replaced
# B.Simon	16-Jul-98	Revised to flag unparsable sections
# B.Simon	02-Oct-98	added call to tp_count
# B.Simon	26-Apr-99	check for data in extension zero
# B.Simon	06-May-99	set index to one for new files w/o sections
# B.Simon	14-Jun-99	set count to one if section could not be parsed
# B.Simon	20-Nov-00	get default extension with iki_access

procedure tp_parse (imname, def_count, root, section, index, count)

char	imname[ARB]		# i: image name
int	def_count		# i: default group count
char	root[ARB]		# o: root name
char	section[ARB]		# o: image section
int	index			# o: group index
int 	count			# o: group count
#--
int	ifield, nc
pointer	sp, image, root2, sect, ext

int	access(), strlen(), fnextn(), envgets(), iki_access()
int	tp_count(), tp_hasgroup()

string	ambiguous  " Ambiguous image name, extension required"

errchk	immap, imunmap

begin
	# Allocate dynamic memory for error string

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (root2, SZ_FNAME, TY_CHAR)
	call salloc (sect, (SZ_FNAME+1)*NFIELD, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)

	# Break the image name into its component parts and
	# get the group index and count

	call tp_break (imname, Memc[sect], NFIELD, SZ_FNAME)

	call tp_group (FIELD(sect,1), FIELD(sect,2), def_count, index, count)

	call strcpy (FIELD(sect,1), root, SZ_FNAME)
	call strcpy (FIELD(sect,2), section, SZ_FNAME)

	# Copy the remaining fields into the section

	do ifield = 3, NFIELD
	    call strcat (FIELD(sect,ifield), section, SZ_FNAME)

	# Add default extension onto image if no extension given

	nc = strlen (root)
	if (root[nc] != '.' && fnextn (root, Memc[ext], SZ_FNAME) == 0) {
	    # Determine the access mode from the default grou[ count

	    if (def_count > 0) {
		nc = envgets ("imtype", Memc[ext], SZ_FNAME)

	    } else {
		if (iki_access (root, Memc[root2], 
				Memc[ext], READ_ONLY) == ERR)
		    call error (1, ambiguous)
	    }
	    
	    call strcat (".", root, SZ_FNAME)
	    call strcat (Memc[ext], root, SZ_FNAME)
	}

	# Set index and count when the image does not contain a section

	if (section[1] == EOS) {
	    if (access (root, 0, 0) == NO) {
		index = 1
	    } else if (tp_hasgroup (root, 1) == YES) {
		index = 1
	    } else {
		index = 0
	    }
	}

	if (count != ERR) {
	    count = max (count, 1)

	} else if (index == ERR) {
	    count = 1

	} else if (def_count > 0) {
	    count = def_count

	} else {
	    count = tp_count (root)
	}

	call sfree (sp)
end
