include	"formfn.h"

# FM_MKFORM -- Create a form and bind it to a window
#
# B.Simon	27-Jan-89	Original
# B.Simon	15-Oct-90	Rewritten to use curses

procedure fm_mkform (win, nfield, lenname, lenvalue, title, ftype, fname)

int	win			# i: Window descriptor
int	nfield			# i: Number of fields in form
int	lenname			# i: Declared length of field name
int	lenvalue		# i: Declared length of field value
char	title[ARB]		# i: Form title
int	ftype[ARB]		# i: Data types of fields
char	fname[lenname,ARB]	# i: Names of fields
#--
extern	formfn
int	nrows, ncols, ifield
pointer	data, name, value, type

begin
	# Get the dimensions of the form

	call wdimen (win, nrows, ncols)

	# Allocate data structure

	call malloc (data, LEN_FMSTRUCT, TY_STRUCT)
	call malloc (FM_NAMARY(data), (lenname+1)*nfield, TY_CHAR)
	call malloc (FM_VALARY(data), (lenvalue+1)*nfield, TY_CHAR)
	call malloc (FM_TTLPTR(data), SZ_LINE, TY_CHAR)
	call malloc (FM_TYPARY(data), nfield, TY_INT)

	# Initialize data structure

	FM_FIELD(data) = 1
	FM_NFIELD(data) = nfield
	FM_NPAGE(data) = nrows - 1
	FM_CHANGE(data) = NO
	FM_LENNAM(data) = lenname
	FM_LENVAL(data) = lenvalue
	call strcpy (title, FM_TITLE(data), SZ_LINE)

	name = FM_NAMARY(data)
	value = FM_VALARY(data)
	type = FM_TYPARY(data)

	do ifield = 1, nfield {
	    call strcpy(fname[1,ifield], Memc[name], lenname)
	    name = name + lenname + 1
	    Memc[value] = EOS
	    value = value + lenvalue + 1
	    Memi[type] = ftype[ifield]
	    type = type + 1
	}

	# Bind data structure and function to window

	call wbindstruct (win, formfn, data)

end

procedure fm_clsform (win)

int	win			# i: Window descriptor
#--
pointer	data

begin
	# Get the structure pointer

	call wgetstruct (win, data)

	# Free the substructures hanging off the main structure

	call mfree (FM_NAMARY(data), TY_CHAR)
	call mfree (FM_VALARY(data), TY_CHAR)
	call mfree (FM_TTLPTR(data), TY_CHAR)
	call mfree (FM_TYPARY(data), TY_INT)

end
