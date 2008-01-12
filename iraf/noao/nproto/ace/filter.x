include	<evvexpr.h>
include	"ace.h"
include	"objs.h"
include	"filter.h"


procedure t_filter ()

pointer	catalog			#I Catalog name
pointer	filt			#I Filter

pointer	sp, cat, obj, cathead(), catnext()
errchk	catopen

begin
	call smark (sp)
	call salloc (catalog, SZ_FNAME, TY_CHAR)
	call salloc (filt, SZ_LINE, TY_CHAR)

	call clgstr ("catalog", Memc[catalog], SZ_FNAME)
	call clgstr ("filter", Memc[filt], SZ_FNAME)

	call catopen (cat, Memc[catalog], Memc[catalog], "")

	for (obj=cathead(cat); obj!=NULL; obj=catnext(cat,obj)) {
	    call printf ("%d\n")
		call pargi (OBJ_ROW(obj))
	}

	call catclose (cat)

	call sfree (sp)
end


bool procedure filter (obj, filt)

pointer	obj			#I Object structure
char	filt[ARB]		#I Filter string
bool	match			#O Filter return value

int	type, locpr()
pointer	o, evvexpr()
extern	filt_op(), filt_func()
errchk	evvexpr

begin
	if (obj == NULL)
	    return (false)
	if (filt[1] == EOS)
	    return (true)

	# Evaluate filter.
	o = evvexpr (filt, locpr (filt_op), obj, locpr (filt_func), obj, 0)
	if (o == NULL)
	    return (false)

	type = O_TYPE(o)
	if (O_TYPE(o) == TY_BOOL)
	    match = (O_VALI(o) == YES)

	call mfree (o, TY_STRUCT)
	if (type != TY_BOOL)
	    call error (1, "Filter expression is not boolean")

	return (match)
end


procedure filt_op (obj, name, o)

pointer	obj			#I Object structure
char	name[ARB]		#I Operand name
pointer	o			#O Pointer to output operand

char	lname[SZ_FNAME]
int	i, strdic()

begin
	call strcpy (name, lname, SZ_FNAME)
	call strlwr (lname)
	i = strdic (lname, lname, SZ_FNAME, FILT_NAMES)
	switch (i) {
	case FILT_NUM:
	    call xvv_initop (o, 0, TY_INT)
	    O_VALI(o) = OBJ_NUM(obj)
	case FILT_X:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_XAP(obj)
	case FILT_Y:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_YAP(obj)
	case FILT_WX:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_WX(obj)
	case FILT_WY:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_WY(obj)
	case FILT_NPIX:
	    call xvv_initop (o, 0, TY_INT)
	    O_VALI(o) = OBJ_NPIX(obj)
	case FILT_FLUX:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_FLUX(obj)
	case FILT_PEAK:
	    call xvv_initop (o, 0, TY_DOUBLE)
	    O_VALD(o) = OBJ_PEAK(obj)
	default:
	    call xvv_error1 ("quantity `%s' not found", name)
	}
end



procedure filt_func (obj, func, args, nargs, o)

pointer	obj			#I Object structure
char	func[ARB]		#I Function
pointer	args[ARB]		#I Arguments
int	nargs			#I Number of arguments
pointer	o			#O Function value operand

int	ifunc, strdic()
pointer	sp, buf
bool	strne()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	ifunc = strdic (func, Memc[buf], SZ_LINE, FILT_FUNCS)
	if (ifunc == 0 || strne (func, Memc[buf]))
	    call xvv_error1 ("unknown function `%s'", func)
end
