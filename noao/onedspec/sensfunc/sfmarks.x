include	<gset.h>
include	"sensfunc.h"

define	GMTYPES	"|point|box|plus|cross|diamond|hline|vline|hebar|vebar|circle|"


# SF_MARKS -- Decode user mark types into GIO mark types.  The input string
# consists of two whitespace separated mark types.

procedure sf_marks (gp, marks)

pointer	gp
char	marks[ARB]

int	i, nscan(), strdic()
pointer	sp, str

int	gmtypes[10]
data	gmtypes /GM_POINT,GM_BOX,GM_PLUS,GM_CROSS,GM_DIAMOND,GM_HLINE,GM_VLINE,
		GM_HEBAR,GM_VEBAR,GM_CIRCLE/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sscan (marks)
	call gargwrd (Memc[str], SZ_LINE)
	if (nscan() == 1) {
	    i = strdic (Memc[str], Memc[str], SZ_LINE, GMTYPES)
	    if (i != 0)
		GP_MARK(gp) = gmtypes[i]
	}
	call gargwrd (Memc[str], SZ_LINE)
	if (nscan() == 2) {
	    i = strdic (Memc[str], Memc[str], SZ_LINE, GMTYPES)
	    if (i != 0)
		GP_MDEL(gp) = gmtypes[i]
	}
	call gargwrd (Memc[str], SZ_LINE)
	if (nscan() == 3) {
	    i = strdic (Memc[str], Memc[str], SZ_LINE, GMTYPES)
	    if (i != 0)
		GP_MADD(gp) = gmtypes[i]
	}

	call sfree (sp)
end
