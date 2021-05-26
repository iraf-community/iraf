/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "proto.h"


/*
 * LISTS -- Access lists for list-structured parameters.
 */

extern	char *eofstr;
extern	char *nullstr;
extern	int	cldebug;


/* READLIST -- Read next value from list-structured parameter *pp and return
 * an operand.  Operand will be UNDEF if there was no file or cannot open the
 * named file (this will generate a query for the param) or eofstr if eof.
 * As a special case, check for the value of the param being the string "stdin"
 *   and read from the current standard input if it is.
 * Call error() if get ferror while reading or can't open list file.
 */
struct operand 
readlist (struct param *pp)
{
	struct	operand result;
	int	bastype;
	char	*line;

	result.o_type = OT_INT;		/* in case we make an undef op	*/
	line = pp->p_listval;

	if ((pp->p_valo.o_type & OT_UNDEF) || *pp->p_val.v_s == '\0') {
	    /* no list file name. */
	    pp->p_flags &= ~P_LEOF;
	    setopundef (&result);
	    return (result);
	}

	if (pp->p_listfp == NULL && !(pp->p_flags & P_LEOF)) {
	    char *filename = pp->p_val.v_s;
	    if (!strcmp (filename, "STDIN") || !strcmp (filename, "stdin"))
		pp->p_listfp = currentask->t_stdin;
	    else if ((pp->p_listfp = fopen (filename, "r")) == NULL) {
		/* should we tell user what's happening?
		cl_error (E_UERR|E_P, "can not open list file `%s'",
		    pp->p_val.v_s);
		 */
		setopundef (&result);
		return (result);
	    }
	}

	bastype = pp->p_type & OT_BASIC;

	if (pp->p_listfp != NULL) {
again:	    fgets (line, SZ_LINE, pp->p_listfp);
	    if (ferror (pp->p_listfp)) {
		closelist (pp);
		/* Don't just let it go as undefined if get an actual error. */
		cl_error (E_UERR|E_P, "list file read err");

	    } else if (feof (pp->p_listfp)) {
		closelist (pp);
		pp->p_flags |= P_LEOF;
		result = makeop (eofstr, OT_STRING);

	    } else {
		char *index(), *nlp, *ip;

		nlp = index (line, '\n');
		if (nlp != NULL)
		    *nlp = '\0';

		/* If not simple list structured struct type parameter (used
		 * to get raw lines from a text file), ignore blank lines and
		 * comments lines in the list.
		 */
		if (bastype != OT_STRING ||
		    pp->p_type & (PT_FILNAM|PT_GCUR|PT_IMCUR|PT_UKEY)) {

		    for (ip=line;  *ip && (*ip == ' ' || *ip == '\t');  ip++)
			;
		    if (*ip == EOS || *ip == '#')
			goto again;
		}
			
		result = makeop (line, bastype);
	    }

	} else
	    result = makeop (eofstr, OT_STRING);

	return (result);
}


/* CLOSELIST -- Close the list file in list-structured param pp.
 * We assume (pp->p_type & PT_LIST) but do check that the file is not
 * already closed and that we're not closing the real stdin.
 */
void 
closelist (register struct param *pp)
{
	if (pp->p_listfp != NULL) {
	    if (pp->p_listfp != stdin)
		fclose (pp->p_listfp);
	    pp->p_listfp = NULL;
	}
}
