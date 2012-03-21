#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "xmlparse.h"


#define	P_DEBUG		0

#define	BUFSIZE		4096
#define	SZ_FNAME	256
#define	SZ_LINE		4096

#define	TY_VOTABLE	1		/* Element types	*/
#define	TY_RESOURCE	2
#define	TY_FIELD	3
#define	TY_PARAM	4
#define	TY_INFO		5
#define	TY_TR		6
#define	TY_TD		7


typedef struct {
    int colnum;				/* column number	*/
    char ID[SZ_FNAME];			/* name attribute	*/
    char name[SZ_FNAME];		/* name attribute	*/
    char ucd[SZ_FNAME];			/* ucd attribute	*/
    char desc[SZ_LINE];			/* description child	*/

    void *next;				/* next FIELD		*/
} FIELD;

typedef struct {
    char name[SZ_FNAME];		/* name attribute	*/
    char value[SZ_FNAME];		/* value attribute	*/
    void *next;				/* next PARAM		*/
} PARAM;

typedef struct {
    char name[SZ_FNAME];		/* name attribute	*/
    char value[SZ_FNAME];		/* value attribute	*/
    void *next;				/* next INFO		*/
} INFO;

typedef struct {
    int  colnum;
    char *dat;
    void *next;				/* next DATA		*/
} DATA;

typedef struct {
    int  rownum;
    DATA *col, 
	 *c_last;
    void *next;				/* next ROW		*/
} ROW;

typedef struct {
    int     nrows, 
	    ncols,
            nparams, 
	    ninfos;
    FIELD   *fields, *f_last;
    PARAM   *params, *p_last;
    INFO    *infos,  *i_last;
    ROW	    *rows,   *r_last;
} VOTable;


int	done		= 0;
int	in_resource	= 0;
int	in_field	= 0;
int	col		= 0;
int	row		= 0;

VOTable *vot		= (VOTable *) NULL;
FIELD   *curfield	= (FIELD *) NULL;
ROW     *currow		= (ROW *) NULL;
DATA    *curcol		= (DATA *) NULL;

char    *el_value	= (char *) NULL;


void   *newType (int type);

void   startElement (void *userData, const char *name, const char **atts);
void   endElement (void *userData, const char *name);
void   charData (void *userData, const XML_Char *s, int len);
void   freeVOTable (void);

void   setField (char *keyw, char *val);
void   printMeta (void);
void   printData (void);

    

int
main(int argc, char * argv[])
{
    char buf[BUFSIZ];
    FILE *in;
    XML_Parser parser;


    if (!argv[1]) {
        in = stdin;

    } else if (!(in = fopen (argv[1], "r"))) {
        printf ("Unable to open input file %s\n", argv[1]);
        return (2);
    }

  
    /*  Create the parser and set the input handlers.
    */
    parser = XML_ParserCreate(NULL);
    XML_SetElementHandler (parser, startElement, endElement);
    XML_SetCharacterDataHandler (parser, charData);

    vot = (VOTable *) newType (TY_VOTABLE);

    do {
        size_t len = fread (buf, 1, sizeof(buf), in);
        done = len < sizeof(buf);
        if (!XML_Parse (parser, buf, len, done)) {
            fprintf (stderr, "Error: %s at line %d\n",
                XML_ErrorString(XML_GetErrorCode(parser)),
                XML_GetCurrentLineNumber(parser));
            return (1);
        }
    } while (!done);

    XML_ParserFree(parser);

    if (in != stdin) 
	fclose (in);

    free ((void *)vot);

    return (0);
}

void *
newType (int type)
{
    switch (type) {
    case TY_VOTABLE: 	return ( (void *) calloc (1, sizeof (VOTable)) );
    case TY_RESOURCE:	break;
    case TY_FIELD:	return ( (void *) calloc (1, sizeof (FIELD)) );
    case TY_PARAM:	return ( (void *) calloc (1, sizeof (PARAM)) );
    case TY_INFO:	return ( (void *) calloc (1, sizeof (INFO)) );
    case TY_TR:		return ( (void *) calloc (1, sizeof (ROW)) );
    case TY_TD:		return ( (void *) calloc (1, sizeof (DATA)) );
    }

    return ((void *)NULL);
}


void
freeVOTable ()
{
}


/***************************************************************************
**  Event Handlers.
*/

void startElement(void *userData, const char *name, const char **atts)
{
    int  att;

    if (P_DEBUG) {
        printf ("s:name='%s'\n", name);
        for (att=0; atts[att]; att++)
             printf (((att % 2) ? "=%s\n" : "    %s"), atts[att]);
    }

    if (strcasecmp (name, "FIELD") == 0) {
	FIELD *new = (FIELD *) newType (TY_FIELD);

	in_field = 1;
	if (vot->fields)
	    vot->f_last->next = new;
	else
	    vot->fields = new;
	vot->f_last = curfield = new;
	vot->ncols = curfield->colnum = ++col;

        for (att=0; atts[att]; att+=2)
	    setField ((char *)atts[att], (char *)atts[att+1]);

    } else if (strcasecmp (name, "PARAM") == 0) {
	PARAM *new = (PARAM *) newType (TY_PARAM);

	if (vot->params)
	    vot->p_last->next = new;
	else
	    vot->params = new;
	vot->p_last = new;
	vot->nparams++;

	if (atts[1]) strcpy (new->name, atts[1]);
	if (atts[3]) strcpy (new->value, atts[3]);

    } else if (strcasecmp (name, "INFO") == 0) {
	INFO *new = (INFO *) newType (TY_INFO);

	if (vot->infos)
	    vot->i_last->next = new;
	else
	    vot->infos = new;
	vot->i_last = new;
	vot->ninfos++;

	if (atts[1]) strcpy (new->name, atts[1]);
	if (atts[3]) strcpy (new->value, atts[3]);

    } else if (strcasecmp (name, "TR") == 0) {
	ROW *new = (ROW *) newType (TY_TR);

	if (vot->rows)
	    vot->r_last->next = new;
	else
	    vot->rows = new;
	vot->r_last = currow = new;
	vot->nrows = ++row;
	currow->rownum = row;

    } else if (strcasecmp (name, "TD") == 0) {
	DATA *new = (DATA *) newType (TY_TD);

	if (currow->col)
	    currow->c_last->next = new;
	else
	    currow->col = new;
	currow->c_last = curcol = new;

    } else if (strcasecmp (name, "RESOURCE") == 0) {
	in_resource = 1;
    }
}


void endElement(void *userData, const char *name)
{
    if (strcasecmp (name, "FIELD") == 0) {
	in_field = 0;

    } else if (strcasecmp (name, "TD") == 0) {
	if (curcol) {
	    curcol->dat = (char *) calloc (1, strlen(el_value) + 1);
	    strcpy (curcol->dat, el_value);
	}

    } else if (strcasecmp (name, "TR") == 0) {
	;

    } else if (strcasecmp (name, "DESCRIPTION") == 0) {
	if (in_field)
	    strcpy (curfield->desc, el_value);

    } else if (strcasecmp (name, "RESOURCE") == 0) {
	in_resource = 0;
printMeta ();
printData ();

    }

    if (el_value) {			/* free the element value 	*/
	free ((char *)el_value);
	el_value = (char *) NULL;
    }
}


void charData (void *userData, const XML_Char *s, int len) 
{
    strncpy ((el_value = (char *)calloc(1,len+1)), s, len);
}


/***************************************************************************
**  Utility / Debug Routines
*/

void
setField (char *keyw, char *val)
{
    if (!keyw || !val)
	return;

    if (strcasecmp (keyw, "name") == 0) 
	strcpy (curfield->name, val);
    else if (strcasecmp (keyw, "ucd") == 0) 
	strcpy (curfield->ucd, val);
    else if (strcasecmp (keyw, "ID") == 0) 
	strcpy (curfield->ID, val);
}


void
printMeta ()
{
    FIELD *f = vot->fields;
    PARAM *p = vot->params;
    INFO  *i = vot->infos;
    int   ii;

    for ( ; f; f=f->next) {
	printf ("field[%02d] name='%s' ucd='%s' ID='%s'\n",
	    f->colnum, f->name, f->ucd, f->ID);
	if (f->desc[0]) printf ("\t  desc='%s'\n", f->desc);
    }
    for (ii=1; p; p=p->next, ii++)
	printf ("param[%02d] name='%s' value='%s'\n", ii, p->name, p->value);
    for (ii=1; i; i=i->next, ii++)
	printf ("info[%02d]  name='%s' value='%s'\n", ii, i->name, i->value);
}

void
printData ()
{
    ROW  *r = (ROW *) NULL;
    DATA *c = (DATA *) NULL;
    int  i, j;


    for (i=0, r=vot->rows; r; r=r->next, i++) {
	printf ("%02d: ", i);
        for (j=0, c=r->col; c; c=c->next, j++) {
	    printf ("%s  ", c->dat);
        }
	printf ("\n");
    }
}
