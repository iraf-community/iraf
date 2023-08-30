/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <unistd.h>

#define import_libc
#define import_stdio
#define import_spp
#define import_error
#define import_xwhen
#include <iraf.h>


typedef int (*PFI)(int *, int*);  /* pointer to function returning int    */

int onint (int *code, int *old_handler);

extern void c_xwhen (int exception, PFI new_handler, PFI *old_handler);
extern int clgstr_(short *param, short *outstr, int *maxch);


void thello_(void)
{
	fputs ("hello, world\n", stdout);
}


void tprint_(void)
{
	char	buf[128];

	sprintf (buf, "\tabcdef %0*d[%-5.2s], %h\n", 5, 99, "wxyz", 12.5);
	fputs (buf, stdout);
}


void tcopy_(void)
{
	FILE	*in, *out;
	int	ch;

	if ((in = fopen ("junk", "r")) == NULL)
	    c_erract (EA_ERROR);
	if ((out = fopen ("junk2", "wb")) == NULL)
	    c_erract (EA_ERROR);

	while ((ch = getc (in)) != EOF)
	    putc (ch, out);

	fclose (in);
	fclose (out);
}


void tscan_(void)
{
	char	buf[SZ_LINE];
	char	str[SZ_LINE];
	char	cval;
	int	ival, nscan, n1, n2;
	int	oldint;
	double	dval;

	c_xwhen (X_INT, (PFI)onint, (PFI *)&oldint);

	printf (">> \n");
	fflush (stdout);

	while (fgets (buf, SZ_LINE, stdin) != NULL) {
	    nscan = sscanf (buf,
		"%n%c %*s %d %lg %s%n", &n1, &cval, &ival, &dval, str, &n2);
	    printf ("nscan=%d: %d '%c' %d %g '%s' %d\n",
		nscan, n1, cval, ival, dval, str, n2);
	    printf (">> \n");
	    fflush (stdout);
	}

	eprintf ("all done\n");
}


int onint (
  int	*code,			/* NOTUSED */
  int	*old_handler
)
{
	write (2, "\7", 1);
	*old_handler = 0;
        return (*code);
}


void tgettk_(void)
{
	XCHAR	fname[SZ_FNAME+1];
	char	token[SZ_LINE+1], delim;
	int	maxch = SZ_FNAME;
	FILE	*fp;

	clgstr_ (c_sppstr("fname"), fname, (int *)&maxch);
	c_strpak (fname, token, maxch);

	fp = fopen (token, "r");
	if (fp == NULL)
	    c_erract (EA_ERROR);

	while (fscanf (fp, " %[^ \t\n,:()!^&+-*/;|?<>]%c", token,&delim) != EOF)
	    eprintf ("%s\n%c\n", token, delim);

	fclose (fp);
}
