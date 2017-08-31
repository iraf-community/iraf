/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* HTONS -- [MACHDEP] Convert a short integer in host format to net format.
 */
int 
htons (int word)
{
	register char *wp;
	static	short w;

	w = word;
	wp = (char *)&w;

	return ((wp[0] << 8) | wp[1]);
}
