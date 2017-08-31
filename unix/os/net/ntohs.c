/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* NTOHS -- [MACHDEP] Convert a short integer in net format to host format.
 */
int 
ntohs (int word)
{
	register char *wp;
	static	short w;

	w = word;
	wp = (char *)&w;

	return ((wp[0] << 8) | wp[1]);
}
