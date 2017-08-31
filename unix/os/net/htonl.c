/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* HTONL -- [MACHDEP] Convert a long integer in host format to net format.
 */
int 
htonl (long lword)
{
	register char *ip, *op;
	static	long hostw, netw;

	hostw = lword;
	ip = (char *)&hostw;
	op = (char *)&netw + 4;

	*--op = *ip++;
	*--op = *ip++;
	*--op = *ip++;
	*--op = *ip++;

	return (netw);
}
