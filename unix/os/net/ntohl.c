/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* NTOHL -- [MACHDEP] Convert a long integer in net format to host format.
 */
int 
ntohl (long lword)
{
	register char *ip, *op;
	static	long hostw, netw;

	netw = lword;
	ip = (char *)&netw;
	op = (char *)&hostw + 4;

	*--op = *ip++;
	*--op = *ip++;
	*--op = *ip++;
	*--op = *ip++;

	return (hostw);
}
