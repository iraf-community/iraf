/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

/* INDEX -- Return pointer to the first occurrence of a character in a string,
 * or null if the char is not found.
 */
char *
index (str, ch)
char	*str;
register int ch;
{
	register char	*ip;
	register int	cch;

	for (ip=str;  (cch = *ip);  ip++)
	    if (cch == ch)
		return (ip);

	return (0);
}
