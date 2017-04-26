#include <stdio.h>

/*
 * STRDUP -- Return a pointer to a copy of the input string.
 */
char *
strdup (s)
char	*s;
{
	char *str;
	int nchars;
	char *malloc();

	nchars = strlen(s) + 1;
	if ((str = malloc (nchars)) == NULL)
	    return (NULL);
	memmove (str, s, nchars);
	return (str);
}
