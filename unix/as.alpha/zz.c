/* Dummy C look-alike version of zsvjmp.  A cc -S on this file gets us close
 * to what we need for zsvjmp.s.
 */
zsvjmp_ (jmpbuf, status)
int **jmpbuf;
int *status;
{
	jmpbuf[0] = status;
	*status = 0;
	setjmp (jmpbuf + 1);
}
