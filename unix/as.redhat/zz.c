/* Compile with gcc -S to get demo assembler code.
 */
zsvjmp_(buf,status)
int *buf;
int *status;
{
	*status = 0;
	buf[0] = *status;
	setjmp (&buf[1]);
}
