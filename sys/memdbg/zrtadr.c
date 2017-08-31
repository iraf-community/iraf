/* ZRTADR -- Return the program address from which the routine calling zrtadr
 * was called.  If zrtadr is called in procedure B and B is called from
 * procedure A, the address returned by zrtadr will be the address in A
 * following the call to procedure B.  This can be used to determine which
 * procedure called B.
 *
 * This is a portable stub for the actual routine, which is machine dependent
 * and normally written in assembler.
 */

int 
zrtadr_ (void)
{
	return (0);
}
