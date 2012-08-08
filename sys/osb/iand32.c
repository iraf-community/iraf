
#define	iand32		iand32_

long
iand32 (long *a, long *b)
{
    long  val = 0;
    int   ia = (int) (*a >> 32), ib = (int) *b;

    val = (ia & ib);
    return ((long) val);
}
