
#define	imul32		imul32_

int
imul32 (long *a, long *b)
{
    int val = 0, ia = (int) *a, ib = (int) *b;

    val = ia * ib;
    return ((int) val);
}
