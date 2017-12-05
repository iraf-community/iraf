/* Copyright 2006-2009 Chisato Yamauchi (C-SODA/ISAS/JAXA)
 */

#define import_spp
#define import_knames
#include <iraf.h>

/* I64TO32 - Convert big endian 64bit integer array into 32bit.
 */
int
I64TO32 (void *a, void *b, XINT *nelems)
{
	char *ip = (char *)a,
	     *op = (char *)b;
	XINT i;


	/*
	 *  in      |--------|
	 *  out   |----|
	 */
	if ( op <= ip ) {
	    for ( i=0 ; i < *nelems ; i++ ) {
		ip += 4;
		*op = *ip;
		op++; ip++;
		*op = *ip;
		op++; ip++;
		*op = *ip;
		op++; ip++;
		*op = *ip;
		op++; ip++;
	    }
	}
	else {

	    char *ipe = (char *)a + *nelems * 8 - 1;
	    char *ope = (char *)b + *nelems * 4 - 1;
	    
	    /*
	     *  in      |--------|
	     *  out           |----|
	     */
	    if ( ipe <= ope ) {
		for ( i=0 ; i < *nelems ; i++ ) {
		    *ope = *ipe;
		    ope--; ipe--;
		    *ope = *ipe;
		    ope--; ipe--;
		    *ope = *ipe;
		    ope--; ipe--;
		    *ope = *ipe;
		    ope--; ipe--;
		    ipe -= 4;
		}
	    }
	    /*
	     *  in      |--------|
	     *  out       |----|
	     */
	    else {
		
		for ( i=0 ; i < *nelems ; i++ ) {
		    /* --------> */
		    ip += 4;
		    if ( op < ip ) {
			*op = *ip;
			op++; ip++;
			*op = *ip;
			op++; ip++;
			*op = *ip;
			op++; ip++;
			*op = *ip;
			op++; ip++;
		    }
		    else {
			op += 4;
			ip += 4;
		    }
		    /* <-------- */
		    if ( ipe < ope ) {
			*ope = *ipe;
			ope--; ipe--;
			*ope = *ipe;
			ope--; ipe--;
			*ope = *ipe;
			ope--; ipe--;
			*ope = *ipe;
			ope--; ipe--;
		    }
		    else {
			ope -= 4;
			ipe -= 4;
		    }
		    ipe -= 4;
		}
	    }
	}

	return 0;
}
