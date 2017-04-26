

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "votParse.h"



typedef struct colum {
    char            name[100];
    char            ucd[1000];
}               col_t;

int 
pseudocode1(char *fname)
{
    handle_t        vot, res, data, tab, field, tr, td, fits, stream, tdata,
                    bin;
    int             nrows, ncols, i, l, m, use_direct = 1;
    char           *str, *extnum, *href, *ucd, *name;

    printf("--------------Test 1-------------------------------------------\n");

    col_t          *col;

    vot = vot_openVOTABLE(fname);

    /* Loop over RESOURCES. */
    res = vot_getRESOURCE(vot);

    printf("Table has toplevel %i RESOURCE elements\n",
	   vot_getLength(res));

    while (res) {
	tab = vot_getTABLE(res);

	/* Print column info. */

	/* Get the data element. */
	data = vot_getDATA(tab);

	/* Get data stored as a TABLEDATA XML block. */
	tdata = vot_getTABLEDATA(data);

	col = (col_t *) calloc(vot_getNCols(tdata), sizeof(col_t));

	for (field = vot_getFIELD(tab), i = 0; field; field = vot_getNext(field), i++) {
	    name = vot_getAttr(field, "name");
	    ucd = vot_getAttr(field, "ucd");

	    if (name != NULL)
		strcpy(col[i].name, name);

	    if (ucd != NULL)
		strcpy(col[i].ucd, ucd);

	    printf("%s, ", col[i].name);
	}

	printf("\n");

	switch (vot_getDATAType(data)) {
	case TY_TABLEDATA:

	    if (use_direct) {
		/* Get the table data cells by direct index. */
		tr = vot_getTR(tdata);
		nrows = vot_getLength(tr);
		ncols = vot_getLength(vot_getTD(tr));

		for (l = 0; l < nrows; l++) {
		    for (m = 0; m < ncols; m++) {
			str = vot_getTableCell(tdata, l, m);
			printf("%s\n", str);
		    }

		    printf("--\n");
		}
	    } else {
		/* Get the table data cells by looping over rows/cols. */
		for (tr = vot_getTR(tdata); tr; tr = vot_getNext(tr)) {
		    for (td = vot_getTD(tr); td; td = vot_getNext(td)) {
			str = vot_getValue(td);
			printf("%s\n", str);
		    }
		}
	    }

	    break;

	case TY_BINARY:
	    /*
	     * Get data stored as inline binary. If the encoding of the
	     * stream is based64 read the sequence of bytes and decode.
	     */

	    bin = vot_getBINARY(data);
	    stream = vot_getSTREAM(bin);

	    if (strcasecmp("base64", vot_getAttr(stream, "encoding")) == 0)
		str = vot_getValue(stream);
	    printf("%s\n", str);

	    break;

	case TY_FITS:
	    /*
	     * Read FITS data. Assumes a particular extension of an MEF is
	     * avaliable for download at the given href.
	     */
	    fits = vot_getFITS(data);
	    extnum = vot_getAttr(fits, "extnum");

	    stream = vot_getSTREAM(fits);
	    href = vot_getAttr(stream, "href");

	    /* Download the FITS file. */

	    break;

	default:
	    printf("Error:  Invalid table DATA type.\n");
	}

	res = vot_getNext(res);
    }


    printf("--------------\\Test 1-----------------------------------------\n");

    return (0);

}

int 
pseudocode2(char *fname)
{
    handle_t        vot, res, p;

    printf("--------------Test 2-------------------------------------------\n");
    vot = vot_openVOTABLE(fname);

    res = vot_getRESOURCE(vot);

    for (p = vot_getChild(res); p; p = vot_getSibling(p)) {
	if (vot_typeOf(p) == TY_PARAM)
	    printf("PARAM name=%s value=%s\n",
		   vot_getAttr(p, "name"), vot_getAttr(p, "value"));
    }

    printf("\n");
    res = vot_getRESOURCE(vot);

    for (p = vot_getPARAM(res); p; p = vot_getNext(p)) {
	printf("PARAM name=%s value=%s\n",
	       vot_getAttr(p, "name"), vot_getAttr(p, "value"));
    }


    printf("------------\\Test 2-------------------------------------------\n");

    return (0);
}

int 
pseudocode3(char *fname)
{
    handle_t        vot, res, param, info;

    printf("--------------Test 3-------------------------------------------\n");
    vot = vot_openVOTABLE(fname);

    if ((info = vot_getINFO(vot))) {
	if (strcasecmp(vot_getAttr(info, "name"), "error") == 0)
	    printf("ERROR: VALUE=%s\n", vot_getAttr(info, "value"));
    } else {
	printf("File OK.\n");
    }


    vot = vot_openVOTABLE(fname);
    res = vot_getRESOURCE(vot);
    param = vot_getPARAM(res);
    info = vot_getINFO(res);

    if ((param) && strcasecmp(vot_getAttr(param, "name"), "error") == 0) {
	/* SCS alternate method where PARAN defines value the error string. */
	printf("ERROR: VALUE=%s\n", vot_getAttr(param, "value"));
    } else if ((info) && strcasecmp(vot_getAttr(info, "name"), "QUERY_STATUS") == 0) {
	/*
	 * All-other DAL methods where PARAM and INFO of the RESOURCE defines
	 * a QUERY_STATUS of the result.
	 */
	if (strcasecmp(vot_getAttr(info, "value"), "OK") == 0)
	    printf("FILE OK.\n");
	else
	    printf("ERROR: Value=%s\n", vot_getValue(info));
    } else
	printf("FILE OK.\n");


    printf("------------\\Test 3-------------------------------------------\n");
    return (0);
}

int 
pseudocode4(void)
{
    handle_t        vot, res, data, tab, f, tr, td, tdata, desc, info;
    int             nrows = 5, ncols = 10, i, j;
    char            colname[50];
    char          **data_m;
    char          **ip;
    char           *tmpstr;

    printf("--------------Test 4-------------------------------------------\n");

    data_m = (char **) calloc((nrows * ncols), sizeof(char *));

    ip = data_m;

    for (i = 0; i < nrows; i++) {
	for (j = 0; j < ncols; j++) {
	    tmpstr = (char *) calloc(30, sizeof(char));
	    sprintf(tmpstr, "row: %i, col: %i", i, j);
	    *ip++ = tmpstr;
	}
    }


    vot = vot_openVOTABLE(NULL);

    res = vot_newNode(vot, TY_RESOURCE);
    vot_setAttr(res, "id", "newtable");

    desc = vot_newNode(vot, TY_DESCRIPTION);
    vot_setValue(desc, "This is a test description.");

    tab = vot_newNode(res, TY_TABLE);

    for (i = 0; i < 10; i++) {
	f = vot_newNode(tab, TY_FIELD);
	sprintf(colname, "col%d", i);
	vot_setAttr(f, "name", colname);
	vot_setAttr(f, "id", colname);
    }

    data = vot_newNode(tab, TY_DATA);
    tdata = vot_newNode(data, TY_TABLEDATA);

    for (i = 0; i < nrows; i++) {
	tr = vot_newNode(tdata, TY_TR);
	for (j = 0; j < ncols; j++) {
	    td = vot_newNode(tr, TY_TD);
	    vot_setValue(td, (char *) data_m[(i * ncols) + j]);
	}
    }

    info = vot_newNode(tab, TY_INFO);
    vot_setAttr(info, "id", "STATUS");
    vot_setAttr(info, "value", "OK");

    vot_writeVOTable(vot, "stdout", 1);


    printf("------------\\Test 4-------------------------------------------\n");

    return (0);
}

int 
pseudocode5_copy(char *fname1, char *fname2)
{
    handle_t        vot2, res2, cres2;
    handle_t        vot1, res1, cres1;
    handle_t        vot3;

    printf("--------------Test 5-------------------------------------------\n");

    vot1 = vot_openVOTABLE(fname1);
    printf("Handles: %i\n", vot_handleCount());
    vot2 = vot_openVOTABLE(fname2);
    printf("Handles: %i\n", vot_handleCount());

    res1 = vot_getRESOURCE(vot1);
    res2 = vot_getRESOURCE(vot2);

    cres1 = vot_copyElement(res1, 0);
    cres2 = vot_copyElement(res2, 0);
    printf("Handles: %i\n", vot_handleCount());

    vot3 = vot_openVOTABLE(NULL);

    vot_attachNode(vot3, cres1);
    vot_attachNode(vot3, cres2);

    vot_writeVOTable(vot3, "stdout", 1);

    printf("Handles: %i\n", vot_handleCount());
    vot_closeVOTABLE(vot1);
    vot_closeVOTABLE(vot2);
    vot_closeVOTABLE(vot3);
    vot_closeVOTABLE(cres2);
    vot_closeVOTABLE(cres1);
    printf("Handles: %i\n", vot_handleCount());
    vot_deleteNode(cres2);
    vot_deleteNode(cres1);
    printf("Handles: %i\n", vot_handleCount());
    vot_writeVOTable(vot3, "stdout", 1);

    printf("------------\\Test 5-------------------------------------------\n");
    return (0);
}

int 
pseudocode5(char *fname1, char *fname2)
{
    handle_t        vot2, res2;
    handle_t        vot1, res1;
    handle_t        vot3;

    printf("--------------Test 5-------------------------------------------\n");

    vot1 = vot_openVOTABLE(fname1);
    vot2 = vot_openVOTABLE(fname2);

    res1 = vot_getRESOURCE(vot1);
    res2 = vot_getRESOURCE(vot2);

    vot3 = vot_openVOTABLE(NULL);

    vot_attachNode(vot3, res1);
    vot_attachNode(vot3, res2);

    vot_writeVOTable(vot3, "stdout", 1);
    printf("Handles: %i\n", vot_handleCount());
    vot_closeVOTABLE(vot1);
    printf("Handles: %i\n", vot_handleCount());
    vot_closeVOTABLE(vot2);
    printf("Handles: %i\n", vot_handleCount());
    vot_closeVOTABLE(vot3);
    printf("Handles: %i\n", vot_handleCount());
    vot_writeVOTable(vot3, "stdout", 1);

    printf("------------\\Test 5-------------------------------------------\n");
    return (0);
}

int 
pseudocode6(void)
{

    printf("--------------Test 6-------------------------------------------\n");
    printf("------------\\Test 6-------------------------------------------\n");
    return (0);
}

void 
copy_test(char *fname)
{
    handle_t        vot, copy;

    printf("--------------Copy Test-------------------------------------------\n");
    vot = vot_openVOTABLE(fname);

    /*
    vot_writeVOTable(vot, "stdout", 1);
    */

    printf("Handles: %i\n", vot_handleCount());


    copy = vot_copyElement(vot, 0);


    /*
    vot_writeVOTable(copy, "stdout", 1);
    */

    printf("Handles: %i\n", vot_handleCount());

    vot_closeVOTABLE(copy);

    printf("Handles: %i\n", vot_handleCount());



    vot_closeVOTABLE(vot);

    printf("Handles: %i\n", vot_handleCount());

    printf("------------\\Copy Test-------------------------------------------\n");

}



int
main(int argc, char **argv)
{
    char           *fname = "zztest.xml";
    char           *fname2 = "zzdummy.xml";


    printf("Hello World!\n");


    /*
    */
    pseudocode1(fname);
    pseudocode2(fname);
    pseudocode3(fname);
    pseudocode4();

    pseudocode5(fname, fname2);
    pseudocode5_copy(fname, fname2);

    pseudocode6();

    copy_test(fname);


    return (0);
}
