/**
 *  VOTPARSE.C -- Public interface procedures for the libVOTable parser.
 *
 *  @file       votParse.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      Public interface procedures for the libVOTable parser.
 */

#include <stdio.h>
#include <stdlib.h>
#define _GNU_SOURCE
#include <string.h>
#include <expat.h>
#include <unistd.h>
#include <assert.h>
#include <ctype.h>
#include <sys/stat.h>

#include <curl/curl.h>
#include <curl/easy.h>

#include "votParseP.h"
#include "votParse.h"
#ifdef HAVE_CFITSIO
#include "fitsio.h"
#endif


#define	BUFSIZE			4096

    
extern char  *strcasestr();


/* Private procedures
 */
static Element *vot_elementDup (handle_t element_h);
static handle_t vot_nodeCreate (int type);
static char    *vot_deWS (char *in);
#ifdef USE_VALIDITY
static int      vot_validParents (int type);
static int      vot_validChildren (int type);
#endif

static void     vot_attachToNode (handle_t parent, handle_t new);
static void     vot_attachSibling (handle_t big_brother, handle_t new);
static void 	vot_dumpXML (Element *node, int level, int indent, FILE *fd);

static void 	vot_htmlHeader (FILE *fd, char *fname);
static void 	vot_htmlTableMeta (FILE *fd, handle_t res, char *ifname);
static void 	vot_htmlTableData (FILE *fd, handle_t res, char *ifname);
static void 	vot_htmlFooter (FILE *fd);

static int 	vot_simpleGetURL (char *url, char *ofname);

#ifdef USE_DEBUG
static void 	vot_printData (Element *tdata);
static void 	votBob (void);
#endif


/** *************************************************************************
 *  Public Interface
 *
 *	    vot = vot_openVOTABLE (filename|str|NULL)
 *	         vot_closeVOTABLE (vot)
 *
 *           res = vot_getRESOURCE  (vot|res)
 *              tab = vot_getTABLE  (res)
 *            field = vot_getFIELD  (tab)
 *
 *              data = vot_getDATA  (tab)
 *
 *        tdata = vot_getTABLEDATA  (data)              // data elements
 *                  tr = vot_getTR  (tdata)
 *                  td = vot_getTD  (tr)
 *             bin = vot_getBINARY  (data)
 *            bin = vot_getBINARY2  (data)
 *              fits = vot_getFITS  (data)
 *
 *            group = vot_getGROUP  (vot|res|tab|group)
 *        fldref = vot_getFIELDRef  (group)
 *        parref = vot_getPARAMRef  (group)
 *
 *       desc = vot_getDESCRIPTION  (handle)
 *            param = vot_getPARAM  (handle)
 *              info = vot_getINFO  (handle)
 *          stream = vot_getSTREAM  (bin|fits)
 *
 *             val = vot_getVALUES  (field|param|info)
 *                min = vot_getMIN  (val)
 *                max = vot_getMAX  (val)
 *             opt = vot_getOPTION  (val)
 *
 *              link = vot_getLINK  (res|info|param|field|table)
 *
 *             sys = vot_getCOOSYS  (vot)       	// Compatability
 *
 *
 *             h = vot_newRESOURCE  (handle_t parent_h)
 *                h = vot_newTABLE  (handle_t parent_h)
 *                h = vot_newFIELD  (handle_t parent_h)
 *                 h = vot_newDATA  (handle_t parent_h)
 *            h = vot_newTABLEDATA  (handle_t parent_h)
 *                   h = vot_newTR  (handle_t parent_h)
 *                   h = vot_newTD  (handle_t parent_h)
 *               h = vot_newBINARY  (handle_t parent_h)
 *              h = vot_newBINARY2  (handle_t parent_h)
 *                 h = vot_newFITS  (handle_t parent_h)
 *                h = vot_newGROUP  (handle_t parent_h)
 *             h = vot_newFIELDRef  (handle_t parent_h)
 *             h = vot_newPARAMRef  (handle_t parent_h)
 *          h = vot_newDESCRIPTION  (handle_t parent_h)
 *                h = vot_newPARAM  (handle_t parent_h)
 *                 h = vot_newINFO  (handle_t parent_h)
 *               h = vot_newSTREAM  (handle_t parent_h)
 *               h = vot_newVALUES  (handle_t parent_h)
 *                  h = vot_newMIN  (handle_t parent_h)
 *                  h = vot_newMAX  (handle_t parent_h)
 *               h = vot_newOPTION  (handle_t parent_h)
 *                 h = vot_newLINK  (handle_t parent_h)
 *               h = vot_newCOOSYS  (handle_t parent_h)
 *
 *
 *            handle = vot_newNode  (parent, type)
 *                    vot_freeNode  (node)
 *                  vot_deleteNode  (node)
 *                  vot_attachNode  (parent, new)
 *        handle = vot_copyElement  (handle_t source_h, handle_t parent_h)
 *
 *
 *             val =  vot_getValue  (handle)
 *             stat = vot_setValue  (handle, value)
 *
 *             attr =  vot_getAttr  (handle, attr)
 *              stat = vot_setAttr  (handle, attr, value)
 *
 *
 *          type = vot_getDATAType  (data)		// Utilities
 * typeStr = vot_getDATATypeString  (data)
 *
 *               nc = vot_getNCols  (tdata_h)
 *               nr = vot_getNRows  (tdata_h)
 *          val = vot_getTableCell  (tdata_h, row, col)
 *            stat = vot_sortTable  (tdata_h, col, string_sort, sort_order)
 *
 *             len = vot_getLength  (elem_h)
 *             N = vot_getNumberOf  (elem_h, type)
 *
 *             col = vot_colByName  (tab_h, name, alt)
 *              col = vot_colByUCD  (tab_h, name, alt)
 *               col = vot_colByID  (tab_h, name, alt)
 *
 *         handle = vot_findByAttr  (parent, name, value)
 *         handle *vot_findInGroup  (group, type)
 *            handle = vot_getNext  (handle)
 *         handle = vot_getSibling  (handle)
 *           handle = vot_getChild  (handle)
 *          handle = vot_getParent  (handle)
 *     handle = vot_getChildOfType  (handle, int type)
 *
 *               int = vot_valueOf  (handle)
 *               type = vot_typeOf  (handle)
 *                 vot_setWarnings  (value)
 *
 *                vot_writeVOTable  (handle, char *fname, int indent)
 *                   vot_writeHTML  (handle, char *fname)
 *                  vot_writeSHTML  (handle, char *fname)
 *                   vot_writeFITS  (handle, char *fname)
 *                    vot_writeASV  (handle, char *fname, int hdr)
 *                    vot_writeBSV  (handle, char *fname, int hdr)
 *                    vot_writeCSV  (handle, char *fname, int hdr)
 *                    vot_writeTSV  (handle, char *fname, int hdr)
 *	        vot_writeDelimited  (handle, char *fname, char delim, int hdr)
 *
 *
 ** *************************************************************************/


Stack   *element_stack  = NULL; /*  This holds a stack of elements. Should be 
				 *  empty most of the time. 
 				 */

Element *vot_struct 	= NULL; /*  This will hold all the VOTs.  The first 
				 *  Element in this structure is a ROOT Element.
				 */

char	*votELevel	= "";	/*  Error Message Level
				 */

int	 votWarn	= 0;	/*  Warn about parsing issues.  Values:
				 *	0    No messages (lax parsing)
				 *	1    Warning messages
				 *	2    Strict parsing
				 */



/** 
 *  vot_openVOTABLE -- Parse a VOTable and return a handle to it
 *
 *  @brief  Parse a VOTable and return a handle to it
 *  @fn     handle_t vot_openVOTABLE (char *arg)
 *
 *  @param  arg 	The source of the table
 *  @return	 	The root node handle of the VOTable
 */
handle_t
vot_openVOTABLE (char *arg)
{
    FILE    *fd = (FILE *) NULL;
    Element *my_element;
    char     buf[BUFSIZE], *ip, urlFname[BUFSIZE];
    size_t   len, nleft = 0, fsize = -1, nread = 0;
    int      done, ret_handle, nerrs;
    XML_Parser parser;

    struct   stat st;

    
    memset (buf, 0, BUFSIZE);
    memset (urlFname, 0, BUFSIZE);


    vot_newHandleTable ();		/* initialize the handle table	*/
    if (element_stack == NULL)
        element_stack = vot_newStack ();
    
    if (vot_struct == NULL)
        vot_struct = vot_newElem (TY_ROOT);
    
    votPush (element_stack, vot_struct);
    
    if (arg == NULL) {
        my_element = vot_newElem (TY_VOTABLE);
        
        if (vot_struct->child)
            vot_struct->last_child->next = my_element;
        else
            vot_struct->child = my_element;
            
        vot_struct->last_child = my_element;
            
        vot_clearStack (element_stack);
            
        my_element->parent = vot_struct;
            
        return (vot_setHandle (my_element));

    } else if (strncmp (arg, "http://", 7) == 0) { /* input from URL	*/
	extern int vot_getURL (char *url, char *fname, int *nbytes);
	int  tfd = 0;

	/*  Open a temp file for the downloaded URL.
	 */
	strcpy (urlFname, "/tmp/votXXXXXX");
	if ((tfd = mkstemp (urlFname) < 0))
	    strcpy (urlFname, "/tmp/votquery");
	close (tfd);

	nerrs = vot_simpleGetURL (arg, urlFname);
        if ( !(fd = fopen (urlFname, "r")) ) {
            fprintf (stderr, "Unable to open url '%s'\n", arg);
            return (0);			/* cannot open file error	*/
        }
	fstat (fileno(fd), &st);
        fsize	= (size_t) st.st_size;

    } else if (strcmp (arg, "-") == 0 || strncasecmp (arg, "stdin", 5) == 0) {
	/* input from stdin	*/
        fd = stdin;
        fsize = -1;

    } else if (strncmp (arg, "file://", 7) == 0) { /* input from URL	*/
        len = strlen (&arg[7]);
        if (!(fd = fopen (&arg[7], "r"))) {
            fprintf (stderr, "Unable to open input file '%s'\n", &arg[7]);
            return (0);			/* cannot open file error	*/
        }
	fstat (fileno(fd), &st);
        fsize = (size_t) st.st_size;

    } else if (access (arg, R_OK) == 0) { 	   /* input from file 	*/ 
        len = strlen (arg);
        if (!(fd = fopen (arg, "r"))) {
            fprintf (stderr, "Unable to open input file '%s'\n", arg);
            return (0);			/* cannot open file error	*/
        }
	fstat (fileno(fd), &st);
        fsize = (size_t) st.st_size;

    } else if (strcasestr (arg, "votable")) {
        /*  input argument is XML string */
	ip = arg;
	len = strlen (arg);

    } else {
        fprintf (stderr, "openVOTable(): Invalid input arg '%s'\n", arg);
	return (-1);
    }

  
    /*  Create the parser and set the input handlers.
    */
    parser = XML_ParserCreate (NULL);
    XML_SetElementHandler (parser, vot_startElement, vot_endElement);
    XML_SetCdataSectionHandler (parser, vot_startCData, vot_endCData);
    XML_SetCharacterDataHandler (parser, vot_charData);
    
    ip    	= arg;			/* initialize		*/
    done  	= 0;
    nleft 	= fsize;
    nread 	= 0;
    
    if (fd) {
        do {
	    memset (buf, 0, BUFSIZE);
            len = fread (buf, 1, sizeof(buf), fd);

	    if (nread == 0) {
		/*  Check that this actually is a VOTable.
		 */
		if (strcasestr (buf, "<votable") == (char *) NULL) {
    		    if (fd != stdin) fclose (fd);
    		    if (urlFname[0]) unlink (urlFname);
		    return (-1);	/* not a votable */
		}
	    }
	    nread += len;

	    if (fd != stdin) {
	        nleft -= len;
                done = nread >= fsize;
	    } else
		done = (len == 0 ? feof (stdin) : 0);
	    

	    if (done && buf[len-1] == '\0')	/* trim trailing null	*/
		while (len && !buf[len-1])
		    len--;
	    if (done && buf[len-1] != '\n')	/* no newline on file 	*/
		buf[len] = '\n';

            if (!XML_Parse (parser, buf, len, done)) {
                fprintf (stderr, "Error: %s at line %d\n",
                    XML_ErrorString (XML_GetErrorCode (parser)),
                    (int)XML_GetCurrentLineNumber (parser));
                return (0);		/* parse error			*/
            }
        } while (!done);

    } else {
        if (!XML_Parse (parser, ip, len, 1)) {
            fprintf (stderr, "Error: %s at line %d\n",
                XML_ErrorString (XML_GetErrorCode (parser)),
                (int)XML_GetCurrentLineNumber (parser));
            return (0);	/* parse error			*/
        }
    }
    XML_ParserFree (parser);

    if (fd && fd != stdin) 
        fclose (fd);
    if (urlFname[0])
	unlink (urlFname);

    vot_clearStack (element_stack);
    
    ret_handle = vot_lookupHandle (vot_struct->last_child);
    
    return (ret_handle);
}


/** 
 *  vot_closeVOTABLE -- Destroy the root node and all of it's children.
 *
 *  @brief  Destroy the root node and all of it's children.
 *  @fn     vot_closeVOTABLE (handle_t vot)
 *
 *  @param  vot 	A handle to the Element that you want deleted
 *  @return		nothing
 *
 *  @warning Destroys the node and all of it's children.
 */
void
vot_closeVOTABLE (handle_t vot)
{
    Element *elem = vot_getElement (vot);
    int my_type = vot_elemType (elem);

    
    if ((my_type != TY_VOTABLE)) {
	votEmsg ("closeVOTABLE() arg must be a VOTABLE tag\n");
        return;
    }
    vot_deleteNode (vot);
}




/*****************************************************************************
 *  Routines to get nodes of a VOTable as a handle.
 ****************************************************************************/

/** 
 *  vot_getRESOURCE -- Gets the RESOURCE node from the parent handle
 *
 *  @brief  Gets the RESOURCE node from the parent handle
 *  @fn     handle_t vot_getRESOURCE (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a RESOURCE
 *  @return 		A handle to the first RESOURCE node, or zero
 */
handle_t
vot_getRESOURCE (handle_t handle)
{
    Element *elem, *child;
    int my_type;
    

    /* Get Element pointer for the element. 
    */
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    /* Make sure it's a node that can have a RESOURCE. 
    */
    if ((my_type != TY_VOTABLE) && (my_type != TY_RESOURCE)) {
        votEmsg ("RESOURCE must be child of a RESOURCE or VOTABLE tag\n");
        return (0);
    }

    /* Go through children until a RESOURCE is found. 
    */
    for (child = elem->child; child; child = child->next)
         if (child->type == TY_RESOURCE)
             break;
    
    /* If nothing found return 0, if found return handle to found Element. 
    */
    return (vot_lookupHandle (child));
}


/**
 *  vot_getTABLE -- Gets the TABLE node from the parent handle
 *
 *  @brief  Gets the TABLE node from the parent handle
 *  @fn     handle_t vot_getTABLE (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a TABLE
 *  @return 		A handle to the first TABLE node, or zero
 */
handle_t
vot_getTABLE (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_RESOURCE) {
        votEmsg ("TABLE must be child of a RESOURCE tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_TABLE)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getFIELD -- Gets the FIELD node from the parent handle
 *
 *  @brief  Gets the FIELD node from the parent handle
 *  @fn     handle_t vot_getFIELD (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a FIELD
 *  @return 		A handle to the first FIELD node, or zero
 */
handle_t
vot_getFIELD (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_TABLE) {
        votEmsg ("FIELD must be child of a TABLE tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_FIELD)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getDATA -- Gets the DATA node from the parent handle
 *
 *  @brief  Gets the DATA node from the parent handle
 *  @fn     handle_t vot_getDATA (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a DATA
 *  @return 		A handle to the first DATA node, or zero
 */
handle_t
vot_getDATA (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_TABLE) {
        votEmsg ("DATA must be child of a TABLE tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_DATA)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getTABLEDATA -- Gets the TABLEDATA node from the parent handle
 *
 *  @brief  Gets the TABLEDATA node from the parent handle
 *  @fn     handle_t vot_getTABLEDATA (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a TABLEDATA
 *  @return 		A handle to the first TABLEDATA node, or zero
 */
handle_t
vot_getTABLEDATA (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_DATA) {
        votEmsg ("TABLEDATA must be child of a DATA tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_TABLEDATA)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getTR -- Gets the TR node from the parent handle
 *
 *  @brief  Gets the TR node from the parent handle
 *  @fn     handle_t vot_getTR (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a TR
 *  @return 		A handle to the first TR node, or zero
 */
handle_t
vot_getTR (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_TABLEDATA) {
        votEmsg ("TR must be child of a TABLEDATA tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_TR)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getTD -- Gets the TD node from the parent handle
 *
 *  @brief  Gets the TD node from the parent handle
 *  @fn     handle_t vot_getTD (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a TD
 *  @return 		A handle to the first TD node, or zero
 */
handle_t
vot_getTD (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_TR) {
        votEmsg ("TD must be child of a TR tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_TD)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getBINARY -- Gets the BINARY node from the parent handle
 *
 *  @brief  Gets the BINARY node from the parent handle
 *  @fn     handle_t vot_getBINARY (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a BINARY
 *  @return 		A handle to the first BINARY node, or zero
 */
handle_t
vot_getBINARY (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_DATA) {
        votEmsg ("BINARY must be child of a DATA tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_BINARY || child->type == TY_BINARY2)
            break;
         
    return (vot_lookupHandle (child));
}


/**
 *  vot_getBINARY2 -- Gets the BINARY2 node from the parent handle
 *
 *  @brief  Gets the BINARY2 node from the parent handle
 *  @fn     handle_t vot_getBINARY2 (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a BINARY2
 *  @return 		A handle to the first BINARY2 node, or zero
 */
handle_t
vot_getBINARY2 (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_DATA) {
        votEmsg ("BINARY2 must be child of a DATA tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_BINARY2)
            break;
         
    return (vot_lookupHandle (child));
}


/**
 *  vot_getFITS -- Gets the FITS node from the parent handle
 *
 *  @brief  Gets the FITS node from the parent handle
 *  @fn     handle_t vot_getFITS (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a FITS
 *  @return 		A handle to the first FITS node, or zero
 */
handle_t
vot_getFITS (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_DATA) {
        votEmsg ("FITS must be child of a DATA tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_FITS)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getGROUP -- Gets the GROUP node from the parent handle
 *
 *  @brief  Gets the GROUP node from the parent handle
 *  @fn     handle_t vot_getGROUP (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a GROUP
 *  @return 		A handle to the first GROUP node, or zero
 */
handle_t
vot_getGROUP (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if ((my_type != TY_VOTABLE) && (my_type != TY_RESOURCE) && 
        (my_type != TY_TABLE) && (my_type != TY_GROUP)) {
            votEmsg (
		"GROUP must be child of a RESOURCE, TABLE, GROUP or VOTABLE\n");
            return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_GROUP)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getFIELDref -- Gets the FIELDref node from the parent handle
 *
 *  @brief  Gets the FIELDref node from the parent handle
 *  @fn     handle_t vot_getFIELDRef (handle_t handle)
 *
 *  @param  handle 	Parent handle containing a FIELDref
 *  @return 		A handle to the first FIELDref node, or zero
 */
handle_t
vot_getFIELDRef (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_GROUP) {
        votEmsg ("FIELDref must be child of a GROUP tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_FIELDREF)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getPARAMRef -- Gets the PARAMref node from the parent handle
 *
 *  @brief  Gets the PARAMRef node from the parent handle
 *  @fn     handle_t vot_getPARAMRef (handle_t handle)
 *
 *  @param  handle      Parent handle containing a PARAMRef
 *  @return             A handle to the first PARAMRef node, or zero
 */
handle_t
vot_getPARAMRef (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_GROUP) {
        votEmsg ("PARAMref must be child of a GROUP tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_PARAMREF)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getDESCRIPTION -- Gets the DESCRIPTION node from the parent handle
 *
 *  @brief  Gets the DESCRIPTION node from the parent handle
 *  @fn     handle_t vot_getDESCRIPTION (handle_t handle)
 *
 *  @param  handle      Parent handle containing a DESCRIPTION
 *  @return             A handle to the first DESCRIPTION node, or zero
 */
handle_t
vot_getDESCRIPTION (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    
    elem = vot_getElement (handle);
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_DESCRIPTION)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getPARAM -- Gets the PARAM node from the parent handle
 *
 *  @brief  Gets the PARAM node from the parent handle
 *  @fn     handle_t vot_getPARAM (handle_t handle)
 *
 *  @param  handle      Parent handle containing a PARAM
 *  @return             A handle to the first PARAM node, or zero
 */
handle_t
vot_getPARAM (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
     
    elem = vot_getElement (handle);
    
    for (child = elem->child; child; child = child->next)
         if (child->type == TY_PARAM)
             break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getINFO -- Gets the INFO node from the parent handle
 *
 *  @brief  Gets the INFO node from the parent handle
 *  @fn     handle_t vot_getINFO (handle_t handle)
 *
 *  @param  handle      Parent handle containing a INFO
 *  @return             A handle to the first INFO node, or zero
 */
handle_t
vot_getINFO (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    
    elem = vot_getElement (handle);
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_INFO)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getSTREAM -- Gets the STREAM node from the parent handle
 *
 *  @brief  Gets the STREAM node from the parent handle
 *  @fn     handle_t vot_getSTREAM (handle_t handle)
 *
 *  @param  handle      Parent handle containing a STREAM
 *  @return             A handle to the first STREAM node, or zero
 */
handle_t
vot_getSTREAM (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if ((my_type != TY_BINARY) && 
	(my_type != TY_BINARY2) && 
	(my_type != TY_FITS)) {
            votEmsg ("STREAM must be child of a BINARY(2) or FITS tag\n");
            return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_STREAM)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getVALUES -- Gets the VALUES node from the parent handle
 *
 *  @brief  Gets the VALUES node from the parent handle
 *  @fn     handle_t vot_getVALUES (handle_t handle)
 *
 *  @param  handle      Parent handle containing a VALUES
 *  @return             A handle to the first VALUES node, or zero
 */
handle_t
vot_getVALUES (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if ((my_type != TY_FIELD) && (my_type != TY_PARAM) && 
       (my_type != TY_INFO)) {
           votEmsg ("VALUES must be child of a FIELD, PARAM or INFO tag\n");
           return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_VALUES)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getMIN -- Gets the MIN node from the parent handle
 *
 *  @brief  Gets the MIN node from the parent handle
 *  @fn     handle_t vot_getMIN (handle_t handle)
 *
 *  @param  handle      Parent handle containing a MIN
 *  @return             A handle to the first MIN node, or zero
 */
handle_t
vot_getMIN (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_VALUES) {
        votEmsg ("MIN must be child of a VALUES tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_MIN)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getMAX -- Gets the MAX node from the parent handle
 *
 *  @brief  Gets the MAX node from the parent handle
 *  @fn     handle_t vot_getMAX (handle_t handle)
 *
 *  @param  handle      Parent handle containing a MAX
 *  @return             A handle to the first MAX node, or zero
 */
handle_t
vot_getMAX (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_VALUES) {
        votEmsg ("MAX must be child of a VALUES tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_MAX)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getOPTION -- Gets the OPTION node from the parent handle
 *
 *  @brief  Gets the OPTION node from the parent handle
 *  @fn     handle_t vot_getOPTION (handle_t handle)
 *
 *  @param  handle      Parent handle containing a OPTION
 *  @return             A handle to the first OPTION node, or zero
 */
handle_t
vot_getOPTION (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if (my_type != TY_VALUES) {
        votEmsg ("OPTION must be child of a VALUES tag\n");
        return (0);
    }
    
    for (child = elem->child; child; child = child->next)
        if (child->type == TY_OPTION)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getLINK -- Gets the LINK node from the parent handle
 *
 *  @brief  Gets the LINK node from the parent handle
 *  @fn     handle_t vot_getLINK (handle_t handle)
 *
 *  @param  handle      Parent handle containing a LINK
 *  @return             A handle to the first LINK node, or zero
 */
handle_t
vot_getLINK (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if ((my_type != TY_RESOURCE) && (my_type != TY_TABLE) && 
       (my_type != TY_FIELD) && (my_type != TY_PARAM) && 
       (my_type != TY_INFO)) {
          votEmsg (
	     "LINK must be child of a RESOURCE, TABLE, FIELD, PARAM or INFO\n");
        return (0);
    }
    
    for (child=elem->child; child; child = child->next)
        if (child->type == TY_LINK)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getCOOSYS -- Gets the COOSYS node from the parent handle
 *
 *  @brief  Gets the COOSYS node from the parent handle
 *  @fn     handle_t vot_getCOOSYS (handle_t handle)
 *
 *  @param  handle      Parent handle containing a COOSYS
 *  @return             A handle to the first COOSYS node, or zero
 */
handle_t
vot_getCOOSYS (handle_t handle)
{
    /* Refer to vot_getRESOURCE for detailed comments on function workings. */
    
    Element *elem, *child;
    int my_type;
    
    elem = vot_getElement (handle);
    my_type = vot_elemType (elem);
    
    if ((my_type != TY_VOTABLE) && (my_type != TY_RESOURCE)) {
        votEmsg ("COOSYS must be child of a RESOURCE or VOTABLE tag\n");
        return (0);
    }
    
    for (child=elem->child; child; child = child->next)
        if (child->type == TY_COOSYS)
            break;
    
    return (vot_lookupHandle (child));
}


/**
 *  vot_getDATAType -- Returns the type of the DATA element.
 * 
 *  @brief  Returns the type of the DATA element.
 *  @fn     char *vot_getDATAType (handle_t data_h)
 *
 *  @param  data_h 	A handle_t to a DATA
 *  @return	 	The type as an int
 */
int
vot_getDATAType (handle_t data_h)
{
    Element *elem = vot_getElement (data_h);
    
    return (elem->child->type);
}


/**
 *  vot_getDATATypeString -- Returns the type of the DATA element as a string.
 * 
 *  @brief  Returns the type of the DATA element as a string.
 *  @fn     char *vot_getDATATypeString (handle_t data_h)
 *
 *  @param  data_h 	A handle_t to a DATA
 *  @return	 	The type as an string
 */
char *
vot_getDATATypeString (handle_t data_h)
{
    Element *elem = vot_getElement (data_h);
    
    return ((elem ? vot_elemName (elem->child) : "none"));
}




/*****************************************************************************
 *  Routines to create new nodes of a VOTable as a handle.
 ****************************************************************************/

/** 
 *  vot_newRESOURCE -- Create new RESOURCE node under the parent handle
 *
 *  @brief  Create new RESOURCE node under the parent handle
 *  @fn     handle_t vot_newRESOURCE (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a RESOURCE
 *  @return 		A handle to the first RESOURCE node, or zero
 */
handle_t
vot_newRESOURCE (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_RESOURCE));
}


/**
 *  vot_newTABLE -- Create new TABLE node under the parent handle
 *
 *  @brief  Create new TABLE node under the parent handle
 *  @fn     handle_t vot_newTABLE (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a TABLE
 *  @return 		A handle to the first TABLE node, or zero
 */
handle_t
vot_newTABLE (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_TABLE));
}


/**
 *  vot_newFIELD -- Create new FIELD node under the parent handle
 *
 *  @brief  Create new FIELD node under the parent handle
 *  @fn     handle_t vot_newFIELD (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a FIELD
 *  @return 		A handle to the first FIELD node, or zero
 */
handle_t
vot_newFIELD (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_FIELD));
}


/**
 *  vot_newDATA -- Create new DATA node under the parent handle
 *
 *  @brief  Create new DATA node under the parent handle
 *  @fn     handle_t vot_newDATA (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a DATA
 *  @return 		A handle to the first DATA node, or zero
 */
handle_t
vot_newDATA (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_DATA));
}


/**
 *  vot_newTABLEDATA -- Create new TABLEDATA node under the parent handle
 *
 *  @brief  Create new TABLEDATA node under the parent handle
 *  @fn     handle_t vot_newTABLEDATA (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a TABLEDATA
 *  @return 		A handle to the first TABLEDATA node, or zero
 */
handle_t
vot_newTABLEDATA (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_TABLEDATA));
}


/**
 *  vot_newTR -- Create new TR node under the parent handle
 *
 *  @brief  Create new TR node under the parent handle
 *  @fn     handle_t vot_newTR (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a TR
 *  @return 		A handle to the first TR node, or zero
 */
handle_t
vot_newTR (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_TR));
}


/**
 *  vot_newTD -- Create new TD node under the parent handle
 *
 *  @brief  Create new TD node under the parent handle
 *  @fn     handle_t vot_newTD (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a TD
 *  @return 		A handle to the first TD node, or zero
 */
handle_t
vot_newTD (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_TD));
}


/**
 *  vot_newBINARY -- Create new BINARY node under the parent handle
 *
 *  @brief  Create new BINARY node under the parent handle
 *  @fn     handle_t vot_newBINARY (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a BINARY
 *  @return 		A handle to the first BINARY node, or zero
 */
handle_t
vot_newBINARY (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_BINARY));
}


/**
 *  vot_newBINARY2 -- Create new BINARY2 node under the parent handle
 *
 *  @brief  Create new BINARY2 node under the parent handle
 *  @fn     handle_t vot_newBINARY2 (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a BINARY2
 *  @return 		A handle to the first BINARY2 node, or zero
 */
handle_t
vot_newBINARY2 (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_BINARY2));
}


/**
 *  vot_newFITS -- Create new FITS node under the parent handle
 *
 *  @brief  Create new FITS node under the parent handle
 *  @fn     handle_t vot_newFITS (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a FITS
 *  @return 		A handle to the first FITS node, or zero
 */
handle_t
vot_newFITS (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_FITS));
}


/**
 *  vot_newGROUP -- Create new GROUP node under the parent handle
 *
 *  @brief  Create new GROUP node under the parent handle
 *  @fn     handle_t vot_newGROUP (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a GROUP
 *  @return 		A handle to the first GROUP node, or zero
 */
handle_t
vot_newGROUP (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_GROUP));
}


/**
 *  vot_newFIELDref -- Create new FIELDref node under the parent handle
 *
 *  @brief  Create new FIELDref node under the parent handle
 *  @fn     handle_t vot_newFIELDRef (handle_t parent_h)
 *
 *  @param  parent_h 	Parent handle of a FIELDref
 *  @return 		A handle to the first FIELDref node, or zero
 */
handle_t
vot_newFIELDRef (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_FIELDREF));
}


/**
 *  vot_newPARAMRef -- Create new PARAMref node under the parent handle
 *
 *  @brief  Create new PARAMRef node under the parent handle
 *  @fn     handle_t vot_newPARAMRef (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a PARAMRef
 *  @return             A handle to the first PARAMRef node, or zero
 */
handle_t
vot_newPARAMRef (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_PARAMREF));
}


/**
 *  vot_newDESCRIPTION -- Create new DESCRIPTION node under the parent handle
 *
 *  @brief  Create new DESCRIPTION node under the parent handle
 *  @fn     handle_t vot_newDESCRIPTION (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a DESCRIPTION
 *  @return             A handle to the first DESCRIPTION node, or zero
 */
handle_t
vot_newDESCRIPTION (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_DESCRIPTION));
}


/**
 *  vot_newPARAM -- Create new PARAM node under the parent handle
 *
 *  @brief  Create new PARAM node under the parent handle
 *  @fn     handle_t vot_newPARAM (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a PARAM
 *  @return             A handle to the first PARAM node, or zero
 */
handle_t
vot_newPARAM (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_PARAM));
}


/**
 *  vot_newINFO -- Create new INFO node under the parent handle
 *
 *  @brief  Create new INFO node under the parent handle
 *  @fn     handle_t vot_newINFO (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a INFO
 *  @return             A handle to the first INFO node, or zero
 */
handle_t
vot_newINFO (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_INFO));
}


/**
 *  vot_newSTREAM -- Create new STREAM node under the parent handle
 *
 *  @brief  Create new STREAM node under the parent handle
 *  @fn     handle_t vot_newSTREAM (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a STREAM
 *  @return             A handle to the first STREAM node, or zero
 */
handle_t
vot_newSTREAM (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_STREAM));
}


/**
 *  vot_newVALUES -- Create new VALUES node under the parent handle
 *
 *  @brief  Create new VALUES node under the parent handle
 *  @fn     handle_t vot_newVALUES (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a VALUES
 *  @return             A handle to the first VALUES node, or zero
 */
handle_t
vot_newVALUES (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_VALUES));
}


/**
 *  vot_newMIN -- Create new MIN node under the parent handle
 *
 *  @brief  Create new MIN node under the parent handle
 *  @fn     handle_t vot_newMIN (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a MIN
 *  @return             A handle to the first MIN node, or zero
 */
handle_t
vot_newMIN (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_MIN));
}


/**
 *  vot_newMAX -- Create new MAX node under the parent handle
 *
 *  @brief  Create new MAX node under the parent handle
 *  @fn     handle_t vot_newMAX (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a MAX
 *  @return             A handle to the first MAX node, or zero
 */
handle_t
vot_newMAX (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_MAX));
}


/**
 *  vot_newOPTION -- Create new OPTION node under the parent handle
 *
 *  @brief  Create new OPTION node under the parent handle
 *  @fn     handle_t vot_newOPTION (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a OPTION
 *  @return             A handle to the first OPTION node, or zero
 */
handle_t
vot_newOPTION (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_OPTION));
}


/**
 *  vot_newLINK -- Create new LINK node under the parent handle
 *
 *  @brief  Create new LINK node under the parent handle
 *  @fn     handle_t vot_newLINK (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a LINK
 *  @return             A handle to the first LINK node, or zero
 */
handle_t
vot_newLINK (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_LINK));
}


/**
 *  vot_newCOOSYS -- Create new COOSYS node under the parent handle
 *
 *  @brief  Create new COOSYS node under the parent handle
 *  @fn     handle_t vot_newCOOSYS (handle_t parent_h)
 *
 *  @param  parent_h    Parent handle of a COOSYS
 *  @return             A handle to the first COOSYS node, or zero
 */
handle_t
vot_newCOOSYS (handle_t parent_h)
{
    return (vot_newNode (parent_h, TY_COOSYS));
}



/****************************************************************************/

struct {
    int    type;		/** element type		*/
    int    parents;		/** allowed parent types	*/
    int    children;		/** allowed child types		*/
} elemParents[] = {
  { TY_ROOT,
    0,
    TY_VOTABLE
  },
  { TY_VOTABLE,
    TY_ROOT,
    TY_DESCRIPTION|TY_COOSYS|TY_INFO|TY_PARAM|TY_GROUP|TY_RESOURCE
  },
  { TY_RESOURCE,
    TY_VOTABLE|TY_RESOURCE,
    TY_DESCRIPTION|TY_COOSYS|TY_INFO|TY_PARAM|TY_GROUP|TY_RESOURCE|TY_LINK|TY_TABLE
  },
  { TY_TABLE,
    TY_RESOURCE,
    TY_DESCRIPTION|TY_FIELD|TY_INFO|TY_PARAM|TY_GROUP|TY_LINK|TY_DATA
  },
  { TY_INFO,
    TY_VOTABLE|TY_RESOURCE|TY_DATA|TY_TABLE,
    TY_DESCRIPTION|TY_VALUES|TY_LINK
  },
  { TY_STREAM,
    TY_BINARY|TY_BINARY2|TY_FITS,
    0
  },
  { TY_FITS,
    TY_DATA,
    0
  },
  { TY_TD,
    TY_TR,
    0
  },
  { TY_TR,
    TY_TABLEDATA,
    TY_TD
  },
  { TY_COOSYS,
    TY_VOTABLE|TY_RESOURCE,
    0
  },
  { TY_DESCRIPTION,
    TY_VOTABLE|TY_RESOURCE|TY_TABLE|TY_FIELD|TY_PARAM|TY_GROUP|TY_INFO,
    0
  },
  { TY_DEFINITIONS,
    0,
    0
  },
  { TY_DATA,
    TY_TABLE,
    TY_TABLEDATA|TY_BINARY|TY_BINARY2|TY_FITS
  },
  { TY_TABLEDATA,
    TY_DATA,
    TY_TR
  },
  { TY_GROUP,
    TY_VOTABLE|TY_RESOURCE|TY_TABLE|TY_GROUP,
    TY_DESCRIPTION|TY_FIELDREF|TY_PARAM|TY_PARAMREF|TY_GROUP
  },
  { TY_PARAM,
    TY_VOTABLE|TY_RESOURCE|TY_TABLE,
    TY_DESCRIPTION|TY_VALUES|TY_LINK
  },
  { TY_FIELD,
    TY_TABLE,
    TY_DESCRIPTION|TY_VALUES|TY_LINK
  },
  { TY_FIELDREF,
    TY_GROUP,
    0
  },
  { TY_PARAMREF,
    TY_GROUP,
    0
  },
  { TY_MIN,
    TY_VALUES,
    0
  },
  { TY_MAX,
    TY_VALUES,
    0
  },
  { TY_OPTION,
    TY_VALUES|TY_OPTION,
    0
  },
  { TY_VALUES,
    TY_PARAM|TY_INFO,
    TY_MIN|TY_MAX|TY_OPTION
  },
  { TY_LINK,
    TY_RESOURCE|TY_TABLE|TY_FIELD|TY_PARAM|TY_INFO,
    0
  },
  { -1, 0, 0 }
};



/**
 *  vot_newNode -- Creates a new blank unlinked node.
 * 
 *  @brief  Creates a new blank unlinked node.
 *  @fn     handle_t vot_newNode (handle_t parent, int type)
 *
 *  @param   parent 	A handle to the Element that you want to add a node to
 *  @param   type 	The type of node you wish to create
 *  @return 		A handle to the created node
 */
handle_t
vot_newNode (handle_t parent, int type)
{
    /*  Refer to vot_newRESOURCE for detailed comments on function workings. 
     */
    handle_t elem_h = 0;
    

    /*  Check that parent is proper for the type we create.
    assert ( (parent & vot_validParents (type)) );
     */

    /*  Attach the new node to the parent.
     */
    elem_h = vot_nodeCreate (type);
    vot_attachToNode (parent, elem_h);
    
    return (elem_h);
}


/**
 *  vot_attachNode -- Adds a node as a child of parent.
 *
 *  @brief  Adds a node as a child of parent.
 *  @fn     vot_attachNode (handle_t parent, handle_t new)
 *
 *  @param  parent 	A handle to the Element that you want to add a node to
 *  @param  new 	A handle to the Element that you want to add
 *  @return		nothing
 */
void
vot_attachNode (handle_t parent, handle_t new)
{
    Element *parent_ptr, *new_ptr;
    handle_t copy;
 
    if ((parent == 0) || (new == 0))
        return;

    /* Make a copy of the Element and it's children. */
    copy = vot_copyElement (new, 0);
    
    /* Get pointers. */
    parent_ptr = vot_getElement (parent);
    new_ptr = vot_getElement (copy);
    
    new_ptr->ref_count++;
    
    /* Make the links, the attached nodes are copies not the original. */
    if (parent_ptr->child)
        parent_ptr->last_child->next = new_ptr;
    else
        parent_ptr->child = new_ptr;
    
    parent_ptr->last_child = new_ptr;
    
    new_ptr->parent = parent_ptr;
}


/**
 *  vot_freeNode -- Destroys the node and all of it's children.
 *
 *  @brief  Destroys the node and all of it's children.
 *  @fn     vot_freeNode (handle_t node)
 *
 *  @param  node 	A handle to the Element that you want deleted
 *  @return		nothing
 */
void
vot_freeNode (handle_t node)
{
    /* Recursive function to delete the Element and it's children. */
    Element *node_ptr;
    handle_t child_handle, sibling_handle;
    

    if (! (node_ptr = vot_getElement (node)) )
	return;
    
    if (node_ptr->child) {
        child_handle = vot_lookupHandle (node_ptr->child);
        vot_freeNode (child_handle);
    } 
    
    if (node_ptr->next) {
        sibling_handle = vot_lookupHandle (node_ptr->next);
        vot_freeNode (sibling_handle);
    }
    
    /* Clean the handle and free the memory. 
    */
    vot_freeHandle (node);
    free (node_ptr);
}


/**
 *  vot_deleteNode -- Destroys the node and all of it's children.
 *
 *  @brief  Destroys the node and all of it's children.
 *  @fn     vot_deleteNode (handle_t element)
 *
 *  @param  element 	A handle to the Element that you want deleted
 *  @return		nothing
 */
void
vot_deleteNode (handle_t element)
{
    /* Delete the node but update the tree. */
    Element *element_ptr, *parent, *prev;
    
    element_ptr = vot_getElement (element);
    parent = element_ptr->parent;
    
    /* Make sure the node is not still reference. Should never be the case. */
    if (element_ptr->ref_count > 1) {
        element_ptr->ref_count--;
        return;  
    }
    
    if (parent) {
        if (parent->child == element_ptr) {
            parent->child = element_ptr->next;
            element_ptr->next = NULL;
        } else {
            for (prev=parent->child; prev->next != element_ptr; prev=prev->next)
		;
            prev->next = element_ptr->next;
            element_ptr->next = NULL;
            
            if (parent->last_child == element_ptr)
                parent->last_child = prev;
        }
    }
    
    vot_freeNode (element);
}


/**
 *  vot_copyElement -- Adds a node as a child of parent.
 *
 *  @brief  Adds a node as a child of parent.
 *  @fn     handle_t vot_copyElement (handle_t src_h, handle_t parent_h)
 *
 *  @param  src_h 	A handle to the Element to copy
 *  @param  parent_h 	A handle to the Elements parent
 *  @return 		A handle_t of the copy of the structure
 */
handle_t
vot_copyElement (handle_t src_h, handle_t parent_h)
{
    /* A recurseive function to copy a node and it's children. */
    Element   *src_ptr, *return_ptr;
    handle_t   return_handle, parent;
    handle_t   src_child_h, src_next_h;
    

    src_ptr = vot_getElement (src_h);
    
    if (src_ptr == 0)
        return (0);

    return_ptr = vot_elementDup (src_h); 	/* copy the source Element   */
    if (!return_ptr)
	return (0);
    return_handle = vot_lookupHandle (return_ptr); /* get the copies handle  */
    
    if (src_ptr->child) { 			/* process children  	     */
        parent = return_handle;
        src_child_h = vot_copyElement (
			    vot_lookupHandle (src_ptr->child), parent);
        
        /* Actually attach the node. No copy.
	*/
        vot_attachToNode (return_handle, src_child_h); 
    } 
    
    if (src_ptr->next) {			/* process siblings 	     */
        src_next_h = vot_copyElement (
			    vot_lookupHandle (src_ptr->next), parent);

        if (parent_h != 0)
            return_ptr->parent = vot_getElement (parent);

        /* Attach the sibling, no copy. 
	*/
        vot_attachSibling (return_handle, src_next_h);
    }
    
    return (return_handle);
}


/** **************************************************************************
 *  Utility methods
 ** *************************************************************************/

/**
 *  vot_getNCols -- Return the nuber of columns in the table structure.
 * 
 *  @brief  Return the nuber of columns in the table structure.
 *  @fn     int vot_getNCols (handle_t tdata_h)
 *
 *  @param  tdata_h 	A handle_t to a TABLEDATA
 *  @return	 	The number of cols
 */
int
vot_getNCols (handle_t tdata_h)
{
    Element *tdata = vot_getElement (tdata_h);
    
    if (tdata)
        return ((atoi(vot_attrGet (tdata->parent->parent->attr, "NCOLS"))));

    return (0);
}


/**
 *  vot_getNRows -- Return the nuber of columns in the table structure.
 * 
 *  @brief  Return the nuber of columns in the table structure.
 *  @fn     int vot_getNRows (handle_t tdata_h)
 *
 *  @param  tdata_h 	A handle_t to a TABLEDATA
 *  @return	 	The number of cols
 */
int
vot_getNRows (handle_t tdata_h)
{
    Element *tdata = vot_getElement (tdata_h);
    
    if (tdata)
        return ( (atoi(vot_attrGet (tdata->parent->parent->attr, "NROWS"))) );

    return (0);
}


/**
 *  vot_getTableCell -- Return the nuber of columns in the structure.
 * 
 *  @brief  Return the nuber of columns in the structure.
 *  @fn     char *vot_getTableCell (handle_t tdata_h, int row, int col)
 *
 *  @param  tdata_h 	A handle_t to a TABLEDATA
 *  @param  row 	An int for a row
 *  @param  col 	An int for a col
 *  @return	 	The content of the cell
 */
char *
vot_getTableCell (handle_t tdata_h, int row, int col)
{
    Element *tdata;
    char *s;
    int cols, rows;
    

    cols = vot_getNCols (tdata_h);
    rows = vot_getNRows (tdata_h);
    
    tdata = vot_getElement (tdata_h);
    
    if ( (row < rows) && (col < cols) ) {
        if (tdata) {
            s = tdata->data[(row * cols) + col];
            return ((s ? s : ""));
        }
    }
    
    return ("");
}


/**
 *  vot_sortTable -- Sort a data table based on the specified column.
 * 
 *  @brief  Sort a data table based on the specified column.
 *  @fn     int vot_sortTable (handle_t tdata_h, int col, int strsort, 
 *				int order))
 *
 *  @param  tdata_h 	A handle_t to a TABLEDATA
 *  @param  col 	An int for a col
 *  @param  strsort 	String sort?
 *  @param  order 	Sort order (1=ascending, -1=descending);
 *  @return	 	return status
 */

static int 	sort_strings = 0;
static int 	sort_column  = -1;
static int 	sort_order   =  1;

int
vot_tableCompare (const void *row1, const void *row2)
{
    char  **a = (char **)row1, 
	  **b = (char **)row2;
    char   *s1, *s2;
    double  x1, x2;
    int     result;


    s1 = a[sort_column];
    s2 = b[sort_column];

    if (!s1 || !s2)
        return (-1);
    if (sort_strings)
        result = (sort_order * strcasecmp (s1, s2));
    else {
	x1 = atof (s1);
	x2 = atof (s2);
	result = (sort_order * ((x1 < x2) ? -1 : (x1 == x2) ? 0 : 1));
    }
    return (result);
}


int
vot_sortTable (handle_t tdata_h, int col, int strsort, int order)
{
    Element *tdata = vot_getElement (tdata_h);
    int       cols = vot_getNCols (tdata_h), 
	      rows = vot_getNRows (tdata_h);
    handle_t  tr, td;
    int    i = 0;
    

    if (col < 0)
	return (ERR);

    sort_column  = col;
    sort_strings = strsort;
    sort_order   = order;

    /*  Sort the compiled table of TDATA strings by rows.  The comparison
     *  function decides which column is used for the sort.
     */
    qsort (&tdata->data[0], rows, (cols * sizeof(char *)), vot_tableCompare);


    /*  Rewrite the TDATA elements.  We've re-ordered the compiled string
     *  table, now we need to reset the <TD> contents accordingly.  Do not
     *  free existing pointers first since we've simply moved around the 
     *  values.
     */
    i = 0;
    for (tr = vot_getTR (tdata_h); tr; tr = vot_getNext(tr)) {
        for (td = vot_getTD(tr); td; td = vot_getNext(td)) {
	    Element *e = vot_getElement (td);
	    char  *s = tdata->data[i++];
	    e->content = (s ? strdup (s) : NULL);
	}
    }

    return (OK);
}


/**
 *  vot_getLength -- Return the number of sibling Elements of the same type.
 * 
 *  @brief  Return the number of sibling Elements of the same type.
 *  @fn     int vot_getLength (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	The status of the set
 */
int
vot_getLength (handle_t elem_h)
{
    Element *elem;
    int type, total = 0;
    

    if ( (elem = vot_getElement (elem_h)) ) 
        type = elem->type;
    else
        return (0);

    while (elem) {
        if (elem->type == type)
            total++;
        elem = elem->next;
    }
    
    return (total);
}


/**
 *  vot_getNumberOf -- Return the number of sibling Elements of the type.
 *
 *  @brief  Return the number of sibling Elements of the type.
 *  @fn     int vot_getNumberOf (handle_t elem_h, int type)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @param  type 	An int of the type of element you wish to count
 *  @return	 	The status of the set
 */
int
vot_getNumberOf (handle_t elem_h, int type)
{
    Element *elem = vot_getElement (elem_h);
    int total = 0;
    
    
    if (elem == NULL)
        return (0);
    
    while (elem) {
        if (elem->type == type)
            total++;
        elem = elem->next;
    }
    
    return (total);
}


/**
 *  vot_colByAttr -- Get the column number (0-indexed) by named attribute.
 * 
 *  @brief  Get the column number (0-indexed) by named attribute.
 *  @fn     int vot_colByAttr (handle_t tab, char *attr, char *name, char *alt)
 *
 *  @param  tab 	A handle_t the parent <TABLE> element
 *  @param  attr 	A string holding the attribute name
 *  @param  name 	A string holding the name attribute
 *  @param  value 	A string holding the alternate name attribute string
 *  @return	 	The matching column number or (-1) if not found
 */
int
vot_colByAttr (int tab, char *attr, char *name, char *alt)
{
    int   n, col = 0, field, ntest = ((alt && alt[0]) ? 2 : 1);
    char  cname[SZ_FNAME], *ctest, *atest;

    for (n=0; n < ntest; n++) {
        ctest = ((n == 0) ? name : alt);
        for (col=0,field=vot_getFIELD (tab); field; field=vot_getNext (field)) {
            memset (cname, 0, SZ_FNAME);
            if ((atest = vot_getAttr (field, attr)))
                strcpy (cname, atest);
            if (cname[0] && strcasecmp (ctest, cname) == 0)
                return (col);
	    col++;
        }
    }
    return (-1);
}


/**
 *  vot_colByName -- Get the column number (0-indexed) by 'name' attribute.
 * 
 *  @brief  Get the column number (0-indexed) by 'name' attribute.
 *  @fn     int vot_colByName (handle_t tab, char *name, char *alt)
 *
 *  @param  tab 	A handle_t the parent <TABLE> element
 *  @param  name 	A string holding the name attribute
 *  @param  value 	A string holding the alternate name attribute string
 *  @return	 	The matching column number or (-1) if not found
 */
int
vot_colByName (int tab, char *name, char *alt)
{
    return ( vot_colByAttr (tab, "name", name, alt) );
}


/**
 *  vot_colByUCD -- Get the column number (0-indexed) by 'ucd' attribute.
 * 
 *  @brief  Get the column number (0-indexed) by 'ucd' attribute.
 *  @fn     int vot_colByUCD (handle_t tab, char *name, char *alt)
 *
 *  @param  tab 	A handle_t the parent <TABLE> element
 *  @param  name 	A string holding the ucd attribute
 *  @param  value 	A string holding the alternate ucd attribute string
 *  @return	 	The matching column number or (-1) if not found
 */
int
vot_colByUCD (int tab, char *name, char *alt)
{
    return ( vot_colByAttr (tab, "ucd", name, alt) );
}


/**
 *  vot_colByID -- Get the column number (0-indexed) by 'id' attribute.
 * 
 *  @brief  Get the column number (0-indexed) by 'id' attribute.
 *  @fn     int vot_colByID (handle_t tab, char *name, char *alt)
 *
 *  @param  tab 	A handle_t the parent <TABLE> element
 *  @param  name 	A string holding the id attribute
 *  @param  value 	A string holding the alternate id attribute string
 *  @return	 	The matching column number or (-1) if not found
 */
int
vot_colByID (int tab, char *name, char *alt)
{
    return ( vot_colByAttr (tab, "id", name, alt) );
}


/**
 *  vot_findByAttr -- Get a handle to an Element with the requested attribute.
 * 
 *  @brief  Get a handle to an Element with the requested attribute.
 *  @fn     handle_t vot_findByAttr (handle_t parent, char *name, char *value)
 *
 *  @param  parent 	A handle_t the parent Element
 *  @param  name 	A string holding the Value type
 *  @param  value 	A string holding the Value value
 *  @return	 	The handle to the element
 */
handle_t
vot_findByAttr (handle_t parent, char *name, char *value)
{
    Element *elem, *my_parent;
    char *elem_value;
    handle_t return_h = 0;
    

    my_parent = vot_getElement (parent);
    elem = my_parent->child;
    
    if ((elem == NULL) || (name == NULL) || (value == NULL))
        return (0);
    
    while (elem) {
        elem_value = vot_attrGet (elem->attr, name);
        
        if ((elem_value != NULL) && (strcasecmp(elem_value, value) == 0)) {
            return_h = vot_lookupHandle (elem);
            break;
        }
        
        elem = elem->next;
    }
    
    return (return_h);
}


/**
 *  vot_findInGroup -- Return a handle array of the requested Element type.
 * 
 *  @brief  Return a handle array of the requested Element type.
 *  @fn     handle_t *vot_findInGroup (handle_t group, int type)
 *
 *  @param  group 	A handle_t the parent Element
 *  @param  type 	Value of the type
 *  @return	 	An array of handles
 */
handle_t
*vot_findInGroup (handle_t group, int type)
{
    Element *my_childs, *my_parent;
    int numberOf = 0;
    handle_t my_childs_h;
    handle_t *handles = NULL;
    int type_parent;
    

    my_parent = vot_getElement (group);
    my_childs = my_parent->child;
    
    type_parent = vot_elemType (my_parent);
    
    if ((type_parent != TY_FIELD) && (type_parent != TY_PARAM))
        return (NULL);
    
    if ((my_childs == NULL) || (type >= NUM_ELEMENTS))
        return (NULL);
    
    my_childs_h = vot_lookupHandle (my_childs);
    numberOf = vot_getNumberOf (my_childs_h, type);
    
    if (numberOf <= 0)
        return (NULL);
    
    handles = (handle_t *) calloc (numberOf, sizeof (handle_t));
    
    /*   FIXME
     *
     *   The way this should work in the API is to return a new 'handle'
     *   where a vot_getNext() behaves as with all other handles to return
     *   a next member of the group.  Where we might have a problem is in
     *   free-ing the handles in that this new group handle is left dangling.
     *
     *   If the concept is that the vot_struct is a root element for the
     *   handle table in each votable then we avoid the above problem by
     *   clearing all handles for a 'vot' which would include any newly
     *   allocated group handles.  The (better) alternative is to allocate a 
     *   handle during the parsing and here we just return the group handle.
     *   Otherwise we might be left dealing with terminating handles in the
     *   vot_struct idea, and would complicate adding nodes to a 'vot' when
     *   editing.  (MJF,  8/4/09)
     */
    while (my_childs) {
        if (vot_elemType (my_childs) == type) {
            numberOf--;
            handles[numberOf] = vot_lookupHandle (my_childs);
        }
        
        my_childs = my_childs->next;
    }
    
    return (handles);
}


/**
 *  vot_getNext -- Return a handle_t of the next Element of the same type.
 *  
 *  @brief  Return a handle_t of the next Element of the same type.
 *  @fn     handle_t vot_getNext (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	A handle of the next Element of the same type
 */
handle_t
vot_getNext (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    int type;
    
    type = vot_elemType (elem);
    for (elem=elem->next; elem; elem = elem->next) {
        if (vot_elemType (elem) == type)
            break;
    }
    
    return (vot_lookupHandle (elem));
}


/**
 *  vot_getSibling -- Return a handle_t of the next signling Element.
 * 
 *  @brief  Return a handle_t of the next Element.
 *  @fn     handle_t vot_getSibling (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	A handle of the next Element
 */
handle_t
vot_getSibling (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    
    return (vot_lookupHandle (elem->next));
}


/**
 *  vot_getChild -- Return a handle_t of the child Element.
 *
 *  @brief  Return a handle_t of the child Element.
 *  @fn     handle_t vot_getChild (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	A handle of the child Element
 */
handle_t
vot_getChild (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    
    return (vot_lookupHandle (elem->child));
}


/**
 *  vot_getParent -- Return the handle of the parent Element.
 *
 *  @brief  Return the handle of the parent Element.
 *  @fn     handle_t vot_getParent (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	A handle of the paretn Element
 */
handle_t
vot_getParent (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    
    return (vot_lookupHandle (elem->parent));
}


/**
 *  vot_getChildOfType -- Get the handle of the next Element of the same type.
 *
 *  @brief  Get the handle of the next Element of the same type.
 *  @fn     handle_t vot_getChildOfType (handle_t elem_h, int type)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @param  type 	An integer of the Element type for find
 *  @return	 	A handle of the Element
 */
handle_t
vot_getChildOfType (handle_t elem_h, int type)
{
    Element *elem;
    
    elem = vot_getElement (elem_h);
    type = vot_elemType (elem);
    
    for (elem = elem->child; elem; elem = elem->next) {
        if (vot_elemType (elem) == type)
            break;
    }
    
    return (vot_lookupHandle (elem));
}


/**
 *  vot_valueOf -- Return type of the Element.
 *
 *  @brief   Return type of the Element.
 *  @fn      int vot_valueOf (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	An integer of the type
 */
int
vot_valueOf (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    return (vot_elemType (elem));		/* ???? FIXME	*/
}


/**
 *  vot_typeOf -- Return type of the Element.
 *
 *  @brief  Return type of the Element.
 *  @fn     int vot_typeOf (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @return	 	An integer of the type
 */
int
vot_typeOf (handle_t elem_h)
{
    return ( vot_elemType (vot_getElement (elem_h)) );
}


/****************************************************************************
 *
 ***************************************************************************/


/**
 *  vot_setValue -- Set the Value for the ELEMENT.
 *
 *  @brief  Set the Value for the ELEMENT.
 *  @fn     int vot_setValue (handle_t elem_h, char *value)
 *
 *  @param  elem_h 	A handle_t the ELEMENT
 *  @param  value 	A string holding the value
 *  @return 		The status of the set
 */
int
vot_setValue (handle_t elem_h, char *value)
{
    Element *cur = vot_getElement (elem_h);
    int len = strlen (value) + 1;

    
    if (value) {
        if(cur->content != NULL)
            free (cur->content);

        cur->content = (char *) calloc (len, sizeof (char));
        
        if (cur->content == NULL) {
            fprintf (stderr, "ERROR:  CALLOC failed for vot_setValue.\n");
            return (0);
        }
        
        strncat (cur->content, value, len);
        return (1);

    } else
        return (0);
}


/**
 *  vot_getValue -- Get the Value for the ELEMENT.
 *
 *  @brief  Get the Value for the ELEMENT.
 *  @fn     char *vot_getValue (handle_t elem_h)
 *
 *  @param  elem_h 	A handle_t the ELEMENT
 *  @return 		A string of the value or the Value
 */
char *
vot_getValue (handle_t elem_h)
{
    Element *elem = vot_getElement (elem_h);
    
#ifdef USE_NULL_VALUE
    return ((elem ? elem->content : NULL));
#else
    return ((elem ? elem->content : ""));
#endif
}


/**
 *  vot_setAttr -- Set the attribute for the Element.
 * 
 *  @brief  Set the attribute for the Element.
 *  @fn     int vot_setAttr (handle_t elem_h, char *attr, char *value)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @param  attr 	A string holding the attribute name
 *  @param  value 	A string holding the attribute value
 *  @return	 	The status of the set
 */
int
vot_setAttr (handle_t elem_h, char *attr, char *value)
{
    Element *elem = vot_getElement (elem_h);
    
    return (vot_attrSet (elem->attr, attr, value));
}


/**
 *  vot_getAttr -- Return the attribute for the Element.
 * 
 *  @brief  Return the attribute for the Element.
 *  @fn     char * vot_getAttr (handle_t elem_h, char *attr)
 *
 *  @param  elem_h 	A handle_t the Element
 *  @param  attr 	A string holding the attribute name
 *  @return	 	A string of the value or the attr
 */
char *
vot_getAttr (handle_t elem_h, char *attr)
{
    Element *elem = vot_getElement (elem_h);
    
    return (vot_attrGet (elem->attr, attr));
}


/**
 *  vot_writeVOTable -- Write the VOTable to the file descriptor.
 *
 *  @brief  Write the VOTable to the file descriptor.
 *  @fn     vot_writeVOTable (handle_t node, char *fname, int indent)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  indent 	Number of spaces to indent at each level
 *  @return		nothing
 */
void
vot_writeVOTable (handle_t node, char *fname, int indent)
{
    FILE *fd = (FILE *) NULL;

    if (strcasecmp (fname, "stdout") == 0 || strncmp (fname, "-", 1) == 0)
	fd = stdout;
    else {
	if ((fd = fopen (fname, "w+")) == (FILE *) NULL) {
	    fprintf (stderr, "Cannot open XML file '%s'\n", fname);
	    return;
	}
    }


    fprintf (fd, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>%s",
	(indent ? "\n" : ""));
    vot_dumpXML (vot_getElement (node), 0, indent, fd);
    fprintf (fd, "\n");

    fflush (fd);
    if (fd != stdout)
	fclose (fd);
}


/**
 *  vot_writeHTML -- Write the VOTable to the file descriptor as HTML.
 *
 *  @brief  Write the VOTable to the file descriptor as HTML.
 *  @fn     vot_writeHTML (handle_t node, char *ifname, char *ofname)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  ifname	Input filename
 *  @param  ofname	Output filename (or "stdout" or "-" for STDOUT)
 *  @return		nothing
 */
void
vot_writeHTML (handle_t node, char *ifname, char *ofname)
{
    FILE *fd = (FILE *) NULL;
    handle_t  res = vot_getRESOURCE (node);
    handle_t  sub_res = 0, top_res = 0;


    if (strcasecmp (ofname, "stdout") == 0 || strncmp (ofname, "-", 1) == 0)
	fd = stdout;
    else {
	if ((fd = fopen (ofname, "w+")) == (FILE *) NULL) {
	    fprintf (stderr, "Cannot open HTML file '%s'\n", ofname);
	    return;
	}
    }

    /*  Write the header.
     */
    vot_htmlHeader (fd, ifname);

    /*  Loop over all the <RESOURCE> elements.
     */
    while (res) {

	/*  Make sure we have a <TABLE> element in the <RESOURCE>.  If not,
	 *  look for a child resource to process.  Only do one level of
	 *  nesting.    -- FIXME --
	 */
	if (!sub_res && vot_getTABLE (res) <= 0) {
	    if ((sub_res = vot_getChildOfType  (res, TY_RESOURCE))) {
		top_res = res;
		res = sub_res;
	    }
	}

        vot_htmlTableMeta (fd, res, ifname);
        vot_htmlTableData (fd, res, NULL);

	res = vot_getNext (res);
	if (res)
	    fprintf (fd, "<hr noshade='3'>\n");
	else if (sub_res) {
	    /*  End of child resources, go back up.
	     */
	    sub_res = 0;
	    res = vot_getNext (top_res);
	}
    }

    /*  Write the footer and close the file. 	
     */
    vot_htmlFooter (fd);
    fflush (fd);
    if (fd != stdout)
	fclose (fd);
}


/**
 *  vot_writeSHTML -- Write the VOTable to the file descriptor as an HTML table.
 *
 *  @brief  Write the VOTable to the file descriptor as an HTML table.
 *  @fn     vot_writeSHTML (handle_t node, char *ifname, char *ofname)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  ifname	Input filename
 *  @param  ofname	Output filename (or "stdout" or "-" for STDOUT)
 *  @return		nothing
 */
void
vot_writeSHTML (handle_t node, char *ifname, char *ofname)
{
    FILE *fd = (FILE *) NULL;
    handle_t  res = vot_getRESOURCE (node);


    if (!res)						/* error	*/
	return;		

    if (strcasecmp (ofname, "stdout") == 0 || strncmp (ofname, "-", 1) == 0)
	fd = stdout;
    else {
	if ((fd = fopen (ofname, "w+")) == (FILE *) NULL) {
	    fprintf (stderr, "Cannot open standalone HTML file '%s'\n", ofname);
	    return;
	}
    }

    /*  Write just the <TABLEDATA> portion of the file.
     */
    vot_htmlTableData (fd, res, ifname);

    fflush (fd);
    if (fd != stdout)
	fclose (fd);
}


/** 
 *  VOT_SIMPLEGETURL -- Utility routine to do a simple URL download to the file.
 */
static int 
vot_simpleGetURL (char *url, char *ofname)
{
    int  stat = 0;
    char lockfile[SZ_FNAME], errBuf[CURL_ERROR_SIZE], fname[SZ_FNAME];
    FILE *fd;
    CURL *curl_handle;



    /*  For the CURL operation to download the file.
     */
    curl_global_init (CURL_GLOBAL_ALL);     	/* init curl session	*/
    curl_handle = curl_easy_init ();

    if ((fd = fopen (ofname, "wb")) == NULL) { 	/* open the output file */
	fprintf (stderr, "Error: cannot open output file '%s'\n", ofname);
        curl_easy_cleanup (curl_handle);
        return (1);
    }

    /*  Set cURL options
     */
    curl_easy_setopt (curl_handle, CURLOPT_URL, url);
    curl_easy_setopt (curl_handle, CURLOPT_NOPROGRESS, 1L);
    curl_easy_setopt (curl_handle, CURLOPT_WRITEDATA, fd);
    curl_easy_setopt (curl_handle, CURLOPT_ERRORBUFFER, errBuf);
    curl_easy_setopt (curl_handle, CURLOPT_FOLLOWLOCATION, 1);

    /*  Do the download.
     */
    if ((stat = curl_easy_perform (curl_handle)) != 0) {
	/*  Error in download, clean up.
	 */
	unlink (fname);
        fclose (fd); 			    	/* close the file 	*/
        curl_easy_cleanup (curl_handle);    	/* cleanup curl stuff 	*/
	return (1);
    }

    fflush (fd);
    fclose (fd); 			    	/* close the file 	*/
    curl_easy_cleanup (curl_handle); 	    	/* cleanup curl stuff 	*/


    /*  Remove the lock file to indicate we are done.
     */
    unlink (lockfile);

    return (0);
}


/**
 *  vot_writeFITS -- Write the VOTable to the file descriptor as a FITS table.
 *
 *  @brief  Write the VOTable to the file descriptor as an FITS table.
 *  @fn     vot_writeFITS (handle_t node, char *fname)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @return		nothing
 */

#ifdef HAVE_CFITSIO

/*  Use native code and CFITSIO to write the file.
 */

#define	MAX_FIELDS		256

static int vot_addFITSMeta (int handle, fitsfile *fp, char *meta, int index);
static int vot_addFieldMeta (int handle, fitsfile *fp, int index);
static int vot_writeFITSData (fitsfile *fp, char **data, char *fmt[], 
				int nrows, int ncols);
static void vot_printerror (int status);


void
vot_writeFITS (handle_t vot, char *oname)
{
    char  *name, *unit, *dtype, *width, **cells, *cell, *asize, *tname;
    char  *ttype[MAX_FIELDS], *tform[MAX_FIELDS], *tunit[MAX_FIELDS], *ch;
    char   extname[SZ_LINE], col[SZ_FNAME];
    int    res, tab, data, tdata, field, handle, hdutype, *widths, *spaces;
    int    i, j, len, ncols, nrows, status = 0, resnum = 1, bitpix = 8;
    long   naxis = 0,  naxes[2] = { 0, 0 };
    fitsfile  *fp;		/* CFITSIO descriptor		*/


    if (access (oname, F_OK) == 0)	/* remove an existing file	*/
	unlink (oname);


    if (fits_create_file (&fp, oname, &status)) /* create new FITS file */
        vot_printerror (status);

    if ( fits_create_img (fp,  bitpix, naxis, naxes, &status) )
        vot_printerror (status);


    /*  Loop over all <RESOURCE> elements in the file, creating an new
     *  extension for each one.
     */
    for (res=vot_getRESOURCE (vot); res; res=vot_getNext(res) ) {

	/*  Get handles for the current resource.
	 */
      	if ((tab   = vot_getTABLE (res)) <= 0)
	    continue;
      	if ((data  = vot_getDATA (tab)) <= 0)
	    continue;
        if ((tdata = vot_getTABLEDATA (data)) <= 0)
	    return;
      	nrows = vot_getNRows (tdata);
      	ncols = vot_getNCols (tdata);


	/*  Allocate space for the data cells.  Read in the cells so we can
 	 *  convert it for output.  Also collect the widths so we can
 	 *  properly size the table.
	 */
	cells  = (char **) calloc (1, (nrows * ncols) * sizeof (char *));
	widths = (int *) calloc (1, ncols * sizeof (int));
	spaces = (int *) calloc (1, ncols * sizeof (int));
        for (i = 0; i < nrows; i++) {
            for (j = 0; j < ncols; j++) {
                cell = cells[i*ncols+j] = vot_getTableCell(tdata, i, j);

		if ((len = strlen (cell)) > widths[j])
		    widths[j] = len;

		if (cell[0] && strchr (cell, (int)' ') && len > 1 && i < 1) {
		    /* trim whitespace 	*/
		    for (ch=&cell[len-1]; isspace(*ch) && ch > cell; ch--)
			*ch = '\0';
		    for (ch=cell; isspace(*ch); ) 
			ch++;
		    for (       ; *ch; ch++) {
			if (*ch == ' ')
		    	    spaces[j]++;
		    }
		}
            }
        }

        memset (&ttype[0], 0, (MAX_FIELDS * sizeof (char *)));
	memset (&tform[0], 0, (MAX_FIELDS * sizeof (char *)));
	memset (&tunit[0], 0, (MAX_FIELDS * sizeof (char *)));

	/*  Move to proper extension HDU.
	 */
        if (fits_movabs_hdu (fp, resnum++, &hdutype, &status)) 
            vot_printerror (status);

        /*  Get the column attributes and set them in the header.
         */
	i = 0;
        for (field=vot_getFIELD(tab); field; field=vot_getNext (field)) {
	    dtype = vot_getAttr (field, "datatype");
	    width = vot_getAttr (field, "width");
	    asize = vot_getAttr (field, "arraysize");

	    memset (col, 0, SZ_FNAME);		/* dummy column name	*/
	    sprintf (col, "col%d", i + 1);	/* one-indexed		*/

	    /*  Required columns.
	     */
	    ttype[i] = ((name=vot_getAttr(field,"name"))  ? name : strdup(col));
	    tunit[i] = ((unit=vot_getAttr(field, "unit")) ? unit : strdup(""));

	    tform[i] = calloc (1, 16);

	    if (strncasecmp (dtype, "char", 4) == 0 ||
	        strncasecmp (dtype, "bool", 4) == 0 ||
	        strncasecmp (dtype, "unsignedByte", 12) == 0) {

		    if (asize && asize[0]  && widths[i]) {
		        sprintf (tform[i], "%dA", 
			    (asize[0] == '*' ? widths[i] : atoi (asize)));
		    } else
		        strcpy (tform[i], "A");

	    } else if (strncasecmp (dtype, "float", 4) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dE", spaces[i]+1);
		else
		    strcpy (tform[i], "E");

	    } else if (strncasecmp (dtype, "double", 4) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dD", spaces[i]+1);
		else
		    strcpy (tform[i], "D");

	    } else if (strncasecmp (dtype, "short", 5) == 0 ||
	        strncasecmp (dtype, "unicodeChar", 11) == 0) {
		    if (spaces[i])
		        sprintf (tform[i], "%dI", spaces[i]+1);
		    else
		        strcpy (tform[i], "I");

	    } else if (strncasecmp (dtype, "int", 3) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dJ", spaces[i]+1);
		else
		    strcpy (tform[i], "J");

	    } else if (strncasecmp (dtype, "long", 4) == 0) {
		if (spaces[i])
		    sprintf (tform[i], "%dJ", spaces[i]+1);
		else
		    strcpy (tform[i], "J");
	    }

	    if (dtype)	free ( (void *) dtype);
	    if (width)	free ( (void *) width);
	    if (asize)	free ( (void *) asize);
	    i++;
        }

        /*  Append a new empty binary table onto the FITS file
	 */

	/*  See if the table has a name attribute, if so, use it as the
	 *  extension name.
	 */
	memset (extname, 0, SZ_LINE);
	tname = vot_getAttr (tab, "name");
	if (!tname || strchr (tname, (int)'?') || strchr (tname, (int)'&'))
	    sprintf (extname, "ext%d", resnum);
	else {
	    strcpy (extname, tname);
	    free ((void *) tname);
	}

        if (fits_create_tbl (fp, BINARY_TBL, nrows, ncols, ttype, tform,
            tunit, extname, &status))
        	vot_printerror (status);


	/*  Add UCD and UTYPE keywords for the FIELDs if defined.
	 */
        for (i=1,field=vot_getFIELD(tab); field; field=vot_getNext (field))
	    vot_addFieldMeta (field, fp, i++);

	/* Add keywords for all the <INFO> and <PARAM> elements.
	 */
      	if ((handle = vot_getINFO (res))) {
	    for (i=1, len=vot_getLength (handle); i <= len; i++) {
	        vot_addFITSMeta (handle, fp, "INFO", i);
	        handle = vot_getNext (handle);
	    }
	}

      	if ((handle = vot_getPARAM (res))) {
	    for (i=1, len=vot_getLength (handle); i <= len; i++) {
	        vot_addFITSMeta (handle, fp, "PARAM", i);
	        handle = vot_getNext (handle);
	    }
	}


	/*  Write the data to the file.
	 */
	if (nrows > 0)
	    vot_writeFITSData (fp, cells, tform, nrows, ncols);

	/*  Free the allocated pointers.
	 */
	for (i=0; i < ncols; i++) {
	    if (ttype[i])  free ((void *) ttype[i]);
	    if (tunit[i])  free ((void *) tunit[i]);
	    if (tform[i])  free ((void *) tform[i]);
	}
	if (cells)	free ((void *) cells);
	if (widths)	free ((void *) widths);
	if (spaces)	free ((void *) spaces);
    }


    vot_closeVOTABLE (vot);			/* close the VOTable  	*/
    if (fits_close_file (fp, &status))       	/* close the FITS file 	*/
         vot_printerror (status);
}


static int 
vot_addFITSMeta (int handle, fitsfile *fp, char *meta, int index)
{
    char  *id, *nam, *val, *unit, keyw[SZ_FNAME], comment[SZ_FNAME];
    int   status = 0;


    if ( (nam = vot_getAttr (handle, "name")) ) {	/* NAME attribute    */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sNAM%d", meta, index);
        sprintf (comment, "%s name attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, nam, comment, &status))
	    vot_printerror ( status );
    }

    if ( (val = vot_getAttr (handle, "value")) ) {	/* VALUE attribute   */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sVAL%d", meta, index);
        sprintf (comment, "%s val attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, val, comment, &status))
	    vot_printerror ( status );
    }

    if ( (id = vot_getAttr (handle, "id")) ) {		/* ID attribute	      */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sID%d", meta, index);
        sprintf (comment, "%s id attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, id, comment, &status))
	    vot_printerror ( status );
    }

    if ( (unit = vot_getAttr (handle, "unit")) ) {	/* UNIT attribute   */
        memset (keyw, 0, SZ_FNAME);
        memset (comment, 0, SZ_FNAME);
        sprintf (keyw, "%3.3sUNI%d", meta, index);
        sprintf (comment, "%s unit attribute", meta);

        if (fits_update_key (fp, TSTRING, keyw, unit, comment, &status))
	    vot_printerror ( status );
    }

    return (0);
}


static int 
vot_addFieldMeta (int handle, fitsfile *fp, int index)
{
    char  *ucd, *utype, *id, keyw[SZ_FNAME];
    int   status = 0;


    if ( (id = vot_getAttr (handle, "id")) ) {		/* ID attribute     */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TID%d", index);

        if (fits_update_key (fp, TSTRING, keyw, id, "ID attribute", &status))
	    vot_printerror ( status );
    }

    if ( (ucd = vot_getAttr (handle, "ucd")) ) {	/* UCD attribute     */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TUCD%d", index);

        if (fits_update_key (fp, TSTRING, keyw, ucd, "UCD attribute", &status))
	    vot_printerror ( status );
    }

    if ( (utype = vot_getAttr (handle, "utype")) ) {	/* UTYPE attribute   */
        memset (keyw, 0, SZ_FNAME);
        sprintf (keyw, "TUTYPE%d", index);

        if (fits_update_key (fp, TSTRING, keyw, utype, "UTYPE attribute",
	    &status))
	        vot_printerror ( status );
    }

    return (0);
}


static int
vot_writeFITSData (fitsfile *fp, char **data, char *fmt[], int nrows, int ncols)
{
    int     i, j, n, type, width, status = 0;
    char    **ccol, *d, *tform, cell[1024], *tok, *sep = " ", *brkt = NULL;
    float  *fcol;
    double *dcol;
    long   *icol;
    short  *scol;
    long    frow = 1, felem = 1, nr = nrows;
    

    for (j = 0; j < ncols; j++) {

	tform = fmt[j];
	width = atoi (tform);
	type = strlen (tform) - 1;

	switch (tform[type]) {
	case 'A':						/* CHAR	    */
//    	    ccol = (char **) calloc (1, (nrows * (width+1) * sizeof (char *)));
    	    ccol = (char **) calloc (1, (nrows * (SZ_LINE) * sizeof (char *)));

    	    for (i = 0; i < nrows; i++) {
		d = data[i * ncols + j];
		ccol[i] = ((d && *d) ? d : (char *) "");
	    }

	    fits_write_col (fp, TSTRING, j+1, frow,felem, nr, ccol, &status);
	    free ((void *) ccol);
	    break;

	case 'D':						/* DOUBLE   */
    	    dcol = (double *) calloc (1, (nrows * (width+1) * sizeof (double)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++) {
		    if (strncasecmp (data[i*ncols+j], "NaN", 3) == 0)
        	        ((double *) dcol)[i] = (double) atof("");
		    else
        	        ((double *) dcol)[i] = (double) atof(data[i*ncols + j]);
		}
	        fits_write_col (fp, TDOUBLE, j+1, frow,felem, nr, dcol, 
			&status);
	    } else {
		double *dp = dcol, *dpr = dp;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    dpr = dp;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *dp++ = (double) atof (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*dp++ = (double) 0.0;

	            fits_write_col (fp, TDOUBLE, j+1, i+1,felem, width, dpr, 
			&status);
		}
	    }

	    free ((void *) dcol);
	    break;

	case 'E':						/* FLOAT    */
    	    fcol = (float *) calloc (1, (nrows * (width+1) * sizeof (float)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((float *) fcol)[i] = (float) atof (data[i * ncols + j]);
	        fits_write_col (fp, TFLOAT, j+1, frow,felem, nr, fcol, &status);
	    } else {
		float *rp = fcol, *rpr = rp;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    rpr = rp;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *rp++ = (float) atof (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*rp++ = (float) 0.0;

	            fits_write_col (fp, TFLOAT, j+1, i+1,felem, width, rpr, 
			&status);
		}
	    }

	    free ((void *) fcol);
	    break;

	case 'S':						/* SHORT    */
	case 'I':						/* SHORT    */
    	    scol = (short *) calloc (1, (nrows * (width+1) * sizeof (short)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((short *) scol)[i] = (short) atoi (data[i * ncols + j]);
	        fits_write_col (fp, TSHORT, j+1, frow,felem, nr, scol, &status);
	    } else {
		short *sp = scol, *spr = sp;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    spr = sp;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *sp++ = (short) atoi (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*sp++ = (short) 0;

	            fits_write_col (fp, TSHORT, j+1, i+1,felem, width, spr, 
			&status);
		}
	    }

	    free ((void *) scol);
	    break;

	case 'J':						/* INT      */
    	    icol = (long *) calloc (1, (nrows * (width+1) * sizeof (long)));

	    if (width == 0) {
    	        for (i = 0; i < nrows; i++)
        	    ((long *) icol)[i] = (long) atoi (data[i * ncols + j]);
	        fits_write_col (fp, TLONG, j+1, frow,felem, nr, icol, &status);
	    } else {
		long *ip = icol, *ipr = ip;

    	        for (i = 0; i < nrows; i++) {
		    brkt = NULL;
		    memset (cell, 0, 1024);
		    strcpy (cell, data[i * ncols + j]);

		    ipr = ip;
		    for (n=1, tok=strtok_r (cell, sep, &brkt); tok;
		         tok=strtok_r (NULL, sep, &brkt), n++)
			    *ip++ = (long) atoi (tok);
		    for ( ; n < width; n++)		/* missing values  */
			*ip++ = (long) 0;

	            fits_write_col (fp, TLONG, j+1, i+1,felem, width, ipr, 
			&status);
		}
	    }

	    free ((void *) icol);
	    break;

	default:
	    fprintf (stderr, "Invalid column type '%c'\n", tform[type]);
	    continue;
	}
    }

    return (0);
}


static void 
vot_printerror (int status) 
{
    if (status) {
       fits_report_error (stderr, status); 	/* print error report 	*/
       exit (status);
    }
    return;
}



#else

#ifdef USE_STILTS

/*  Use an extern STILTS command to do the conversion.
 */
void
vot_writeFITS (handle_t node, char *fname)
{
    char tmp[SZ_FNAME], cmd[SZ_FNAME];

    memset (cmd, 0, SZ_FNAME);
    memset (tmp, 0, SZ_FNAME);

    sprintf (tmp, "/tmp/vo%d", (int)getpid());

    vot_writeVOTable (node, tmp, 0);

    /*  FIXME  */
    sprintf (cmd, 
	"/usr/local/bin/stilts -stderr /dev/null tcopy %s %s ofmt=fits-basic",
	tmp, fname);

    system (cmd);

    unlink (tmp);
}

#else

/*  No-Op version of method.
 */
void
vot_writeFITS (handle_t node, char *fname)
{
}

#endif
#endif



/***************************************
 *  START of HTML Utility procedures.
 ***************************************/


/**
 *  VOT_HTMLHEADER -- Printer the HTML header.
 */
static void
vot_htmlHeader (FILE *fd, char *fname)
{
    fprintf (fd, 
	"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n");
    fprintf (fd, "<html><head><title>File: %s</title></head><body>\n", fname);
    fflush (fd);
}


/**
 *  VOT_HTMLTABLEMETA -- Print the VOTable metadata for the RESOURCE.
 */
static void
vot_htmlTableMeta (FILE *fd, handle_t res, char *ifname)
{
    handle_t   desc  = vot_getDESCRIPTION (res);
    handle_t   info  = vot_getINFO (res);
    handle_t   param = vot_getPARAM (res);
    
    char   *val = NULL;


    /*
	RESOURCE  ID=<id>  type=<type>  name=<name>

	Description: 

	INFO    <name>  <value>
	PARAM	<name>	<value>
	  :       :	   :
     */

    fprintf (fd, "<table border='0' width='90%%'><tbody><tr>");

    /*  Print general votable metadata.
     */
    fprintf (fd, "<td><b>FILE NAME</b>:</td><td colspan='3'>%s</td></tr>", 
	ifname);
    fprintf (fd, "<tr><td><b>RESOURCE</b>:</td><td colspan='3'>");
    if ((val = vot_getAttr (res, "ID")))    fprintf (fd, " ID='%s'", val);
    if ((val = vot_getAttr (res, "name")))  fprintf (fd, " name='%s'", val);
    if ((val = vot_getAttr (res, "type")))  fprintf (fd, " type='%s'", val);
    fprintf (fd, "</td></tr>\n");

    fprintf (fd, "<tr><td valign='top'><b>DESCRIPTION:</b></td>");
    fprintf (fd, "<td colspan='3'>%s</td></tr>",
	(desc ? vot_getValue (desc) : ""));

    fprintf (fd, "<tr><td>&nbsp;</td><td colspan='3'/></tr>\n");

    /*  Print the RESOURCE-specific <INFO> elements.
     */
    while (info) {
        fprintf (fd, "<tr><td><b>INFO</b></td><td>%s</td><td>%s</td>",
	   vot_getAttr (info, "name"), vot_getAttr (info, "value"));
	if ((desc = vot_getDESCRIPTION(info)))
           fprintf (fd, "<td>%s</td>", vot_getValue (desc));
	else 
           fprintf (fd, "<td/>");
        fprintf (fd, "</tr>");

	info = vot_getNext (info);
    }

    /*  Print the RESOURCE-specific <PARAM> elements.
     */
    while (param) {
        fprintf (fd, "<tr><td><b>PARAM</b></td><td>%s</td><td>%s</td>",
	   vot_getAttr (param, "name"), vot_getAttr (param, "value"));
	if ((desc = vot_getDESCRIPTION (param)))
           fprintf (fd, "<td>%s</td>", vot_getValue (desc));
	else 
           fprintf (fd, "<td/>");
        fprintf (fd, "</tr>");

	param = vot_getNext (param);
    }
    fprintf (fd, "<tr><td>&nbsp;</td><td colspan='3'/></tr>\n");

    fprintf (fd, "</tr></tbody></table>\n");
    fflush (fd);
}


/**
 *  VOT_HTMLTABLEDATA -- Print the VOTable TABLEDATA for the RESOURCE.
 */
static void
vot_htmlTableData (FILE *fd, handle_t res, char *ifname)
{
    handle_t  tab, data, tdata, field, tr, td;
    register  int i, nrows, ncols;
    char     *name, *id, *ucd, *s;


    /*  Display options.  For the moment, these are hardcoded values, later
     *  we'll make these settable from the application.
     */
    int	   tborder	= 1;			
    char  *hcolor	= "eec";		/* header color		*/
    char  *ecolor	= "ccc";		/* even-row color	*/
    char  *ocolor	= "eee";		/* odd-row color	*/


    if ((tab = vot_getTABLE (res)) <= 0)	/* get handles		*/
	return;
    if ((data = vot_getDATA (tab)) <= 0)
	return;
    if ((tdata = vot_getTABLEDATA (data)) <= 0)
	return;
    nrows = vot_getNRows (tdata);
    ncols = vot_getNCols (tdata);


    fprintf (fd, "<table border='%d'>\n", tborder);
    if (ifname)
	fprintf (fd, "<tcaption>File: %s</tcaption>\n", ifname);

    /* Print the Column header names, find or make a reasonable value.
    */
    fprintf (fd, "<thead><tr style=\"background:#%s\">\n", hcolor);
    for (field=vot_getFIELD (tab),i=0; field; field = vot_getNext (field),i++) {
        name = vot_getAttr (field, "name");
        id   = vot_getAttr (field, "id");
        ucd  = vot_getAttr (field, "ucd");

        if (name || id || ucd)
            fprintf (fd, "<th>%s</th>", (name ? name : (id ? id : ucd)) );
        else
            fprintf (fd, "<th>col%d</th>", i);
    }
    fprintf (fd, "</tr></thead>\n");


    /*  Now dump the data.
    */
    fprintf (fd, "<tbody>\n");
    for (tr=vot_getTR (tdata),i=0; tr; tr=vot_getNext(tr),i++) {
        fprintf (fd, " <tr style=\"background:#%s\">\n",
	    (((i % 2) == 0) ? ecolor : ocolor));

	/*  Print table cells.  If we have a url, make it a link.
	 */
        for (td=vot_getTD(tr),i=0; td; td=vot_getNext(td),i++) {
            s = ((s = vot_getValue (td)) ? s : "");
	    if (strncasecmp ("http://", s, 7) == 0) 
                fprintf (fd, "  <td><a href=\"%s\">%s</a></td>\n", s, s);
	    else 
                fprintf (fd, "  <td>%s</td>\n", s);
	}
        fprintf (fd, " </tr>\n");
    }
    fprintf (fd, "</tbody>\n");

    fprintf (fd, "</table>\n");
    fflush (fd);
}


/**
 *  VOT_HTMLFOOTER -- Printer the HTML footer.
 */
static void
vot_htmlFooter (FILE *fd)
{
    fprintf (fd, "</body>\n</html>\n"); 	/* write the footer 	*/
    fflush (fd);
}


/***************************************
 *  END of HTML Utility procedures.
 ***************************************/



/**
 *  vot_writeDelimited -- Write the VOTable as a delimited text file.
 *
 *  @brief  Write the VOTable as a delimited text file.
 *  @fn     vot_writeDelimited (handle_t vot, FILE *fd, char delim, int hdr)
 *
 *  @param  vot 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  hdr		Write an output header?
 *  @return		nothing
 */
void
vot_writeDelimited (handle_t vot, char *fname, char delim, int hdr)
{
    char  *name, *id, *ucd, *s;
    int   res, tab, data, tdata, field, tr, td;         /* handles      */
    int   i=0, ncols=0;
    FILE *fd = (FILE *) NULL;


    if (strcasecmp (fname, "stdout") == 0 || strncmp (fname, "-", 1) == 0)
	fd = stdout;
    else
	fd = fopen (fname, "w+");

    res = vot_getRESOURCE (vot);        /* get RESOURCES                */
    if (vot_getLength (res) > 1) {
        fprintf (stderr, "Error: multiple RESOURCES not supported\n");
        return;
    }

    if ((tab = vot_getTABLE (res)) <= 0)
	return;
    if ((data = vot_getDATA (tab)) <= 0)
	return;
    if ((tdata = vot_getTABLEDATA (data)) <= 0)
	return;
    ncols = vot_getNCols (tdata);

    /* Print the Column header names.
    */
    if (hdr) {
        fprintf (fd, "# ");
	i = 0;
        for (field=vot_getFIELD (tab); field; field = vot_getNext (field)) {
            name = vot_getAttr (field, "name");     /* find reasonable value */
            id   = vot_getAttr (field, "id");
            ucd  = vot_getAttr (field, "ucd");
            if (name || id || ucd)
                fprintf (fd, "%s", (name ? name : (id ? id : ucd)) );
            else
                fprintf (fd, "col%d", i);
            if (i < (ncols-1))
                fprintf (fd, "%c", delim);
	    i++;
        }
        fprintf (fd, "\n");
    }
                
            
    /* Now dump the data.
    */
    for (tr=vot_getTR (tdata); tr; tr=vot_getNext(tr)) {
        for (td=vot_getTD(tr),i=0; td; td=vot_getNext(td),i++) {
	    s = ((s =vot_getValue (td)) ? s : "");
	    if (strchr (s, (int) delim))
	        fprintf (fd, "\"%s\"", s);
	    else
	        fprintf (fd, "%s", s);
	    if (i < (ncols-1))
	        fprintf (fd, "%c", delim);
	}
	fprintf (fd, "\n");
    }

    fflush (fd);
    if (fd != stdout)
	fclose (fd);
}


/**
 *  vot_writeASV -- Write the VOTable to the file descriptor as a ASV file
 *
 *  @brief  Write the VOTable to the file descriptor as a ASV file
 *  @fn     vot_writeASV (handle_t node, char *fname, int header)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  header	Write output header?
 *  @return		nothing
 */
void
vot_writeASV (handle_t node, char *fname, int header)
{
    vot_writeDelimited (node, fname, ' ', header);
}


/**
 *  vot_writeBSV -- Write the VOTable to the file descriptor as a BSV file
 *
 *  @brief  Write the VOTable to the file descriptor as a BSV file
 *  @fn     vot_writeBSV (handle_t node, char *fname, int header)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  header	Write output header?
 *  @return		nothing
 */
void
vot_writeBSV (handle_t node, char *fname, int header)
{
    vot_writeDelimited (node, fname, '|', header);
}


/**
 *  vot_writeCSV -- Write the VOTable to the file descriptor as a CSV file
 *
 *  @brief  Write the VOTable to the file descriptor as a CSV file
 *  @fn     vot_writeCSV (handle_t node, char *fname, int header)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  header	Write output header?
 *  @return		nothing
 */
void
vot_writeCSV (handle_t node, char *fname, int header)
{
    vot_writeDelimited (node, fname, ',', header);
}


/**
 *  vot_writeTSV -- Write the VOTable to the file descriptor as a TSV file
 *
 *  @brief  Write the VOTable to the file descriptor as a TSV file
 *  @fn     vot_writeTSV (handle_t node, char *fname, int header)
 *
 *  @param  node 	A handle to an Element that you to print
 *  @param  fname	Output filename (or "stdout" or "-" for STDOUT)
 *  @param  header	Write output header?
 *  @return		nothing
 */
void
vot_writeTSV (handle_t node, char *fname, int header)
{
    vot_writeDelimited (node, fname, '\t', header);
}


/**
 *  vot_setWarnings --  Set the warning level.
 *
 *  @brief  Set the warning level.     
 *  @fn     vot_setWarnings (int value)
 *
 *  @param  value 	Warning level
 *  @return		nothing
 */
void
vot_setWarnings (int value)
{
    switch ((votWarn = value)) {
    case 0:	votELevel = "";		    break;
    case 1:	votELevel = "Warning: ";    break;
    case 2:	votELevel = "Error: ";	    break;
    }
}


/**
 *  votEmsg -- Error message print utility.
 */
void
votEmsg (char *msg)
{
    if (votWarn)
	fprintf (stderr, "%s%s", votELevel, msg);
}



/****************************************************************************
 *  Private procedures.
 ****************************************************************************/

/**
 *  vot_cleanUp
 *
 *  @brief	Free all the handles and Element nodes.
static void
vot_cleanUp (void)
{
    vot_handleCleanup ();
        
    if (vot_struct == NULL)
        vot_struct = vot_newElem (TY_ROOT);
    
    vot_struct->parent     = NULL;
    vot_struct->child      = NULL;
    vot_struct->last_child = NULL;
    vot_struct->next       = NULL;
}
 */


/**
 *  vot_elementDup -- Duplicate the input Element.
 *
 *  @brief  Duplicate the input Element.
 *  @fn     Element * vot_elementDup (handle_t element_h)
 *
 *  @param  element_h 	A handle_t to the ELEMENT you want to copy
 *  @return 		An ELEMENT type
 */
static Element *
vot_elementDup (handle_t element_h)
{
    Element *new, *src;
    handle_t new_h;
    int      type;
    AttrList *attr;
    

    src = vot_getElement (element_h); 	/* get the element to copy 	*/
    type = vot_typeOf (element_h);  	/* get the type 		*/
    
    if (type >= NUM_ELEMENTS)
        return ((void *) NULL);

    new_h = vot_nodeCreate (type);  	/* make a blank Node 		*/
    new = vot_getElement (new_h); 	/* get the pointer 		*/
    
    
    /* Copy the attributes. 
    */
    for (attr=src->attr->attributes; attr; attr = attr->next)
        vot_attrSet (new->attr, attr->name, attr->value);
    
    /* Copy the content. 
    */
    if (src->content) {
	int len = strlen (src->content);

        new->content = (char *) calloc ((len + 2), sizeof(char));
	new->isCData = src->isCData;
        strncpy (new->content, src->content, len);
    }
    
    return (new);  			/* return the copy 	*/
}


/**
 *  vot_nodeCreate -- Create a new blank unlinked node.
 * 
 *  @brief  Create a new blank unlinked node.
 *  @fn     handle_t vot_nodeCreate (int type)
 *
 *  @param  type 	The type of node you wish to create
 *  @return 		A handle to the created node
 */
static handle_t
vot_nodeCreate (int type)
{
    /* Make a new blank node and give it a handle. */
    Element *elem = vot_newElem (type);
    
    return (vot_setHandle (elem));
}


/**
 *  vot_attachToNode -- Adds a node as a child of parent.
 *
 *  @brief  Adds a node as a child of parent.
 *  @fn     vot_attachToNode (handle_t parent, handle_t new)
 *
 *  @param  parent 	A handle to the Element that you want to add a node to
 *  @param  new 	A handle to the Element that you want to add
 *  @return		nothing
 */
static void
vot_attachToNode (handle_t parent, handle_t new)
{
    Element *parent_ptr, *new_ptr;
 
    /* Sanity check. 
    assert ( (new & vot_validChildren (parent)) );
     */
    if ((parent == 0) || (new == 0))
        return;
    
    parent_ptr = vot_getElement (parent);
    new_ptr = vot_getElement (new);
    
    new_ptr->ref_count++;
    
    /* Make links. */
    if (parent_ptr->child)
        parent_ptr->last_child->next = new_ptr;
    else
        parent_ptr->child = new_ptr;
    
    parent_ptr->last_child = new_ptr;
    
    new_ptr->parent = parent_ptr;
}


/**
 *  vot_attachSibling -- Adds a node as a sibling of big_brother.
 *
 *  @brief  Adds a node as a sibling of big_brother.
 *  @fn     vot_attachSibling (handle_t big_brother, handle_t new)
 *
 *  @param  big_brother Handle to the Element you want to add a node to
 *  @param  new 	A handle to the Element that you want to add
 *  @return		nothing
 */
static void
vot_attachSibling (handle_t big_brother, handle_t new)
{
    Element *big_brother_ptr, *new_ptr;
 
    /* Sanity check. */
    if ((big_brother == 0) || (new == 0))
        return;
    
    /* Get relevant pointers. 
    */
    big_brother_ptr = vot_getElement (big_brother);
    new_ptr = vot_getElement (new);
    
    new_ptr->ref_count++; 	/* Up reference count. DEFUNCT. */
    
    /* Make the links. 
    */
    if (big_brother_ptr->next)
        big_brother_ptr->last_child->next = new_ptr;
    else
        big_brother_ptr->next = new_ptr;
    
    big_brother_ptr->last_child = new_ptr;
    
    new_ptr->parent = big_brother_ptr;
}


/**
 *  vot_dumpXML -- Prints the document tree as readable XML.
 *
 *  @brief  Prints the document tree as readable XML.
 *  @fn     vot_dumpXML (Element *node, int level, int indent, FILE *fd)
 *
 *  @param  node 	A pointer to the Element that you want to print from.
 *  @param  level 	The number of tabs to format the output.
 *  @param  indent 	Number of spaces to indent at each level.
 *  @param  fd 		The file descriptor to send the output to.
 *  @return		nothing
 */
static void
vot_dumpXML (Element *node, int level, int indent, FILE *fd)
{
    register int i, space = indent;

    
    /* If the element is NULL, there is nothing to print.
    */
    if (node == NULL)
        return;
    
    /* Make spaces based on how deep we are and print the formatted Element. 
    */
    for (i = 0; space && i < (space * level); i++) 
	fprintf (fd, " ");
    fprintf (fd, "%s", vot_elemXML (node));
    
    /* If there are children, recurse to them, print function returns. 
    */
    if (node->child) {
	if (indent) 
	    fprintf (fd, "\n");
        vot_dumpXML (node->child, (level + 1), indent, fd);
        
        /* Print the content between the tags.  */
        if (node->content) {
	    if (node->isCData)
                fprintf (fd, "<![CDATA[%s]]>", node->content);
	    else
                fprintf (fd, "%s", vot_deWS(node->content));
        }
        
	/*  Make space and print the closing XML tag.
	 */
        for (i = 0; space && i < (space * level); i++) 
	    fprintf (fd, " ");
        fprintf (fd, "%s", vot_elemXMLEnd (node));
        
    } else  {   	/* This node has no children, beginning of base case. */
        if (node->content) {
	    if (node->isCData)
                fprintf (fd, "<![CDATA[%s]]>", node->content);
	    else
                fprintf (fd, "%s", vot_deWS(node->content));
        }
        
        /* Print the closing XML tag. 
	*/
        fprintf (fd, "%s", vot_elemXMLEnd (node));
    }

    if (indent) 
	fprintf (fd, "\n");
    
    /* If there are siblings, recurse through them. 
    */
    if (node->next)
        vot_dumpXML (node->next, level, indent, fd);
    
    /* At this point there should be no more children or sibling on this node.
    */
    fflush (fd);
}


/**
 *  vot_deWS -- Determine whether the input string is nothing but whitespace.
 */
static char *
vot_deWS (char *in)
{
    char *ip = in;

    for (ip=in; *ip && isspace (*ip); ip++) ;
    return ((*ip ? in : ""));
}


#ifdef USE_VALIDITY
/**
 *  vot_validParents -- Return the mask of valid parents for the type.
 */
static int
vot_validParents (int type)
{
    int i;
    
    for (i=0; i < NUM_ELEMENTS; i++) {
	if (elemParents[i].type >= 0 && elemParents[i].type)
	    return (elemParents[i].parents);
    }

    return (0);
}


/**
 *  vot_validChildren -- Return the mask of valid children for the type.
 */
static int
vot_validChildren (int type)
{
    int i;
    
    for (i=0; i < NUM_ELEMENTS; i++) {
	if (elemParents[i].type >= 0 && elemParents[i].type)
	    return (elemParents[i].children);
    }

    return (0);
}
#endif



/*  Debug utility
 */
#ifdef USE_DEBUG
static void votBob (void) { }


/**
 *  vot_printData -- Print the table matrix.
 * 
 *  @brief  Print the table matrix.
 *  @fn     vot_printData (Element *tdata)
 *
 *  @param  tdata 	A pointer to the TABLEDATA Element that you want print
 *  @return		nothing
 */
static void
vot_printData (Element *tdata)
{
    Element *r = NULL, *c = NULL;
    handle_t r_h, c_h;
    int  i, j, cols, ncells, rows;


    if (tdata->type != TY_TABLEDATA) {
        fprintf (stderr, "ERROR: Must be a TABLEDATA element to print.\n");
        return;
    }
    
    cols = atoi (vot_attrGet (tdata->parent->parent->attr, "NCOLS"));
    rows = atoi (vot_attrGet (tdata->parent->parent->attr, "NROWS"));
    ncells  = rows * cols;
    
    if (ncells == 0)	/* e.g. a metadata votable return	*/
	return;

    r_h = vot_getTR (vot_lookupHandle (tdata));
    r = vot_getElement (r_h);
    
    c_h = vot_getTD (vot_lookupHandle (r));
    c = vot_getElement (c_h);

    for (i = 0; r; i++) {
        printf ("%02d: ", i);
        
        for (j=0; c; j++) {
            printf ("%s  ", (tdata->data[(i * cols) + j]));
            c = c->next;
        }
        r = r->next;
        
        if (r)
            c = r->child;
        
        printf ("\n");
    }
}

#endif
