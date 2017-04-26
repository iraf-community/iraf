/**
 *  VOTPARSE.H -- Public procedure declarations for the VOTable interface.
 *
 *  @file       votParse.h
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      Public procedure declarations for the VOTable interface.
 */


/**
 *  VOTable element types
 */
#define	NUM_ELEMENTS	25

#define	TY_ROOT		000000000	/* Element Type Definitions	*/
#define	TY_VOTABLE	000000001
#define	TY_RESOURCE	000000002
#define	TY_FIELD	000000004
#define	TY_PARAM	000000010
#define	TY_INFO		000000020
#define	TY_TR		000000040
#define	TY_TD		000000100
#define TY_TABLE    	000000200
#define TY_STREAM   	000000400
#define TY_FITS    	000001000
#define TY_GROUP   	000002000
#define TY_FIELDREF 	000004000
#define TY_PARAMREF 	000010000
#define TY_MIN      	000020000
#define TY_MAX      	000040000
#define TY_OPTION   	000100000
#define TY_VALUES   	000200000
#define TY_LINK     	000400000
#define TY_DATA     	001000000
#define TY_DESCRIPTION 	002000000
#define TY_TABLEDATA    004000000
#define TY_BINARY     	010000000
#define TY_BINARY2     	020000000

#define TY_COOSYS     	100000000	/* deprecated elements		*/
#define TY_DEFINITIONS  200000000


#ifndef	OK				/* Utility values		*/
#define	OK		0
#endif
#ifndef	ERR
#define	ERR		1
#endif

#ifndef TRUE
#define TRUE    	1
#endif
#ifndef FALSE
#define FALSE   	0
#endif


#ifndef  handle_t
#define  handle_t	int
#endif


/** *************************************************************************
 *  Public LIBVOTABLE interface.
 ** ************************************************************************/

handle_t vot_openVOTABLE (char *arg);
void 	 vot_closeVOTABLE (handle_t vot);

handle_t vot_getRESOURCE (handle_t handle);
handle_t vot_getTABLE (handle_t handle);
handle_t vot_getFIELD (handle_t handle);
handle_t vot_getDATA (handle_t handle);
handle_t vot_getTABLEDATA (handle_t handle);
handle_t vot_getTR (handle_t handle);
handle_t vot_getTD (handle_t handle);
handle_t vot_getBINARY (handle_t handle);
handle_t vot_getBINARY2 (handle_t handle);
handle_t vot_getFITS (handle_t handle);
handle_t vot_getGROUP (handle_t handle);
handle_t vot_getFIELDRef (handle_t handle);
handle_t vot_getPARAMRef (handle_t handle);
handle_t vot_getDESCRIPTION (handle_t handle);
handle_t vot_getPARAM (handle_t handle);
handle_t vot_getINFO (handle_t handle);
handle_t vot_getSTREAM (handle_t handle);
handle_t vot_getVALUES (handle_t handle);
handle_t vot_getMIN (handle_t handle);
handle_t vot_getMAX (handle_t handle);
handle_t vot_getOPTION (handle_t handle);
handle_t vot_getLINK (handle_t handle);
handle_t vot_getCOOSYS (handle_t handle);

handle_t vot_newRESOURCE (handle_t parent_h);
handle_t vot_newTABLE (handle_t parent_h);
handle_t vot_newFIELD (handle_t parent_h);
handle_t vot_newDATA (handle_t parent_h);
handle_t vot_newTABLEDATA (handle_t parent_h);
handle_t vot_newTR (handle_t parent_h);
handle_t vot_newTD (handle_t parent_h);
handle_t vot_newBINARY (handle_t parent_h);
handle_t vot_newBINARY2 (handle_t parent_h);
handle_t vot_newFITS (handle_t parent_h);
handle_t vot_newGROUP (handle_t parent_h);
handle_t vot_newFIELDRef (handle_t parent_h);
handle_t vot_newPARAMRef (handle_t parent_h);
handle_t vot_newDESCRIPTION (handle_t parent_h);
handle_t vot_newPARAM (handle_t parent_h);
handle_t vot_newINFO (handle_t parent_h);
handle_t vot_newSTREAM (handle_t parent_h);
handle_t vot_newVALUES (handle_t parent_h);
handle_t vot_newMIN (handle_t parent_h);
handle_t vot_newMAX (handle_t parent_h);
handle_t vot_newOPTION (handle_t parent_h);
handle_t vot_newLINK (handle_t parent_h);
handle_t vot_newCOOSYS (handle_t parent_h);

int 	 vot_getDATAType (handle_t data_h);
char    *vot_getDATATypeString (handle_t data_h);

/****************************************************************************/

handle_t vot_newNode (handle_t parent, int type);
void 	 vot_freeNode (handle_t delete_me);
void 	 vot_attachNode (handle_t parent, handle_t new);
void 	 vot_deleteNode (handle_t element);
handle_t vot_copyElement (handle_t src_h, handle_t parent_h);


/*****************************************************************************
 *  Utility methods
 ****************************************************************************/

int 	 vot_getNCols (handle_t tdata_h);
int 	 vot_getNRows (handle_t tdata_h);
char    *vot_getTableCell (handle_t tdata_h, int row, int col);
int      vot_sortTable (handle_t tdata_h, int col, int sort_strings, int order);
int 	 vot_getLength (handle_t elem_h);
int 	 vot_getNumberOf (handle_t elem_h, int type);

int	 vot_colByAttr (handle_t tab, char *attr, char *name, char *alt);
int	 vot_colByName (handle_t tab, char *name, char *alt);
int	 vot_colByUCD (handle_t tab, char *name, char *alt);
int	 vot_colByID (handle_t tab, char *name, char *alt);

handle_t vot_findByAttr (handle_t parent, char *name, char *value);
handle_t *vot_findInGroup (handle_t group, int type);
handle_t vot_getNext (handle_t elem_h);
handle_t vot_getSibling(handle_t elem_h);
handle_t vot_getChild (handle_t elem_h);
handle_t vot_getParent (handle_t elem_h);
handle_t vot_getChildOfType (handle_t elem_h, int type);
int 	 vot_valueOf (handle_t elem_h);
int 	 vot_typeOf (handle_t elem_h);
int      vot_handleCount ();

int 	 vot_setValue (handle_t elem_h, char *value);
char    *vot_getValue(handle_t elem_h);
int 	 vot_setAttr (handle_t elem_h, char *attr, char *value);
char    *vot_getAttr (handle_t elem_h, char *attr);

void 	 vot_setWarnings (int value);
void 	 votEmsg (char *msg);


/****************************************************************************
 * Write
 ***************************************************************************/

void 	 vot_writeVOTable (handle_t node, char *fname, int indent);
void 	 vot_writeHTML (handle_t node, char *ifname, char *ofname);
void 	 vot_writeSHTML (handle_t node, char *ifname, char *ofname);
void 	 vot_writeASV (handle_t node, char *fname, int hdr);
void 	 vot_writeBSV (handle_t node, char *fname, int hdr);
void 	 vot_writeCSV (handle_t node, char *fname, int hdr);
void 	 vot_writeTSV (handle_t node, char *fname, int hdr);
void 	 vot_writeFITS (handle_t node, char *fname);

