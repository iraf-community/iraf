/*  VOTPARSE.I -- SWIG Interface definition file.
 */

%module libvot
%{
#define	handle_t	int


extern handle_t  vot_openVOTABLE (char *arg);
extern void 	 vot_closeVOTABLE (handle_t vot);

extern handle_t  vot_getRESOURCE (handle_t handle);
extern handle_t  vot_getTABLE (handle_t handle);
extern handle_t  vot_getFIELD (handle_t handle);
extern handle_t  vot_getDATA (handle_t handle);
extern handle_t  vot_getTABLEDATA (handle_t handle);
extern handle_t  vot_getTR (handle_t handle);
extern handle_t  vot_getTD (handle_t handle);
extern handle_t  vot_getBINARY (handle_t handle);
extern handle_t  vot_getBINARY2 (handle_t handle);
extern handle_t  vot_getFITS (handle_t handle);
extern handle_t  vot_getGROUP (handle_t handle);
extern handle_t  vot_getFIELDRef (handle_t handle);
extern handle_t  vot_getPARAMRef (handle_t handle);
extern handle_t  vot_getDESCRIPTION (handle_t handle);
extern handle_t  vot_getPARAM (handle_t handle);
extern handle_t  vot_getINFO (handle_t handle);
extern handle_t  vot_getSTREAM (handle_t handle);
extern handle_t  vot_getVALUES (handle_t handle);
extern handle_t  vot_getMIN (handle_t handle);
extern handle_t  vot_getMAX (handle_t handle);
extern handle_t  vot_getOPTION (handle_t handle);
extern handle_t  vot_getLINK (handle_t handle);
extern handle_t  vot_getCOOSYS (handle_t handle);

extern handle_t  vot_newRESOURCE (handle_t parent_h);
extern handle_t  vot_newTABLE (handle_t parent_h);
extern handle_t  vot_newFIELD (handle_t parent_h);
extern handle_t  vot_newDATA (handle_t parent_h);
extern handle_t  vot_newTABLEDATA (handle_t parent_h);
extern handle_t  vot_newTR (handle_t parent_h);
extern handle_t  vot_newTD (handle_t parent_h);
extern handle_t  vot_newBINARY (handle_t parent_h);
extern handle_t  vot_newBINARY2 (handle_t parent_h);
extern handle_t  vot_newFITS (handle_t parent_h);
extern handle_t  vot_newGROUP (handle_t parent_h);
extern handle_t  vot_newFIELDRef (handle_t parent_h);
extern handle_t  vot_newPARAMRef (handle_t parent_h);
extern handle_t  vot_newDESCRIPTION (handle_t parent_h);
extern handle_t  vot_newPARAM (handle_t parent_h);
extern handle_t  vot_newINFO (handle_t parent_h);
extern handle_t  vot_newSTREAM (handle_t parent_h);
extern handle_t  vot_newVALUES (handle_t parent_h);
extern handle_t  vot_newMIN (handle_t parent_h);
extern handle_t  vot_newMAX (handle_t parent_h);
extern handle_t  vot_newOPTION (handle_t parent_h);
extern handle_t  vot_newLINK (handle_t parent_h);
extern handle_t  vot_newCOOSYS (handle_t parent_h);

extern handle_t  vot_newNode (handle_t parent, int type);
extern void 	 vot_freeNode (handle_t delete_me);
extern void 	 vot_attachNode (handle_t parent, handle_t new);
extern void 	 vot_deleteNode (handle_t element);
extern handle_t  vot_copyElement (handle_t src_h, handle_t parent_h);

extern int 	 vot_getNCols (handle_t tdata_h);
extern int 	 vot_getNRows (handle_t tdata_h);
extern char     *vot_getTableCell (handle_t tdata_h, int row, int col);
extern int 	 vot_getLength (handle_t elem_h);
extern int 	 vot_getNumberOf (handle_t elem_h, int type);

extern int       vot_colByAttr(handle_t tab, char *attr, char *name, char *alt);
extern int       vot_colByName (handle_t tab, char *name, char *alt);
extern int       vot_colByUCD (handle_t tab, char *name, char *alt);
extern int       vot_colByID (handle_t tab, char *name, char *alt);

extern handle_t  vot_findByAttr (handle_t parent, char *name, char *value);
extern handle_t *vot_findInGroup (handle_t group, int type);
extern handle_t  vot_getNext (handle_t elem_h);
extern handle_t  vot_getSibling(handle_t elem_h);
extern handle_t  vot_getChild (handle_t elem_h);
extern handle_t  vot_getParent (handle_t elem_h);
extern handle_t  vot_ChildOfType (handle_t elem_h, int type);
extern int 	 vot_valueOf (handle_t elem_h);
extern int 	 vot_typeOf (handle_t elem_h);

extern int 	 vot_setValue (handle_t elem_h, char *value);
extern char     *vot_getValue (handle_t elem_h);
extern int 	 vot_setAttr (handle_t elem_h, char *attr, char *value);
extern char     *vot_getAttr (handle_t elem_h, char *attr);

extern void      vot_writeVOTable (handle_t node, FILE *fd);
extern void      vot_setWarnings (int value);
%}




#define	handle_t	int


extern handle_t  vot_openVOTABLE (char *arg);
extern void 	 vot_closeVOTABLE (handle_t vot);

extern handle_t  vot_getRESOURCE (handle_t handle);
extern handle_t  vot_getTABLE (handle_t handle);
extern handle_t  vot_getFIELD (handle_t handle);
extern handle_t  vot_getDATA (handle_t handle);
extern handle_t  vot_getTABLEDATA (handle_t handle);
extern handle_t  vot_getTR (handle_t handle);
extern handle_t  vot_getTD (handle_t handle);
extern handle_t  vot_getBINARY (handle_t handle);
extern handle_t  vot_getBINARY2 (handle_t handle);
extern handle_t  vot_getFITS (handle_t handle);
extern handle_t  vot_getGROUP (handle_t handle);
extern handle_t  vot_getFIELDRef (handle_t handle);
extern handle_t  vot_getPARAMRef (handle_t handle);
extern handle_t  vot_getDESCRIPTION (handle_t handle);
extern handle_t  vot_getPARAM (handle_t handle);
extern handle_t  vot_getINFO (handle_t handle);
extern handle_t  vot_getSTREAM (handle_t handle);
extern handle_t  vot_getVALUES (handle_t handle);
extern handle_t  vot_getMIN (handle_t handle);
extern handle_t  vot_getMAX (handle_t handle);
extern handle_t  vot_getOPTION (handle_t handle);
extern handle_t  vot_getLINK (handle_t handle);
extern handle_t  vot_getCOOSYS (handle_t handle);

extern int 	 vot_getDATAType (handle_t data_h);
extern char     *vot_getDATATypeString (handle_t data_h);

extern handle_t  vot_newRESOURCE (handle_t parent_h);
extern handle_t  vot_newTABLE (handle_t parent_h);
extern handle_t  vot_newFIELD (handle_t parent_h);
extern handle_t  vot_newDATA (handle_t parent_h);
extern handle_t  vot_newTABLEDATA (handle_t parent_h);
extern handle_t  vot_newTR (handle_t parent_h);
extern handle_t  vot_newTD (handle_t parent_h);
extern handle_t  vot_newBINARY (handle_t parent_h);
extern handle_t  vot_newBINARY2 (handle_t parent_h);
extern handle_t  vot_newFITS (handle_t parent_h);
extern handle_t  vot_newGROUP (handle_t parent_h);
extern handle_t  vot_newFIELDRef (handle_t parent_h);
extern handle_t  vot_newPARAMRef (handle_t parent_h);
extern handle_t  vot_newDESCRIPTION (handle_t parent_h);
extern handle_t  vot_newPARAM (handle_t parent_h);
extern handle_t  vot_newINFO (handle_t parent_h);
extern handle_t  vot_newSTREAM (handle_t parent_h);
extern handle_t  vot_newVALUES (handle_t parent_h);
extern handle_t  vot_newMIN (handle_t parent_h);
extern handle_t  vot_newMAX (handle_t parent_h);
extern handle_t  vot_newOPTION (handle_t parent_h);
extern handle_t  vot_newLINK (handle_t parent_h);
extern handle_t  vot_newCOOSYS (handle_t parent_h);

extern handle_t  vot_newNode (handle_t parent, int type);
extern void 	 vot_freeNode (handle_t delete_me);
extern void 	 vot_attachNode (handle_t parent, handle_t new);
extern void 	 vot_deleteNode (handle_t element);
extern handle_t  vot_copyElement (handle_t src_h, handle_t parent_h);

extern int 	 vot_getNCols (handle_t tdata_h);
extern int 	 vot_getNRows (handle_t tdata_h);
extern char     *vot_getTableCell (handle_t tdata_h, int row, int col);
extern int 	 vot_getLength (handle_t elem_h);
extern int 	 vot_getNumberOf (handle_t elem_h, int type);

extern int       vot_colByAttr(handle_t tab, char *attr, char *name, char *alt);
extern int       vot_colByName (handle_t tab, char *name, char *alt);
extern int       vot_colByUCD (handle_t tab, char *name, char *alt);
extern int       vot_colByID (handle_t tab, char *name, char *alt);

extern handle_t  vot_findByAttr (handle_t parent, char *name, char *value);
extern handle_t *vot_findInGroup (handle_t group, int type);
extern handle_t  vot_getNext (handle_t elem_h);
extern handle_t  vot_getSibling(handle_t elem_h);
extern handle_t  vot_getChild (handle_t elem_h);
extern handle_t  vot_getParent (handle_t elem_h);
extern handle_t  vot_ChildOfType (handle_t elem_h, int type);
extern int 	 vot_valueOf (handle_t elem_h);
extern int 	 vot_typeOf (handle_t elem_h);

extern int 	 vot_setValue (handle_t elem_h, char *value);
extern char     *vot_getValue (handle_t elem_h);
extern int 	 vot_setAttr (handle_t elem_h, char *attr, char *value);
extern char     *vot_getAttr (handle_t elem_h, char *attr);

extern void      vot_writeVOTable (handle_t node, FILE *fd);
