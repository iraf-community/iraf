/**
 *  VOTPARSEP.H -- Internal LIBVOTABLE definitions.
 *
 *  @file       votParseP.h
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      Internal LIBVOTABLE definitions.
 */


#include <expat.h>

#define	VOT_DOC_VERSION		"1.2"	/** VOTable document version (write)  */

#define	VOT_XSI			"http://www.w3.org/2001/XMLSchema-instance"
#define	VOT_SCHEMA_LOC		"http://www.ivoa.net/xml/VOTable/v1.1  http://www.ivoa.net/xml/VOTable/v1.1"
#define	VOT_XMLNS		"http://www.ivoa.net/xml/VOTable/v1.1"


#define SZ_ATTRNAME             32      /** size of attribute name            */
#define SZ_ATTRVAL              2048    /** size of attribute value           */
#define SZ_FNAME                255     /** size of filename+path             */
#define SZ_XMLTAG               1024    /** max length of entire XML tag      */
#define SZ_LINE                 4096    /** handy size                        */

#define MAX_ATTR                100     /** max size of an attribute/value    */
#define HANDLE_INCREMENT        1024000 /** incr size of handle table         */


#ifdef  min
#undef  min
#endif
#define min(a,b)	((a<b)?a:b)

#ifdef  max
#undef  max
#endif
#define max(a,b)	((a>b)?a:b)




/**
 *  @brief  Handle type definition
 */
#ifndef  handle_t
#define  handle_t       int
#endif



/** 
 *  @struct AttrList.
 *  @brief 		Information for an attribute.
 *  @param name 	A string of the attributes name.
 *  @param value 	A string of the attributes value.
 *  @param *next 	A pointer to the next element.
 */
typedef struct {
    char   name[SZ_ATTRNAME];
    char   value[SZ_ATTRVAL];
    void  *next;
} AttrList;


/**
 *  @struct AttrBlock
 *  @brief 		Information for a block of attributes.
 *  @param req 		A '|' delimited string of required attribute names.
 *  @param opt 		A '|' delimited string of optional attribute names.
 *  @param attributes 	A pointer to an AttrList structure.
 */
typedef struct {
    char  *req;
    char  *opt;
    void  *attributes;
} AttrBlock;


/**
 *  @struct Element
 *
 *  @brief This is a structure that hold the information for an XML element.
 */
typedef struct elem_t {
    unsigned int type;        /** @brief   Type of element this is            	*/
    AttrBlock *attr;          /** @brief   A pointer to a block of attributes 	*/
    char *content;            /** @brief   Content of the tag elements        	*/
    int   isCData;            /** @brief   Is value a CDATA string?           	*/
    int   handle;             /** @brief   handle value			      	*/

    struct elem_t *next;      /** @brief   Ptr to the next element (sibling)  	*/
    struct elem_t *last_child;/** @brief   Ptr the the last child element     	*/
    struct elem_t *child;     /** @brief   Ptr the the child element          	*/
    struct elem_t *parent;    /** @brief   Ptr to the parent element          	*/

    char  **data;             /** @brief   Ptr to the data matrix             	*/

    unsigned char ref_count;  /** @brief   No. refrences to this Element      	*/
} Element;


/**
 *  @struct 	Node
 *  @brief 	Struct that holds a stack Node containing an Element
 */
typedef struct node {
    Element *element;
    void    *next;
} Node;


/**
 *  @struct 	Stack
 *  @brief 	This is a structure that holds the information for a stack
 */
typedef struct {
    Node *head;
    int   level;
} Stack;



/** ***************************************************************************
 *
 *  Public Internal Methods.  The procedures are used to implement the
 *  library, however are not part of the public interface.
 *
 ** **************************************************************************/

/*  votAttribute.c
 */
int  	 vot_attrSet (AttrBlock *ablock, char *name, char *value);
char    *vot_attrGet (AttrBlock *ablock, char *name);
char    *vot_attrXML (AttrBlock *ablock);

/*  votElement.c
 */
int 	 vot_eType (char *name);
char    *vot_elemName (Element *e);
int 	 vot_elemType (Element *e);
char    *vot_elemXML (Element *e);
char    *vot_elemXMLEnd (Element *e);
Element *vot_newElem (unsigned int type);

/*  votHandle.c
 */
handle_t  vot_setHandle (Element *elem);
handle_t  vot_lookupHandle (Element *elem);
void 	  vot_freeHandle (handle_t handle);
Element  *vot_getElement (handle_t handle);
void 	  vot_newHandleTable (void);
int       vot_handleCount (void);
void 	  vot_handleCleanup (void);
void      vot_handleError (char *msg);

/*  votParseCB.c
 */
void 	vot_endElement (void *userData, const char *name);
void  	vot_startElement (void *userData, const char *name, const char **atts);
void  	vot_charData (void *userData, const XML_Char *s, int len);
void  	vot_startCData (void *userData);
void  	vot_endCData (void *userData);

/*  votStack.c
 */
void 	 votPush (Stack *st, Element *elem);
Element *votPop (Stack *st);
Element *votPeek (Stack *st);

Stack   *vot_newStack (void);
int 	 vot_isEmpty (Stack *st);
void 	 vot_clearStack (Stack *st);
void 	 vot_printStack (Stack *st);
