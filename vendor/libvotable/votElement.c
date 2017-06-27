/**
 *  VOTELEMENT.C -- (Private) Method to manage XML elements.
 *
 *  @file       votElement.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      (Private) Methods to manage XML elements.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include "votParseP.h"
#include "votParse.h"

static void vot_setDefaultAttrs (AttrBlock *ablock);

struct {
    int  type;		/** element type		*/
    char *name;		/** element name		*/
} elemTypes[] = {
    { TY_ROOT,		"ROOT"		},
    { TY_VOTABLE,	"VOTABLE"	},
    { TY_RESOURCE,	"RESOURCE"	},
    { TY_FIELD,		"FIELD"		},
    { TY_PARAM,		"PARAM"		},
    { TY_INFO,		"INFO"		},
    { TY_TR,		"TR"		},
    { TY_TD,		"TD"		},
    { TY_TABLE,		"TABLE"		},
    { TY_TABLEDATA,	"TABLEDATA"	},
    { TY_DATA,		"DATA"		},
    { TY_STREAM,	"STREAM"	},
    { TY_FITS,		"FITS"		},
    { TY_GROUP,		"GROUP"		},
    { TY_FIELDREF,	"FIELDREF"	},
    { TY_PARAMREF,	"PARAMREF"	},
    { TY_MIN,		"MIN"		},
    { TY_MAX,		"MAX"		},
    { TY_OPTION,	"OPTION"	},
    { TY_VALUES,	"VALUES"	},
    { TY_LINK,		"LINK"		},
    { TY_COOSYS,	"COOSYS"	},
    { TY_DESCRIPTION,	"DESCRIPTION"	},
    { TY_DEFINITIONS,	"DEFINITIONS"	},
    { -1,		NULL		}
};


/**
 *  Definition of Required and Optional attributes of VOTable elements.
 */
struct {
    int  type;		/** element type		*/
    char *req;		/** required attrs		*/
    char *opt;		/** optional attrs		*/
} elemAttrs[] = {
 { TY_ROOT, 	"", 
	 	""							    },
 { TY_VOTABLE, 	"", 
	 	"ID|version|"						    },
 { TY_RESOURCE, "",
	 	"ID|name|type|utype|"					    },
 { TY_TABLE, 	"",
	 	"ID|name|ucd|utype|ref|nrows|ncols|"			    },
 { TY_INFO, 	"name|value|",
	 	"ID|unit|ucd|utype|ref|"				    },
 { TY_STREAM, 	"",
	 	"type|href|actuate|encoding|expires|rights|serialization"   },
 { TY_FITS, 	"",
	 	"extnum|"						    },
 { TY_TD, 	"",
	 	"encoding|serialization"				    },
 { TY_TR, 	"",
	 	""							    },
 { TY_COOSYS, 	"",
	 	"ID|equinox|epoch|system|"				    },
 { TY_DESCRIPTION, "",
	 	""							    },
 { TY_DEFINITIONS, "",
	 	""							    },
 { TY_DATA, 	"",
	 	""							    },
 { TY_TABLEDATA,"",
	 	"nrows|ncols"						    },
 { TY_GROUP, 	"",
	 	"ID|name|ucd|utype|ref|"				    },
 { TY_PARAM, 	"datatype|name|value|",
	 	"ID|unit|ucd|utype|ref|precision|width|arraysize|"	    },
 { TY_FIELD, 	"datatype|name|type|xtype|",
	 	"ID|unit|ucd|utype|ref|precision|width|arraysize|" 	    },
 { TY_FIELDREF, "ref|",
	 	"ucd|utype"						    },
 { TY_PARAMREF, "ref|",
	 	"ucd|utype"						    },
 { TY_MIN, 	"value|",
	 	"inclusive|"						    },
 { TY_MAX, 	"value|",
	 	"inclusive|"						    },
 { TY_OPTION, 	"value|",
	 	"name|"							    },
 { TY_VALUES, 	"",
	 	"ID|type|null|ref|"					    },
 { TY_LINK, 	"action|",
	 	"ID|content-role|content-type|title|value|href|"	    },
  { -1,		NULL,		NULL		}
};



/** 
 *  vot_elemType -- Get the integer value (ID) of the Element (private method)
 *
 *  @brief  Get the integer value (ID) of the Element (private method)
 *  @fn     int vot_elemType (Element *e)
 *
 *  @param  e 		A pointer to the Element that you want the type of
 *  @return 		An integer corresponding to the type of the element
 */
int 
vot_elemType (Element *e)
{
    return ( (e == NULL ? -1 : e->type) );
}


/** 
 *  vot_elemName -- Get the name of the Element (private method).
 *
 *  @brief  Get the name of the Element (private method)
 *  @fn     char *vot_elemName  (Element *e)
 *
 *  @param  *e 		A pointer to the Element that you want the name of
 *  @return 		A string pointer to the name of the element
 */
char *
vot_elemName  (Element *e)
{
    register int i;
    
    for (i=0; elemTypes[i].type >= 0; i++) {
        if (e->type == elemTypes[i].type)
            return (elemTypes[i].name);
    }
    return (NULL);
}


/** 
 *  vot_eType -- Get the integer value (ID) of the name (private method).
 *
 *  @brief  Get the integer value (ID) of the name (private method)
 *  @fn     int vot_eType (char *name)
 *
 *  @param  name 	Name of the desired type
 *  @return 		An integer corresponding to the type of the element
 */
int
vot_eType (char *name)
{
    register  int  i;

    for (i=0; elemTypes[i].type >= 0; i++) {
        if (strcasecmp (name, elemTypes[i].name) == 0)
            return (elemTypes[i].type);
    }
    return (-1);
}


/** 
 *  vot_elemXMLEnd -- Build a string of the ending XML Tag (private method)
 *
 *  @brief  Build a string of the ending XML Tag (private method)
 *  @fn     char *vot_elemXMLEnd (Element *e)
 *
 *  @param  *e 		A pointer to an Element
 *  @return 		A string that contains the ending XML tag for e
 */
char *
vot_elemXMLEnd  (Element *e)
{
    char *XML_out = (char *) calloc (SZ_XMLTAG, sizeof (char));
    
    sprintf (XML_out, "</%s>", vot_elemName (e));
    return (XML_out);
}


/** 
 *  vot_elemXML -- Builds a string of the opening XML Tag (private method)
 *
 *  @brief  Builds a string of the opening XML Tag (private method)
 *  @fn     char *vot_elemXML (Element *e)
 *
 *  @param  *e 		A pointer to an Element
 *  @return 		A string that contains the opening XML tag for e
 */

#define outstr(s)	strcat(XML_out,s);
#define outattr(a,s)	{outstr(a);outstr(s);outstr("\"");}

char *
vot_elemXML (Element *e)
{
    char *XML_out = (char *) calloc (SZ_XMLTAG, sizeof (char));
    char *name = vot_elemName (e);

    
    outstr ("<");
    outstr (name);
    if (strcasecmp (name, "VOTABLE") == 0) {
	outattr (" version=\"", VOT_DOC_VERSION);
	outattr (" xmlns:xsi=\"", VOT_XSI);
	outattr (" xsi:schemaLocation=\"", VOT_SCHEMA_LOC);
	outattr (" xmlns=\"", VOT_XMLNS);
    } else
        outstr (vot_attrXML (e->attr));
    outstr (">");
    
    return (XML_out);
}


/** 
 *  vot_newElem -- Allocate a new structure of the given type (private method)
 *
 *  @brief  Allocate a new structure of the given type (private method)
 *  @fn     Element *vot_newElem (unsigned int type)
 *
 *  @param  type 	An integer that defines the type of Element
 *  @return 		An new Element structure
 */

Element *
vot_newElem (unsigned int type)
{
    register  int  i;
    Element   *new;
    
    
    new             = (Element *) calloc (1, sizeof (Element));
    new->attr       = (AttrBlock *) calloc (1, sizeof (AttrBlock));
    new->type       = type;
    
    for (i=0; elemTypes[i].type >= 0; i++) {
        if (type == elemAttrs[i].type) {
            new->attr->req = elemAttrs[i].req;
            new->attr->opt = elemAttrs[i].opt;
            vot_setDefaultAttrs (new->attr);
	    new->handle = -1;
            return (new);
        }
    }

    free ((void *) new->attr);
    free ((void *) new);

    return ((Element *) NULL);
}


/**
 *  vot_setDefaultAttrs -- Create all required attributes
 *
 *  @brief   Create all required attributes (static private method)
 *  @fn      vot_setDefaultAttrs (AttrBlock *ablock)
 *
 *  @param   attr 	An AttrBlock to insert these attributes.
 *  @returns		Nothing
 */
static void
vot_setDefaultAttrs (AttrBlock *ablock)
{
    char  req_attr[MAX_ATTR], *tok = req_attr, *name;

    if (ablock->req) {
        memset (req_attr, 0, MAX_ATTR);
        strcpy (req_attr, ablock->req);

        while ((name = strtok (tok, "|")) != NULL) {
            tok = NULL;
	    if (strcasecmp ("datatype", name) == 0) {
		/*
                vot_attrSet (ablock, "arraysize", "*");
		*/
                vot_attrSet (ablock, name, "char");
	    } else
                vot_attrSet (ablock, name, "");
        }
    }
}
