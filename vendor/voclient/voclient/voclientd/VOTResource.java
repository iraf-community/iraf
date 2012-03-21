/**
 * VOTResource.java
 *
 */

package voclient;


/**
 *
 */
public class VOTResource implements Comparable {

    /*  Class data.
     */
    private java.lang.String[]   tags;
    private java.lang.String 	 shortName;
    private java.lang.String 	 title;
    private java.lang.String 	 description;
    private java.lang.String 	 publisher;
    private java.lang.String[]   waveband;
    private java.lang.String 	 identifier;
    private java.lang.String 	 updated;
    private java.lang.String[]   subject;
    private java.lang.String[]	 type;
    private java.lang.String[]   contentLevel;
    private java.lang.Integer 	 regionOfRegard;
    private java.lang.String 	 version;
    private java.lang.String 	 resourceID;
    private java.lang.String[]	 capabilityName;
    private java.lang.String[]	 capabilityClass;
    private java.lang.String[]	 capabilityStandardID;
    private java.lang.String[]	 capabilityValidationLevel;
    private java.lang.String[]	 interfaceClass;
    private java.lang.String[]	 interfaceVersion;
    private java.lang.String[]	 interfaceRole;
    private java.lang.String[]	 accessURL;
    private java.lang.String[]	 supportedInputParam;
    private java.lang.Integer[]	 maxRadius;
    private java.lang.Integer[]	 maxRecords;
    private java.lang.String 	 publisherID;
    private java.lang.String 	 referenceURL;

    private int   numCapabilities    = 0;
    private int   numInterfaces      = 0;
    private int   numStdCapabilities = 0;

    private int      index  	     = 0;
    private int      rank  	     = 0;
    private String[] sterms;

    private static final boolean  DEBUG        = false; 
    private static final boolean  VDEBUG       = false; 



    /*  Constructors
     */
    public VOTResource() {
    }

    public VOTResource(int index) {
	this.index = index;
    }

    public VOTResource(String[] search_terms) {
	int len = search_terms.length;
	this.sterms = new String[len];

	for (int i=0; i < len; i++)
	    sterms[i] = sterms[i].trim().toLowerCase();
	System.arraycopy (search_terms, 0, this.sterms, 0, len);
    }

    public VOTResource(int index, String[] search_terms) {
	int len = search_terms.length;
	this.sterms = new String[len];
	this.index = index;

	for (int i=0; i < len; i++)
	    sterms[i] = sterms[i].trim().toLowerCase();
	System.arraycopy (search_terms, 0, this.sterms, 0, len);
    }


    /*  Set/Get methods for the data.
     */

    /**
     *		Resource ranking
     */
    public int getIndex() { return index; 	}

    public int getRank()  { return rank; 	}

    public void setRank(String[] sterms)
    {
	int rank =  0, count;
	int len = sterms.length;
	this.sterms = new String[len];

	for (int i=0; i < len; i++)
	    sterms[i] = sterms[i].trim().toLowerCase();
	System.arraycopy (sterms, 0, this.sterms, 0, len);


        try {

	    /*
	    rank = rank + score (10, 2, true, this.title.split("[, #.:;]"));
	    rank = rank + score ( 3, 1, false, 
	    		this.description.split("[, #.:;]"));
	    */
	    rank = rank + score (10, 2, true,this.title.split("[, #.]"));
	    rank = rank + score ( 3, 1, false,this.description.split("[, #.]"));

	    rank = rank + score ( 2, 1, true, this.subject);
	    rank = rank + score ( 2, 1, true, this.waveband);
	    rank = rank + score ( 1, 1, false, this.subject);
	    if (this.type != null)
	        rank = rank + score (1, 0, false, this.type);
	    //if (this.capabilityStandardID.length > 0)
	    //    rank = rank + score ( 3, 1, this.capabilityStandardID);

	    if (VDEBUG)
		System.err.println (this.index + "  Resource rank="+rank);
            this.rank = rank;

        } catch (Exception ex) {
            System.err.println ("Exception in setRank(): " + ex.getMessage());
		;
        }
    }


    /** 
     *	Count the number of times any of the words in the 'sterms' occurs
     *  in the 'value' string.
     */
    private int score  (int full, int partial, boolean bonus, String[] value)
    {
	int score = 0;
	String val = null;
	int  slen=0, vlen=0, nmatch=0;


	if (value == null || value.length == 0)
	    return (0);

	for (int j=0; j < this.sterms.length; j++) {
	    slen = this.sterms[j].length();
	    nmatch = 0;

	    for (int i=0; i < value.length; i++) {
		val = value[i].trim().toLowerCase();
	        vlen = val.length();

		if (vlen == 0)
		    continue;

		if (vlen == slen) {
		    // Look for a full match of strings.
	            if (val.compareTo(this.sterms[j]) == 0) {
		        score += full;
			nmatch++;
		    }
		} else if (vlen < slen && vlen > 3) {
		    //  See if the value is a substring of the search word
	            if (this.sterms[j].indexOf(val) >= 0) {
		        score += partial;
			nmatch++;
		    }
		} else if (vlen > slen && vlen > 3) {
		    //  See if the search word is a substring of the value
	            if (val.indexOf(this.sterms[j]) >= 0) {
		        score += partial;
			nmatch++;
		    }
		}
	    }

	    //  Bonus points for matching all words.
	    if (nmatch == this.sterms.length && bonus)
	        score += (5 * full);
	}
	return (score);
    }



    /**
     *		Tags
     */
    public java.lang.String[] getTags() {
        return tags;
    }
    public void setTags(java.lang.String[] tags) {
        this.tags = tags;
    }


    /**
     *		ShortName
     */
    public java.lang.String getShortName() {
        return shortName;
    }
    public void setShortName(java.lang.String shortName) {
        this.shortName = shortName;
    }


    /**
     *		Title
     */
    public java.lang.String getTitle() {
        return title;
    }
    public void setTitle(java.lang.String title) {
        this.title = title;
    }


    /**
     *		Description
     */
    public java.lang.String getDescription() {
        return description;
    }
    public void setDescription(java.lang.String description) {
        this.description = description;
    }


    /**
     *		Publisher
     */
    public java.lang.String getPublisher() {
        return publisher;
    }
    public void setPublisher(java.lang.String publisher) {
        this.publisher = publisher;
    }


    /**
     *		Waveband
     */
    public java.lang.String[] getWaveband() {
        return waveband;
    }
    public void setWaveband(java.lang.String[] waveband) {
        this.waveband = waveband;
    }


    /**
     *		Identifier
     */
    public java.lang.String getIdentifier() {
        return identifier;
    }
    public void setIdentifier(java.lang.String identifier) {
        this.identifier = identifier;
    }


    /**
     *		Updated
     */
    public java.lang.String getUpdated() {
        return updated;
    }
    public void setUpdated(java.lang.String updated) {
        this.updated = updated;
    }


    /**
     *		Subject
     */
    public java.lang.String[] getSubject() {
        return subject;
    }
    public void setSubject(java.lang.String[] subject) {
        this.subject = subject;
    }


    /**
     *		Type
     */
    public java.lang.String[] getType() {
        return type;
    }
    public void setType(java.lang.String[] type) {
        this.type = type;
    }


    /**
     *		ContentLevel
     */
    public java.lang.String[] getContentLevel() {
        return contentLevel;
    }
    public void setContentLevel(java.lang.String[] contentLevel) {
        this.contentLevel = contentLevel;
    }


    /**
     *		RegionOfRegard
     */
    public java.lang.Integer getRegionOfRegard() {
        return regionOfRegard;
    }
    public void setRegionOfRegard(java.lang.Integer regionOfRegard) {
        this.regionOfRegard = regionOfRegard;
    }


    /**
     *		Version
     */
    public java.lang.String getVersion() {
        return version;
    }
    public void setVersion(java.lang.String version) {
        this.version = version;
    }


    /**
     *		ResourceID
     */
    public java.lang.String getResourceID() {
        return resourceID;
    }
    public void setResourceID(java.lang.String resourceID) {
        this.resourceID = resourceID;
    }


    /**
     *		CapabilityName
     */
    public java.lang.String[] getCapabilityName() {
        return capabilityName;
    }
    public void setCapabilityName(java.lang.String[] capabilityName) {
        this.capabilityName = capabilityName;
    }


    /**
     *		CapabilityClass
     */
    public java.lang.String[] getCapabilityClass() {
        return capabilityClass;
    }
    public void setCapabilityClass(java.lang.String[] capabilityClass) {
        this.capabilityClass = capabilityClass;
	if (this.capabilityClass != null)
            this.numCapabilities = capabilityClass.length;
	else
            this.numCapabilities = 0;
    }

    public int getNumInterfaces()	{ return numInterfaces; 	}
    public int getNumCapabilities()	{ return numCapabilities; 	}
    public int getNumStdCapabilities()	{ return numStdCapabilities; 	}


    /**
     *		CapabilityStandardID
     */
    public java.lang.String[] getCapabilityStandardID() {
        return capabilityStandardID;
    }
    public String getCapStdIDStr(int i) {
        return capabilityStandardID[i];
    }
    public void setCapabilityStandardID(java.lang.String[] capabilityStdID) {
        this.capabilityStandardID = capabilityStdID;

	if (capabilityStdID != null) {
	    for (int i=0; i < capabilityStdID.length; i++) {
	        if (capabilityStdID[i].contains("std")) {
		    this.numStdCapabilities += 1;
	        }
	    }
	}
    }


    /**
     *		CapabilityValidationLevel
     */
    public java.lang.String[] getCapabilityValidationLevel() {
        return capabilityValidationLevel;
    }
    public void setCapabilityValidationLevel(
	java.lang.String[] capabilityValidationLevel) {
            this.capabilityValidationLevel = capabilityValidationLevel;
    }


    /**
     *		InterfaceClass
     */
    public java.lang.String[] getInterfaceClass() {
        return interfaceClass;
    }
    public void setInterfaceClass(java.lang.String[] interfaceClass) {
        this.interfaceClass = interfaceClass;
	if (interfaceClass != null)
            this.numInterfaces = interfaceClass.length;
	else
            this.numInterfaces = 0;
    }


    /**
     *		InterfaceVersion
     */
    public java.lang.String[] getInterfaceVersion() {
        return interfaceVersion;
    }
    public void setInterfaceVersion(java.lang.String[] interfaceVersion) {
        this.interfaceVersion = interfaceVersion;
    }


    /**
     *		InterfaceRole
     */
    public java.lang.String[] getInterfaceRole() {
        return interfaceRole;
    }
    public void setInterfaceRole(java.lang.String[] interfaceRole) {
        this.interfaceRole = interfaceRole;
    }


    /**
     *		AccessURL
     */
    public java.lang.String[] getAccessURL() {
        return accessURL;
    }
    public void setAccessURL(java.lang.String[] accessURL) {
        this.accessURL = accessURL;
    }


    /**
     *		SupportedInputParams
     */
    public java.lang.String[] getSupportedInputParam() {
        return supportedInputParam;
    }
    public void setSupportedInputParam(java.lang.String[] supportedInputParam) {
        this.supportedInputParam = supportedInputParam;
    }


    /**
     *		MaxRadius
     */
    public java.lang.Integer[] getMaxRadius() {
        return maxRadius;
    }
    public void setMaxRadius(java.lang.Integer[] maxRadius) {
        this.maxRadius = maxRadius;
    }


    /**
     *		MaxRecords
     */
    public java.lang.Integer[] getMaxRecords() {
        return maxRecords;
    }
    public void setMaxRecords(java.lang.Integer[] maxRecords) {
        this.maxRecords = maxRecords;
    }


    /**
     *		PublisherID
     */
    public java.lang.String getPublisherID() {
        return publisherID;
    }
    public void setPublisherID(java.lang.String publisherID) {
        this.publisherID = publisherID;
    }


    /**
     *		ReferenceURL
     */
    public java.lang.String getReferenceURL() {
        return referenceURL;
    }
    public void setReferenceURL(java.lang.String referenceURL) {
        this.referenceURL = referenceURL;
    }

    /*  ------------------------------------------------------------------ */


    /**
     *	setByFieldName -- Set a VOTResource value given the field name.
     */
    public void setByFieldName (String field, String str)
    {

        if ( str.equals("") )
	    return;

    
	URLUTF8Encoder enc = new URLUTF8Encoder();
	String value = enc.encode (str);
 

        if ( field.equalsIgnoreCase("tags") ) {
             setTags (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("shortName") ) {
             setShortName (value);

        } else if ( field.equalsIgnoreCase("title") ) {
             setTitle (value);

        } else if ( field.equalsIgnoreCase("description") ) {
             setDescription (value);

        } else if ( field.equalsIgnoreCase("publisher") ) {
             setPublisher (value);

        } else if ( field.equalsIgnoreCase("waveband") ) {
             setWaveband (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("identifier") ) {
             setIdentifier (value);

        } else if ( field.equalsIgnoreCase("updated") ) {
             setUpdated (value);

        } else if ( field.equalsIgnoreCase("subject") ) {
             setSubject (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("type") ) {
             setType (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("contentLevel") ) {
             setContentLevel (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("regionOfRegard") ) {
             setRegionOfRegard (Integer.parseInt(value));

        } else if ( field.equalsIgnoreCase("version") ) {
             setVersion (value);

        } else if ( field.equalsIgnoreCase("capabilityName") ) {
             setCapabilityName (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("capabilityClass") ) {
             setCapabilityClass (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("capabilityStandardID") ) {
             setCapabilityStandardID (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("capabilityID") ) {
             setCapabilityStandardID (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("capabilityValidationLevel") ) {
             setCapabilityValidationLevel (splitStrValues(value));
	     if (value.charAt(0) == '#')
	         this.numCapabilities = countHashes (value.substring(1));
	     else
	         this.numCapabilities = 1;

        } else if ( field.equalsIgnoreCase("interfaceClass") ) {
             setInterfaceClass (splitStrValues(value));
	     if (value.charAt(0) == '#')
	         this.numInterfaces = countHashes (value.substring(1));
	     else
	         this.numCapabilities = 1;

        } else if ( field.equalsIgnoreCase("interfaceVersion") ) {
             setInterfaceVersion (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("interfaceRole") ) {
             setInterfaceRole (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("accessURL") ) {
             setAccessURL (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("supportedInputParam") ) {
             setSupportedInputParam (splitStrValues(value));

        } else if ( field.equalsIgnoreCase("maxRadius") ) {
             setMaxRadius (splitIntValues(value));

        } else if ( field.equalsIgnoreCase("maxRecords") ) {
             setMaxRecords (splitIntValues(value));

        } else if ( field.equalsIgnoreCase("publisherID") ) {
             setPublisherID (value);

        } else if ( field.equalsIgnoreCase("referenceURL") ) {
             setReferenceURL (value);
        }

	return;
    }


    /**
     *	splitStrValues -- Split a string of string values on the '#' char.
     */
    private String[] splitStrValues (String val)
    {
	String[] s;

	if (val == null)
	    return (null);

	if (val.charAt(0) == '#') 
	    s = val.substring(1).split("#");
	else
	    s = val.substring(0).split("#");

	return ( s );
    }


    private int countHashes (String s)
    {
	int len = s.length();
	int nhashes = 0;

	if (s != null) {
	    for (int i=0; i < len; i++) {
	        if (s.charAt(i) == '#')
		    nhashes++;
	    }

	    if (nhashes == 0 && s.charAt(0) != '#')
	        return (1);
	}

	return (nhashes);
    }


    /**
     *	splitIntValues -- Split a string of integer values on the '#' char.
     */
    private java.lang.Integer[] splitIntValues (String val)
    {
	String[] sa = splitStrValues (val);
//	if (sa == null || sa.length == 0)
//	    return (null);

	java.lang.Integer[] iary = new java.lang.Integer[sa.length];

        for (int i=0; i < sa.length; i++)
            iary[i] = ( sa[i].equals("") ? 0 : Integer.parseInt(sa[i]) );

	return ( iary );
    }


    /**
     *	expandResources -- Expand a VOTResource record to an array of
     *  individual records expanded for each capability.
     */
    public VOTResource[] expandResources () 
    {
	VOTResource[] vra = new VOTResource[numCapabilities];
 	int	len;

        try {

	  if (DEBUG) {
		System.err.println ("numCap = " + numCapabilities +
		    "    numInt = " + numInterfaces +
		    "    numStdCap = " + numStdCapabilities);
	  }

	  for (int i=0; i < numCapabilities; i++) {

	    if (DEBUG)
		System.err.println ("cap="+i+"  ivorn = " +this.identifier);

	    vra[i] = new VOTResource();

	    /* Copy the current record entries we want to be common.
	    */
            vra[i].tags                	     = this.tags;
            vra[i].shortName           	     = this.shortName;
            vra[i].title               	     = this.title;
            vra[i].description         	     = this.description;
            vra[i].publisher           	     = this.publisher;
            vra[i].waveband            	     = this.waveband;
            vra[i].identifier          	     = this.identifier;
            vra[i].updated             	     = this.updated;
            vra[i].subject             	     = this.subject;
            vra[i].type                	     = this.type;
            vra[i].contentLevel        	     = this.contentLevel;
            vra[i].version             	     = this.version;
            vra[i].regionOfRegard      	     = this.regionOfRegard;
            vra[i].supportedInputParam 	     = this.supportedInputParam;
            vra[i].publisherID         	     = this.publisherID;
            vra[i].referenceURL        	     = this.referenceURL;


	    /* Allocate the array values we'll be changing.
	    */
            vra[i].capabilityName            = new String[1];
            vra[i].capabilityClass           = new String[1];
            vra[i].capabilityStandardID      = new String[1];
            vra[i].capabilityValidationLevel = new String[1];
            vra[i].interfaceClass            = new String[1];
            vra[i].interfaceVersion          = new String[1];
            vra[i].interfaceRole             = new String[1];
            vra[i].accessURL                 = new String[1];
            vra[i].maxRadius                 = new java.lang.Integer[1];
            vra[i].maxRecords                = new java.lang.Integer[1];


	    /* Fill in with the current capability.
	    */

	    if (this.capabilityName != null) {
	        len = this.capabilityName.length;
	        if (len > 0 && i < len)
                    vra[i].capabilityName[0] = this.capabilityName[i];
	    }

	    if (this.capabilityClass != null) {
	        len = this.capabilityClass.length;
	        if (len > 0 && i < len)
                    vra[i].capabilityClass[0] = this.capabilityClass[i];
	    }

	    if (this.capabilityStandardID != null) {
	        len = this.capabilityStandardID.length;
	        if (len > 0 && i < len)
                    vra[i].capabilityStandardID[0] = 
			this.capabilityStandardID[i];
	    }

	    if (this.capabilityValidationLevel != null) {
	        len = this.capabilityValidationLevel.length;
	        if (len > 0 && i < len)
                    vra[i].capabilityValidationLevel[0] = 
			this.capabilityValidationLevel[i];
	    }

	    if (this.accessURL != null) {
	        len = this.accessURL.length;
	        if (len > 0 && i < len)
                    vra[i].accessURL[0] = this.accessURL[i];
	    }

	    if (this.interfaceClass != null) {
	        len = this.interfaceClass.length;
	        if (len > 0 && i < len)
                    vra[i].interfaceClass[0] = this.interfaceClass[i];
	    }

	    if (this.interfaceVersion != null) {
	        len = this.interfaceVersion.length;
	        if (len > 0 && i < len)
                    vra[i].interfaceVersion[0] = this.interfaceVersion[i];
	    }

	    if (this.interfaceRole != null) {
	        len = this.interfaceRole.length;
	        if (len > 0 && i < len)
                    vra[i].interfaceRole[0] = this.interfaceRole[i];
	    }

	    /*  --- FIXME  --------------------------------------------
	    if (this.maxRadius != null) {
	        len = this.maxRadius.length;
	        if (len > 0 && i < len)
                    vra[i].maxRadius[0] = this.maxRadius[i];
	    }

	    if (this.maxRecords != null) {
	        len = this.maxRecords.length;
	        if (len > 0 && i < len)
                    vra[i].maxRecords[0] = this.maxRecords[i];
	    }
	    //  --- FIXME  --------------------------------------------*/
	  }

        } catch (Exception ex) {
            System.err.println ("Exception in expandResources(): " +
                ex.getMessage());
            ex.printStackTrace();
        }

	return (vra);
    }


    private boolean __hashCodeCalc = false;
    public synchronized int hashCode() {

        if (__hashCodeCalc) {
            return 0;
        }

        __hashCodeCalc = true;
        int _hashCode = 1;


	if ( getTags() != null ) {
	    _hashCode += getTags().hashCode();
	}
	if ( getShortName() != null ) {
	    _hashCode += getShortName().hashCode();
	}
	if ( getTitle() != null ) {
	    _hashCode += getTitle().hashCode();
	}
	if ( getDescription() != null ) {
	    _hashCode += getDescription().hashCode();
	}
	if ( getPublisher() != null ) {
	    _hashCode += getPublisher().hashCode();
	}
	if ( getWaveband() != null ) {
	    _hashCode += getWaveband().hashCode();
	}
	if ( getIdentifier() != null ) {
	    _hashCode += getIdentifier().hashCode();
	}
	if ( getUpdated() != null ) {
	    _hashCode += getUpdated().hashCode();
	}
	if ( getSubject() != null ) {
	    _hashCode += getSubject().hashCode();
	}
	if ( getType() != null ) {
	    _hashCode += getType().hashCode();
	}
	if ( getContentLevel() != null ) {
	    _hashCode += getContentLevel().hashCode();
	}
	if ( getRegionOfRegard() != null ) {
	    _hashCode += getRegionOfRegard().hashCode();
	}
	if ( getVersion() != null ) {
	    _hashCode += getVersion().hashCode();
	}
	if ( getResourceID() != null ) {
	    _hashCode += getResourceID().hashCode();
	}
	if ( getCapabilityName() != null ) {
	    _hashCode += getCapabilityName().hashCode();
	}
	if ( getCapabilityClass() != null ) {
	    _hashCode += getCapabilityClass().hashCode();
	}
	if ( getCapabilityStandardID() != null ) {
	    _hashCode += getCapabilityStandardID().hashCode();
	}
	if ( getCapabilityValidationLevel() != null ) {
	    _hashCode += getCapabilityValidationLevel().hashCode();
	}
	if ( getInterfaceClass() != null ) {
	    _hashCode += getInterfaceClass().hashCode();
	}
	if ( getInterfaceVersion() != null ) {
	    _hashCode += getInterfaceVersion().hashCode();
	}
	if ( getInterfaceRole() != null ) {
	    _hashCode += getInterfaceRole().hashCode();
	}
	if ( getAccessURL() != null ) {
	    _hashCode += getAccessURL().hashCode();
	}
	if ( getSupportedInputParam() != null ) {
	    _hashCode += getSupportedInputParam().hashCode();
	}
	if ( getMaxRadius() != null ) {
	    _hashCode += getMaxRadius().hashCode();
	}
	if ( getMaxRecords() != null ) {
	    _hashCode += getMaxRecords().hashCode();
	}
	if ( getPublisherID() != null ) {
	    _hashCode += getPublisherID().hashCode();
	}
	if ( getReferenceURL() != null ) {
	    _hashCode += getReferenceURL().hashCode();
	}
        __hashCodeCalc = false;
        return _hashCode;
    }



    public int compareTo (Object o)
    {
        VOTResource that;

        that = (VOTResource) o;
        if (this.rank > that.rank)
             return -1;
        if (this.rank < that.rank)
             return  1;

        //return (Math.max (this.index,that.index));
        return ((this.index > that.index) ? -1 : 1);
    }





    /**
     * Provides a method to encode any string into a URL-safe
     * form.
     * Non-ASCII characters are first encoded as sequences of
     * two or three bytes, using the UTF-8 algorithm, before being
     * encoded as %HH escapes.
     *
     * Created: 17 April 1997
     * Author: Bert Bos <bert@w3.org>
     *
     * URLUTF8Encoder: http://www.w3.org/International/URLUTF8Encoder.java
     *
     * Copyright © 1997 World Wide Web Consortium, (Massachusetts
     * Institute of Technology, European Research Consortium for
     * Informatics and Mathematics, Keio University). All Rights Reserved. 
     * This work is distributed under the W3C® Software License [1] in the
     * hope that it will be useful, but WITHOUT ANY WARRANTY; without even
     * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     * PURPOSE.
     *
     * [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
     */

    private class URLUTF8Encoder
    {

      final String[] hex = {
        "%00", "%01", "%02", "%03", "%04", "%05", "%06", "%07",
        "%08", "%09", "%0a", "%0b", "%0c", "%0d", "%0e", "%0f",
        "%10", "%11", "%12", "%13", "%14", "%15", "%16", "%17",
        "%18", "%19", "%1a", "%1b", "%1c", "%1d", "%1e", "%1f",
        "%20", "%21", "%22", "%23", "%24", "%25", "%26", "%27",
        "%28", "%29", "%2a", "%2b", "%2c", "%2d", "%2e", "%2f",
        "%30", "%31", "%32", "%33", "%34", "%35", "%36", "%37",
        "%38", "%39", "%3a", "%3b", "%3c", "%3d", "%3e", "%3f",
        "%40", "%41", "%42", "%43", "%44", "%45", "%46", "%47",
        "%48", "%49", "%4a", "%4b", "%4c", "%4d", "%4e", "%4f",
        "%50", "%51", "%52", "%53", "%54", "%55", "%56", "%57",
        "%58", "%59", "%5a", "%5b", "%5c", "%5d", "%5e", "%5f",
        "%60", "%61", "%62", "%63", "%64", "%65", "%66", "%67",
        "%68", "%69", "%6a", "%6b", "%6c", "%6d", "%6e", "%6f",
        "%70", "%71", "%72", "%73", "%74", "%75", "%76", "%77",
        "%78", "%79", "%7a", "%7b", "%7c", "%7d", "%7e", "%7f",
        "%80", "%81", "%82", "%83", "%84", "%85", "%86", "%87",
        "%88", "%89", "%8a", "%8b", "%8c", "%8d", "%8e", "%8f",
        "%90", "%91", "%92", "%93", "%94", "%95", "%96", "%97",
        "%98", "%99", "%9a", "%9b", "%9c", "%9d", "%9e", "%9f",
        "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%a7",
        "%a8", "%a9", "%aa", "%ab", "%ac", "%ad", "%ae", "%af",
        "%b0", "%b1", "%b2", "%b3", "%b4", "%b5", "%b6", "%b7",
        "%b8", "%b9", "%ba", "%bb", "%bc", "%bd", "%be", "%bf",
        "%c0", "%c1", "%c2", "%c3", "%c4", "%c5", "%c6", "%c7",
        "%c8", "%c9", "%ca", "%cb", "%cc", "%cd", "%ce", "%cf",
        "%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7",
        "%d8", "%d9", "%da", "%db", "%dc", "%dd", "%de", "%df",
        "%e0", "%e1", "%e2", "%e3", "%e4", "%e5", "%e6", "%e7",
        "%e8", "%e9", "%ea", "%eb", "%ec", "%ed", "%ee", "%ef",
        "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",
        "%f8", "%f9", "%fa", "%fb", "%fc", "%fd", "%fe", "%ff"
      };
    
      /**
       * Encode a string to the "x-www-form-urlencoded" form, enhanced
       * with the UTF-8-in-URL proposal. This is what happens:
       *
       * <ul>
       * <li><p>The ASCII characters 'a' through 'z', 'A' through 'Z',
       *        and '0' through '9' remain the same.
       *
       * <li><p>The unreserved characters - _ . ! ~ * ' ( ) remain the same.
       *
       * <li><p>The space character ' ' is converted into a plus sign '+'.
       *
       * <li><p>All other ASCII characters are converted into the
       *        3-character string "%xy", where xy is
       *        the two-digit hexadecimal representation of the character
       *        code
       *
       * <li><p>All non-ASCII characters are encoded in two steps: first
       *        to a sequence of 2 or 3 bytes, using the UTF-8 algorithm;
       *        secondly each of these bytes is encoded as "%xx".
       * </ul>
       *
       * @param s The string to be encoded
       * @return The encoded string
       */
      public String encode(String s)
      {
        StringBuffer sbuf = new StringBuffer();
        int len = s.length();
        for (int i = 0; i < len; i++) {
          int ch = s.charAt(i);
          if ('A' <= ch && ch <= 'Z') {		// 'A'..'Z'
            sbuf.append((char)ch);
          } else if ('a' <= ch && ch <= 'z') {	// 'a'..'z'
    	       sbuf.append((char)ch);
          } else if ('0' <= ch && ch <= '9') {	// '0'..'9'
    	       sbuf.append((char)ch);
          } else if (ch == ' ') {			// space
    	       //sbuf.append('+');
    	       sbuf.append(' ');
          } else if (ch == '-' || ch == '_'		// unreserved
              || ch == '.' || ch == '!'
              || ch == '~' || ch == '*'
              || ch == '\'' || ch == '('
              || ch == ')') {
            sbuf.append((char)ch);
          } else if (ch <= 0x007f) {		// other ASCII
    	       //sbuf.append(hex[ch]);
    	       sbuf.append((char)ch);
          } else if (ch <= 0x07FF) {		// non-ASCII <= 0x7FF
    	       sbuf.append(hex[0xc0 | (ch >> 6)]);
    	       sbuf.append(hex[0x80 | (ch & 0x3F)]);
          } else {					// 0x7FF < ch <= 0xFFFF
    	       sbuf.append(hex[0xe0 | (ch >> 12)]);
    	       sbuf.append(hex[0x80 | ((ch >> 6) & 0x3F)]);
    	       sbuf.append(hex[0x80 | (ch & 0x3F)]);
          }
        }
        return sbuf.toString();
      }

    }
}
