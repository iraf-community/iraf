/*
 * QRAttribute.java
 * $ID*
 */

package dalclient;

import java.util.*;


/**
 * Query record attribute class.  Describes an attribute of a dataset
 * descriptor or table row.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public class QRAttribute {
    /** The value of the attribute, stored as a string. */
    String value;

    /** Create an initially empty attribute. */
    public QRAttribute() {
	value = "";
    }

    /**
     * Create an attribute and initialize the value string.
     *
     * @param  s	The value string
     */
    public QRAttribute(String s) {
	value = s;
    }

    /** Return the attribute value as a boolean. */
    public boolean boolValue() {
	return (new Boolean(value).booleanValue());
    }

    /** Return the attribute value as an integer. */
    public int intValue() {
	return (new Integer(value).intValue());
    }

    /** Return the attribute value as a float. */
    public float floatValue() {
	return (new Float(value).floatValue());
    }

    /** Return the attribute value as a double. */
    public double doubleValue() {
	return (new Double(value).doubleValue());
    }

    /** Return the attribute value as a String. */
    public String stringValue() {
	return (value);
    }
}
