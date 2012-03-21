/*
 * QueryRecord.java
 * $ID*
 */

package dalclient;

import java.util.*;
import java.net.*;
import java.io.*;


/**
 * Query record class.  Holds a table row or dataset descriptor containing
 * a set of keyword=value pairs for the record.  For a dataset, the keywords
 * are the data model attributes for the dataset descriptor.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public class QueryRecord {
    /** The keywords are stored as a hash table indexed by the keyword name. */
    LinkedHashMap params = new LinkedHashMap();

    /**
     * Create a new QueryRecord using the given keyword map.
     *
     * @param  m	The map containing the map describing the row
     */
    public QueryRecord(LinkedHashMap m) {
	params = m;
    }

    /** Get the number of fields in the query response record. */
    public int getAttributeCount() {
	return (params.size());
    }

    /**
     * Get an attribute given the corresponding keyword name.
     *
     * @param  key	The attribute name
     */
    public QRAttribute getAttribute(String key) {
	String value = null;

	if (key.equalsIgnoreCase("ra") || key.equalsIgnoreCase("dec")) {
	    String pos = (String) params.get("pos");	// get Pos attr
	    if (pos != null) {
	        String p[] = pos.split(" ");		// split the value
	        value = (String) (key.equalsIgnoreCase("ra") ? p[0] : p[1]);
	    } else {
		//  For SIAP and other older services.
	        value = (String) params.get(key.toLowerCase());
	    }

	} else {
	    value = (String) params.get(key.toLowerCase());
	}
	return ((value == null) ? null : new QRAttribute(value.trim()));
    }

    /** Get the map itself. */
    public LinkedHashMap getMap() {
	return (params);
    }

    /**
     * Fetch the dataset referenced by the access reference and store the
     * returned object in a file, using an automatically generated filename.
     * By default we try to create the file in the current working directory.
     * We try to guess the file extension from the "Format" dataset attribute,
     * if given, otherwise a ".tmp" filename is used.
     */
    public String getDataset() throws Exception {
	QRAttribute v = getAttribute("Format");
	File cwd = new File(".");
	String suffix = null;

	if (v != null) {
	    String s = v.stringValue();
	    int last = s.lastIndexOf('/');
	    if (last > 0)
		suffix = "." + s.substring(last + 1);
	}

	File path = cwd.createTempFile("data", suffix, cwd);
	return (getDataset(path.toString()) ? path.toString() : null);
    }

    /**
     * Get the dataset pointed to by the AccessReference attribute and store
     * the returned object in the specified filename.  Returns true if the
     * download is successful, false if there is no AccessReference attribute
     * for the dataset.
     *
     * @param  path	The output file pathname
     */
    public boolean getDataset(String path) throws Exception {
	QRAttribute v = getAttribute("AccessReference");
	if (v == null)
	    return (false);

	readURL(v.stringValue(), path);
	return (true);
    }

    /**
     * ReadURL -- Copy the contents of a URL to the named output file as a 
     * binary stream.
     *
     * @param  url	The URL to be read
     * @param  path	The output file pathname
     */
    public long readURL(String url, String path) throws Exception {
	URL link = new URL(url);
	InputStream in = link.openStream();
	FileOutputStream out = new FileOutputStream(path);

	byte buf[] = new byte[4096];  int n;  long size;
	for (size=0;  (n = in.read(buf, 0, buf.length)) > 0;  size += n)
	    out.write(buf, 0, n);

	return (size);
    }

    /**
     * ReadURL -- Copy the contents of a URL to the named output file as a 
     * binary stream.
     *
     * @param  url	The URL to be read
     * @param  path	The output file pathname
     */
    public long readURL(String url) throws Exception {
	URL link = new URL(url);
	InputStream in = link.openStream();

	byte buf[] = new byte[4096];  int n;  long size;
	for (size=0;  (n = in.read(buf, 0, buf.length)) > 0;  size += n)
	    System.out.write(buf, 0, n);

	return (size);
    }
}
