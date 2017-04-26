/**
 * NVOTestRegIntSoap.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2.1 Jun 14, 2005 (09:15:57 EDT) WSDL2Java emitter.
 */

package v10.riws.net.ivoa;

public interface NVOTestRegIntSoap extends java.rmi.Remote {

    /**
     * <b>Input: predicate </b>Custom simple predicate search (placeholder
     * for ADQL imp)<br><b>Output:</b> VOTable 1.1 Unique Resource per Row
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTPredicate(java.lang.String predicate) throws java.rmi.RemoteException;

    /**
     * <b>Input: predicate </b>Custom simple predicate search (placeholder
     * for ADQL imp)<br><b>Input: VOTStyleOption </b> 1 = Unique Resource/Row,
     * 2 = Unique Interface/Row<br><b>Output:</b>VOTable 1.1 with Fields
     * set by VOTStyleOption
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTPredOpt(java.lang.String predicate, int VOTStyleOption) throws java.rmi.RemoteException;

    /**
     * <b>Input: standard capability </b>conesearch, SimpleImageAccess,
     * SimpleSpectralAccess<br><b>Output:</b> VOTable 1.1 Unique Resource
     * per Row
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapability(java.lang.String capability) throws java.rmi.RemoteException;

    /**
     * <b>Input: predicate </b>Custom simple predicate search (e.g.
     * title like '%galex%')<br><b>Input: standard capability </b>conesearch,
     * SimpleImageAccess, SimpleSpectralAccess<br><b>Output:</b> VOTable
     * 1.1 Unique Resource per Row
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapabilityPredicate(java.lang.String predicate, java.lang.String capability) throws java.rmi.RemoteException;

    /**
     * <b>Input: predicate </b>Custom simple predicate search (e.g.
     * title like '%galex%')<br><b>Input: standard capability </b>conesearch,
     * SimpleImageAccess, SimpleSpectralAccess<br><b>Input: VOTStyleOption
     * </b> 1 = Unique Resource/Row, 2 = Unique Interface/Row<br><b>Output:</b>VOTable
     * 1.1 with Fields set by VOTStyleOption
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapabilityPredOpt(java.lang.String predicate, java.lang.String capability, int VOTStyleOption) throws java.rmi.RemoteException;

    /**
     * <b>Input: predicate </b>Custom simple predicate search (e.g.
     * title like '%galex%')<br><b>Input: standard capability </b>conesearch,
     * SimpleImageAccess, SimpleSpectralAccess<br><b>Input: waveband </b>Optical,UV,x-ray,EUV,radio,Infrared,Gamma-ray,Millimeter<br><b>Input:
     * VOTStyleOption </b>1 = Unique Resource/Row, 2 = Unique Interface/Row<br><b>Output:</b>
     * VOTable 1.1 with Fields set by VOTStyleOption
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapBandPredOpt(java.lang.String predicate, java.lang.String capability, java.lang.String waveband, int VOTStyleOption) throws java.rmi.RemoteException;

    /**
     * <b>Input: keywords </b> enter text keywords (e.g. galex, redshift,
     * binary star,...)<br><b>Input: andKeys </b> true = AND, false = OR<br><b>Output:</b>
     * VOTable 1.1 Unique Resource per Row
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTKeyword(java.lang.String keywords, boolean andKeys) throws java.rmi.RemoteException;

    /**
     * <b>Input: keywords </b> enter text keywords (e.g. galex, redshift,
     * binary star,...)<br><b>Input: andKeys </b> true = AND, false = OR<br><b>Input:
     * VOTStyleOption </b>1 = Unique Resource/Row, 2 = Unique Interface/Row<br><b>Output:</b>
     * VOTable 1.1 with Fields set by VOTStyleOption
     */
    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTKeyOpt(java.lang.String keywords, boolean andKeys, int VOTStyleOption) throws java.rmi.RemoteException;
}
