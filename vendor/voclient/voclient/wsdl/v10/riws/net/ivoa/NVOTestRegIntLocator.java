/**
 * NVOTestRegIntLocator.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2.1 Jun 14, 2005 (09:15:57 EDT) WSDL2Java emitter.
 */

package v10.riws.net.ivoa;

public class NVOTestRegIntLocator extends org.apache.axis.client.Service implements v10.riws.net.ivoa.NVOTestRegInt {

    public NVOTestRegIntLocator() {
    }


    public NVOTestRegIntLocator(org.apache.axis.EngineConfiguration config) {
        super(config);
    }

    public NVOTestRegIntLocator(java.lang.String wsdlLoc, javax.xml.namespace.QName sName) throws javax.xml.rpc.ServiceException {
        super(wsdlLoc, sName);
    }

    // Use to get a proxy class for NVOTestRegIntSoap12
    private java.lang.String NVOTestRegIntSoap12_address = "http://vaotest.stsci.edu/directory/NVORegInt.asmx";

    public java.lang.String getNVOTestRegIntSoap12Address() {
        return NVOTestRegIntSoap12_address;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String NVOTestRegIntSoap12WSDDServiceName = "NVOTestRegIntSoap12";

    public java.lang.String getNVOTestRegIntSoap12WSDDServiceName() {
        return NVOTestRegIntSoap12WSDDServiceName;
    }

    public void setNVOTestRegIntSoap12WSDDServiceName(java.lang.String name) {
        NVOTestRegIntSoap12WSDDServiceName = name;
    }

    public v10.riws.net.ivoa.NVOTestRegIntSoap getNVOTestRegIntSoap12() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
        try {
            endpoint = new java.net.URL(NVOTestRegIntSoap12_address);
        }
        catch (java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getNVOTestRegIntSoap12(endpoint);
    }

    public v10.riws.net.ivoa.NVOTestRegIntSoap getNVOTestRegIntSoap12(java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            v10.riws.net.ivoa.NVOTestRegIntSoap12Stub _stub = new v10.riws.net.ivoa.NVOTestRegIntSoap12Stub(portAddress, this);
            _stub.setPortName(getNVOTestRegIntSoap12WSDDServiceName());
            return _stub;
        }
        catch (org.apache.axis.AxisFault e) {
            return null;
        }
    }

    public void setNVOTestRegIntSoap12EndpointAddress(java.lang.String address) {
        NVOTestRegIntSoap12_address = address;
    }


    // Use to get a proxy class for NVOTestRegIntSoap
    private java.lang.String NVOTestRegIntSoap_address = "http://vaotest.stsci.edu/directory/NVORegInt.asmx";

    public java.lang.String getNVOTestRegIntSoapAddress() {
        return NVOTestRegIntSoap_address;
    }

    // The WSDD service name defaults to the port name.
    private java.lang.String NVOTestRegIntSoapWSDDServiceName = "NVOTestRegIntSoap";

    public java.lang.String getNVOTestRegIntSoapWSDDServiceName() {
        return NVOTestRegIntSoapWSDDServiceName;
    }

    public void setNVOTestRegIntSoapWSDDServiceName(java.lang.String name) {
        NVOTestRegIntSoapWSDDServiceName = name;
    }

    public v10.riws.net.ivoa.NVOTestRegIntSoap getNVOTestRegIntSoap() throws javax.xml.rpc.ServiceException {
       java.net.URL endpoint;
        try {
            endpoint = new java.net.URL(NVOTestRegIntSoap_address);
        }
        catch (java.net.MalformedURLException e) {
            throw new javax.xml.rpc.ServiceException(e);
        }
        return getNVOTestRegIntSoap(endpoint);
    }

    public v10.riws.net.ivoa.NVOTestRegIntSoap getNVOTestRegIntSoap(java.net.URL portAddress) throws javax.xml.rpc.ServiceException {
        try {
            v10.riws.net.ivoa.NVOTestRegIntSoapStub _stub = new v10.riws.net.ivoa.NVOTestRegIntSoapStub(portAddress, this);
            _stub.setPortName(getNVOTestRegIntSoapWSDDServiceName());
            return _stub;
        }
        catch (org.apache.axis.AxisFault e) {
            return null;
        }
    }

    public void setNVOTestRegIntSoapEndpointAddress(java.lang.String address) {
        NVOTestRegIntSoap_address = address;
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     * This service has multiple ports for a given interface;
     * the proxy implementation returned may be indeterminate.
     */
    public java.rmi.Remote getPort(Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        try {
            if (v10.riws.net.ivoa.NVOTestRegIntSoap.class.isAssignableFrom(serviceEndpointInterface)) {
                v10.riws.net.ivoa.NVOTestRegIntSoap12Stub _stub = new v10.riws.net.ivoa.NVOTestRegIntSoap12Stub(new java.net.URL(NVOTestRegIntSoap12_address), this);
                _stub.setPortName(getNVOTestRegIntSoap12WSDDServiceName());
                return _stub;
            }
            if (v10.riws.net.ivoa.NVOTestRegIntSoap.class.isAssignableFrom(serviceEndpointInterface)) {
                v10.riws.net.ivoa.NVOTestRegIntSoapStub _stub = new v10.riws.net.ivoa.NVOTestRegIntSoapStub(new java.net.URL(NVOTestRegIntSoap_address), this);
                _stub.setPortName(getNVOTestRegIntSoapWSDDServiceName());
                return _stub;
            }
        }
        catch (java.lang.Throwable t) {
            throw new javax.xml.rpc.ServiceException(t);
        }
        throw new javax.xml.rpc.ServiceException("There is no stub implementation for the interface:  " + (serviceEndpointInterface == null ? "null" : serviceEndpointInterface.getName()));
    }

    /**
     * For the given interface, get the stub implementation.
     * If this service has no port for the given interface,
     * then ServiceException is thrown.
     */
    public java.rmi.Remote getPort(javax.xml.namespace.QName portName, Class serviceEndpointInterface) throws javax.xml.rpc.ServiceException {
        if (portName == null) {
            return getPort(serviceEndpointInterface);
        }
        java.lang.String inputPortName = portName.getLocalPart();
        if ("NVOTestRegIntSoap12".equals(inputPortName)) {
            return getNVOTestRegIntSoap12();
        }
        else if ("NVOTestRegIntSoap".equals(inputPortName)) {
            return getNVOTestRegIntSoap();
        }
        else  {
            java.rmi.Remote _stub = getPort(serviceEndpointInterface);
            ((org.apache.axis.client.Stub) _stub).setPortName(portName);
            return _stub;
        }
    }

    public javax.xml.namespace.QName getServiceName() {
        return new javax.xml.namespace.QName("ivoa.net.riws.v10", "NVOTestRegInt");
    }

    private java.util.HashSet ports = null;

    public java.util.Iterator getPorts() {
        if (ports == null) {
            ports = new java.util.HashSet();
            ports.add(new javax.xml.namespace.QName("ivoa.net.riws.v10", "NVOTestRegIntSoap12"));
            ports.add(new javax.xml.namespace.QName("ivoa.net.riws.v10", "NVOTestRegIntSoap"));
        }
        return ports.iterator();
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(java.lang.String portName, java.lang.String address) throws javax.xml.rpc.ServiceException {
        
if ("NVOTestRegIntSoap12".equals(portName)) {
            setNVOTestRegIntSoap12EndpointAddress(address);
        }
        else 
if ("NVOTestRegIntSoap".equals(portName)) {
            setNVOTestRegIntSoapEndpointAddress(address);
        }
        else 
{ // Unknown Port Name
            throw new javax.xml.rpc.ServiceException(" Cannot set Endpoint Address for Unknown Port" + portName);
        }
    }

    /**
    * Set the endpoint address for the specified port name.
    */
    public void setEndpointAddress(javax.xml.namespace.QName portName, java.lang.String address) throws javax.xml.rpc.ServiceException {
        setEndpointAddress(portName.getLocalPart(), address);
    }

}
