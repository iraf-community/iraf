/**
 * NVOTestRegIntSoap12Stub.java
 *
 * This file was auto-generated from WSDL
 * by the Apache Axis 1.2.1 Jun 14, 2005 (09:15:57 EDT) WSDL2Java emitter.
 */

package v10.riws.net.ivoa;

public class NVOTestRegIntSoap12Stub extends org.apache.axis.client.Stub implements v10.riws.net.ivoa.NVOTestRegIntSoap {
    private java.util.Vector cachedSerClasses = new java.util.Vector();
    private java.util.Vector cachedSerQNames = new java.util.Vector();
    private java.util.Vector cachedSerFactories = new java.util.Vector();
    private java.util.Vector cachedDeserFactories = new java.util.Vector();

    static org.apache.axis.description.OperationDesc [] _operations;

    static {
        _operations = new org.apache.axis.description.OperationDesc[8];
        _initOperationDesc1();
    }

    private static void _initOperationDesc1(){
        org.apache.axis.description.OperationDesc oper;
        org.apache.axis.description.ParameterDesc param;
        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTPredicate");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "predicate"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTPredicateResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[0] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTPredOpt");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "predicate"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTStyleOption"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"), int.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTPredOptResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[1] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTCapability");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "capability"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTCapabilityResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[2] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTCapabilityPredicate");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "predicate"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "capability"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTCapabilityPredicateResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[3] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTCapabilityPredOpt");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "predicate"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "capability"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTStyleOption"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"), int.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTCapabilityPredOptResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[4] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTCapBandPredOpt");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "predicate"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "capability"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "waveband"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTStyleOption"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"), int.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTCapBandPredOptResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[5] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTKeyword");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "keywords"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "andKeys"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "boolean"), boolean.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTKeywordResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[6] = oper;

        oper = new org.apache.axis.description.OperationDesc();
        oper.setName("VOTKeyOpt");
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "keywords"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "string"), java.lang.String.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "andKeys"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "boolean"), boolean.class, false, false);
        oper.addParameter(param);
        param = new org.apache.axis.description.ParameterDesc(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTStyleOption"), org.apache.axis.description.ParameterDesc.IN, new javax.xml.namespace.QName("http://www.w3.org/2001/XMLSchema", "int"), int.class, false, false);
        oper.addParameter(param);
        oper.setReturnType(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE"));
        oper.setReturnClass(net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
        oper.setReturnQName(new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTKeyOptResult"));
        oper.setStyle(org.apache.axis.constants.Style.WRAPPED);
        oper.setUse(org.apache.axis.constants.Use.LITERAL);
        _operations[7] = oper;

    }

    public NVOTestRegIntSoap12Stub() throws org.apache.axis.AxisFault {
         this(null);
    }

    public NVOTestRegIntSoap12Stub(java.net.URL endpointURL, javax.xml.rpc.Service service) throws org.apache.axis.AxisFault {
         this(service);
         super.cachedEndpoint = endpointURL;
    }

    public NVOTestRegIntSoap12Stub(javax.xml.rpc.Service service) throws org.apache.axis.AxisFault {
        if (service == null) {
            super.service = new org.apache.axis.client.Service();
        } else {
            super.service = service;
        }
        ((org.apache.axis.client.Service)super.service).setTypeMappingVersion("1.2");
            java.lang.Class cls;
            javax.xml.namespace.QName qName;
            javax.xml.namespace.QName qName2;
            java.lang.Class beansf = org.apache.axis.encoding.ser.BeanSerializerFactory.class;
            java.lang.Class beandf = org.apache.axis.encoding.ser.BeanDeserializerFactory.class;
            java.lang.Class enumsf = org.apache.axis.encoding.ser.EnumSerializerFactory.class;
            java.lang.Class enumdf = org.apache.axis.encoding.ser.EnumDeserializerFactory.class;
            java.lang.Class arraysf = org.apache.axis.encoding.ser.ArraySerializerFactory.class;
            java.lang.Class arraydf = org.apache.axis.encoding.ser.ArrayDeserializerFactory.class;
            java.lang.Class simplesf = org.apache.axis.encoding.ser.SimpleSerializerFactory.class;
            java.lang.Class simpledf = org.apache.axis.encoding.ser.SimpleDeserializerFactory.class;
            java.lang.Class simplelistsf = org.apache.axis.encoding.ser.SimpleListSerializerFactory.class;
            java.lang.Class simplelistdf = org.apache.axis.encoding.ser.SimpleListDeserializerFactory.class;
            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">CoordinateSystem>system");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.CoordinateSystemSystem.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Field>type");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.FieldType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Link>content-role");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.LinkContentRole.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Resource>type");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.ResourceType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Stream>actuate");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.StreamActuate.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Stream>type");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.StreamType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">Values>type");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.ValuesType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", ">VOTABLE>version");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.VOTABLEVersion.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "anyTEXT");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.AnyTEXT.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "ArrayOfChoice1");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.ArrayOfChoice1.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Binary");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Binary.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "CoordinateSystem");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.CoordinateSystem.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(simplesf);
            cachedDeserFactories.add(simpledf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Data");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Data.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "dataType");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.DataType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "encodingType");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.EncodingType.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Field");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Field.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "FieldRef");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.FieldRef.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "FITS");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.FITS.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Group");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Group.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Info");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Info.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(simplesf);
            cachedDeserFactories.add(simpledf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Link");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Link.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Max");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Max.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Min");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Min.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Option");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Option.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Param");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Param.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "ParamRef");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.ParamRef.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Resource");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Resource.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Stream");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Stream.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(simplesf);
            cachedDeserFactories.add(simpledf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Table");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Table.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "TableData");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.TableData.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Td");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Td.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(simplesf);
            cachedDeserFactories.add(simpledf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Tr");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Tr.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "Values");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Values.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "VOTABLE");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(beansf);
            cachedDeserFactories.add(beandf);

            qName = new javax.xml.namespace.QName("http://www.ivoa.net/xml/VOTable/v1.1", "yesno");
            cachedSerQNames.add(qName);
            cls = net.ivoa.www.xml.VOTable.v1_1.Yesno.class;
            cachedSerClasses.add(cls);
            cachedSerFactories.add(enumsf);
            cachedDeserFactories.add(enumdf);

    }

    protected org.apache.axis.client.Call createCall() throws java.rmi.RemoteException {
        try {
            org.apache.axis.client.Call _call = super._createCall();
            if (super.maintainSessionSet) {
                _call.setMaintainSession(super.maintainSession);
            }
            if (super.cachedUsername != null) {
                _call.setUsername(super.cachedUsername);
            }
            if (super.cachedPassword != null) {
                _call.setPassword(super.cachedPassword);
            }
            if (super.cachedEndpoint != null) {
                _call.setTargetEndpointAddress(super.cachedEndpoint);
            }
            if (super.cachedTimeout != null) {
                _call.setTimeout(super.cachedTimeout);
            }
            if (super.cachedPortName != null) {
                _call.setPortName(super.cachedPortName);
            }
            java.util.Enumeration keys = super.cachedProperties.keys();
            while (keys.hasMoreElements()) {
                java.lang.String key = (java.lang.String) keys.nextElement();
                _call.setProperty(key, super.cachedProperties.get(key));
            }
            // All the type mapping information is registered
            // when the first call is made.
            // The type mapping information is actually registered in
            // the TypeMappingRegistry of the service, which
            // is the reason why registration is only needed for the first call.
            synchronized (this) {
                if (firstCall()) {
                    // must set encoding style before registering serializers
                    _call.setEncodingStyle(null);
                    for (int i = 0; i < cachedSerFactories.size(); ++i) {
                        java.lang.Class cls = (java.lang.Class) cachedSerClasses.get(i);
                        javax.xml.namespace.QName qName =
                                (javax.xml.namespace.QName) cachedSerQNames.get(i);
                        java.lang.Object x = cachedSerFactories.get(i);
                        if (x instanceof Class) {
                            java.lang.Class sf = (java.lang.Class)
                                 cachedSerFactories.get(i);
                            java.lang.Class df = (java.lang.Class)
                                 cachedDeserFactories.get(i);
                            _call.registerTypeMapping(cls, qName, sf, df, false);
                        }
                        else if (x instanceof javax.xml.rpc.encoding.SerializerFactory) {
                            org.apache.axis.encoding.SerializerFactory sf = (org.apache.axis.encoding.SerializerFactory)
                                 cachedSerFactories.get(i);
                            org.apache.axis.encoding.DeserializerFactory df = (org.apache.axis.encoding.DeserializerFactory)
                                 cachedDeserFactories.get(i);
                            _call.registerTypeMapping(cls, qName, sf, df, false);
                        }
                    }
                }
            }
            return _call;
        }
        catch (java.lang.Throwable _t) {
            throw new org.apache.axis.AxisFault("Failure trying to get the Call object", _t);
        }
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTPredicate(java.lang.String predicate) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[0]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTPredicate");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTPredicate"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {predicate});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTPredOpt(java.lang.String predicate, int VOTStyleOption) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[1]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTPredOpt");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTPredOpt"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {predicate, new java.lang.Integer(VOTStyleOption)});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapability(java.lang.String capability) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[2]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTCapability");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTCapability"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {capability});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapabilityPredicate(java.lang.String predicate, java.lang.String capability) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[3]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTCapabilityPredicate");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTCapabilityPredicate"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {predicate, capability});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapabilityPredOpt(java.lang.String predicate, java.lang.String capability, int VOTStyleOption) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[4]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTCapabilityPredOpt");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTCapabilityPredOpt"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {predicate, capability, new java.lang.Integer(VOTStyleOption)});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTCapBandPredOpt(java.lang.String predicate, java.lang.String capability, java.lang.String waveband, int VOTStyleOption) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[5]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTCapBandPredOpt");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTCapBandPredOpt"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {predicate, capability, waveband, new java.lang.Integer(VOTStyleOption)});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTKeyword(java.lang.String keywords, boolean andKeys) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[6]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTKeyword");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTKeyword"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {keywords, new java.lang.Boolean(andKeys)});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

    public net.ivoa.www.xml.VOTable.v1_1.VOTABLE VOTKeyOpt(java.lang.String keywords, boolean andKeys, int VOTStyleOption) throws java.rmi.RemoteException {
        if (super.cachedEndpoint == null) {
            throw new org.apache.axis.NoEndPointException();
        }
        org.apache.axis.client.Call _call = createCall();
        _call.setOperation(_operations[7]);
        _call.setUseSOAPAction(true);
        _call.setSOAPActionURI("ivoa.net.riws.v10/VOTKeyOpt");
        _call.setEncodingStyle(null);
        _call.setProperty(org.apache.axis.client.Call.SEND_TYPE_ATTR, Boolean.FALSE);
        _call.setProperty(org.apache.axis.AxisEngine.PROP_DOMULTIREFS, Boolean.FALSE);
        _call.setSOAPVersion(org.apache.axis.soap.SOAPConstants.SOAP12_CONSTANTS);
        _call.setOperationName(new javax.xml.namespace.QName("ivoa.net.riws.v10", "VOTKeyOpt"));

        setRequestHeaders(_call);
        setAttachments(_call);
 try {        java.lang.Object _resp = _call.invoke(new java.lang.Object[] {keywords, new java.lang.Boolean(andKeys), new java.lang.Integer(VOTStyleOption)});

        if (_resp instanceof java.rmi.RemoteException) {
            throw (java.rmi.RemoteException)_resp;
        }
        else {
            extractAttachments(_call);
            try {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) _resp;
            } catch (java.lang.Exception _exception) {
                return (net.ivoa.www.xml.VOTable.v1_1.VOTABLE) org.apache.axis.utils.JavaUtils.convert(_resp, net.ivoa.www.xml.VOTable.v1_1.VOTABLE.class);
            }
        }
  } catch (org.apache.axis.AxisFault axisFaultException) {
  throw axisFaultException;
}
    }

}
