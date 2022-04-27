DATA: lo_document TYPE REF TO if_ixml_document,
      lo_renderer TYPE REF TO if_ixml_renderer,
      lv_xml      TYPE xstring,
      lv_doc      TYPE string.

PERFORM init_dom CHANGING lo_document lo_renderer lv_xml.
PERFORM build_dom USING lo_document.
PERFORM create_xml_string USING lo_renderer CHANGING lv_doc.

FORM init_dom CHANGING eo_document TYPE REF TO if_ixml_document
                       eo_renderer TYPE REF TO if_ixml_renderer
                       ev_xml      TYPE xstring.

  DATA: lo_ixml           TYPE REF TO if_ixml,
        lo_stream_factory TYPE REF TO if_ixml_stream_factory,
        lo_ostream        TYPE REF TO if_ixml_ostream,
        lo_encoding       TYPE REF TO if_ixml_encoding.

**-- Create the Main Factory
  lo_ixml = cl_ixml=>create( ).

**-- Create the Initial Document
  eo_document = lo_ixml->create_document( ).

**-- Create an Output Stream
  lo_stream_factory = lo_ixml->create_stream_factory( ).
  lo_ostream = lo_stream_factory->create_ostream_xstring( string = ev_xml ).

**-- Create an Encoding
  lo_encoding = lo_ixml->create_encoding( byte_order    = if_ixml_encoding=>co_none
                                          character_set = 'UTF-8' ).

**-- Set the Encoding
  lo_ostream->set_encoding( encoding = lo_encoding ).

* Set Pretty Print
*  lo_ostream->set_pretty_print( pretty_print = abap_true ).

**-- Create a Renderer
  eo_renderer = lo_ixml->create_renderer( document = eo_document
                                          ostream  = lo_ostream ).

ENDFORM.

FORM build_dom USING io_doc TYPE REF TO if_ixml_document.

  DATA: lo_element            TYPE REF TO if_ixml_element,
        lo_start_process      TYPE REF TO if_ixml_element,
        lo_parameters         TYPE REF TO if_ixml_element,
        lo_process_parameters TYPE REF TO if_ixml_element,
        lo_workflow_parameter TYPE REF TO if_ixml_element.


  lo_element = io_doc->create_simple_element_ns( name   = 'Envelope'
                                                 parent = io_doc
                                                 prefix = 'soap' ).
  lo_element->set_attribute_ns( name   = 'xsi'
                                prefix = 'xmlns'
                                value  = 'http://www.w3.org/2001/XMLSchema-instance' ).
  lo_element->set_attribute_ns( name   = 'xsd'
                                prefix = 'xmlns'
                                value  = 'http://www.w3.org/2001/XMLSchema' ).
  lo_element->set_attribute_ns( name   = 'soap'
                                prefix = 'xmlns'
                                value  = 'http://schemas.xmlsoap.org/soap/envelope/' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Body'
                                                 parent = lo_element
                                                 prefix = 'soap' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'StartProcess'
                                                 parent = lo_element ).
  lo_element->set_attribute_ns( name   = 'xmlns'
                                value  = 'http://tempuri.org/' ).

  lo_start_process = io_doc->create_simple_element_ns( name   = 'StartProcess'
                                                       parent = lo_element ).

  lo_element = io_doc->create_simple_element_ns( name   = 'user'
                                                 parent = lo_start_process
                                                 value  = 'sapuser' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'password'
                                                 parent = lo_start_process
                                                 value  = '********' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'targetUser'
                                                 parent = lo_start_process
                                                 value  = 'sapuser' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'language'
                                                 parent = lo_start_process
                                                 value  = 'Turkish' ).

  lo_parameters = io_doc->create_simple_element_ns( name   = 'parameters'
                                                    parent = lo_start_process ).

  lo_process_parameters = io_doc->create_simple_element_ns( name   = 'ProcessParameters'
                                                            parent = lo_parameters ).

  lo_workflow_parameter = io_doc->create_simple_element_ns( name   = 'WorkflowParameter'
                                                            parent = lo_process_parameters ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Name'
                                                 parent = lo_workflow_parameter
                                                 value  = 'vrbJsonMetin1' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Value'
                                                 parent = lo_workflow_parameter
                                                 value  = '{"BELNR": "4500104971"}' ).

  lo_workflow_parameter = io_doc->create_simple_element_ns( name   = 'WorkflowParameter'
                                                            parent = lo_process_parameters ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Name'
                                                 parent = lo_workflow_parameter
                                                 value  = 'vrbJsonMetin2' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Value'
                                                 parent = lo_workflow_parameter
                                                 value  = '{"KTEXT": "CHIVAS REGAL 12Y 40% 1L"}' ).

  lo_workflow_parameter = io_doc->create_simple_element_ns( name   = 'WorkflowParameter'
                                                            parent = lo_process_parameters ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Name'
                                                 parent = lo_workflow_parameter
                                                 value  = 'vrbJsonMetin3' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Value'
                                                 parent = lo_workflow_parameter
                                                 value  = '{"LGORT": "3001"}' ).

  lo_element = io_doc->create_simple_element_ns( name   = 'Process'
                                                 parent = lo_parameters
                                                 value  = 'TedarikciSiparisTakipSureci' ).



ENDFORM.

FORM create_xml_string USING io_renderer TYPE REF TO if_ixml_renderer
                    CHANGING ev_doc TYPE string.

  DATA lo_conv TYPE REF TO cl_abap_conv_in_ce.

  io_renderer->render( ).

**--convert xstring to string
  lo_conv = cl_abap_conv_in_ce=>create( encoding    = 'UTF-8'
                                        endian      = 'L'
                                        ignore_cerr = 'X'
                                        replacement = '#'
                                        input       = lv_xml ).

  lo_conv->read( IMPORTING data = ev_doc ).

ENDFORM.

******************************************************************

*<?xml version="1.0" encoding="utf-8"?>
*<soap:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/">
*  <soap:Body>
*    <StartProcess xmlns="http://tempuri.org/">
*      <user>sapuser</user>
*      <password>********</password>
*      <targetUser>sapuser</targetUser>
*      <language>Turkish</language>
*      <parameters>
*        <ProcessParameters>
*          <WorkflowParameter>
*            <Name>vrbJsonMetin1</Name>
*            <Value>{"BELNR": "4500104971"}</Value>
*          </WorkflowParameter>
*          <WorkflowParameter>
*            <Name>vrbJsonMetin2</Name>
*            <Value>{"KTEXT": "CHIVAS REGAL 12Y 40% 1L"}</Value>
*          </WorkflowParameter>
*          <WorkflowParameter>
*            <Name>vrbJsonMetin3</Name>
*            <Value>{"LGORT": "3001"}</Value>
*          </WorkflowParameter>
*        </ProcessParameters>
*        <Process>TedarikciSiparisTakipSureci</Process>
*      </parameters>
*    </StartProcess>
*  </soap:Body>
*</soap:Envelope>