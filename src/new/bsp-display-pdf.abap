*** Event Handler ***
        DATA: lt_content      TYPE cms_tab_txt_tline,
              lv_bin_file     TYPE xstring,
              lv_bin_filesize TYPE i.

        FIELD-SYMBOLS: <ls_0128> LIKE LINE OF lt_0128_ext.

        table ?= cl_htmlb_manager=>get_data( request = request
                                             name    = 'tableView'
                                             id      = 'LT_0128_EXT' ).
        table_event ?= table->data.

        READ TABLE lt_0128_ext ASSIGNING <ls_0128>
            INDEX table_event->selectedrowindex.
        IF sy-subrc = 0.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              id                      = <ls_0128>-txtid
              language                = <ls_0128>-sprsl
              name                    = <ls_0128>-obnam
              object                  = <ls_0128>-objct
            TABLES
              lines                   = lt_content
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.
        ENDIF.
			
  DATA: lv_fm_name            TYPE rs38l_fnam,
        ls_control_parameters TYPE ssfctrlop,
        ls_output_options     TYPE ssfcompop,
        ls_job_output_info    TYPE ssfcrescl,
        lt_lines              TYPE TABLE OF tline.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZHR_BILDIRIM_MEKTUP'
    IMPORTING
      fm_name            = lv_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  ls_control_parameters-getotf    = 'X'.
  ls_control_parameters-no_dialog = 'X'.
  ls_control_parameters-device    = 'PRINTER'.
  ls_output_options-tdprinter     = 'I9SWIN'.

  CALL FUNCTION lv_fm_name
    EXPORTING
      control_parameters = ls_control_parameters
      output_options     = ls_output_options
      ip_datum           = <ls_0128>-begda
    IMPORTING
      job_output_info    = ls_job_output_info
    TABLES
      gt_line            = lt_content
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      format                = 'PDF'
      max_linewidth         = 132
    IMPORTING
      bin_filesize          = lv_bin_filesize
      bin_file              = lv_bin_file
    TABLES
      otf                   = ls_job_output_info-otfdata
      lines                 = lt_lines
    EXCEPTIONS
      err_max_linewidth     = 1
      err_format            = 2
      err_conv_not_possible = 3
      OTHERS                = 4.

        CREATE OBJECT cached_response TYPE cl_http_response
          EXPORTING
            add_c_msg = 1.

        cached_response->set_data( data   = lv_bin_file
                                   length = lv_bin_filesize ).

        cached_response->set_header_field( name  = if_http_header_fields=>content_type
                                           value = 'application/pdf' ).
        cached_response->set_status( code = 200 reason = 'OK' ).
        cached_response->server_cache_expire_rel( expires_rel = 180 ).

        CALL FUNCTION 'GUID_CREATE'
          IMPORTING
            ev_guid_32 = guid.

        CALL METHOD cl_http_ext_csif=>access_info_get
          CHANGING
            crep_http = lv_crep_http
          EXCEPTIONS
            failed    = 1
            OTHERS    = 2.

        CONCATENATE runtime->application_url '/' guid l_pernr '.pdf' INTO pdf_link_128.

        cl_http_server=>server_cache_upload( url      = pdf_link_128
                                             response = cached_response ).

        CONCATENATE 'http://' lv_crep_http-http_serv ':'
        lv_crep_http-http_port pdf_link_128 INTO pdf_link_128.
		
*** main.htm ***

<%@page language="abap" %>
<%@extension name="htmlb" prefix="htmlb" %>
<%@extension name="phtmlb" prefix="phtmlb" %>

<htmlb:content design = "DESIGN2003"
               id     = "infotypeMain" >
  <htmlb:page title="<%= otr(ZHR_XSS/ZXSS_PAV_BORDRO) %>" >
    <htmlb:form method="get" >

      <htmlb:button id       = "display128"
                    text     = "Görüntüle"
                    disabled = "FALSE"
                    onClick  = "myClickHandler" />
      <Script> window.open("<%= pdf_link_128 %>", "Bildirim",status=0,toolbar=0,resizable=1,width='%100',height='%100') </SCRIPT>
 
    </htmlb:form>
  </htmlb:page>
</htmlb:content>
<Script src="cookie.js"></SCRIPT>
<Script language=javascript></script>