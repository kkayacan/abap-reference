*Usage
CONSTANTS c_limit TYPE i VALUE 990.
  
IF lines( lt_acgl[] ) > c_limit.
  lv_function_name = 'Z_BAPI_ACC_DOCUMENT_POST_DIV'.
  ls_intermediate_acc-gl_account = '8990000008'.
  SELECT SINGLE skat~txt50
	FROM skat
    JOIN t001 ON t001~ktopl = skat~ktopl
    WHERE skat~spras = @sy-langu
    AND t001~bukrs = @ls_doch-comp_code
    AND skat~saknr = @ls_intermediate_acc-gl_account
    INTO @ls_intermediate_acc-item_text.
  ls_intermediate_acc-tax_code = space.
  ls_intermediate_acc-profit_ctr = lt_acgl[ 1 ]-profit_ctr.
  CALL FUNCTION 'Z_PARAM_ACC_DOCUMENT_POST_DIV'
    EXPORTING
      iv_limit            = c_limit
      iv_doc_type         = 'ZZ'
      is_intermediate_acc = ls_intermediate_acc.
ELSE.
  lv_function_name = 'BAPI_ACC_DOCUMENT_POST'.
ENDIF.

CALL FUNCTION lv_function_name
  EXPORTING
    documentheader = ls_doch
  IMPORTING
    obj_key        = lv_obj_key
  TABLES
    accountgl      = lt_acgl
    accountpayable = lt_acpy
    accounttax     = lt_actx
    currencyamount = lt_cura
    criteria       = lt_crit
    return         = lt_ret.

*Function group TOP include
DATA:
  gv_limit            TYPE  i,
  gv_doc_type         TYPE  bapiache09-doc_type,
  gs_intermediate_acc TYPE  BAPIACGL09.

FUNCTION z_param_acc_document_post_div.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LIMIT) TYPE  I DEFAULT 990
*"     VALUE(IV_DOC_TYPE) TYPE  BAPIACHE09-DOC_TYPE
*"     VALUE(IS_INTERMEDIATE_ACC) TYPE  BAPIACGL09
*"----------------------------------------------------------------------

  gv_limit            = iv_limit.
  gv_doc_type	        = iv_doc_type.
  gs_intermediate_acc = is_intermediate_acc.

ENDFUNCTION.

FUNCTION z_bapi_acc_document_post_div.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(DOCUMENTHEADER) TYPE  BAPIACHE09
*"  EXPORTING
*"     VALUE(OBJ_KEY) TYPE  BAPIACHE09-OBJ_KEY
*"  TABLES
*"      ACCOUNTGL STRUCTURE  BAPIACGL09 OPTIONAL
*"      ACCOUNTPAYABLE STRUCTURE  BAPIACAP09 OPTIONAL
*"      ACCOUNTTAX STRUCTURE  BAPIACTX09 OPTIONAL
*"      CURRENCYAMOUNT STRUCTURE  BAPIACCR09
*"      CRITERIA STRUCTURE  BAPIACKEC9 OPTIONAL
*"      RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  TYPES lvt_result TYPE p LENGTH 16 DECIMALS 8.

  DATA ls_documentheader TYPE bapiache09.
  DATA lv_obj_key        TYPE bapiache09-obj_key.
  DATA lt_accountgl      TYPE STANDARD TABLE OF bapiacgl09.
  DATA lt_accountpayable TYPE STANDARD TABLE OF bapiacap09.
  DATA lt_accounttax     TYPE STANDARD TABLE OF bapiactx09.
  DATA lt_currencyamount TYPE STANDARD TABLE OF bapiaccr09.
  DATA lt_criteria       TYPE STANDARD TABLE OF bapiackec9.
  DATA lt_return         TYPE STANDARD TABLE OF bapiret2.
  DATA lt_docs           TYPE gtt_docs.
  DATA lv_index          TYPE sy-tabix.

  CLEAR: obj_key, return[].

  IF NOT gv_limit > 1.
    APPEND VALUE #( type = 'E' id = 'ZACID_PI' number = '001' ) TO return.
    RETURN.
  ENDIF.

  DATA(lv_doc_count) = ( ceil( CONV lvt_result( lines( accountgl[] ) )
                       / gv_limit ) ) + 1.

  DO lv_doc_count TIMES.

    DATA(lv_current_doc) = sy-index.

    CLEAR: ls_documentheader,
           lv_obj_key,
           lt_accountgl,
           lt_accountpayable,
           lt_accounttax,
           lt_currencyamount,
           lt_criteria,
           lt_return.

    CASE lv_current_doc.
      WHEN 1.

        PERFORM build_first_doc USING documentheader
                                      accountpayable[]
                                      accounttax[]
                                      accountgl[]
                                      currencyamount[]
                                      criteria[]
                                      gs_intermediate_acc
                             CHANGING ls_documentheader
                                      lt_accountpayable
                                      lt_accounttax
                                      lt_accountgl
                                      lt_currencyamount
                                      lt_criteria.

      WHEN OTHERS.

        PERFORM build_next_doc USING documentheader
                                     gv_doc_type
                                     accountgl[]
                                     currencyamount[]
                                     criteria[]
                                     gs_intermediate_acc
                                     lv_current_doc
                                     gv_limit
                            CHANGING ls_documentheader
                                     lt_accountgl
                                     lt_currencyamount
                                     lt_criteria
                                     lt_accounttax.

    ENDCASE.

    CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
      EXPORTING
        documentheader = ls_documentheader
      IMPORTING
        obj_key        = lv_obj_key
      TABLES
        accountgl      = lt_accountgl
        accountpayable = lt_accountpayable
        accounttax     = lt_accounttax
        currencyamount = lt_currencyamount
        criteria       = lt_criteria
        return         = lt_return.

    APPEND LINES OF lt_return TO return.

    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<ls_return>) WHERE type CA 'AEX'.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.
      DATA(lv_error_occured) = abap_true.
      EXIT.
    ENDIF.

    CASE lv_current_doc.
      WHEN 1.
        obj_key = lv_obj_key.
      WHEN OTHERS.
        APPEND VALUE #( obj_key = lv_obj_key ) TO lt_docs.
    ENDCASE.

  ENDDO.

  CASE lv_error_occured.
    WHEN abap_true.
      CLEAR obj_key.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
    WHEN abap_false.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.
      PERFORM update_text USING obj_key lt_docs.
  ENDCASE.

ENDFUNCTION.

FORM build_first_doc USING is_header  TYPE bapiache09
                           it_payable TYPE bapiacap09_tab
                           it_tax     TYPE bapiactx09_tab
                           it_gl      TYPE bapiacgl09_tab
                           it_amount  TYPE bapiaccr09_tab
                           it_crit    TYPE bapiackec9_tab
                           is_intermediate_acc TYPE bapiacgl09
                  CHANGING cs_header  TYPE bapiache09
                           ct_payable TYPE bapiacap09_tab
                           ct_tax     TYPE bapiactx09_tab
                           ct_gl      TYPE bapiacgl09_tab
                           ct_amount  TYPE bapiaccr09_tab
                           ct_crit    TYPE bapiackec9_tab.

  DATA: lv_itemno     TYPE bapiaccr09-itemno_acc,
        lv_amount     TYPE bapiaccr09-amt_doccur,
        ls_gl         LIKE LINE OF ct_gl,
        lv_currency   TYPE bapiaccr09-currency,
        lt_gl_amounts TYPE SORTED TABLE OF gst_gl_amount
        WITH UNIQUE KEY tax_code.

  cs_header = is_header.

  LOOP AT it_payable INTO DATA(ls_payable).
    LOOP AT it_amount INTO DATA(ls_amount) WHERE itemno_acc = ls_payable-itemno_acc.
      ADD 1 TO lv_itemno.
      LOOP AT it_crit INTO DATA(ls_crit) WHERE itemno_acc = ls_payable-itemno_acc.
        ls_crit-itemno_acc = lv_itemno.
        APPEND ls_crit TO ct_crit.
      ENDLOOP.
      ADD ls_amount-amt_doccur TO lv_amount.
      ls_amount-itemno_acc = lv_itemno.
      lv_currency = ls_amount-currency.
      APPEND ls_amount TO ct_amount.
    ENDLOOP.
    ls_payable-itemno_acc = lv_itemno.
    APPEND ls_payable TO ct_payable.
  ENDLOOP.

  LOOP AT it_tax INTO DATA(ls_tax).
    LOOP AT it_amount INTO ls_amount WHERE itemno_acc = ls_tax-itemno_acc.
      ADD 1 TO lv_itemno.
      LOOP AT it_crit INTO ls_crit WHERE itemno_acc = ls_tax-itemno_acc.
        ls_crit-itemno_acc = lv_itemno.
        APPEND ls_crit TO ct_crit.
      ENDLOOP.
      ADD ls_amount-amt_doccur TO lv_amount.
      ls_amount-itemno_acc = lv_itemno.
      APPEND ls_amount TO ct_amount.
    ENDLOOP.
    ls_tax-itemno_acc = lv_itemno.
    APPEND ls_tax TO ct_tax.
  ENDLOOP.

  LOOP AT it_gl ASSIGNING FIELD-SYMBOL(<ls_gl>).
    LOOP AT it_amount ASSIGNING FIELD-SYMBOL(<ls_amount>)
      WHERE itemno_acc = <ls_gl>-itemno_acc.
      COLLECT VALUE gst_gl_amount( tax_code   = <ls_gl>-tax_code
                                   amt_doccur = <ls_amount>-amt_doccur )
                                   INTO lt_gl_amounts.
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_gl_amounts ASSIGNING FIELD-SYMBOL(<ls_gl_amount>).

    ADD 1 TO lv_itemno.
    ls_gl = is_intermediate_acc.
    ls_gl-tax_code   = <ls_gl_amount>-tax_code.
    ls_gl-itemno_acc = lv_itemno.
    APPEND ls_gl TO ct_gl.

    CLEAR ls_amount.
    ls_amount-itemno_acc = lv_itemno.
    ls_amount-curr_type  = '00'.
    ls_amount-amt_doccur = <ls_gl_amount>-amt_doccur.
    ls_amount-currency   = lv_currency.
    APPEND ls_amount TO ct_amount.

  ENDLOOP.

ENDFORM.

FORM build_next_doc USING is_header   TYPE bapiache09
                          iv_doc_type TYPE bapiache09-doc_type
                          it_gl       TYPE bapiacgl09_tab
                          it_amount   TYPE bapiaccr09_tab
                          it_crit     TYPE bapiackec9_tab
                          is_intermediate_acc TYPE bapiacgl09
                          iv_current_doc TYPE i
                          iv_limit       TYPE i
                 CHANGING cs_header   TYPE bapiache09
                          ct_gl       TYPE bapiacgl09_tab
                          ct_amount   TYPE bapiaccr09_tab
                          ct_crit     TYPE bapiackec9_tab
                          ct_tax      TYPE bapiactx09_tab.

  DATA: lv_itemno     TYPE bapiaccr09-itemno_acc,
        lv_amount     TYPE bapiaccr09-amt_doccur,
        lv_currency   TYPE bapiaccr09-currency,
        lt_gl_amounts TYPE SORTED TABLE OF gst_gl_amount
        WITH UNIQUE KEY tax_code.

  cs_header = is_header.
  cs_header-doc_type = iv_doc_type.

  DATA(lv_begin) = ( ( iv_current_doc - 2 ) * iv_limit  ) + 1.
  DATA(lv_end)   = lv_begin + iv_limit - 1.

  LOOP AT it_gl INTO DATA(ls_gl) FROM lv_begin TO lv_end.
    LOOP AT it_amount INTO DATA(ls_amount) WHERE itemno_acc = ls_gl-itemno_acc.
      ADD 1 TO lv_itemno.
      LOOP AT it_crit INTO DATA(ls_crit) WHERE itemno_acc = ls_gl-itemno_acc.
        ls_crit-itemno_acc = lv_itemno.
        APPEND ls_crit TO ct_crit.
      ENDLOOP.
      ADD ls_amount-amt_doccur TO lv_amount.
      ls_amount-itemno_acc = lv_itemno.
      lv_currency = ls_amount-currency.
      APPEND ls_amount TO ct_amount.
    ENDLOOP.
    ls_gl-itemno_acc = lv_itemno.
    APPEND ls_gl TO ct_gl.
  ENDLOOP.

  LOOP AT it_gl ASSIGNING FIELD-SYMBOL(<ls_gl>) FROM lv_begin TO lv_end.
    LOOP AT it_amount ASSIGNING FIELD-SYMBOL(<ls_amount>)
      WHERE itemno_acc = <ls_gl>-itemno_acc.
      COLLECT VALUE gst_gl_amount( tax_code   = <ls_gl>-tax_code
                                   amt_doccur = <ls_amount>-amt_doccur )
                                   INTO lt_gl_amounts.
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_gl_amounts ASSIGNING FIELD-SYMBOL(<ls_gl_amount>).

    ADD 1 TO lv_itemno.
    ls_gl = is_intermediate_acc.
    ls_gl-tax_code   = <ls_gl_amount>-tax_code.
    ls_gl-itemno_acc = lv_itemno.
    APPEND ls_gl TO ct_gl.

    CLEAR ls_amount.
    ls_amount-itemno_acc = lv_itemno.
    ls_amount-curr_type  = '00'.
    ls_amount-amt_doccur = <ls_gl_amount>-amt_doccur * -1.
    ls_amount-currency   = lv_currency.
    APPEND ls_amount TO ct_amount.

  ENDLOOP.

ENDFORM.

FORM update_text USING ip_first_doc
                       it_next_docs TYPE gtt_docs.

  DATA lt_lines TYPE tlinetab.

  PERFORM wait_for_commit USING ip_first_doc.
  lt_lines = VALUE #( FOR doc IN it_next_docs ( tdformat = '*'
                                                tdline   = doc-obj_key(10) )  ) .
  PERFORM save_text USING ip_first_doc lt_lines.

  LOOP AT it_next_docs ASSIGNING FIELD-SYMBOL(<ls_next_doc>).
    PERFORM wait_for_commit USING <ls_next_doc>-obj_key.
    lt_lines = VALUE #( ( tdformat = '*'
                          tdline   = ip_first_doc(10) )  ) .
    PERFORM save_text USING <ls_next_doc>-obj_key lt_lines.
  ENDLOOP.

  COMMIT WORK.

ENDFORM.

FORM wait_for_commit USING ip_obj_key.
  DO 10 TIMES.
    SELECT SINGLE @abap_true FROM bkpf INTO @DATA(lv_exists)
      WHERE bukrs = @ip_obj_key+10(4)
        AND belnr = @ip_obj_key(10)
        AND gjahr = @ip_obj_key+14(4).
    CASE sy-subrc.
      WHEN 0.      EXIT.
      WHEN OTHERS. WAIT UP TO 1 SECONDS.
    ENDCASE.
  ENDDO.
ENDFORM.

FORM save_text USING ip_key
                     it_lines TYPE tlinetab.

  CALL FUNCTION 'SAVE_TEXT'
    EXPORTING
      header          = VALUE thead( tdobject   = 'BELEG'
                                     tdname     = |{ ip_key+10(4) }{ ip_key(10) }{ ip_key+14(4) }|
                                     tdid       = '0001'
                                     tdspras    = sy-langu
                                     tdlinesize = '072'
                                     mandt      = sy-mandt )
      savemode_direct = abap_true
    TABLES
      lines           = it_lines
    EXCEPTIONS
      id              = 1
      language        = 2
      name            = 3
      object          = 4
      OTHERS          = 5.

ENDFORM.