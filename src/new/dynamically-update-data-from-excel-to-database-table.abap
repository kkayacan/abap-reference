* https://blogs.sap.com/2019/09/20/dynamically-update-data-from-excel-to-database-table/

REPORT zr_dynamic_table_update_excel NO STANDARD PAGE HEADING.

*** --- Data Declarations
DATA:lt_excel       TYPE TABLE OF alsmex_tabline,
     lt_dref        TYPE REF TO data,
     ls_dref        TYPE REF TO data,
     lv_col         TYPE i,
     lo_alv         TYPE REF TO cl_salv_table,
     lt_table_filds TYPE TABLE OF dfies.
*** --- Field Symbols
FIELD-SYMBOLS : <fs_table> TYPE any .
FIELD-SYMBOLS : <ft_table> TYPE STANDARD TABLE.
FIELD-SYMBOLS : <dyn_field> .

*** --- Selection screen designing
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS:p_file  TYPE rlgrap-filename OBLIGATORY,
           p_table TYPE dd02l-tabname OBLIGATORY,
           p_test  AS CHECKBOX DEFAULT abap_true.
SELECTION-SCREEN END OF BLOCK b1.

*** --- value request for p_file
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  CALL FUNCTION 'F4_FILENAME'
    IMPORTING
      file_name = p_file.
*** --- START-OF-SELECTION
START-OF-SELECTION.
*create OBJECT lr_descr.
*** --- Assigning fields symbols for Tables
  CREATE DATA lt_dref TYPE TABLE OF (p_table).
  CREATE DATA ls_dref TYPE (p_table).
*** --- Assign field symbol with table type of DDIC
  ASSIGN lt_dref->* TO <ft_table>.
*** --- Assign field symbol with Structure type of DDIC
  ASSIGN ls_dref->* TO <fs_table>.
*** --- Call the Function module ALSM_EXCEL_TO_INTERNAL_TABLE
  CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
    EXPORTING
      filename                = p_file
      i_begin_col             = 1
      i_begin_row             = 1
      i_end_col               = 99
      i_end_row               = 999999
    TABLES
      intern                  = lt_excel
    EXCEPTIONS
      inconsistent_parameters = 1
      upload_ole              = 2
      OTHERS                  = 3.
  IF sy-subrc EQ 0.
*** --- Sort
    SORT lt_excel BY row.

    LOOP AT lt_excel INTO DATA(ls_excel).

*** --- Adding count to skip the mapping for MANDT field
      lv_col = ls_excel-col + 1.
      ASSIGN COMPONENT lv_col OF STRUCTURE <fs_table> TO <dyn_field>.
      IF sy-subrc = 0.
***-- Compare Excel sheet column data and Dynamic table fields
            IF ls_excel-row = 1.
              CALL FUNCTION 'DDIF_FIELDINFO_GET'
                EXPORTING
                  tabname        = p_table
                TABLES
                  dfies_tab      = lt_table_filds
                EXCEPTIONS
                  not_found      = 1
                  internal_error = 2
                  OTHERS         = 3.
              READ TABLE lt_table_filds INTO DATA(ls_table_fields) INDEX lv_col.
              IF sy-subrc = 0.
                IF ls_table_fields-fieldname NE ls_excel-value.
                  WRITE: 'Excel sheet data and Dynamic table fields are not matching'.
                  EXIT.
                ENDIF.
              ENDIF.
            ELSE.
              <dyn_field> = ls_excel-value.
            ENDIF.
      ENDIF.
      IF ls_excel-row GT 1.
        AT END OF row.
          APPEND <fs_table> TO <ft_table>.
          CLEAR <fs_table>.
        ENDAT.
      ENDIF.
    ENDLOOP.

    IF <ft_table> IS NOT INITIAL.
      IF p_test IS INITIAL.
*** --- Modify
        MODIFY (p_table) FROM TABLE <ft_table>.
        IF sy-subrc EQ 0.
          COMMIT WORK.
          DATA(lv_lines) = lines( <ft_table> ).
          WRITE: TEXT-002,lv_lines.
        ELSE.
          ROLLBACK WORK.
          MESSAGE TEXT-003 TYPE 'E' DISPLAY LIKE 'I'.
        ENDIF.
      ELSE.
*** --- Factory Method
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   =  lo_alv   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = <ft_table>
        ).
*** --- Display
        lo_alv->display( ).
      ENDIF.
    ENDIF.
ENDIF.