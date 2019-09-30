# abap-reference
Template programs, utility procedures and code samples for ABAP and other SAP development tools. Contributions are welcome. 

Templates, programs and classes (src/lib-package folder) are activated in an ABAP 7.50 SP 4 system and exported via [abapGit](https://github.com/larshp/abapGit). Latest [release](https://github.com/kkayacan/sap-lib/releases) can be imported into an ABAP 7.02 SP 8 system.

#### Disclaimer
Code in snippets folder are samples and are not guaranteed to work by just copying and pasting. Adjust it according to your requirement.

#### Folder structure
- [src](src/)
  - [lib-package](src/lib-package/) Templates and programs exported via abapGit
    - [program](src/lib-package/program/) Programs
    - [template](src/lib-package/template/) Templates
  - [objects](src/objects/) Frequently used standard objects
  - [snippets](src/snippets/)

### Index

##### Programs
- [Extract ABAP development objects to html/text files](src/lib-package/program/ydtp_mass_download.prog.abap)
  
##### Templates
- [SLIS ALV report with user interaction functionality](src/lib-package/template/yp_alv_template.prog.abap)
- [SALV mini template](src/lib-package/template/yp_salv_mini.prog.abap)
- [ALV grid in docking container](src/lib-package/template/yp_docking.prog.abap)

##### Snippets
- Algorithm
  - [Parallel cursor](src/snippets/parallel-cursor.abap)
  
- ALV
  - [List variant search help for SLIS ALV](src/snippets/REUSE_ALV_VARIANT_F4.abap)
  - [Build LVC field catalog](src/snippets/build-lvc-fieldcatalog.abap)
    
- Data dictionary
  - [Get domain value text](src/snippets/get-domain-value-text.abap)
  
- Date
  - [Find month names](src/snippets/MONTH_NAMES_GET.abap)
  
- Dialog/Screen
  - [Force enter key with SAPGUI_SET_FUNCTIONCODE](src/snippets/SAPGUI_SET_FUNCTIONCODE.abap)
  
- Excel
  - [Create native Excel file in background and send as email attachment](src/snippets/create-excel-bg-and-send-mail.abap)
  - [Create Excel file with OLE2 interface](src/snippets/ole2-excel.abap)
  
- FI
  - [Split items of BAPI_ACC_DOCUMENT_POST](src/snippets/BAPI_ACC_DOCUMENT_POST-split-items.abap)
  - [Get GL account for tax indicator](src/snippets/FI_TAX_GET_TAX_ACCOUNTS.abap)
  - [Tax calculation](src/snippets/tax-calculation.abap)
  
- HR/HCM
  - [Get active plan variant](src/snippets/RH_GET_PLVAR.abap)
  - [Infotype subtype search help](src/snippets/HR_F4_GET_SUBTYPE.abap)
  - [Search help function for data with time constraint](src/snippets/HR_F4HELP_IN_INTERVAL.abap)
  - [Search help function for bank accounts](src/snippets/HR_BANK_SEARCH.abap)

- Message
  - [Display BAPIRET2 table](src/snippets/message-display.abap)
  - [Convert BDC messages to BAPIRET2](src/snippets/CONVERT_BDCMSGCOLL_TO_BAPIRET2.abap)
  
- Popup
  - [Ask for values with popup](src/snippets/POPUP_GET_VALUES.abap)

- Search help
  - [F4IF_INT_TABLE_VALUE_REQUEST generic use](src/snippets/F4IF_INT_TABLE_VALUE_REQUEST.abap)

- Selection screen
  - [SELECT_OPTIONS_RESTRICT](src/snippets/SELECT_OPTIONS_RESTRICT.abap)
  
- Smartforms
  - [Suppress dialog and download pdf](src/snippets/suppress-smartforms-dialog-and-download-pdf.abap)
  
- Standard text
  - [Sample use for SAVE_TEXT](src/snippets/SAVE_TEXT.abap)
  
- String/text
  - [Split single line string into multi line text](src/snippets/RKD_WORD_WRAP.abap)
  - [Convert text data to UTF-8](src/snippets/convert-text-to-utf8.abap)