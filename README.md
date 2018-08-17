# sap-lib
Template programs, utility procedures and code samples for ABAP and other SAP development tools. Contributions are welcome. 

Templates, programs and classes (abap/lib-package folder) are activated in an ABAP 7.50 SP 4 system and exported via [abapGit](https://github.com/larshp/abapGit). Latest [release](https://github.com/kkayacan/sap-lib/releases) can be imported into an ABAP 7.02 SP 8 system.

#### Disclaimer
Code in snippets folder are samples and are not guaranteed to work by just copying and pasting. Adjust it according to your requirement.

#### Folder structure
- [abap](abap/)
  - [lib-package](abap/lib-package/) Templates and programs exported via abapGit
    - [program](abap/lib-package/program/) Programs
    - [template](abap/lib-package/template/) Templates
  - [objects](abap/objects/) Frequently used standard objects
  - [snippets](abap/snippets/)

#### Index

##### Programs
- [Extract ABAP development objects to html/text files](abap/lib-package/program/ydtp_mass_download.prog.abap)
  
##### Templates
- [SLIS ALV report with user interaction functionality](abap/lib-package/template/yp_alv_template.prog.abap)
- [SALV mini template](abap/lib-package/template/yp_salv_mini.prog.abap)
- [ALV grid in docking container](abap/lib-package/template/yp_docking.prog.abap)

##### Snippets
- Algorithm
  - [Parallel cursor](abap/snippets/parallel-cursor.abap)
  
- ALV
  - [List variant search help for SLIS ALV](abap/snippets/REUSE_ALV_VARIANT_F4.abap)
  - [Build LVC field catalog](abap/snippets/build-lvc-fieldcatalog.abap)
    
- Data dictionary
  - [Get domain value text](abap/snippets/get-domain-value-text.abap)
  
- Date
  - [Find month names](abap/snippets/MONTH_NAMES_GET.abap)
  
- Dialog/Screen
  - [Force enter key with SAPGUI_SET_FUNCTIONCODE](abap/snippets/SAPGUI_SET_FUNCTIONCODE.abap)
  
- Excel
  - [Create native Excel file in background and send as email attachment](abap/snippets/create-excel-bg-and-send-mail.abap)
  
- FI
  - [Split items of BAPI_ACC_DOCUMENT_POST](abap/snippets/BAPI_ACC_DOCUMENT_POST-split-items.abap)
  - [Get GL account for tax indicator](abap/snippets/FI_TAX_GET_TAX_ACCOUNTS.abap)
  - [Tax calculation](abap/snippets/tax-calculation.abap)
  
- HR/HCM
  - [Get active plan variant](abap/snippets/RH_GET_PLVAR.abap)
  - [Infotype subtype search help](abap/snippets/HR_F4_GET_SUBTYPE.abap)
  - [Search help function for data with time constraint](abap/snippets/HR_F4HELP_IN_INTERVAL.abap)
  - [Search help function for bank accounts](abap/snippets/HR_BANK_SEARCH.abap)

- Message
  - [Display BAPIRET2 table](abap/snippets/message-display.abap)
  - [Convert BDC messages to BAPIRET2](abap/snippets/CONVERT_BDCMSGCOLL_TO_BAPIRET2.abap)
  
- Popup
  - [Ask for values with popup](abap/snippets/POPUP_GET_VALUES.abap)

- Search help
  - [F4IF_INT_TABLE_VALUE_REQUEST generic use](abap/snippets/F4IF_INT_TABLE_VALUE_REQUEST.abap)

- Selection screen
  - [SELECT_OPTIONS_RESTRICT](abap/snippets/SELECT_OPTIONS_RESTRICT.abap)
  
- Smartforms
  - [Suppress dialog and download pdf](abap/snippets/suppress-smartforms-dialog-and-download-pdf.abap)
  
- Standard text
  - [Sample use for SAVE_TEXT](abap/snippets/SAVE_TEXT.abap)
  
- String/text
  - [Split single line string into multi line text](abap/snippets/RKD_WORD_WRAP.abap)