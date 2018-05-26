# sap-lib
Template programs, utility procedures and code samples for ABAP and other SAP development tools. Contributions are welcome. 

Templates, programs and classes (abap/lib-package folder) are activated in an ABAP 7.50 SP 4 system and exported via [abapGit](https://github.com/larshp/abapGit). Latest release can be imported into an ABAP 7.31 SP 2 system.

#### Disclaimer
Code in snippets folder are samples and are not guaranteed to work by just copying and pasting. Adjust it according to your requirement.

##### Folder structure
- [abap](abap/)
  - [lib-package](abap/lib-package/) Templates and programs exported via abapGit
    - [prog](abap/lib-package/prog/) Programs
    - [tmplt](abap/lib-package/tmplt/) Templates
  - [snippets](abap/snippets/)

##### Index
- ALV
  - [List variant search help for SLIS ALV](abap/snippets/REUSE_ALV_VARIANT_F4.abap)
  - [SLIS ALV report template with user interaction functionality](abap/lib-package/tmplt/slisalv/)
  
- Code extraction/download/backup
  - [Program to extract ABAP development objects to html/text files](abap/lib-package/prog/massdownload/)
  
- Data dictionary
  - [Get domain value text](abap/snippets/get-domain-value-text.abap)
  
- Excel
  - [Create native Excel file in background and send as email attachment](abap/snippets/create-excel-bg-and-send-mail.abap)
  
- FI
  - [Split items of BAPI_ACC_DOCUMENT_POST](abap/snippets/BAPI_ACC_DOCUMENT_POST-split-items.abap)
  - [Get GL account for tax indicator](abap/snippets/FI_TAX_GET_TAX_ACCOUNTS.abap)
  - [Tax calculation](abap/snippets/tax-calculation.abap)
  
- Message
  - [Message display](abap/snippets/message-display.abap)
  
- Popup
  - [Ask for values with popup](abap/snippets/POPUP_GET_VALUES.abap)

- Search help
  - [F4IF_INT_TABLE_VALUE_REQUEST generic use](abap/snippets/F4IF_INT_TABLE_VALUE_REQUEST.abap)

- Selection screen
  - [SELECT_OPTIONS_RESTRICT](abap/snippets/SELECT_OPTIONS_RESTRICT.abap)
  
- Standard text
  - [Sample use for SAVE_TEXT](abap/snippets/SAVE_TEXT.abap)
  
- String/text
  - [Split single line string into multi line text](abap/snippets/RKD_WORD_WRAP.abap)