*----------------------------------------------------------------------*
***INCLUDE ZFORMS.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form envia_email
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM envia_email.

*Variáveis para mandar o e-mail
  DATA : lo_mime_helper TYPE REF TO cl_gbt_multirelated_service,
         lo_bcs         TYPE REF TO cl_bcs,
         lo_doc_bcs     TYPE REF TO cl_document_bcs,
         lo_recipient   TYPE REF TO if_recipient_bcs,
         lt_soli        TYPE TABLE OF soli,
         ls_soli        TYPE soli,
         lv_status      TYPE bcs_rqst.


  " Create the main object of the mail.
  CREATE OBJECT lo_mime_helper.

  " Create the mail content.-----"NEW WAY"
  DATA(string) = '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 5.0//EN">'
  && '<HTML><BODY>Hi Dear,<P>Content Section!</P></BODY></HTML>'.

  lt_soli = cl_document_bcs=>string_to_soli( string ).

  CALL METHOD lo_mime_helper->set_main_html
    EXPORTING
      content     = lt_soli
      description = 'Test Email'.

  " Set the subject of the mail.
  lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
                  i_subject          = 'Subject of our email'
                  i_importance       = '9'                " 1~High Priority  5~Average priority 9~Low priority
                  i_multirel_service = lo_mime_helper ).

  lo_bcs = cl_bcs=>create_persistent( ).

  lo_bcs->set_document( i_document = lo_doc_bcs ).

  lo_recipient = cl_cam_address_bcs=>create_internet_address(
    i_address_string =  'berenger.bruno@gmail.com' ).

  lo_bcs->add_recipient( i_recipient = lo_recipient ).

  " Change the status.
  lv_status = 'N'.
  CALL METHOD lo_bcs->set_status_attributes
    EXPORTING
      i_requested_status = lv_status.

  TRY.
      lo_bcs->send( ).
      COMMIT WORK.
    CATCH cx_bcs INTO DATA(lx_bcs).
      ROLLBACK WORK.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- p_anexo
*&      <-- lv_string
*&      <-- lv_len
*&      <-- lv_last_occ
*&      <-- filenm
*&      <-- p_filepath
*&      <-- p_gs_chamados
*&      <-- p_gt_chamados
*&---------------------------------------------------------------------*
