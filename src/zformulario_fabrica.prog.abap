*&---------------------------------------------------------------------*
*& Report ZFORMULARIO_FABRICA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zformulario_fabrica.

INCLUDE zforms.

DATA: it_files TYPE filetable,
      wa_files TYPE file_table,
      v_rc     TYPE i.

*Variávies para upload do arquivo na AL11
DATA:
  lv_e_flg_open_error	  TYPE esp1_boolean, "
*  lv_i_file_front_end    TYPE esp1_boolean, "
  lv_fe_file_not_exists	TYPE esp1_boolean, "
  lv_i_file_appl        TYPE rcgfiletr-ftappl VALUE '/usr/sap/trans/', "
  lv_e_os_message	      TYPE c, "
  lv_fe_file_read_error	TYPE c, "
  lv_ap_no_authority    TYPE c, "
  lv_i_file_overwrite	  TYPE esp1_boolean, "   ESP1_FALSE
  lv_ap_file_open_error	TYPE esp1_boolean, "
  lv_ap_file_exists	    TYPE esp1_boolean, "
  lv_ap_convert_error	  TYPE esp1_boolean,
  filenm                TYPE epsfilnam,
  filepath              TYPE epsdirnam,
  lv_string             TYPE string,
  lv_len                TYPE i,
  lv_pos                TYPE i,
  lv_last_occ           TYPE i.

*Variáveis para receber o input e passar para a tabela transparente
DATA: gt_chamados TYPE TABLE OF zbv_chamados,
      gs_chamados TYPE zbv_chamados.

*Variáveis para mostrar o pop-up na tela
DATA: go_alv       TYPE REF TO cl_salv_table,
      lr_functions TYPE REF TO cl_salv_functions_list.

*Variáveis para eliminar o mandt do alv popup
DATA: gr_columns TYPE REF TO cl_salv_columns_table.
DATA: gr_column TYPE REF TO cl_salv_column_table.

*Variáveis para mostrar o pop-up na tela
DATA: i_start_column TYPE i VALUE 5,
      i_start_line   TYPE i VALUE 6,
      i_end_column   TYPE i VALUE 120,
      i_end_line     TYPE i VALUE 20,
      i_title        TYPE string VALUE 'ALV',
      i_popup        TYPE flag VALUE 'X'.

DATA: tipo_chamado TYPE zbv_chamados-tipo_chamado.

*Constantes para manipulação da tela
CONSTANTS: scr_group_ana TYPE char3 VALUE 'ANA',
           scr_group_est TYPE char3 VALUE 'EST',
           scr_group_exe TYPE char3 VALUE 'EXE',
           analise       TYPE zbv_chamados-tipo_chamado VALUE 'Análise',
           estimativa    TYPE zbv_chamados-tipo_chamado VALUE 'Estimativa',
           execucao      TYPE zbv_chamados-tipo_chamado VALUE 'Execução'.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_analis RADIOBUTTON GROUP g1 USER-COMMAND radio DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 3(10) FOR FIELD p_analis. "TEXT-002.
    PARAMETERS p_estima RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 16(10) FOR FIELD p_estima. "TEXT-003.
    PARAMETERS p_execu RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 29(10) FOR FIELD p_execu. "TEXT-004.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_acao   TYPE zbv_chamados-acao,
              p_client TYPE zbv_chamados-cliente,
              p_id_cha TYPE zbv_chamados-id_chamado,
              p_ambien TYPE zbv_chamados-ambiente,
              p_solici TYPE zbv_chamados-solicitante,
              p_tecnol TYPE zbv_chamados-tecnologia,
              p_modulo TYPE zbv_chamados-modulo,
              p_em_atd TYPE zbv_chamados-em_atendimento,
              p_comple TYPE zbv_chamados-complexidade,
              p_priori TYPE zbv_chamados-prioridade,
              p_anexo  TYPE zbv_chamados-anexo,
              p_cen_er TYPE zbv_chamados-cenario_erro,
              p_cen_su TYPE zbv_chamados-cenario_sucesso,
              p_ac_prd TYPE zbv_chamados-acesso_produtivo MODIF ID ana,
              p_apl_no TYPE zbv_chamados-aplicacao_nota MODIF ID est,
              p_id_est TYPE zbv_chamados-id_estimativa MODIF ID exe.

SELECTION-SCREEN END OF BLOCK blk2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_anexo.
*  CALL FUNCTION 'F4_FILENAME'
*    EXPORTING
*      program_name  = syst-cprog
*      dynpro_number = syst-dynnr
*      field_name    = ' '
*    IMPORTING
*      file_name     = p_anexo.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    CHANGING
      file_table              = it_files
      rc                      = v_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.


  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
************************** identifica o arquivo selecionado e joga para o parametro de seleção
    READ TABLE it_files INTO wa_files INDEX 1.
    p_anexo = wa_files-filename.
  ENDIF.



*Manipulação para tela dinâmica
AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_analis = abap_true.
      tipo_chamado = analise.
      IF screen-group1 = scr_group_est OR screen-group1 = scr_group_exe.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_estima = abap_true.
      tipo_chamado = estimativa.
      IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_exe.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ELSEIF p_execu = abap_true.
      tipo_chamado = execucao.
      IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_est.
        screen-active = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDIF.
  ENDLOOP.
*Manipulação para tela dinâmica

START-OF-SELECTION.

  IF p_acao   IS INITIAL OR p_client IS INITIAL OR
     p_id_cha IS INITIAL OR p_solici IS INITIAL OR
     p_ambien IS INITIAL OR p_tecnol IS INITIAL OR
     p_modulo IS INITIAL OR p_priori IS INITIAL OR
     p_cen_er IS INITIAL OR p_cen_su IS INITIAL.

    MESSAGE TEXT-006 TYPE 'I'.

  ELSE.

    IF p_anexo IS INITIAL.

    ELSE.

*Início DO processo para pegar o nome DO arquivo.
      lv_string = p_anexo.
      lv_len = strlen( lv_string ).

      DO.

        SEARCH lv_string FOR '\'.

        IF sy-subrc = '0'.
          lv_pos = sy-fdpos + 1.
          lv_len = lv_len - sy-fdpos - 1.
          lv_string = lv_string+lv_pos(lv_len).
          lv_last_occ = lv_last_occ + sy-fdpos + 1. "last occurence number
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
*Fim do processo para pegar o nome do arquivo.

*Inicio do processo de atribuição do nome do arquivo ao path destino
      filenm = lv_string.
      filepath = lv_i_file_appl.

      CONCATENATE filepath lv_string INTO lv_i_file_appl.
*Inicio do processo de atribuição do nome do arquivo ao path destino

      gs_chamados-anexo = lv_i_file_appl.
      gs_chamados-tipo_chamado = tipo_chamado.
      gs_chamados-acao = p_acao.
      gs_chamados-cliente = p_client.
      gs_chamados-id_chamado = p_id_cha.
      gs_chamados-ambiente = p_ambien.
      gs_chamados-solicitante = p_solici.
      gs_chamados-tecnologia = p_tecnol.
      gs_chamados-modulo = p_modulo.
      gs_chamados-em_atendimento = p_em_atd.
      gs_chamados-complexidade = p_comple.
      gs_chamados-prioridade = p_priori.
      gs_chamados-cenario_erro = p_cen_er.
      gs_chamados-cenario_sucesso = p_cen_su.
      APPEND gs_chamados TO gt_chamados.

      INSERT zbv_chamados FROM gs_chamados.

      IF sy-subrc = 0.

        CALL FUNCTION 'C13Z_FILE_UPLOAD_BINARY'
          EXPORTING
            i_file_front_end   = p_anexo
            i_file_appl        = lv_i_file_appl
            i_file_overwrite   = lv_i_file_overwrite
          IMPORTING
            e_flg_open_error   = lv_e_flg_open_error
            e_os_message       = lv_e_os_message
          EXCEPTIONS
            fe_file_not_exists = 1
            fe_file_read_error = 2
            ap_no_authority    = 3
            ap_file_open_error = 4
            ap_file_exists     = 5
            ap_convert_error   = 6
            OTHERS             = 7.
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

*        CALL FUNCTION 'C13Z_FILE_UPLOAD_ASCII'  "NOTRANSL: EHS: Upload einer ASCII-Datei vom PC zum Applikationsserver
*          EXPORTING
*            i_file_front_end   = p_anexo
*            i_file_appl        = lv_i_file_appl
*            i_file_overwrite   = lv_i_file_overwrite
*          IMPORTING
*            e_flg_open_error   = lv_e_flg_open_error
*            e_os_message       = lv_e_os_message
*          EXCEPTIONS
*            fe_file_not_exists = 1
*            fe_file_read_error = 2
*            ap_no_authority    = 3
*            ap_file_open_error = 4
*            ap_file_exists     = 5
*            ap_convert_error   = 6.

        PERFORM envia_email.

        COMMIT WORK.
      ENDIF.

      TRY.
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = go_alv
            CHANGING
              t_table = gt_chamados[] ).

* Obtem as colunas
          gr_columns = go_alv->get_columns( ).

* Selecionar a coluna correta
          TRY.
              gr_column ?= gr_columns->get_column( 'MANDT' ).
            CATCH cx_salv_not_found.
          ENDTRY.

* Inicio - Eliminar a coluna correta do relatório
          TRY.
              gr_column->set_technical( value = if_salv_c_bool_sap=>true )."      CATCH cx_salv_not_found.
          ENDTRY.
* Fim - Eliminar a coluna correta do relatório

*Inicio - Display Alv Popup
          lr_functions = go_alv->get_functions( ).
          lr_functions->set_all( 'X' ).
          go_alv->get_columns( )->set_optimize( ).

          IF go_alv IS BOUND.
            IF i_popup = 'X'.
              go_alv->set_screen_popup(
                start_column = i_start_column
                end_column = i_end_column
                start_line = i_start_line
                end_line = i_end_line ).
            ENDIF.

            go_alv->display( ).
            MESSAGE TEXT-005 TYPE 'S'.
          ENDIF.
        CATCH cx_salv_msg.
      ENDTRY.
*Fim - Display Alv Popup

      LEAVE PROGRAM.

    ENDIF.
  ENDIF.
