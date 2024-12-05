*&---------------------------------------------------------------------*
*& Report ZFORMULARIO_FABRICA_OO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zformulario_fabrica_oo.

INCLUDE <icon>.

TYPES:
  BEGIN OF ty_anexos.
    INCLUDE TYPE zbv_anexos.
TYPES: icon TYPE icon-id,
  END OF ty_anexos.

*Type para ALV DMS
TYPES: BEGIN OF ty_alv_dms,
         original TYPE bapi_doc_files2-originaltype,
         icon     TYPE icon-id,
         titulo   TYPE string, "bapi_doc_files2-description
         nome     TYPE bapi_doc_files2-changed_by,
         data_cri TYPE sy-datum,
       END OF ty_alv_dms.

DATA:
      gs_zbv_tmp_em_abrt TYPE zbv_tmp_em_abrt.

*Variáveis para caixa de seleção de arquivo
DATA:
  lt_files              TYPE filetable,
*  gs_files  TYPE file_table,
  v_rc                  TYPE i,
  lv_action             TYPE i,
  n_linhas              TYPE n,
  n_li_gt_documentfiles TYPE n,
  txt_anexo_pre         TYPE string VALUE 'Foram selecionados',
  txt_anexo_pos         TYPE string VALUE 'arquivos!',
  txt_anexo             TYPE string.

DATA:
  tipo_chamado     TYPE zbv_chamados-tipo_chamado,
  tipo_chamado_dms TYPE zbv_chamados-tipo_chamado.

DATA: gt_chamados_out TYPE TABLE OF zbv_chamados,
      gs_chamados_out TYPE zbv_chamados,
      gt_anexos_out   TYPE TABLE OF zbv_anexos,
      gs_anexos_out   TYPE zbv_anexos.

DATA:
  gs_clientes_out TYPE zbv_clientes,
  gs_acao_out     TYPE zbv_acao.

DATA:
  gv_dokar_out         TYPE dokar          VALUE 'SAT',
  gv_doknr_out         TYPE doknr,
  gv_doktl_out         TYPE doktl_d        VALUE '000',
  gv_dokvr_out         TYPE dokvr          VALUE '00',
  gv_dep_cat_out       TYPE cv_storage_cat VALUE 'DMS_C1_ST',
  gt_documentfiles_out TYPE TABLE OF bapi_doc_files2,
  gs_documentdata_out  TYPE bapi_doc_draw2,
  gs_return_out        TYPE bapiret2.

CONSTANTS: scr_group_ana  TYPE char3 VALUE 'ANA',
           scr_group_est  TYPE char3 VALUE 'EST',
           scr_group_exe  TYPE char3 VALUE 'EXE',
           scr_group_con  TYPE char3 VALUE 'CON',
           scr_group_alt  TYPE char3 VALUE 'ALT',
           analise        TYPE zbv_chamados-tipo_chamado VALUE 'Análise',
           estimativa     TYPE zbv_chamados-tipo_chamado VALUE 'Estimativa',
           execucao       TYPE zbv_chamados-tipo_chamado VALUE 'Execução',
           p_tipo_chamado TYPE string VALUE 'p_tp_ch'.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS main.

*Variaveis Globais

*Estruturas para identificação do cliente e ação
    CLASS-DATA:
      gs_clientes TYPE zbv_clientes,
      gs_acao     TYPE zbv_acao.

*Variáveis para envio de e-mail
    CLASS-DATA:
      lo_mime_helper TYPE REF TO cl_gbt_multirelated_service,
      lo_bcs         TYPE REF TO cl_bcs,
      lo_doc_bcs     TYPE REF TO cl_document_bcs,
      lo_recipient   TYPE REF TO if_recipient_bcs,
      lt_soli        TYPE TABLE OF soli,
      ls_soli        TYPE soli,
      lv_status      TYPE bcs_rqst,
      string         TYPE string,
      string2        TYPE string,
      string3        TYPE string,
      string4        TYPE string,
      string5        TYPE string,
      string6        TYPE string,
      string7        TYPE string,
      string8        TYPE string,
      string9        TYPE string,
      string_final   TYPE string,
      style1         TYPE string,
      style2         TYPE string,
      style3         TYPE string,
      style4         TYPE string,
      ass_fab        TYPE string VALUE 'Fábrica de Software'.


*Variáveis para upload de multi-anexos
    CLASS-DATA:
      gt_anexos    TYPE TABLE OF ty_anexos,
      gs_anexos    TYPE ty_anexos,
      lv_extension TYPE dappl.

*Variáveis para upload de arquivo
    CLASS-DATA:
      lv_e_flg_open_error	  TYPE esp1_boolean, "
*  lv_i_file_front_end    TYPE esp1_boolean, "
      lv_fe_file_not_exists	TYPE esp1_boolean, "
      lv_i_file_appl        TYPE rcgfiletr-ftappl, "
      lv_e_os_message	      TYPE c, "
      lv_fe_file_read_error	TYPE c, "
      lv_ap_no_authority    TYPE c, "
      lv_i_file_overwrite	  TYPE esp1_boolean VALUE esp1_true, "   ESP1_FALSE
      lv_ap_file_open_error	TYPE esp1_boolean, "
      lv_ap_file_exists	    TYPE esp1_boolean, "
      lv_ap_convert_error	  TYPE esp1_boolean,
      filenm                TYPE epsfilnam,
      filepath              TYPE rcgfiletr-ftappl VALUE '/usr/sap/trans/',
      lv_string             TYPE string,
      lv_len                TYPE i,
      lv_pos                TYPE i,
      lv_last_occ           TYPE i.

*Tabelas para criação de documento via BAPI DMS
    CLASS-DATA:
      gt_documentfiles TYPE TABLE OF bapi_doc_files2,
      gt_alv_dms       TYPE TABLE OF ty_alv_dms,
      lt_documentfiles TYPE TABLE OF bapi_doc_files2.

*Estrutura para criação de documento via BAPI DMS
    CLASS-DATA:
      gs_alv_dms       TYPE ty_alv_dms,
      gs_documentfiles LIKE LINE OF gt_documentfiles,
      gs_documentdatax TYPE bapi_doc_drawx2,
      ls_documentfiles TYPE bapi_doc_files2,
      gs_documentdata  TYPE bapi_doc_draw2,
      gs_return        TYPE bapiret2,
      ls_return        TYPE bapiret2.

*Variáveis para criação de documento via BAPI DMS
    CLASS-DATA:
      gv_dokar     TYPE dokar          VALUE 'SAT',
      gv_doknr     TYPE doknr,
      gv_e_doknr   TYPE doknr,
      gv_doktl     TYPE doktl_d        VALUE '000',
      gv_dokvr     TYPE dokvr          VALUE '00',
      gv_functions TYPE xfeld,
      gv_text      TYPE dktxt          VALUE 'Documento para chamado:',
      gv_desc      TYPE dktxt,
      gv_dep_cat   TYPE cv_storage_cat VALUE 'DMS_C1_ST',
      gv_status    TYPE dokst,
      gv_tool_text TYPE string,
      gv_col_text  TYPE char10.

*Estruturas para visualização de documento DMS via BAPI
    CLASS-DATA:
      lt_doc_struc  TYPE TABLE OF bapi_doc_structure,
      lt_doc_files2 TYPE TABLE OF bapi_doc_files2,
      lt_doc_comp   TYPE TABLE OF bapi_doc_comp,
      lv_fdoc       TYPE string.


*Variáveis para abertura de chamado
    CLASS-DATA:
      gt_chamados TYPE TABLE OF zbv_chamados,
      gs_chamados TYPE zbv_chamados.

*Variáveis para display de ALV Popup Chamados
    CLASS-DATA:
      go_alv       TYPE REF TO cl_salv_table,
      lr_functions TYPE REF TO cl_salv_functions_list.

*Variáveis Columns Chamados
    CLASS-DATA:
      gr_columns TYPE REF TO cl_salv_columns_table,
      gr_column  TYPE REF TO cl_salv_column_table.

*Variáveis de linhas e colunas Alv Popup Chamados
    CLASS-DATA:
      i_start_column TYPE i VALUE 5,
      i_start_line   TYPE i VALUE 6,
      i_end_column   TYPE i VALUE 110,
      i_end_line     TYPE i VALUE 10,
      i_title        TYPE string VALUE 'ALV Chamados',
      i_popup        TYPE flag VALUE 'X'.

*Variáveis para display de ALV Popup Anexos
    CLASS-DATA:
      go_alv_anexos       TYPE REF TO cl_salv_table,
      lr_functions_anexos TYPE REF TO cl_salv_functions_list.

*Variáveis Columns Anexos
    CLASS-DATA:
      gr_columns_anexos TYPE REF TO cl_salv_columns_table,
      gr_column_anexos  TYPE REF TO cl_salv_column_table.

*Variáveis de linhas e colunas Alv Popup Anexos
    CLASS-DATA:
      i_start_column_anx TYPE i VALUE 5,
      i_start_line_anx   TYPE i VALUE 16,
      i_end_column_anx   TYPE i VALUE 110,
      i_end_line_anx     TYPE i VALUE 20,
      i_title_anx        TYPE string VALUE 'ALV Anexos',
      i_popup_anx        TYPE flag VALUE 'X'.

    CLASS-DATA:
      ls_ans TYPE c.

    CONSTANTS:
      status_inicial TYPE string VALUE 'Aberto'.

  PRIVATE SECTION.

*   Métodos e atributos para o processamento do programa
    CLASS-METHODS:
      set_data, envia_email, upload_arquivo, display_alv_popup, monta_estrutura.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK blk1 WITH FRAME TITLE TEXT-000.
  SELECTION-SCREEN BEGIN OF LINE.

    SELECTION-SCREEN POSITION 21.
    PARAMETERS p_analis RADIOBUTTON GROUP g1 USER-COMMAND radio DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 23(7) TEXT-002. "Análise

    SELECTION-SCREEN POSITION 38.
    PARAMETERS p_estima RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 40(10) TEXT-003. "Estimativa

    SELECTION-SCREEN POSITION 58.
    PARAMETERS p_execu RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 60(8) TEXT-004. "Execução

    SELECTION-SCREEN POSITION 77.
    PARAMETERS p_alter RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 79(15) TEXT-024. "Alterar Chamado

  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK blk1.

SELECTION-SCREEN BEGIN OF BLOCK blk3 WITH FRAME TITLE TEXT-026. "Observações

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(23) TEXT-027. "Para o campo de Anexos:
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(45) TEXT-028. "º Para um anexo selecionar apenas um arquivo.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(69) TEXT-029. "º Para mais um anexo, será necessário selecionar todos de uma só vez!
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 2(43) TEXT-030. "Para limpar todos os campos da tela aperte:
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 4(2) TEXT-031. "º
    SELECTION-SCREEN PUSHBUTTON 8(12) btn1 USER-COMMAND btn VISIBLE LENGTH 10.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blk3.

SELECTION-SCREEN BEGIN OF BLOCK blk2 WITH FRAME TITLE TEXT-001. "Campos do Formulário

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(13) TEXT-025 MODIF ID alt. "Tipo Chamado:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_tp_ch   TYPE zbv_chamados-tipo_chamado MODIF ID alt AS LISTBOX VISIBLE LENGTH 22.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(5) TEXT-007. "Ação:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_acao   TYPE zbv_chamados-acao AS LISTBOX VISIBLE LENGTH 22.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(8) TEXT-008. "Cliente:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_client TYPE zbv_chamados-cliente AS LISTBOX VISIBLE LENGTH 15.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(11) TEXT-009. "ID Chamado:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_id_cha TYPE zbv_chamados-id_chamado.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(9) TEXT-010. "Ambiente:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_ambien TYPE zbv_chamados-ambiente AS LISTBOX VISIBLE LENGTH 7.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(12) TEXT-011. "Solicitante:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_solici TYPE zbv_chamados-solicitante AS LISTBOX VISIBLE LENGTH 20.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(11) TEXT-012. "Tecnologia:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_tecnol TYPE zbv_chamados-tecnologia AS LISTBOX VISIBLE LENGTH 9.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(7) TEXT-013. "Módulo:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_modulo TYPE zbv_chamados-modulo AS LISTBOX VISIBLE LENGTH 6.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(15) TEXT-014. "Em atendimento:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_em_atd TYPE zbv_chamados-em_atendimento AS LISTBOX VISIBLE LENGTH 4.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(13) TEXT-015. "Complexidade:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_comple TYPE zbv_chamados-complexidade AS LISTBOX VISIBLE LENGTH 15.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(11) TEXT-016. "Prioridade:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_priori TYPE zbv_chamados-prioridade AS LISTBOX VISIBLE LENGTH 10.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(6) TEXT-017. "Anexo:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_anexo  TYPE zbv_chamados-anexo.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(16) TEXT-018. "Cenário de Erro:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_cen_er TYPE zbv_chamados-cenario_erro.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(19) TEXT-019. "Cenário de Sucesso:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_cen_su TYPE zbv_chamados-cenario_sucesso.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(17) TEXT-020 MODIF ID ana. "Acesso Produtivo:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_ac_prd TYPE zbv_chamados-acesso_produtivo MODIF ID ana AS LISTBOX VISIBLE LENGTH 4.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) TEXT-021 MODIF ID est. "Aplicação de nota:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_apl_no TYPE zbv_chamados-aplicacao_nota MODIF ID est AS LISTBOX VISIBLE LENGTH 4.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(14) TEXT-022 MODIF ID exe. "ID Estimativa:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_id_est TYPE zbv_chamados-id_estimativa MODIF ID exe.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
    SELECTION-SCREEN COMMENT 1(18) TEXT-023 MODIF ID con. "Consultor Técnico:
    SELECTION-SCREEN POSITION 21.
    PARAMETERS: p_co_tec TYPE zbv_chamados-consultor_tecnico MODIF ID con AS LISTBOX VISIBLE LENGTH 25.
  SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK blk2.


AT SELECTION-SCREEN.
  CASE sy-ucomm.
    WHEN 'BTN'.
      PERFORM btn_limpa_tela.
      MODIFY SCREEN.
  ENDCASE.

*Abertura da caixa para selecionar o anexo
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_anexo.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      multiselection          = abap_true
      window_title            = 'Selecione os arquivos'
    CHANGING
      file_table              = lt_files
      rc                      = v_rc
*     user_action             = lv_action
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF sy-subrc = 0.
    DESCRIBE TABLE lt_files LINES n_linhas.
    CONCATENATE txt_anexo_pre n_linhas txt_anexo_pos INTO txt_anexo SEPARATED BY space.
    p_anexo = txt_anexo.
  ELSE.
    MESSAGE TEXT-032 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao selecionar os arquivos para upload!
  ENDIF.


*Manipulação para tela dinâmica
AT SELECTION-SCREEN OUTPUT.
  btn1 = TEXT-033. "Limpa Campos

  IF p_analis = abap_true.
    tipo_chamado = analise.
    tipo_chamado_dms = 'Ana'.

*    PERFORM limpa_campos_tela.

    LOOP AT SCREEN.

*Verificar nos outros tipos
      IF screen-name = p_tipo_chamado. "'p_tp_ch'.
        screen-invisible = 1.
      ENDIF.

      IF p_em_atd = 'N' OR p_em_atd IS INITIAL.
        IF screen-group1 = scr_group_est OR screen-group1 = scr_group_exe
          OR screen-group1 = scr_group_con OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = scr_group_est OR screen-group1 = scr_group_exe OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ELSEIF p_estima = abap_true.
    tipo_chamado = estimativa.
    tipo_chamado_dms = 'Est'.

*    PERFORM limpa_campos_tela.

    LOOP AT SCREEN.
      IF p_em_atd = 'N' OR p_em_atd IS INITIAL.
        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_exe
          OR screen-group1 = scr_group_con OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_exe OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSEIF p_execu = abap_true.
    tipo_chamado = execucao.
    tipo_chamado_dms = 'Exe'.

*    PERFORM limpa_campos_tela.

    LOOP AT SCREEN.
      IF p_em_atd = 'N' OR p_em_atd IS INITIAL.
        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_est
          OR screen-group1 = scr_group_con OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ELSE.
        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_est OR screen-group1 = scr_group_alt.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ELSEIF p_alter = abap_true.
    IF p_tp_ch IS INITIAL.
      p_tp_ch = analise.
    ENDIF.

    IF p_tp_ch EQ analise.
      tipo_chamado = analise.
      tipo_chamado_dms = 'Ana'.

      LOOP AT SCREEN.

        IF screen-group1 = scr_group_est OR screen-group1 = scr_group_exe.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    ELSEIF p_tp_ch EQ estimativa.
      tipo_chamado = estimativa.
      tipo_chamado_dms = 'Est'.

      LOOP AT SCREEN.

        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_exe.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.

    ELSEIF p_tp_ch EQ execucao.
      tipo_chamado = execucao.
      tipo_chamado_dms = 'Exe'.

      LOOP AT SCREEN.

        IF screen-group1 = scr_group_ana OR screen-group1 = scr_group_est.
          screen-active = 0.
          MODIFY SCREEN.
        ENDIF.

      ENDLOOP.
    ENDIF.

    IF p_id_cha IS NOT INITIAL.

      CLEAR: gt_chamados_out.

      IF p_tp_ch IS INITIAL
        OR p_id_cha IS INITIAL
        OR p_client IS INITIAL
        OR p_acao IS INITIAL.

        MESSAGE TEXT-036 TYPE 'S' DISPLAY LIKE 'E'. "Necessário informar os campos chave: Tipo, ID, Cliente e Ação.

      ELSE.

        SELECT * FROM zbv_chamados
        INTO TABLE gt_chamados_out
        WHERE tipo_chamado = p_tp_ch
        AND   id_chamado   = p_id_cha
        AND   cliente      = p_client
        AND   acao         = p_acao.

        IF sy-subrc <> 0.
          MESSAGE 'Chamado não existe na base de dados!' TYPE 'S' DISPLAY LIKE 'E'.

        ELSE.

          READ TABLE gt_chamados_out INTO gs_chamados_out INDEX 1.

          gv_doknr_out = gs_chamados_out-anexo.

*      "BAPI para pegar os dados do documento DMS e jogar na tabela gt_documentfiles
          CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
            EXPORTING
              documenttype    = gv_dokar_out
              documentnumber  = gv_doknr_out
              documentpart    = gv_doktl_out
              documentversion = gv_dokvr_out
            IMPORTING
              documentdata    = gs_documentdata_out
              return          = gs_return_out
            TABLES
              documentfiles   = gt_documentfiles_out.

          IF sy-subrc <> 0.
            MESSAGE TEXT-034 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao recuparar dados do documento do DMS!
          ENDIF.
*
          IF gt_documentfiles_out IS NOT INITIAL.

            MESSAGE 'Quantidade de anexos recuperada!' TYPE 'S'.

          ELSE.
            MESSAGE 'Chamado não possui anexos no documento de DMS!' TYPE 'S'.
          ENDIF.

          p_anexo = 'Selecionar novos anexos para adicionar'. "Precisar TRATAR
*        tipo_chamado = gs_chamados-tipo_chamado.
          p_acao = gs_chamados_out-acao.
          p_client = gs_chamados_out-cliente.
*        gs_chamados-id_chamado = p_id_cha.
          p_ambien = gs_chamados_out-ambiente.
          p_solici = gs_chamados_out-solicitante.
          p_tecnol = gs_chamados_out-tecnologia .
          p_modulo = gs_chamados_out-modulo.
          p_em_atd = gs_chamados_out-em_atendimento.
          p_comple = gs_chamados_out-complexidade.
          p_priori = gs_chamados_out-prioridade.
          p_cen_er = gs_chamados_out-cenario_erro.
          p_cen_su = gs_chamados_out-cenario_sucesso.

          IF tipo_chamado = analise.
            p_ac_prd = gs_chamados_out-acesso_produtivo.
          ELSEIF tipo_chamado = estimativa.
            p_apl_no = gs_chamados_out-aplicacao_nota.
          ELSE.
            p_id_est = gs_chamados_out-id_estimativa.
          ENDIF.

          IF gs_chamados_out-consultor_tecnico IS NOT INITIAL.
            p_co_tec = gs_chamados_out-consultor_tecnico.
          ENDIF.

          LOOP AT SCREEN.
            IF screen-name = 'P_TP_CH'
               OR screen-name = 'P_ACAO'
               OR screen-name = 'P_ID_CHA'
               OR screen-name = 'P_CLIENT'.

              screen-input = '0'.
              MODIFY SCREEN.
            ENDIF.
          ENDLOOP.


          MODIFY SCREEN.

        ENDIF.

      ENDIF. "p_tp_ch IS INITIAL


    ENDIF. "p_id_cha IS NOT INITIAL.

  ENDIF. "p_analis = abap_true.


CLASS lcl_main IMPLEMENTATION.

  METHOD main.

    IF p_acao IS INITIAL OR p_client IS INITIAL OR
     p_id_cha IS INITIAL OR p_solici IS INITIAL OR
     p_ambien IS INITIAL OR p_tecnol IS INITIAL OR
     p_modulo IS INITIAL OR p_priori IS INITIAL OR
     p_cen_er IS INITIAL OR p_cen_su IS INITIAL.

      MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'. "Necessário preencher os campos obrigatórios.

    ELSE.

      SELECT * FROM zbv_chamados
        INTO TABLE @DATA(gt_chamados_check)
        WHERE tipo_chamado = @tipo_chamado
        AND   id_chamado   = @p_id_cha
        AND   cliente      = @p_client
        AND   acao         = @p_acao.

      IF gt_chamados_check IS INITIAL.

        set_data( ).

      ELSEIF p_alter = abap_true.

*        MESSAGE 'Chamado já existe' TYPE 'I' DISPLAY LIKE 'E'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Alteração de chamado' "Criar TEXT-000
            text_question         = 'Chamado já existe! Deseja alterá-lo?' "Criar TEXT-000
            text_button_1         = 'Sim' "Criar TEXT-000
            icon_button_1         = 'ICON_CHECKED'
            text_button_2         = 'Cancelar' "Criar TEXT-000
            icon_button_2         = 'ICON_CANCEL'
            display_cancel_button = ' '
            start_column          = 45
            start_row             = 12
            popup_type            = 'ICON_MESSAGE_WARNING'
          IMPORTING
            answer                = ls_ans.

        IF ls_ans = '1'.
*          EXIT.
          set_data( ).
        ENDIF.

      ELSE.
        MESSAGE TEXT-035 TYPE 'S' DISPLAY LIKE 'E'. "Chamado já existe, se desejar, marque a opção de alteração de chamado.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD set_data.

    monta_estrutura( ).

  ENDMETHOD.

  METHOD envia_email.

    " Create the main object of the mail.
    CREATE OBJECT lo_mime_helper.

    IF p_alter = abap_false.

*      " Create the mail content.-----"NEW WAY"
*      string = '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 5.0//EN">'
*      && '<HTML><BODY>'.
*
*      string2 = '<P>O chamado'.
*
*      string3 = 'foi criado com sucesso!</P></BODY></HTML>'.

      string = '<!DOCTYPE html><html lang="pt-BR"><head><style> body { font-family: Arial, sans-serif; margin: 0; padding: 0; background-color: #f4f4f4; color: #333; }'.

      style1 = '.email-container { max-width: 600px; margin: 20px auto; padding: 20px; background-color: #fff; border: 1px solid #ddd; border-radius: 8px; }'.

      style2 = '.email-header { background-color: #007bff; color: #fff; padding: 20px; text-align: center; border-radius: 8px 8px 0 0; } .email-header h1 { margin: 0; font-size: 24px; } .email-body { padding: 20px; }'.

      style3 = '.email-body p { font-size: 16px; line-height: 1.5; margin-bottom: 15px; } .status { font-weight: bold; color: #007bff; } .email-footer { padding: 10px; background-color: #f1f1f1; text-align: center; border-radius: 0 0 8px 8px; }'.

      style4 = '.email-footer p { font-size: 14px; margin: 0;}</style></head><body><div class="email-container"><div class="email-header"><h1>Abertura de chamado'.

      string2 = '</h1></div><div class="email-body"><p>Prezado,'.

      string3 = '.</p><p>Estamos entrando em contato para informá-lo(a) que o seu chamado foi criado com sucesso!</p>'.

      string4 = '<p><span class="status">Dados do Chamado:</span></p><p><span class="status">ID:'.

      string5 = '</span></p><p><span class="status">Cliente:'.

      string6 = '</span></p><p><span class="status">Tipo:'.

      string7 = '</span></p><p><span class="status">Ação:'.

      string8 = '</span></p><p>Atenciosamente,<br>'.

      string9 = '</p></p></div><div class="email-footer"><p>Obrigado pela confiança no nosso atendimento.</p></div></div></body></html>'.

      CONCATENATE string style1
                  style2 style3
                  style4
                  string2 gs_chamados-solicitante
                  string3
                  string4 gs_chamados-id_chamado
                  string5 gs_chamados-cliente
                  string6 gs_chamados-tipo_chamado
                  string7 gs_chamados-acao
                  string8 ass_fab
                  string9 INTO string_final SEPARATED BY space.

    ELSE.

*      " Create the mail content.-----"NEW WAY"
*      string = '<!DOCTYPE html PUBLIC “-//IETF//DTD HTML 5.0//EN">'
*      && '<HTML><BODY>'.
*
*      string2 = '<P>O chamado'.
*
*      string3 = 'foi alterado com sucesso!</P></BODY></HTML>'.

      string = '<!DOCTYPE html><html lang="pt-BR"><head><style> body { font-family: Arial, sans-serif; margin: 0; padding: 0; background-color: #f4f4f4; color: #333; }'.

      style1 = '.email-container { max-width: 600px; margin: 20px auto; padding: 20px; background-color: #fff; border: 1px solid #ddd; border-radius: 8px; }'.

      style2 = '.email-header { background-color: #007bff; color: #fff; padding: 20px; text-align: center; border-radius: 8px 8px 0 0; } .email-header h1 { margin: 0; font-size: 24px; } .email-body { padding: 20px; }'.

      style3 = '.email-body p { font-size: 16px; line-height: 1.5; margin-bottom: 15px; } .status { font-weight: bold; color: #007bff; } .email-footer { padding: 10px; background-color: #f1f1f1; text-align: center; border-radius: 0 0 8px 8px; }'.

      style4 = '.email-footer p { font-size: 14px; margin: 0;}</style></head><body><div class="email-container"><div class="email-header"><h1>Alteração de chamado'.

      string2 = '</h1></div><div class="email-body"><p>Prezado,'.

      string3 = '.</p><p>Estamos entrando em contato para informá-lo(a) que o seu chamado foi alterado com sucesso!</p>'.

      string4 = '<p><span class="status">Dados do Chamado:</span></p><p><span class="status">ID:'.

      string5 = '</span></p><p><span class="status">Cliente:'.

      string6 = '</span></p><p><span class="status">Tipo:'.

      string7 = '</span></p><p><span class="status">Ação:'.

      string8 = '</span></p><p>Atenciosamente,<br>'.

      string9 = '</p></p></div><div class="email-footer"><p>Obrigado pela confiança no nosso atendimento.</p></div></div></body></html>'.

      CONCATENATE string style1
                  style2 style3
                  style4
                  string2 gs_chamados-solicitante
                  string3
                  string4 gs_chamados-id_chamado
                  string5 gs_chamados-cliente
                  string6 gs_chamados-tipo_chamado
                  string7 gs_chamados-acao
                  string8 ass_fab
                  string9 INTO string_final SEPARATED BY space.

    ENDIF.



*    CONCATENATE string p_solici string2 INTO DATA(string_parcial).
*
*    CONCATENATE string_parcial p_id_cha string3 INTO DATA(string_final) SEPARATED BY space.

    lt_soli = cl_document_bcs=>string_to_soli( string_final ).

    CALL METHOD lo_mime_helper->set_main_html
      EXPORTING
        content     = lt_soli
        description = 'Email'.

    " Set the subject of the mail.
    TRY.
        lo_doc_bcs = cl_document_bcs=>create_from_multirelated(
                      i_subject          = 'Abertura de Chamado'
                      i_importance       = '9'                " 1~High Priority  5~Average priority 9~Low priority
                      i_multirel_service = lo_mime_helper ).
      CATCH cx_document_bcs.

    ENDTRY.


    TRY.
        lo_bcs = cl_bcs=>create_persistent( ).

        lo_bcs->set_document( i_document = lo_doc_bcs ).

        TRY.
            lo_recipient = cl_cam_address_bcs=>create_internet_address(
            i_address_string =  'berenger.bruno@gmail.com' ).
          CATCH cx_address_bcs.

        ENDTRY.

        lo_bcs->add_recipient( i_recipient = lo_recipient ).

        TRY.
            lo_recipient = cl_cam_address_bcs=>create_internet_address(
            i_address_string =  'bruno.bereger@t-systems.com' ).
          CATCH cx_address_bcs.

        ENDTRY.

        lo_bcs->add_recipient( i_recipient = lo_recipient ).

*        TRY.
*            lo_recipient = cl_cam_address_bcs=>create_internet_address(
*            i_address_string =  'berenger.bruno@gmail.com' ).
*          CATCH cx_address_bcs.
*
*        ENDTRY.
*
*        lo_bcs->add_recipient( i_recipient = lo_recipient ).

        " Change the status.
        lv_status = 'N'.

        CALL METHOD lo_bcs->set_status_attributes
          EXPORTING
            i_requested_status = lv_status.
      CATCH cx_send_req_bcs.
        MESSAGE 'Erro no método set_status_attributes!' TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

    TRY.
        lo_bcs->send( ).
        COMMIT WORK.
      CATCH cx_bcs INTO DATA(lx_bcs).
        ROLLBACK WORK.
    ENDTRY.
  ENDMETHOD.

  METHOD upload_arquivo.

    IF p_alter = abap_true.

      gs_chamados-anexo = gv_doknr_out.
      gs_documentdata-documentnumber = gv_doknr_out.
      gs_documentdata-documenttype = gv_dokar.
      gs_documentdata-description = gv_desc. "gv_desc
      gs_documentdata-username = sy-uname.
      gs_documentdata-statusintern = ' '. "gv_status
      gs_documentdata-validfromdate = sy-datum.

      DESCRIBE TABLE gt_documentfiles_out LINES DATA(lines).

      gs_documentdatax-wsapplication1 = abap_true.
      gs_documentdatax-docfile1 = abap_true.
      gs_documentdatax-datacarrier1 = abap_true.

      GET TIME STAMP FIELD DATA(time_stamp2).

      LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_files2>).

        ls_documentfiles-documenttype = gv_dokar.
        ls_documentfiles-originaltype = condense( CONV string( lines + 1 ) ).

        ls_documentfiles-created_by = sy-uname.
        ls_documentfiles-storagecategory = gv_dep_cat.
        ls_documentfiles-docfile = <fs_files2>-filename.
        ls_documentfiles-created_at = time_stamp2.

        CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
          EXPORTING
            full_name     = <fs_files2>-filename
          IMPORTING
            stripped_name = ls_documentfiles-description.

        IF sy-subrc <> 0.
          MESSAGE 'Erro ao armazenar o nome do arquivo!' TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

*Pega a extensão do arquivo
        CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
          EXPORTING
            filename  = <fs_files2>-filename
            uppercase = 'X'
          IMPORTING
            extension = ls_documentfiles-wsapplication.

        INSERT ls_documentfiles INTO TABLE lt_documentfiles.

        lines += 1.
        CLEAR: ls_documentfiles.

      ENDLOOP.

    ELSE.

      SHIFT p_id_cha LEFT DELETING LEADING '0'.

      gs_documentdata-documentnumber = gv_doknr. "Acredito que não precisa
      gs_documentdata-documenttype = gv_dokar.
      gs_documentdata-description = gv_desc. "gv_desc
      gs_documentdata-username = sy-uname.
      gs_documentdata-statusintern = ' '. "gv_status
      gs_documentdata-validfromdate = sy-datum.

      GET TIME STAMP FIELD DATA(time_stamp).

      LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_files>).

        ls_documentfiles-documenttype = gv_dokar.
        ls_documentfiles-created_by = sy-uname.
        ls_documentfiles-storagecategory = gv_dep_cat.
        ls_documentfiles-docfile = <fs_files>-filename.
        ls_documentfiles-checkedin = 'X'.
        ls_documentfiles-created_at = time_stamp.

        CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
          EXPORTING
            full_name     = <fs_files>-filename
          IMPORTING
            stripped_name = ls_documentfiles-description.

        IF sy-subrc <> 0.
          MESSAGE 'Erro ao armazenar o nome do arquivo!' TYPE 'I' DISPLAY LIKE 'E'.
        ENDIF.

*Pega a extensão do arquivo
        CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
          EXPORTING
            filename  = <fs_files>-filename
            uppercase = 'X'
          IMPORTING
            extension = ls_documentfiles-wsapplication.

        INSERT ls_documentfiles INTO TABLE lt_documentfiles.

        IF sy-subrc = 4 AND lt_files IS INITIAL.
          MESSAGE 'Chamado criado sem anexos, porém a pasta da documentação foi criada' TYPE 'I' DISPLAY LIKE 'W'.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD monta_estrutura.
*COLOCAR DENTRO DE UM NOVO MÉTODO ------------------------------------------------------------

    "Processo para enviar os dados via BAPI para criação do DOC no DMS

    upload_arquivo( ).

*-------------------------------------------------------------------------

*Preenchimento da estrutura do chamado
*    gs_chamados-anexo = gv_doknr. "Preenchido depois da criação do DOC no DMS
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
    gs_chamados-status = status_inicial.
    gs_chamados-data_abertura = sy-datum.
    gs_chamados-hora_abertura = sy-uzeit.

    IF p_co_tec IS NOT INITIAL.
      gs_chamados-consultor_tecnico = p_co_tec.
    ENDIF.

    IF tipo_chamado = analise.
      gs_chamados-acesso_produtivo = p_ac_prd.
    ELSEIF tipo_chamado = estimativa.
      gs_chamados-aplicacao_nota = p_apl_no.
    ELSE.
      gs_chamados-id_estimativa = p_id_est.

      IF gs_chamados-id_estimativa IS NOT INITIAL.

        SELECT SINGLE * FROM zbv_chamados
          INTO @DATA(gs_chamados_est)
          WHERE tipo_chamado  = @estimativa
          AND   id_chamado    = @p_id_cha
          AND   cliente       = @p_client
          AND   acao          = @p_acao.

        gs_chamados-horas_estimadas = gs_chamados_est-horas_estimadas.

      ENDIF.

    ENDIF.

*    APPEND gs_chamados TO gt_chamados.
*-------------------------------------------------------------------------


    IF p_alter = abap_true.

      APPEND gs_chamados TO gt_chamados.

      MODIFY zbv_chamados FROM gs_chamados.
*      MODIFY zbv_anexos FROM TABLE gt_anexos. Verificar a possibilidade realizar alteração no DMS via BAPI_DOCUMENT_CHANGE2

      CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
        EXPORTING
          documenttype    = gv_dokar
          documentnumber  = gv_doknr_out
          documentpart    = gv_doktl
          documentversion = gv_dokvr
          documentdata    = gs_documentdata
          documentdatax   = gs_documentdatax
        IMPORTING
          return          = gs_return
        TABLES
          documentfiles   = lt_documentfiles.

      IF gs_return-type CA 'WEA'.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        COMMIT WORK.
        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
          EXPORTING
            wait = abap_true.

        envia_email( ).
        display_alv_popup( ).
      ENDIF.
    ELSE.

      MOVE-CORRESPONDING gs_chamados TO gs_zbv_tmp_em_abrt.
      gs_zbv_tmp_em_abrt-n_registro = 1.
      gs_zbv_tmp_em_abrt-data_ini = sy-datum.
      gs_zbv_tmp_em_abrt-hora_ini = sy-uzeit.

      INSERT zbv_tmp_em_abrt FROM gs_zbv_tmp_em_abrt.
*
*      INSERT zbv_chamados FROM gs_chamados.

      IF sy-subrc = 0.
        MESSAGE 'Chamado criado com sucesso!' TYPE 'S'.

*        INSERT zbv_anexos FROM TABLE gt_anexos.
        CALL FUNCTION 'BAPI_DOCUMENT_CREATE2'
          EXPORTING
            documentdata    = gs_documentdata
          IMPORTING
            documenttype    = gv_dokar
            documentnumber  = gv_doknr
            documentpart    = gv_doktl
            documentversion = gv_dokvr
            return          = gs_return
          TABLES
            documentfiles   = lt_documentfiles.

        IF gs_return-type CA 'EA'.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        ELSE.

          MESSAGE 'Documento DMS criado com sucesso!' TYPE 'S'.

          gs_chamados-anexo = gv_doknr.

          INSERT zbv_chamados FROM gs_chamados.

          COMMIT WORK.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          APPEND gs_chamados TO gt_chamados.

          envia_email( ).
          display_alv_popup( ).

        ENDIF.

      ELSE.
        "VERIFICAR POIS JÁ FOI TRATADO NO INICIO DO PROCESSO
        MESSAGE 'O chamado já existe!' TYPE 'I' DISPLAY LIKE 'S'.
        MESSAGE 'Os dados não foram inseridos na tabela de anexo!' TYPE 'I' DISPLAY LIKE 'S'.

      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD display_alv_popup.
    DATA:
      lv_icon     TYPE icon-id,
      lv_data_cri TYPE sy-datum,
      lv_titulo   TYPE string.


*    TRY.
*Cria Salv table Chamados
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table = gt_chamados[] ).

* Obtem as colunas Chamados
    gr_columns = go_alv->get_columns( ).

* Selecionar a coluna correta Chamados
    TRY.
        gr_column ?= gr_columns->get_column( 'MANDT' ).
      CATCH cx_salv_not_found.
    ENDTRY.

* Inicio - Eliminar a coluna correta do relatório Chamados
    TRY.
        gr_column->set_technical( value = if_salv_c_bool_sap=>true )."
      CATCH cx_salv_not_found.

    ENDTRY.
* Fim - Eliminar a coluna correta do relatório Chamados

*MONTAGEM ALV DMS -----------------------------------------------------------------
    IF p_alter = abap_true.
      gv_doknr = gv_doknr_out.
    ENDIF.

    IF gv_doknr IS NOT INITIAL.
      CLEAR: gt_documentfiles,
             gs_return.

      "BAPI para recuperar os dados do documento DMS
      CALL FUNCTION 'BAPI_DOCUMENT_GETDETAIL2'
        EXPORTING
          documenttype    = gv_dokar
          documentnumber  = gv_doknr
          documentpart    = gv_doktl
          documentversion = gv_dokvr
        IMPORTING
          documentdata    = gs_documentdata
          return          = gs_return
        TABLES
          documentfiles   = gt_documentfiles.

      IF gs_return IS INITIAL.

        IF gt_documentfiles IS NOT INITIAL.

          LOOP AT gt_documentfiles INTO gs_documentfiles ##INTO_OK.

            CASE gs_documentfiles-wsapplication.
              WHEN 'DO1' OR 'DOC' OR 'WRD'.
                lv_icon = icon_doc.
              WHEN 'PDF'.
                lv_icon = icon_pdf.
              WHEN 'TXT'.
                lv_icon = icon_wri.
              WHEN 'XLS'.
                lv_icon = icon_xls.
              WHEN 'JPG' OR 'PNG'.
                lv_icon = icon_jpg.
              WHEN OTHERS.
                lv_icon = icon_intensify.
            ENDCASE.

            lv_data_cri = gs_documentfiles-created_at(8).

            "Descrição do documento
            CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
              EXPORTING
                full_name     = gs_documentfiles-docfile
              IMPORTING
                stripped_name = lv_titulo.

            IF gs_return-type <> 0.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            INSERT VALUE #( original = gs_documentfiles-originaltype
                            icon     = lv_icon
                            titulo   = lv_titulo "gs_documentfiles-description
                            nome     = gs_documentfiles-created_by
                            data_cri = lv_data_cri ) INTO TABLE gt_alv_dms.

            CLEAR: gs_documentfiles,
                   lv_icon,
                   lv_titulo,
                   lv_data_cri.

          ENDLOOP.


          "Cria Salv table Anexos
          cl_salv_table=>factory(
                    IMPORTING
                      r_salv_table = go_alv_anexos
                    CHANGING
                      t_table = gt_alv_dms[] ).

          "Obtem as colunas Anexos
          gr_columns_anexos = go_alv_anexos->get_columns( ).

*---------------------------------------------------------------------------
          "Selecionar a coluna correta Anexos
          TRY.
              gr_column_anexos ?= gr_columns_anexos->get_column( 'ORIGINAL' ).
            CATCH cx_salv_not_found.
          ENDTRY.

          "Inicio - Eliminar a coluna correta do relatório Anexos
          TRY.
              gr_column_anexos->set_technical( value = if_salv_c_bool_sap=>true )."
            CATCH cx_salv_not_found.
          ENDTRY.
          "Fim - Eliminar a coluna correta do relatório Anexos

          gr_column_anexos ?= gr_columns_anexos->get_column( 'ICON' ).
          gr_column_anexos->set_alignment( if_salv_c_alignment=>centered ).
          gr_column_anexos->set_long_text( 'Ícone' ).
          gr_column_anexos->set_output_length( 5 ).

          gr_column_anexos ?= gr_columns_anexos->get_column( 'TITULO' ).
          gr_column_anexos->set_alignment( if_salv_c_alignment=>centered ).
          gr_column_anexos->set_long_text( 'Nome do Arquivo' ).
          gr_column_anexos->set_output_length( 20 ).
*---------------------------------------------------------------------------

          "Inicio - Display Alv Popup Chamados
          lr_functions = go_alv->get_functions( ).
          lr_functions->set_all( 'X' ).
          go_alv->get_columns( )->set_optimize( ).

          "Inicio - Display Alv Popup Anexos
          lr_functions_anexos = go_alv_anexos->get_functions( ).
          lr_functions_anexos->set_all( 'X' ).
          go_alv_anexos->get_columns( )->set_optimize( ).
*        go_alv_anexos->get_columns( )->set_column_position( columnname = 'ICON' position = 1 ).

          IF go_alv IS BOUND AND go_alv_anexos IS BOUND.
            IF i_popup = 'X'.
              go_alv->set_screen_popup(
                start_column = i_start_column
                end_column = i_end_column
                start_line = i_start_line
                end_line = i_end_line ).

              go_alv_anexos->set_screen_popup(
                start_column = i_start_column_anx
                end_column = i_end_column_anx
                start_line = i_start_line_anx
                end_line = i_end_line_anx ).
            ENDIF.

            go_alv->display( ).
            go_alv_anexos->display( ).
            MESSAGE TEXT-005 TYPE 'S'. "O Chamado foi criado com sucesso!

          ENDIF.
          "Fim - Display Alv Popup

        ELSE.
          "Inicio - Display Alv Popup Chamados
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
            MESSAGE TEXT-005 TYPE 'S'. "O Chamado foi criado com sucesso!

          ENDIF.
*            CATCH cx_salv_msg.
*          ENDTRY.
          "Fim - Display Alv Popup

        ENDIF.
      ENDIF.
    ENDIF.



  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  lcl_main=>main( ).

*&---------------------------------------------------------------------*
*& Form limpa_campos_tela
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM limpa_campos_tela .
  CLEAR: p_anexo ,
         p_acao  ,
         p_client,
         p_id_cha,
         p_ambien,
         p_solici,
         p_tecnol,
         p_modulo,
*         p_em_atd,
         p_comple,
         p_priori,
         p_cen_er,
         p_cen_su,
         p_co_tec,
         p_ac_prd,
         p_apl_no,
         p_id_est.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form btn_limpa_tela
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM btn_limpa_tela .
  CLEAR:   p_anexo ,
           p_acao  ,
           p_client,
           p_id_cha,
           p_ambien,
           p_solici,
           p_tecnol,
           p_modulo,
           p_em_atd,
           p_comple,
           p_priori,
           p_cen_er,
           p_cen_su,
           p_co_tec,
           p_ac_prd,
           p_apl_no,
           p_id_est,
           lt_files,
           gt_anexos_out,
           n_li_gt_documentfiles,
           n_linhas,
           txt_anexo,
           gv_doknr_out.
ENDFORM.
