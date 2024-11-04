*&---------------------------------------------------------------------*
*& Report ZRELATORIO_FABRICA_V2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio_fabrica_v2.

INCLUDE <icon>.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.

    METHODS: on_link_click_id   FOR EVENT link_click OF
      cl_salv_events_table
      IMPORTING row column.

    METHODS:
      data_changed
        FOR EVENT data_changed
          OF cl_gui_alv_grid IMPORTING er_data_changed
                                       e_onf4
                                       e_onf4_before
                                       e_onf4_after
                                       e_ucomm.

    METHODS:
      data_changed_hora
        FOR EVENT data_changed
          OF cl_gui_alv_grid IMPORTING er_data_changed
                                       e_onf4
                                       e_onf4_before
                                       e_onf4_after
                                       e_ucomm.

ENDCLASS.                    "lcl_handle_events DEFINITION

DATA:
  tempo_minutos TYPE mcwmit-be_ae,
  tempo_total   TYPE p DECIMALS 3.

DATA: gt_zbv_tmp_em_abrt TYPE TABLE OF zbv_tmp_em_abrt,
      gs_zbv_tmp_em_abrt TYPE zbv_tmp_em_abrt.

DATA: gt_zbv_tmp_dev TYPE TABLE OF zbv_tmp_dev,
      gs_zbv_tmp_dev TYPE zbv_tmp_dev.

DATA: gt_zbv_tmp_em_atnd TYPE TABLE OF zbv_tmp_em_atnd,
      gs_zbv_tmp_em_atnd TYPE zbv_tmp_em_atnd.

DATA: gt_zbv_tmp_parado TYPE TABLE OF zbv_tmp_parado,
      gs_zbv_tmp_parado TYPE zbv_tmp_parado.

DATA: gt_zbv_tmp_tst_func TYPE TABLE OF zbv_tmp_tst_func,
      gs_zbv_tmp_tst_func TYPE zbv_tmp_tst_func.

DATA: gt_zbv_tmp_tst_clnt TYPE TABLE OF zbv_tmp_tst_clnt,
      gs_zbv_tmp_tst_clnt TYPE zbv_tmp_tst_clnt.

DATA:
  gs_zbv_comentario_antes TYPE zbv_comentarios,
  gs_zbv_horas_antes      TYPE zbv_ger_horas.

"Tipo para registro de campos no relatorio de log
TYPES: BEGIN OF ty_detail_tab,
         nome  TYPE string,
         valor TYPE string,
       END OF ty_detail_tab.


"Tipo para inclusão de Semáforo Chamados
TYPES: BEGIN OF ty_zbv_chamados.
         INCLUDE TYPE zbv_chamados.
TYPES:   icon TYPE icon-id,
       END OF ty_zbv_chamados.

"Tipo para inclusão de Icones Anexos
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
         data_atu TYPE sy-datum,
       END OF ty_alv_dms.

DATA:
      lt_detail_tab TYPE ty_detail_tab OCCURS 29.

*Variável para guardar quantidade de linhas da tabela
DATA:
  log_linha TYPE i.

*Variáveis TextEditor Erro
CONSTANTS: c_line_lenght TYPE i VALUE 1200."255.

*Variáveis TextEditor Erro
DATA:
  ok_code                 TYPE sy-ucomm,
  lt_table(c_line_lenght) TYPE c OCCURS 0,
  lo_editor_cen_er        TYPE REF TO cl_gui_textedit,
  lo_editor_cen_er_con    TYPE REF TO cl_gui_custom_container.

*Variáveis TextEditor Sucesso
DATA:
  lt_table_su(c_line_lenght) TYPE c OCCURS 0,
  lo_editor_cen_su           TYPE REF TO cl_gui_textedit,
  lo_editor_cen_su_con       TYPE REF TO cl_gui_custom_container.

*Variáveis TextEditor Comentário em detalhes
DATA:
  lt_table_com(c_line_lenght) TYPE c OCCURS 0,
  lo_editor_com               TYPE REF TO cl_gui_textedit,
  lo_editor_con_com           TYPE REF TO cl_gui_custom_container.

*Variáveis TextEditor Erro
*DATA: comentarios                   TYPE string.

DATA: gv_screen TYPE sy-dynnr.

DATA: wa_zbv_chamados TYPE zbv_chamados.

*Tabela de Chamados
DATA: gt_zbv_chamados       TYPE TABLE OF ty_zbv_chamados,
      gs_zbv_chamados       TYPE ty_zbv_chamados,
      gs_zbv_chamados_sc    TYPE zbv_chamados,
      gs_zbv_chamados_antes TYPE zbv_chamados.

*Tabela de Comentários
DATA:
  gt_zbv_comentarios TYPE TABLE OF zbv_comentarios,
  gs_zbv_comentarios TYPE zbv_comentarios.

*Tabela de Anexos
DATA:
  gt_zbv_anexos TYPE TABLE OF zbv_anexos,
  gs_zbv_anexos TYPE zbv_anexos.

*Tabela de Atualização de Horas
DATA:
  gt_zbv_horas  TYPE TABLE OF zbv_ger_horas,
  gs_zbv_horas  TYPE zbv_ger_horas,
  lv_hora_temp  TYPE zbv_ger_horas-horas_utilizadas,
  lv_hora_total TYPE zbv_ger_horas-horas_utilizadas.

*Tabela de Logs de Atualização
DATA:
  gt_zbv_log_atuali   TYPE TABLE OF zbv_log_atuali,
  gs_zbv_log_atuali   TYPE zbv_log_atuali,
  n_gt_zbv_log_atuali TYPE TABLE OF zbv_log_atuali.

"Declaração das variáveis alv_grid - COMENTARIO
DATA: lo_grid_com      TYPE REF TO cl_gui_alv_grid,
      lo_container_com TYPE REF TO cl_gui_custom_container,
      lv_okcode_1001   TYPE sy-ucomm,
      lv_okcode_1002   TYPE sy-ucomm,
      lv_okcode_1003   TYPE sy-ucomm,
      lv_okcode_1004   TYPE sy-ucomm,
      lv_okcode_1006   TYPE sy-ucomm,
      lt_fieldcat_com  TYPE lvc_t_fcat,
      ls_layout_com    TYPE lvc_s_layo,
      ls_variant_com   TYPE disvariant,
      lt_tool_bar      TYPE ui_functions,
      lv_salvou_item   TYPE char1.

*Variáveis do ALV Grid Anexo
DATA: lo_grid_anx      TYPE REF TO cl_gui_alv_grid,
      lo_container_anx TYPE REF TO cl_gui_custom_container,
      lv_okcode_1005   TYPE sy-ucomm,
      lt_fieldcat_anx  TYPE lvc_t_fcat,
      ls_layout_anx    TYPE lvc_s_layo,
      ls_variant_anx   TYPE disvariant.

*Variáveis do ALV Grid Horas
DATA: lo_grid_hrs      TYPE REF TO cl_gui_alv_grid,
      lo_container_hrs TYPE REF TO cl_gui_custom_container,
      lv_okcode_1007   TYPE sy-ucomm,
      lt_fieldcat_hrs  TYPE lvc_t_fcat,
      ls_layout_hrs    TYPE lvc_s_layo,
      ls_variant_hrs   TYPE disvariant.

*Variáveis do ALV Grid Log atualização
DATA: lo_grid_log      TYPE REF TO cl_gui_alv_grid,
      lo_container_log TYPE REF TO cl_gui_custom_container,
      lv_okcode_1008   TYPE sy-ucomm,
      lt_fieldcat_log  TYPE lvc_t_fcat,
      ls_layout_log    TYPE lvc_s_layo,
      ls_variant_log   TYPE disvariant.

*Variáveis para caixa de seleção de arquivo
DATA:
  lt_files            TYPE filetable,
*  gs_files  TYPE file_table,
  v_rc                TYPE i,
  lv_action           TYPE i,
  filepath            TYPE rcgfiletr-ftappl VALUE '/usr/sap/trans/',
  lv_i_file_appl      TYPE rcgfiletr-ftappl,
  filenm              TYPE epsfilnam,
  lv_i_file_overwrite	TYPE esp1_boolean VALUE esp1_true,
  lv_e_os_message	    TYPE c,
  lv_e_flg_open_error	TYPE esp1_boolean.

*-- cl_salv_table Chamados

DATA: gr_table     TYPE REF TO cl_salv_table,
      lo_alv_error TYPE REF TO cx_salv_msg,
      gr_functions TYPE REF TO cl_salv_functions,
      gr_columns   TYPE REF TO cl_salv_columns_table,
      gr_column    TYPE REF TO cl_salv_column_table,
      gr_events    TYPE REF TO cl_salv_events_table.

*-------------------------------------------------------------------

*Tabelas para criação de documento via BAPI DMS
DATA:
  gt_documentfiles TYPE TABLE OF bapi_doc_files2,
  gt_alv_dms       TYPE TABLE OF ty_alv_dms,
  lt_documentfiles TYPE TABLE OF bapi_doc_files2.

*Estrutura para criação de documento via BAPI DMS
DATA:
  gs_alv_dms       TYPE ty_alv_dms,
  gs_documentfiles LIKE LINE OF gt_documentfiles,
  gs_documentdatax TYPE bapi_doc_drawx2,
  ls_documentfiles TYPE bapi_doc_files2,
  gs_documentdata  TYPE bapi_doc_draw2,
  gs_return        TYPE bapiret2,
  ls_return        TYPE bapiret2.

*Variáveis para criação de documento via BAPI DMS
DATA:
  gv_dokar     TYPE dokar          VALUE 'SAT',
  gv_doknr     TYPE doknr,
  gv_e_doknr   TYPE doknr,
  gv_doktl     TYPE doktl_d        VALUE '000',
  gv_dokvr     TYPE dokvr          VALUE '00',
  gv_functions TYPE xfeld,
  gv_text      TYPE dktxt          VALUE 'Documento para chamado:',
  gv_desc      TYPE dktxt,
  gv_dep_cat   TYPE cv_storage_cat VALUE 'DMS_C1_ST'.

*Estruturas para visualização de documento DMS via BAPI
DATA:
  lt_doc_struc  TYPE TABLE OF bapi_doc_structure,
  lt_doc_files2 TYPE TABLE OF bapi_doc_files2,
  lt_doc_comp   TYPE TABLE OF bapi_doc_comp,
  lv_fdoc       TYPE string.

DATA:
  lv_icon     TYPE icon-id,
  lv_data_cri TYPE sy-datum,
  lv_data_atu TYPE sy-datum,
  lv_titulo   TYPE string.

*-------------------------------------------------------------------

DATA rows TYPE lvc_t_row.
DATA wa_row TYPE lvc_s_row.

CONSTANTS:
  mandt             TYPE lvc_fname VALUE 'MANDT',
  tipo_chamado      TYPE lvc_fname VALUE 'TIPO_CHAMADO',
  id_chamado        TYPE lvc_fname VALUE 'ID_CHAMADO',
  status            TYPE lvc_fname VALUE 'STATUS',
  acao              TYPE lvc_fname VALUE 'ACAO',
  cliente           TYPE lvc_fname VALUE 'CLIENTE',
  consultor_tecnico TYPE lvc_fname VALUE 'CONSULTOR_TECNICO',
  ambiente          TYPE lvc_fname VALUE 'AMBIENTE',
  solicitante       TYPE lvc_fname VALUE 'SOLICITANTE',
  tecnologia        TYPE lvc_fname VALUE 'TECNOLOGIA',
  modulo            TYPE lvc_fname VALUE 'MODULO',
  em_atendimento    TYPE lvc_fname VALUE 'EM_ATENDIMENTO',
  complexidade      TYPE lvc_fname VALUE 'COMPLEXIDADE',
  prioridade        TYPE lvc_fname VALUE 'PRIORIDADE',
  anexo             TYPE lvc_fname VALUE 'ANEXO',
  cenario_erro      TYPE lvc_fname VALUE 'CENARIO_ERRO',
  cenario_sucesso   TYPE lvc_fname VALUE 'CENARIO_SUCESSO',
  acesso_produtivo  TYPE lvc_fname VALUE 'ACESSO_PRODUTIVO',
  aplicacao_nota    TYPE lvc_fname VALUE 'APLICACAO_NOTA',
  id_estimativa     TYPE lvc_fname VALUE 'ID_ESTIMATIVA',
  data_abertura     TYPE lvc_fname VALUE 'DATA_ABERTURA',
  hora_abertura     TYPE lvc_fname VALUE 'HORA_ABERTURA',
  data_atualizacao  TYPE lvc_fname VALUE 'DATA_ATUALIZACAO',
  hora_atualizacao  TYPE lvc_fname VALUE 'HORA_ATUALIZACAO',
  data_encerramento TYPE lvc_fname VALUE 'DATA_ENCERRAMENTO',
  hora_encerramento TYPE lvc_fname VALUE 'HORA_ENCERRAMENTO',
  comentario        TYPE lvc_fname VALUE 'COMENTARIO',
  icon              TYPE lvc_fname VALUE 'ICON',
  tab_comentario    TYPE lvc_fname VALUE 'ZBV_COMENTARIOS',
  tab_anexos        TYPE lvc_fname VALUE 'ZBV_ANEXOS',
  nome_arquivo      TYPE lvc_fname VALUE 'NOME_ARQUIVO',
  caminho_destino   TYPE lvc_fname VALUE 'CAMINHO_DESTINO',
  horas_utilizadas  TYPE lvc_fname VALUE 'HORAS_UTILIZADAS',
  tab_horas         TYPE lvc_fname VALUE 'ZBV_GER_HORAS'.

*Variáveis de manipulação de tela
CONSTANTS:
  back         TYPE string VALUE 'BACK',
  add          TYPE string VALUE 'ADD',
  del          TYPE string VALUE 'DEL',
  exit         TYPE string VALUE 'EXIT',
  save         TYPE string VALUE 'SAVE',
  down         TYPE string VALUE 'DOWN',
  exec         TYPE string VALUE 'EXEC',
  ok           TYPE string VALUE 'OK',
  vis          TYPE string VALUE 'VIS',
  vis_com      TYPE string VALUE 'VIS_COM',
  vis_cen_erro TYPE string VALUE 'VIS_CEN_ERRO',
  vis_cen_suc  TYPE string VALUE 'VIS_CEN_SUC',
  vis_anexos   TYPE string VALUE 'VIS_ANEXOS',
  ger_horas    TYPE string VALUE 'GER_HORAS',
  log_atl      TYPE string VALUE 'LOG_ATL'.

CONSTANTS:
  aberto    TYPE string VALUE 'Aberto',
  encerrado TYPE string VALUE 'Encerrado',
  em_atendi TYPE string VALUE 'Em atendimento',
  cancelado TYPE string VALUE 'Cancelado',
  parado    TYPE string VALUE 'Parado'.

DATA:
  lv_text  TYPE string,
  gs_color TYPE lvc_s_colo.

DATA: event_handler TYPE REF TO lcl_handle_events.

CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_link_click_id.

    "Limpeza de tabelas, estruturas e liberação dos objetos
    CLEAR: lt_table,lt_table_su,gs_zbv_chamados, lv_hora_total. " gs_zbv_chamados-horas_utilizadas.
    FREE: lo_editor_cen_er,lo_editor_cen_su.

    READ TABLE gt_zbv_chamados INTO gs_zbv_chamados INDEX row.
    IF sy-subrc = 0.
      APPEND gs_zbv_chamados-cenario_erro TO lt_table[].
      APPEND gs_zbv_chamados-cenario_sucesso TO lt_table_su[].

      MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_antes.

      SELECT * FROM zbv_ger_horas INTO TABLE gt_zbv_horas
        WHERE   tipo_chamado = gs_zbv_chamados-tipo_chamado
        AND     id_chamado   = gs_zbv_chamados-id_chamado
        AND     cliente      = gs_zbv_chamados-cliente
        AND     acao         = gs_zbv_chamados-acao.

      IF gs_zbv_chamados-tipo_chamado = 'Execução'.
        IF ( ( gs_zbv_chamados-horas_estimadas - gs_zbv_chamados-horas_utilizadas )  <= 15 ).
          MESSAGE 'Chamado está prestes a estourar as horas estimatidas!' TYPE 'I' DISPLAY LIKE 'W'.
        ENDIF.
      ENDIF.

      CALL SCREEN 1001 STARTING AT 1 1
                   ENDING AT 120 30.

      gr_table->refresh( refresh_mode = if_salv_c_refresh=>full ).

    ELSE.
      MESSAGE TEXT-002 TYPE 'I' DISPLAY LIKE 'E'. "ID do chamado do Hotspot não encontrado!
    ENDIF.

  ENDMETHOD.                    "on_link_click

  METHOD data_changed.

    "Verificação de permissão de modificação
    AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
    CASE sy-subrc.
      WHEN 0.

        LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_good_cells>).
          READ TABLE gt_zbv_comentarios ASSIGNING FIELD-SYMBOL(<fs_zbv_comentarios>) INDEX <fs_good_cells>-row_id.
          CASE <fs_good_cells>-fieldname.
            WHEN comentario.
              IF <fs_zbv_comentarios>-criado_por = sy-uname OR <fs_zbv_comentarios>-id_linha = 0.

                MOVE-CORRESPONDING <fs_zbv_comentarios> TO gs_zbv_comentario_antes.
*                APPEND gs_zbv_comentario_antes TO gt_zbv_comentario_antes.

                <fs_zbv_comentarios>-comentario = <fs_good_cells>-value.
                <fs_zbv_comentarios>-data_atualizacao = sy-datum.
                <fs_zbv_comentarios>-hora_atualizacao = sy-uzeit.
                <fs_zbv_comentarios>-criado_por = sy-uname.

                gs_zbv_chamados-data_atualizacao = <fs_zbv_comentarios>-data_atualizacao.
                gs_zbv_chamados-hora_atualizacao = <fs_zbv_comentarios>-hora_atualizacao.
                gs_zbv_chamados-atualizado_por = <fs_zbv_comentarios>-criado_por.

              ELSE.
                MESSAGE 'Somente quem criou o comentário pode modificá-lo.' TYPE 'I'.
                LEAVE TO SCREEN 0.
                CONTINUE.
              ENDIF.

          ENDCASE.
        ENDLOOP.
*        gs_zbv_chamados-data_atualizacao = <fs_zbv_comentarios>-data_atualizacao.
*        gs_zbv_chamados-hora_atualizacao = <fs_zbv_comentarios>-hora_atualizacao.
*        gs_zbv_chamados-atualizado_por = <fs_zbv_comentarios>-criado_por.

      WHEN OTHERS.
        MESSAGE 'Você não possui autorização!' TYPE 'E'.
    ENDCASE.
  ENDMETHOD.

  METHOD data_changed_hora.

    "Verificação de permissão de modificação
    AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
    CASE sy-subrc.
      WHEN 0.

        LOOP AT er_data_changed->mt_good_cells ASSIGNING FIELD-SYMBOL(<fs_good_cells>).
          READ TABLE gt_zbv_horas ASSIGNING FIELD-SYMBOL(<fs_zbv_horas>) INDEX <fs_good_cells>-row_id.
          CASE <fs_good_cells>-fieldname.
            WHEN horas_utilizadas.
              IF <fs_zbv_horas>-criado_por = sy-uname OR <fs_zbv_horas>-id_linha = 0.

                MOVE-CORRESPONDING <fs_zbv_horas> TO gs_zbv_horas_antes.

                <fs_zbv_horas>-horas_utilizadas = <fs_good_cells>-value.
                <fs_zbv_horas>-data_atualizacao = sy-datum.
                <fs_zbv_horas>-hora_atualizacao = sy-uzeit.
                <fs_zbv_horas>-criado_por = sy-uname.
                <fs_zbv_horas>-cliente = gs_zbv_chamados-cliente.
                <fs_zbv_horas>-acao = gs_zbv_chamados-acao.

                gs_zbv_chamados-data_atualizacao = <fs_zbv_horas>-data_atualizacao.
                gs_zbv_chamados-hora_atualizacao = <fs_zbv_horas>-hora_atualizacao.
                gs_zbv_chamados-atualizado_por = <fs_zbv_horas>-criado_por.

              ELSE.

                MESSAGE 'Somente quem criou o comentário pode modificá-lo.' TYPE 'I'.
                LEAVE TO SCREEN 0.
                CONTINUE.

              ENDIF.
          ENDCASE.
        ENDLOOP.

*    gs_zbv_chamados-data_atualizacao = <fs_zbv_horas>-data_atualizacao.
*    gs_zbv_chamados-hora_atualizacao = <fs_zbv_horas>-hora_atualizacao.

      WHEN OTHERS.
        MESSAGE 'Você não possui autorização!' TYPE 'E'.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.                    "lcl_handle_events IMPLEMENTATION


*----- selection-screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "Tela de Seleção para lista de chamados

  SELECT-OPTIONS:
  so_solic FOR wa_zbv_chamados-solicitante NO INTERVALS NO-EXTENSION,
  so_co_te FOR wa_zbv_chamados-consultor_tecnico NO INTERVALS NO-EXTENSION,
  so_id_ch FOR wa_zbv_chamados-id_chamado NO INTERVALS NO-EXTENSION,
  so_tp_ch FOR wa_zbv_chamados-tipo_chamado NO INTERVALS NO-EXTENSION,
  so_statu FOR wa_zbv_chamados-status NO INTERVALS NO-EXTENSION,
  so_prior FOR wa_zbv_chamados-prioridade NO-EXTENSION,
  so_dt_ab FOR wa_zbv_chamados-data_abertura.

SELECTION-SCREEN END OF BLOCK b1.


*---- start-of-selection
START-OF-SELECTION.

*-- read data into internal table
  PERFORM get_data.

  CHECK gt_zbv_chamados IS NOT INITIAL.
  SORT gt_zbv_chamados ASCENDING BY tipo_chamado id_chamado.

*-- display the table itab with CL_SALV_TABLE
  PERFORM create_salv_table.

  PERFORM build_layout.

  PERFORM display_output.

END-OF-SELECTION.

FORM get_data.
  SELECT * FROM zbv_chamados
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_chamados
      WHERE solicitante IN so_solic
      AND id_chamado IN so_id_ch
      AND data_abertura IN so_dt_ab
      AND consultor_tecnico IN so_co_te
      AND status IN so_statu
      AND prioridade IN so_prior
      AND tipo_chamado IN so_tp_ch.

  LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_zbv_chamados>).
    IF <fs_zbv_chamados>-status = aberto.
      <fs_zbv_chamados>-icon = icon_green_light.
    ELSEIF <fs_zbv_chamados>-status = em_atendi.
      <fs_zbv_chamados>-icon = icon_yellow_light.
    ELSEIF <fs_zbv_chamados>-status = encerrado.
      <fs_zbv_chamados>-icon = icon_checked.
    ELSEIF <fs_zbv_chamados>-status = cancelado.
      <fs_zbv_chamados>-icon = icon_incomplete.
    ELSEIF <fs_zbv_chamados>-status = parado.
      <fs_zbv_chamados>-icon = icon_red_light.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " GET_DATA


*&---------------------------------------------------------------------
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------
FORM display_output .
  gr_table->display( ).
ENDFORM.


" DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*& Form build_layout
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_layout.

  PERFORM set_toolbar_function.

*  gr_table->get_columns( )->set_optimize( abap_true ).
  gr_table->get_display_settings( )->set_striped_pattern( abap_true ).
  gr_table->get_display_settings( )->set_list_header( TEXT-003 ). "Relatório de Chamados Fábrica
  gr_table->get_columns( )->set_column_position( columnname = icon position = 1 ).
*-- column
  gr_columns = gr_table->get_columns( ).

  PERFORM exclude_column.
  PERFORM column_centered.

  TRY.
      gr_column ?= gr_columns->get_column( id_chamado ).
      gr_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

      PERFORM salv_table_events.
    CATCH cx_salv_not_found.

  ENDTRY.


ENDFORM.


*&---------------------------------------------------------------------*
*& Form create_salv_table
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM create_salv_table .
  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = gr_table
        CHANGING
          t_table      = gt_zbv_chamados.
    CATCH cx_salv_msg.

  ENDTRY.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form set_toolbar_function
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_toolbar_function .
*--toolbar function
  gr_functions = gr_table->get_functions( ).
  gr_functions->set_all( abap_true ).
ENDFORM.


*&---------------------------------------------------------------------*
*& Form salv_table_events
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM salv_table_events .
*-- events
  gr_events = gr_table->get_event( ).
  CREATE OBJECT event_handler.
  SET HANDLER event_handler->on_link_click_id FOR gr_events.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form exclude_column
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exclude_column.
  TRY.
      gr_column ?= gr_columns->get_column( mandt ).
      gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      gr_column ?= gr_columns->get_column( cenario_erro ).
      gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      gr_column ?= gr_columns->get_column( cenario_sucesso ).
      gr_column->set_technical( value = if_salv_c_bool_sap=>true ).
    CATCH cx_salv_not_found.

  ENDTRY.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form column_centered
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM column_centered .
  TRY.

      gr_column ?= gr_columns->get_column( tipo_chamado ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).

      gr_column ?= gr_columns->get_column( id_chamado ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( status ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 14 ).

      gr_column ?= gr_columns->get_column( acao ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 23 ).

      gr_column ?= gr_columns->get_column( cliente ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( consultor_tecnico ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 20 ).

      gr_column ?= gr_columns->get_column( ambiente ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( solicitante ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 20 ).

      gr_column ?= gr_columns->get_column( tecnologia ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( modulo ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 6 ).

      gr_column ?= gr_columns->get_column( em_atendimento ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 14 ).

      gr_column ?= gr_columns->get_column( complexidade ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 12 ).

      gr_column ?= gr_columns->get_column( prioridade ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( anexo ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_short_text(' ').
      gr_column->set_medium_text(' ').
      gr_column->set_long_text('DMS ID').
      gr_column->set_output_length( 23 ).

*      gr_column ?= gr_columns->get_column( cenario_erro ).
*      gr_column->set_alignment( if_salv_c_alignment=>centered ).
*
*      gr_column ?= gr_columns->get_column( cenario_sucesso ).
*      gr_column->set_alignment( if_salv_c_alignment=>centered ).

      gr_column ?= gr_columns->get_column( acesso_produtivo ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( aplicacao_nota ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( id_estimativa ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( data_abertura ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( hora_abertura ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( data_atualizacao ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( hora_atualizacao ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( data_encerramento ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( hora_encerramento ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_short_text(' ').
      gr_column->set_medium_text(' ').
      gr_column->set_long_text('Tempo Total').
      gr_column->set_output_length( 11 ).

      gr_column ?= gr_columns->get_column( icon ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'ICONE' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').

    CATCH cx_salv_not_found.

  ENDTRY.

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1001 OUTPUT.
  SET PF-STATUS 'STATUS_1001'.
  SET TITLEBAR 'TITLE_1001'.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1001 INPUT.
  CASE lv_okcode_1001.
    WHEN vis_com.
      IF lv_salvou_item = 'X'.
        lo_grid_com->refresh_table_display( ).

      ELSE.
        CALL SCREEN 1002 STARTING AT 2 2 "15 5
                         ENDING AT 130 25.
      ENDIF.

    WHEN vis_cen_erro.
      CALL SCREEN 1003 STARTING AT 2 2 "10 5
                       ENDING AT 120 20.

    WHEN vis_cen_suc.
      CALL SCREEN 1004 STARTING AT 2 2 "10 5
                       ENDING AT 120 20.

    WHEN vis_anexos.
      CALL SCREEN 1005 STARTING AT 2 2 "15 5
                             ENDING AT 130 25.

    WHEN ger_horas.
      IF lv_salvou_item = 'X'.
        lo_grid_hrs->refresh_table_display( ).
      ELSE.
        CALL SCREEN 1007 STARTING AT 2 2 "15 5
                               ENDING AT 130 25.
      ENDIF.

    WHEN log_atl.
      CALL SCREEN 1008 STARTING AT 2 2 "15 5
                             ENDING AT 130 25.

    WHEN ok.
      IF gs_zbv_chamados-tipo_chamado = 'Execução'.
        IF ( ( gs_zbv_chamados-horas_estimadas - gs_zbv_chamados-horas_utilizadas )  <= 15 ).
          MESSAGE 'Chamado está prestes a estourar as horas estimatidas!' TYPE 'I' DISPLAY LIKE 'W'.
        ENDIF.
      ENDIF.

      PERFORM f_refresh_tela_1001.
      LEAVE TO SCREEN 0.

    WHEN exit.
      LEAVE PROGRAM.

    WHEN save.
      CLEAR: lv_hora_temp, log_linha.

      DATA gs_detail_tab TYPE ty_detail_tab.

      TYPE-POOLS: abap.

      DATA: ls_components TYPE abap_compdescr.
      DATA: lo_strucdescr TYPE REF TO cl_abap_structdescr.

      lo_strucdescr ?= cl_abap_typedescr=>describe_by_data( gs_zbv_chamados_antes ).

      PERFORM f_obtem_dados_logs.

      DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.


      IF gs_zbv_chamados-status = 'Encerrado'. "encerrado.
        gs_zbv_chamados-data_encerramento = sy-datum.
        gs_zbv_chamados-hora_encerramento = sy-uzeit.

        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Encerrar a contagem do chamado em aberto
            PERFORM encerra_contagem_em_aberto.

          WHEN 'Desenvolvimento'.
            "Encerrar a contagem do tempo em Desenvolvimento
            PERFORM encerra_contagem_dev.

          WHEN 'Em atendimento'.
            "Encerrar a contagem do tempo em Atendimento
            PERFORM encerra_contagem_em_atnd.

          WHEN 'Parado'.
            "Encerra contagem do tempo Parado
            PERFORM encerra_contagem_parado.

          WHEN 'Teste Funcional'.
            "Encerrar a contagem do TEMPO em TESTE FUNCIONAL
            PERFORM encerra_contagem_tst_func.

          WHEN 'Teste Cliente'.
            "Encerrar a contagem do TEMPO em teste cliente
            PERFORM encerra_contagem_tst_clnt.

          WHEN OTHERS.
        ENDCASE.


        "Fazer o calculo do tempo total
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from       = gs_zbv_chamados-data_abertura
            date_to         = gs_zbv_chamados-data_encerramento
            time_from       = gs_zbv_chamados-hora_abertura
            time_to         = gs_zbv_chamados-hora_encerramento
          IMPORTING
            delta_time      = tempo_minutos
*           DELTA_UNIT      =
          EXCEPTIONS
            from_greater_to = 1
            OTHERS          = 2.

        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          tempo_total = tempo_minutos / 60.
          gs_zbv_chamados-tempo_total = tempo_total.

          MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

          MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

        ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

        "salvar em uma tabela de log de atendimento

      ELSEIF gs_zbv_chamados-status = 'Em atendimento'. "em_atendimento.

        "Seleciona todos os registros de chamados em Desenvolvimento
        SELECT * FROM zbv_tmp_em_atnd
          INTO TABLE gt_zbv_tmp_em_atnd
          WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
          AND   id_chamado   = gs_zbv_chamados-id_chamado
          AND   cliente      = gs_zbv_chamados-cliente
          AND   acao         = gs_zbv_chamados-acao.

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_tmp_em_atnd INTO gs_zbv_tmp_em_atnd INDEX sy-dbcnt.

        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Encerrar a contagem do chamado em aberto
            PERFORM encerra_contagem_em_aberto.

            "Iniciar a contagem do Tempo Em Atendimento
            PERFORM registra_contagem_em_atnd.

          WHEN 'Cancelado'.

          WHEN 'Desenvolvimento'.
            "Encerrar a contagem do tempo em Desenvolvimento
            PERFORM encerra_contagem_dev.

            "Iniciar a contagem do Tempo Em Atendimento
            PERFORM registra_contagem_em_atnd.

          WHEN 'Em atendimento'.
            "nada acontece

          WHEN 'Parado'.
            "Encerra contagem do tempo Parado
            PERFORM encerra_contagem_parado.

            "Iniciar a contagem do tempo Em Atendimento
            PERFORM registra_contagem_em_atnd.

          WHEN 'Teste Funcional'.
            "Encerrar a contagem do TEMPO em TESTE FUNCIONAL
            PERFORM encerra_contagem_tst_func.

            "Iniciar a contagem do tempo Em Atendimento
            PERFORM registra_contagem_em_atnd.

          WHEN 'Teste Cliente'.
            "Encerrar a contagem do TEMPO em teste cliente
            PERFORM encerra_contagem_tst_clnt.

            "Iniciar a contagem do tempo Em Atendimento
            PERFORM registra_contagem_em_atnd.

          WHEN 'Encerrado'.
            "Verificar possível tratativa
          WHEN OTHERS.
        ENDCASE.


      ELSEIF gs_zbv_chamados-status = 'Desenvolvimento'.

        "Seleciona todos os registros de chamados em Desenvolvimento
        SELECT * FROM zbv_tmp_dev
          INTO TABLE gt_zbv_tmp_dev
          WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
          AND   id_chamado   = gs_zbv_chamados-id_chamado
          AND   cliente      = gs_zbv_chamados-cliente
          AND   acao         = gs_zbv_chamados-acao.

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_tmp_dev INTO gs_zbv_tmp_dev INDEX sy-dbcnt.


        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Encerrar a contagem do tempo em aberto
            PERFORM encerra_contagem_em_aberto.

            "Iniciar a contagem do tempo em desenvolvimento
            PERFORM registra_contagem_dev.

          WHEN 'Cancelado'.

          WHEN 'Desenvolvimento'.
            "Nada Acontece

          WHEN 'Em atendimento'.
            "Encerra a contatem do tempo Em Atendimento
            PERFORM encerra_contagem_em_atnd.

            "Iniciar a contagem do tempo em Desenvolvimento
            PERFORM registra_contagem_dev.

          WHEN 'Parado'.
            "Encerra contagem do tempo Parado
            PERFORM encerra_contagem_parado.

            "Iniciar a contagem do tempo em Desenvolvimento
            PERFORM registra_contagem_dev.

          WHEN 'Teste Funcional'.
            "Encerrar a contagem do TEMPO em TESTE FUNCIONAL
            PERFORM encerra_contagem_tst_func.

            "Iniciar a contagem do tempo em Desenvolvimento
            PERFORM registra_contagem_dev.

          WHEN 'Teste Cliente'.
            "Encerrar a contagem do TEMPO em teste cliente
            PERFORM encerra_contagem_tst_clnt.

            "Iniciar a contagem do tempo em Desenvolvimento
            PERFORM registra_contagem_dev.

          WHEN 'Encerrado'.
            "Verificar possível tratativa
          WHEN OTHERS.
        ENDCASE.

      ELSEIF gs_zbv_chamados-status = 'Parado'. "parado.

        "Seleciona todos os registros de chamados parados
        SELECT * FROM zbv_tmp_parado
          INTO TABLE gt_zbv_tmp_parado
          WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
          AND   id_chamado   = gs_zbv_chamados-id_chamado
          AND   cliente      = gs_zbv_chamados-cliente
          AND   acao         = gs_zbv_chamados-acao.

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_tmp_parado INTO gs_zbv_tmp_parado INDEX sy-dbcnt.


        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Iniciar a contagem do chamado parado
            PERFORM encerra_contagem_em_aberto.

            PERFORM registra_contagem_parado.

          WHEN 'Cancelado'.

          WHEN 'Desenvolvimento'.
            "Encerrar a contagem do TEMPO Em Atendimento
            PERFORM encerra_contagem_dev.

            "Iniciar a contagem do TEMPO PARADO
            PERFORM registra_contagem_parado.

          WHEN 'Em atendimento'.
            "Encerrar a contagem do TEMPO Em Atendimento
            PERFORM encerra_contagem_em_atnd.

            "Iniciar a contagem do TEMPO PARADO
            PERFORM registra_contagem_parado.

          WHEN 'Parado'.
            "Contagem do TEMPO PARADO CONTINUA

          WHEN 'Teste Funcional'.
            "Encerrar a contagem do TEMPO em TESTE FUNCIONAL
            PERFORM encerra_contagem_tst_func.

            "Iniciar a contagem do TEMPO PARADO
            PERFORM registra_contagem_parado.

          WHEN 'Teste Cliente'.
            "Encerrar a contagem do TEMPO em TESTE FUNCIONAL
            PERFORM encerra_contagem_tst_clnt.

            "Iniciar a contagem do TEMPO PARADO
            PERFORM registra_contagem_parado.

          WHEN 'Encerrado'.
            "Verificar possível tratativa
          WHEN OTHERS.
        ENDCASE.

      ELSEIF gs_zbv_chamados-status = 'Teste Funcional'.

        "Seleciona todos os registros do chamadoo em Teste Funcional
        SELECT * FROM zbv_tmp_tst_func
          INTO TABLE gt_zbv_tmp_tst_func
          WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
          AND   id_chamado   = gs_zbv_chamados-id_chamado
          AND   cliente      = gs_zbv_chamados-cliente
          AND   acao         = gs_zbv_chamados-acao.

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_tmp_tst_func INTO gs_zbv_tmp_tst_func INDEX sy-dbcnt.

        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Encerrar a contagem do tempo em aberto
            PERFORM encerra_contagem_em_aberto.

            "Iniciar a contagem do TEMPO TESTE FUNCIONAL
            PERFORM registra_contagem_tst_func.

          WHEN 'Cancelado'.

          WHEN 'Desenvolvimento'.
            "Iniciar a contagem do tempo em Desenvolvimento
            PERFORM encerra_contagem_dev.

            "Iniciar a contagem do TEMPO TESTE FUNCIONAL
            PERFORM registra_contagem_tst_func.

          WHEN 'Em atendimento'.
            "Iniciar a contagem do tempo Em Atendimento
            PERFORM encerra_contagem_em_atnd.

            "Iniciar a contagem do TEMPO TESTE FUNCIONAL
            PERFORM registra_contagem_tst_func.

          WHEN 'Parado'.
            "Contagem do TEMPO PARADO ENCERRADO
            PERFORM encerra_contagem_parado.

            "Inicia contagem do TEMPO TESTE FUNCIONAL
            PERFORM registra_contagem_tst_func.

          WHEN 'Teste Funcional'.
            "se está com o mesmo status nada acontece

          WHEN 'Teste Cliente'.
            "Encerrar a contagem do TEMPO em TESTE CLIENTE
            PERFORM encerra_contagem_tst_clnt.

            "Inicia contagem do TEMPO TESTE FUNCIONAL
            PERFORM registra_contagem_tst_func.

          WHEN 'Encerrado'.
            "Verificar possível tratativa

          WHEN OTHERS.
        ENDCASE.

      ELSEIF gs_zbv_chamados-status = 'Teste Cliente'.

        "Seleciona todos os registros do chamadoo em Teste Cliente
        SELECT * FROM zbv_tmp_tst_clnt
          INTO TABLE gt_zbv_tmp_tst_clnt
          WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
          AND   id_chamado   = gs_zbv_chamados-id_chamado
          AND   cliente      = gs_zbv_chamados-cliente
          AND   acao         = gs_zbv_chamados-acao.

        "Le o ultimo registro da tabela
        READ TABLE gt_zbv_tmp_tst_func INTO gs_zbv_tmp_tst_func INDEX sy-dbcnt.

        CASE gs_zbv_chamados_antes-status.
          WHEN 'Aberto'.
            "Encerrar a contagem do tempo em aberto
            PERFORM encerra_contagem_em_aberto.

            "Iniciar a contagem do TEMPO TESTE CLIENTE
            PERFORM registra_contagem_tst_clnt.

          WHEN 'Cancelado'.

          WHEN 'Desenvolvimento'.
            "Encerrar a contagem do Tempo em Desenvolvimento
            PERFORM encerra_contagem_dev.

            "Iniciar a contagem do TEMPO TESTE CLIENTE
            PERFORM registra_contagem_tst_clnt.

          WHEN 'Em atendimento'.
            "Encerrar a contagem do Tempo Em Atendimento
            PERFORM encerra_contagem_em_atnd.

            "Iniciar a contagem do TEMPO TESTE CLIENTE
            PERFORM registra_contagem_tst_clnt.

          WHEN 'Parado'.
            "Encerrar contagem do TEMPO PARADO
            PERFORM encerra_contagem_parado.

            "Inicia contagem do TEMPO TESTE CLIENTE
            PERFORM registra_contagem_tst_clnt.

          WHEN 'Teste Funcional'.
            "Encerrar contagem do TEMPO Teste Funcional
            PERFORM encerra_contagem_tst_func.

            "Inicia contagem do TEMPO TESTE CLIENTE
            PERFORM registra_contagem_tst_clnt.

          WHEN 'Teste Cliente'.
            "Se mantém o mesmo status nada acontece

          WHEN 'Encerrado'.
            "Verificar possível tratativa
          WHEN OTHERS.
        ENDCASE.

      ENDIF.

      gs_zbv_chamados-data_atualizacao = sy-datum.
      gs_zbv_chamados-hora_atualizacao = sy-uzeit.
      gs_zbv_chamados-atualizado_por = sy-uname.
      MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

      LOOP AT lo_strucdescr->components INTO ls_components.
        gs_detail_tab-nome = ls_components-name.
        ASSIGN COMPONENT sy-tabix OF STRUCTURE gs_zbv_chamados_antes TO FIELD-SYMBOL(<fs_antes>).
        gs_detail_tab-valor = <fs_antes>.
        APPEND gs_detail_tab TO lt_detail_tab.
        CLEAR: gs_detail_tab.
        UNASSIGN <fs_antes>.

      ENDLOOP.

      LOOP AT lt_detail_tab ASSIGNING FIELD-SYMBOL(<fs_lt_detail_tab>).
        ASSIGN COMPONENT sy-tabix OF STRUCTURE gs_zbv_chamados TO FIELD-SYMBOL(<fs_final>).

        IF <fs_lt_detail_tab>-valor <> <fs_final>.

          IF <fs_lt_detail_tab>-nome = 'DATA_ATUALIZACAO'  OR
             <fs_lt_detail_tab>-nome = 'HORA_ATUALIZACAO'  OR
             <fs_lt_detail_tab>-nome = 'DATA_ENCERRAMENTO' OR
             <fs_lt_detail_tab>-nome = 'HORA_ENCERRAMENTO' OR
             <fs_lt_detail_tab>-nome = 'HORAS_UTILIZADAS'  OR
             <fs_lt_detail_tab>-nome = 'ATUALIZADO_POR'.
            CONTINUE.
          ELSE.

            MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
            gs_zbv_log_atuali-atualizacao = 'Campos'.
            gs_zbv_log_atuali-campo = <fs_lt_detail_tab>-nome.
            gs_zbv_log_atuali-antes = <fs_lt_detail_tab>-valor.
            gs_zbv_log_atuali-depois = <fs_final>.

            IF log_linha = 0.
              gs_zbv_log_atuali-n_registro = 1.
            ELSE.
              gs_zbv_log_atuali-n_registro = log_linha + 1.
            ENDIF.

            INSERT zbv_log_atuali FROM gs_zbv_log_atuali.
*          APPEND gs_zbv_log_atuali TO gt_zbv_log_atuali.
            log_linha += 1.

          ENDIF.

        ENDIF.

      ENDLOOP.

      CLEAR: gs_zbv_chamados_antes,
             lt_detail_tab,
             ls_components.

      FREE: lo_strucdescr.


      MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_antes.

      MODIFY zbv_chamados FROM gs_zbv_chamados_sc.
      CHECK sy-subrc = 0.

      PERFORM f_refresh_tela_1001.

      IF gs_zbv_chamados-tipo_chamado = 'Execução'.
        IF ( ( gs_zbv_chamados-horas_estimadas - gs_zbv_chamados-horas_utilizadas )  <= 15 ).
          MESSAGE 'Chamado está prestes a estourar as horas estimatidas!' TYPE 'I' DISPLAY LIKE 'W'.
        ENDIF.
      ENDIF.

      LEAVE TO SCREEN 0.

  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form textEditor_erro
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM textEditor_erro .
  IF lo_editor_cen_er IS NOT BOUND. "IS INITIAL.
    "Criar Container (Necessário criar o container no Layout da tela)
    CREATE OBJECT lo_editor_cen_er_con
      EXPORTING
        container_name              = 'CONTAINER_ERRO' "container_erro
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    "Instância obj to tipo textedit
    CREATE OBJECT lo_editor_cen_er
      EXPORTING
        parent = lo_editor_cen_er_con.
*        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
*        wordwrap_position          = 70 "c_line_lenght
*        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    "Passa as linhas de texto via tabela interna
    lo_editor_cen_er->set_text_as_r3table( lt_table[] ).


    "Se houver conteúdo salvo, devemos somente mostrar o textarea sem edição.
    lo_editor_cen_er->set_readonly_mode( ).


    "Remove os botões (toolbar)
*    lo_editor_cen_er->set_toolbar_mode( 0 ).

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form textEditor_sucesso
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM textEditor_sucesso.
  IF lo_editor_cen_su IS NOT BOUND. "IS INITIAL.
    "Criar Container (Necessário criar o container no Layout da tela)
    CREATE OBJECT lo_editor_cen_su_con
      EXPORTING
        container_name              = 'CONTAINER_SUCESSO' "container_sucesso
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    "Instância obj to tipo textedit
    CREATE OBJECT lo_editor_cen_su
      EXPORTING
        parent = lo_editor_cen_su_con.
*        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
*        wordwrap_position          = c_line_lenght
*        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    "Passa as linhas de texto via tabela interna
    lo_editor_cen_su->set_text_as_r3table( lt_table_su[] ).

    "Se houver conteúdo salvo, devemos somente mostrar o textarea sem edição.
    lo_editor_cen_su->set_readonly_mode( ).

    "Remove os botões (toolbar)
*    lo_editor_cen_su->set_toolbar_mode( 0 ).

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_build_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      --> P_
*&      <-- LT_FIELDCAT[]
*&---------------------------------------------------------------------*
FORM f_build_fieldcat USING VALUE(p_fieldname) TYPE c
                            VALUE(p_field)     TYPE c
                            VALUE(p_table)     TYPE c
                            VALUE(p_coltext)   TYPE c
                            VALUE(p_edit)      TYPE c
*                            VALUE(p_seltext_l) TYPE c
                            VALUE(p_outputlen) TYPE i
                         CHANGING t_fieldcat   TYPE lvc_t_fcat.

  DATA: ls_fieldcat LIKE LINE OF t_fieldcat[].
  ls_fieldcat-fieldname = p_fieldname.
  ls_fieldcat-ref_field = p_field.
  ls_fieldcat-ref_table = p_table.
  ls_fieldcat-coltext   = p_coltext.
  ls_fieldcat-edit      = p_edit.
  ls_fieldcat-just      = 'C'.
  ls_fieldcat-outputlen = p_outputlen.
  APPEND ls_fieldcat TO t_fieldcat[].

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1002 OUTPUT.
  SET PF-STATUS 'STATUS_1002'.
  SET TITLEBAR 'TITLE_1002'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1002 INPUT.
  CASE lv_okcode_1002.
    WHEN ok.
      CLEAR: lv_salvou_item.
      LEAVE TO SCREEN 0.
    WHEN save OR 'SALVAR'.
      "gt_zbv_comentarios
      PERFORM f_salvar_alteracoes.
    WHEN add.

      CLEAR: log_linha.

      "Validação de permissão de Modificar (Adicionar)
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.

          "Obtem os dados da tabela de comentarios
          PERFORM f_obtem_dados_comentarios.

          READ TABLE gt_zbv_comentarios INTO gs_zbv_comentarios INDEX sy-dbcnt.
          IF sy-subrc = 0.

            "Obtem os dados da tabela de log
            PERFORM f_obtem_dados_logs.
            DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

            CLEAR: gs_zbv_comentarios-comentario.
            gs_zbv_comentarios-id_linha += 1.
            gs_zbv_comentarios-data_atualizacao = sy-datum.
            gs_zbv_comentarios-hora_atualizacao = sy-uzeit.
            gs_zbv_comentarios-criado_por = sy-uname.
            gs_zbv_comentarios-cliente = gs_zbv_chamados-cliente.
            gs_zbv_comentarios-acao = gs_zbv_chamados-acao.
            APPEND gs_zbv_comentarios TO gt_zbv_comentarios.

            MOVE-CORRESPONDING gs_zbv_comentarios TO gs_zbv_log_atuali.
            gs_zbv_log_atuali-atualizado_por = gs_zbv_comentarios-criado_por.
            gs_zbv_log_atuali-atualizacao = 'Adicionar'.
            gs_zbv_log_atuali-campo = 'Linha - Comentário'.
            gs_zbv_log_atuali-antes = ' '.
            gs_zbv_log_atuali-depois = ' '.

            IF log_linha = 0.
              gs_zbv_log_atuali-n_registro = 1.
            ELSE.
              gs_zbv_log_atuali-n_registro = log_linha + 1.
            ENDIF.

            MODIFY zbv_comentarios FROM TABLE gt_zbv_comentarios.
            CHECK sy-subrc = 0.

            INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

            IF sy-subrc = 0.
              COMMIT WORK.
              PERFORM f_obtem_dados_comentarios.
              MESSAGE TEXT-004 TYPE 'I' DISPLAY LIKE 'S'. "Linha adicionada com sucesso

            ELSE.
              ROLLBACK WORK.
              MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao adicionar linha!

            ENDIF.
          ELSE.
            MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'. "Erro no processo de adicionar linha!
          ENDIF.

        WHEN OTHERS.
          MESSAGE 'Você não tem autorização' TYPE 'W'.
      ENDCASE.

    WHEN del.

      CLEAR: log_linha.

      CALL METHOD lo_grid_com->get_selected_rows
        IMPORTING
          et_index_rows = rows.


      LOOP AT rows INTO wa_row.
        READ TABLE gt_zbv_comentarios INTO gs_zbv_comentarios INDEX wa_row-index.

        "Verificar se possível permissão para Eliminar(deletar)
        AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '06'.
        CASE sy-subrc.
          WHEN 0.

            IF gs_zbv_comentarios-criado_por = sy-uname.

              "Obtem os dados da tabela de log
              PERFORM f_obtem_dados_logs.
              DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

              DELETE FROM zbv_comentarios WHERE id_linha     = gs_zbv_comentarios-id_linha
                                          AND   tipo_chamado = gs_zbv_comentarios-tipo_chamado
                                          AND   id_chamado   = gs_zbv_comentarios-id_chamado
                                          AND   cliente      = gs_zbv_comentarios-cliente
                                          AND   acao         = gs_zbv_comentarios-acao.

              IF sy-subrc = 0.
                gs_zbv_chamados-data_atualizacao = sy-datum.
                gs_zbv_chamados-hora_atualizacao = sy-uzeit.
                gs_zbv_chamados-atualizado_por = sy-uname.

                MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

                MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
                gs_zbv_log_atuali-atualizacao = 'Deletar'.
                gs_zbv_log_atuali-campo = 'Linha - Comentário'.
                gs_zbv_log_atuali-antes = gs_zbv_comentarios-comentario.
                gs_zbv_log_atuali-depois = ' '.
                gs_zbv_log_atuali-cliente = gs_zbv_comentarios-cliente.
                gs_zbv_log_atuali-acao = gs_zbv_comentarios-acao.

                IF log_linha = 0.
                  gs_zbv_log_atuali-n_registro = 1.
                ELSE.
                  gs_zbv_log_atuali-n_registro = log_linha + 1.
                ENDIF.

                MODIFY zbv_chamados FROM gs_zbv_chamados_sc.
                CHECK sy-subrc = 0.

                INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

                COMMIT WORK.
                MESSAGE TEXT-007 TYPE 'I' DISPLAY LIKE 'S'. "Comentário removido com sucesso!
              ELSE.
                ROLLBACK WORK.
                MESSAGE TEXT-008 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao remover a linha do comentário!

              ENDIF. "DELETE FROM zbv_comentarios

            ELSE.
              MESSAGE 'Somente quem criou o comentário pode eliminá-lo!' TYPE 'W'.

            ENDIF. "gs_zbv_comentarios-criado_por = sy-uname

          WHEN OTHERS.
            MESSAGE 'Você não possui permissão' TYPE 'W'.
        ENDCASE.

      ENDLOOP.


    WHEN vis. "'VIS'.

      DATA no_rows TYPE n.

      CLEAR: rows, wa_row, no_rows.

      CALL METHOD lo_grid_com->get_selected_rows
        IMPORTING
          et_index_rows = rows.

      DESCRIBE TABLE rows LINES no_rows.

      IF no_rows > 1.
        MESSAGE TEXT-034 TYPE 'I' DISPLAY LIKE 'W'. "Não é possível visualizar mais de um comentário por vez

      ELSE.
        READ TABLE rows INTO wa_row INDEX 1.
        IF sy-subrc = 0.
          READ TABLE gt_zbv_comentarios INTO gs_zbv_comentarios INDEX wa_row-index.

          IF sy-subrc = 0.
            APPEND gs_zbv_comentarios-comentario TO lt_table_com.
            CALL SCREEN 1006 STARTING AT 10 5
                         ENDING AT 120 20.
          ELSE.
            MESSAGE TEXT-033 TYPE 'I' DISPLAY LIKE 'E'. "Nenhum comentário foi selecionado
          ENDIF.
        ENDIF.
      ENDIF.

    WHEN back.
      CLEAR: lv_salvou_item.
      LEAVE TO SCREEN 0.

    WHEN exit. "'CANCEL'.
      CLEAR: lv_salvou_item.
      LEAVE TO SCREEN 0.

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_1002 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_show_grid_1002 OUTPUT.

  FREE:lt_fieldcat_com.

  PERFORM f_obtem_dados_comentarios.

  IF gt_zbv_comentarios IS INITIAL.
    CLEAR: gs_zbv_comentarios.
    gs_zbv_comentarios-id_linha = 0.
    gs_zbv_comentarios-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_comentarios-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_comentarios-cliente = gs_zbv_chamados-cliente.
    gs_zbv_comentarios-acao = gs_zbv_chamados-acao.
*    gs_zbv_comentarios-criado_por = sy-uname.
    APPEND gs_zbv_comentarios TO gt_zbv_comentarios.
  ENDIF.

*  ls_layout_com-cwidth_opt = 'X'.

  PERFORM f_eliminar_botoes_alv.

*  PERFORM f_build_fieldcat USING:
*    'ID_LINHA' 'ID_LINHA' 'ZBV_COMENTARIOS' 'ID Linha' '' 8 CHANGING lt_fieldcat_com[].

  PERFORM f_build_fieldcat USING:
    id_chamado       id_chamado       tab_comentario 'ID Chamado'       '' 17 CHANGING lt_fieldcat_com[].
  PERFORM f_build_fieldcat USING:
    tipo_chamado     tipo_chamado     tab_comentario 'Tipo Chamado'     '' 14 CHANGING lt_fieldcat_com[].
  PERFORM f_build_fieldcat USING:
    'CRIADO_POR'     'CRIADO_POR'     tab_comentario 'Criado por' '' 18 CHANGING lt_fieldcat_com[].
  PERFORM f_build_fieldcat USING:
    data_atualizacao data_atualizacao tab_comentario 'Data Atualização' '' 18 CHANGING lt_fieldcat_com[].
  PERFORM f_build_fieldcat USING:
    hora_atualizacao hora_atualizacao tab_comentario 'Hora Atualização' '' 18 CHANGING lt_fieldcat_com[].
  PERFORM f_build_fieldcat USING:
    comentario       comentario       tab_comentario 'Comentário'       'X' 50 CHANGING lt_fieldcat_com[].

  IF lo_grid_com IS NOT BOUND. "lo_grid_com IS INITIAL.
    CREATE OBJECT lo_container_com
      EXPORTING
        container_name = 'CONTAINER_COMENTARIO'. "container_comentario.

    CREATE OBJECT lo_grid_com
      EXPORTING
        i_parent = lo_container_com.

    "Permite seleção múltipla de linhas
    lo_grid_com->set_ready_for_input( 1 ).

    "Permite alteração da célula
    lo_grid_com->register_edit_event(
      i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    lo_grid_com->set_table_for_first_display(
      EXPORTING
        it_toolbar_excluding = lt_tool_bar
*        is_variant      = ls_variant_com
*        is_layout       = ls_layout_com
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = lt_fieldcat_com[]
        it_outtab       = gt_zbv_comentarios
    ).

    lo_grid_com->set_gridtitle( TEXT-009 ). "Lista de comentários

    SET HANDLER event_handler->data_changed FOR lo_grid_com.
  ELSE.
    lo_grid_com->refresh_table_display( ).
  ENDIF.
ENDMODULE.


FORM f_eliminar_botoes_alv.

*  APPEND cl_gui_alv_grid=>mc_fc_excl_all                TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_evt_delayed_change_select  TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_evt_delayed_move_curr_cell TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_evt_enter                  TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_evt_modified               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_auf                     TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_average                 TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_back_classic            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_abc                TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_chain              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_crbatch            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_crweb              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_lineitems          TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_master_data        TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_more               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_report             TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_xint               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_call_xxl                TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_check                   TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_col_invisible           TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_col_optimize            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_count                   TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_current_variant         TO lt_tool_bar.
*  APPEND cl_gui_alv_grid=>mc_fc_data_save               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_delete_filter           TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_deselect_all            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_detail                  TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdata               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_expcrdesig              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_expcrtempl              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_expmdb                  TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_extend                  TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_f4                      TO lt_tool_bar.
*  APPEND cl_gui_alv_grid=>mc_fc_filter                  TO lt_tool_bar.
*  APPEND cl_gui_alv_grid=>mc_fc_find                    TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_fix_columns             TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_graph                   TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_help                    TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_info                    TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_load_variant            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_append_row          TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy                TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_copy_row            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_cut                 TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_delete_row          TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_insert_row          TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_move_row            TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste               TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_paste_new_row       TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_loc_undo                TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_maintain_variant        TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_maximum                 TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_minimum                 TO lt_tool_bar.
*  APPEND cl_gui_alv_grid=>mc_fc_pc_file                 TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_print                   TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_print_back              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_print_prev              TO lt_tool_bar.
  APPEND cl_gui_alv_grid=>mc_fc_refresh                 TO lt_tool_bar.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_salvar_alteracoes
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_salvar_alteracoes .

  CLEAR: log_linha,
         gs_zbv_comentarios.

  MODIFY zbv_comentarios FROM TABLE gt_zbv_comentarios.
  IF sy-subrc = 0.

    "Obtem os dados da tabela de log
    PERFORM f_obtem_dados_logs.
    DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

    MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

    MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
    gs_zbv_log_atuali-campo = 'Linha - Comentário'.

*    READ TABLE gt_zbv_comentarios INTO gs_zbv_comentarios INDEX gs_zbv_comentario_antes-id_linha + 1.

    LOOP AT gt_zbv_comentarios INTO gs_zbv_comentarios WHERE id_linha = gs_zbv_comentario_antes-id_linha.
      gs_zbv_log_atuali-antes = gs_zbv_comentario_antes-comentario.
      gs_zbv_log_atuali-depois = gs_zbv_comentarios-comentario.
    ENDLOOP.

    IF gs_zbv_comentario_antes-comentario IS NOT INITIAL.
      gs_zbv_log_atuali-atualizacao = 'Alterar'.
    ELSE.
      gs_zbv_log_atuali-atualizacao = 'Criado'.
    ENDIF.

    IF log_linha = 0.
      gs_zbv_log_atuali-n_registro = 1.
    ELSE.
      gs_zbv_log_atuali-n_registro = log_linha + 1.
    ENDIF.

    MODIFY zbv_chamados FROM gs_zbv_chamados_sc.
    CHECK sy-subrc = 0.

    INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

    IF sy-subrc = 0.
      COMMIT WORK.
      lv_salvou_item = 'X'.
      PERFORM f_obtem_dados_comentarios.
      MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'S'. "Dados salvos na tabela com sucesso!
      lo_grid_com->refresh_table_display( ).
      CLEAR: lv_salvou_item,
      gs_zbv_comentario_antes.
*    LEAVE TO SCREEN 0.
    ELSE.
      ROLLBACK WORK.
      MESSAGE TEXT-011 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao salvar dados na tabela!

    ENDIF.
  ELSE.
    MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao salvar comentario na tabela do banco!
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_obtem_dados_comentarios
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_obtem_dados_comentarios .
  SELECT * FROM zbv_comentarios
            INTO TABLE gt_zbv_comentarios
              WHERE id_chamado = gs_zbv_chamados-id_chamado
              AND   tipo_chamado = gs_zbv_chamados-tipo_chamado
              AND   cliente      = gs_zbv_chamados-cliente
              AND   acao         = gs_zbv_chamados-acao.
ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1003 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1003 OUTPUT.
  SET PF-STATUS 'STATUS_1003'.
  SET TITLEBAR 'TITLE_1003'.

*TextEditor Cenario de Erro
  PERFORM textEditor_erro.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1003 INPUT.
  CASE lv_okcode_1003.
    WHEN back OR ok. "OR 'VOLTAR'.
      LEAVE TO SCREEN 0.
      FREE: lo_editor_cen_er, lo_editor_cen_er_con.
    WHEN exit.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_1004 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1004 OUTPUT.
  SET PF-STATUS 'STATUS_1004'.
  SET TITLEBAR 'TITLE_1004'.

  PERFORM textEditor_sucesso.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1004 INPUT.
  CASE lv_okcode_1004.
    WHEN back OR ok. "OR 'VOLTAR'.
      LEAVE TO SCREEN 0.
      FREE: lo_editor_cen_su, lo_editor_cen_su_con.
    WHEN exit. "OR 'SAIR'.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form f_refresh_tela_1001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_refresh_tela_1001 .

  IF gr_table IS BOUND.
    CLEAR: gt_zbv_chamados.

    PERFORM get_data.
    SORT gt_zbv_chamados ASCENDING BY id_chamado.
    gr_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
    cl_gui_cfw=>flush( ).
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form refresh_call_screen_1001
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM refresh_call_screen_1001.

  IF gr_table IS BOUND.
    CLEAR: gt_zbv_chamados.

    PERFORM get_data.
    PERFORM build_layout.
    SORT gt_zbv_chamados ASCENDING BY id_chamado.
    gr_table->refresh( refresh_mode = if_salv_c_refresh=>full ).
*  cl_gui_cfw=>flush( ).
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Module LOOP_SCREEN_1001 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE loop_screen_1001 OUTPUT.
* SET PF-STATUS 'xxxxxxxx'.
* SET TITLEBAR 'xxx'.


  "Verificação de permissão de Registro
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD 'AG'.
  CASE sy-subrc.
    WHEN 0.
      LOOP AT SCREEN.
        IF screen-name = 'GS_ZBV_CHAMADOS-TIPO_CHAMADO'
        OR screen-name = 'GS_ZBV_CHAMADOS-ID_CHAMADO'
        OR screen-name = 'GS_ZBV_CHAMADOS-CLIENTE'
        OR screen-name = 'GS_ZBV_CHAMADOS-ACAO'.

          screen-input = '0'.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.
    WHEN OTHERS.
*  IF sy-uname EQ 'MVBBERENGER'.

      IF gs_zbv_chamados-tipo_chamado EQ 'Análise'.
        LOOP AT SCREEN.
          IF screen-name = 'VISUALIZAR_COMENTARIOS'
          OR screen-name = 'VISUALIZAR_CENARIO_ERRO'
          OR screen-name = 'VISUALIZAR_CENARIO_SUCESSO'
          OR screen-name = 'VISUALIZAR_ANEXOS'.
            screen-input = '1'.
            MODIFY SCREEN.
          ELSE.
            screen-input = '0'.
            screen-values_in_combo = '0'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-STATUS'.
            screen-intensified = '1'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-ID_ESTIMATIVA'
          OR screen-name = 'GS_ZBV_CHAMADOS-APLICACAO_NOTA'.
            screen-active = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      ELSEIF gs_zbv_chamados-tipo_chamado EQ 'Estimativa'.
        LOOP AT SCREEN.
          IF screen-name = 'VISUALIZAR_COMENTARIOS'
          OR screen-name = 'VISUALIZAR_CENARIO_ERRO'
          OR screen-name = 'VISUALIZAR_CENARIO_SUCESSO'
          OR screen-name = 'VISUALIZAR_ANEXOS'.
            screen-input = '1'.
            MODIFY SCREEN.
          ELSE.
            screen-input = '0'.
            screen-values_in_combo = '0'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-STATUS'.
            screen-intensified = '1'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-ACESSO_PRODUTIVO'
          OR screen-name = 'GS_ZBV_CHAMADOS-ID_ESTIMATIVA'.
            screen-active = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.

      ELSEIF gs_zbv_chamados-tipo_chamado EQ 'Execução'.
        LOOP AT SCREEN.
          IF screen-name = 'VISUALIZAR_COMENTARIOS'
          OR screen-name = 'VISUALIZAR_CENARIO_ERRO'
          OR screen-name = 'VISUALIZAR_CENARIO_SUCESSO'
          OR screen-name = 'VISUALIZAR_ANEXOS'.
            screen-input = '1'.
            MODIFY SCREEN.
          ELSE.
            screen-input = '0'.
            screen-values_in_combo = '0'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-STATUS'.
            screen-intensified = '1'.
            MODIFY SCREEN.
          ENDIF.

          IF screen-name = 'GS_ZBV_CHAMADOS-ACESSO_PRODUTIVO'
          OR screen-name = 'GS_ZBV_CHAMADOS-APLICACAO_NOTA'.
            screen-active = '0'.
            screen-input = '1'.
            MODIFY SCREEN.
          ENDIF.
        ENDLOOP.
      ENDIF.

*    ENDIF. "sy-uname EQ 'MVBBERENGER'
  ENDCASE.

ENDMODULE.


*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_1005 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_show_grid_1005 OUTPUT.
*----------------------------------------------------------------------------------
  FREE:lt_fieldcat_anx.

  PERFORM f_obtem_dados_anexos.

  PERFORM f_eliminar_botoes_alv.

*----------------------------------------------------------------------------------

*ALTERAR O FIELDCAT PARA OS DA TABELA DE ANEXOS

  PERFORM f_build_fieldcat USING:
    'ICON'     'ICON'   'gt_alv_dms'   'Tipo'         '' 5 CHANGING lt_fieldcat_anx[].

  PERFORM f_build_fieldcat USING:
    'TITULO'   'TITULO' 'gt_alv_dms'   'Título'       '' 25 CHANGING lt_fieldcat_anx[].

  PERFORM f_build_fieldcat USING:
    'DATA_CRI' 'DATA_CRI' 'gt_alv_dms' 'Data Criação' '' 10 CHANGING lt_fieldcat_anx[].

  PERFORM f_build_fieldcat USING:
    'DATA_ATU' 'DATA_ATU' 'gt_alv_dms' 'Data Criação' '' 10 CHANGING lt_fieldcat_anx[].

  PERFORM f_build_fieldcat USING:
    'NOME'     'NOME'   'gt_alv_dms'   'Criado_por'   '' 15 CHANGING lt_fieldcat_anx[].

*  PERFORM f_build_fieldcat USING:
*    'DATA_ATUALIZACAO' 'DATA_ATUALIZACAO' 'ZBV_COMENTARIOS' 'Data Atualização' '' 18 CHANGING lt_fieldcat_com[].
*  PERFORM f_build_fieldcat USING:
*    'HORA_ATUALIZACAO' 'HORA_ATUALIZACAO' 'ZBV_COMENTARIOS' 'Hora Atualização' '' 18 CHANGING lt_fieldcat_com[].

*----------------------------------------------------------------------------------

  IF lo_grid_anx IS NOT BOUND. "IS INITIAL.
    CREATE OBJECT lo_container_anx
      EXPORTING
        container_name = 'CONTAINER_ANEXO'.

    CREATE OBJECT lo_grid_anx
      EXPORTING
        i_parent = lo_container_anx.

    "Permite seleção múltipla de linhas
    lo_grid_anx->set_ready_for_input( 1 ).

    "Permite alteração da célula
*    lo_grid_anx->register_edit_event(
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    lo_grid_anx->set_table_for_first_display(
      EXPORTING
        it_toolbar_excluding = lt_tool_bar
*        is_variant      = ls_variant_anx
*        is_layout       = ls_layout_anx
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = lt_fieldcat_anx[]
        it_outtab       = gt_alv_dms
    ).

    lo_grid_anx->set_gridtitle( TEXT-013 ). "Lista de Anexos

*    SET HANDLER event_handler->data_changed FOR lo_grid_com.
  ELSE.
    lo_grid_anx->refresh_table_display( ).
  ENDIF.

*----------------------------------------------------------------------------------
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form f_obtem_dados_anexos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_obtem_dados_anexos .

  gv_doknr = gs_zbv_chamados-anexo.

  IF gv_doknr IS NOT INITIAL.
    CLEAR: gt_documentfiles,
           gs_return,
           gt_alv_dms.

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
          lv_data_atu = gs_documentfiles-changed_at(8).

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
                          data_cri =  lv_data_cri
                          data_atu =  lv_data_atu )
                          INTO TABLE gt_alv_dms.

          CLEAR: gs_documentfiles,
                 lv_icon,
                 lv_titulo,
                 lv_data_cri,
                 lv_data_atu.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1005 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1005 OUTPUT.
  SET PF-STATUS 'STATUS_1005'.
  SET TITLEBAR 'TITLE_1005'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1005  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1005 INPUT.
  CASE lv_okcode_1005.
    WHEN down.
*----------------------------------------------------------------------
      CLEAR: rows, wa_row.

      CALL METHOD lo_grid_anx->get_selected_rows
        IMPORTING
          et_index_rows = rows.

      LOOP AT rows INTO wa_row.
        READ TABLE gt_documentfiles INTO gs_documentfiles INDEX wa_row-index.

        "BAPI para fazer os checks para exibição
        CALL FUNCTION 'BAPI_DOCUMENT_CHECKOUTVIEW2'
          EXPORTING
            documenttype      = gv_dokar
            documentnumber    = gv_doknr
            documentpart      = gv_doktl
            documentversion   = gv_dokvr
            documentfile      = gs_documentfiles
            getstructure      = '1'
            getcomponents     = 'X'
          IMPORTING
            return            = ls_return
          TABLES
            documentstructure = lt_doc_struc
            documentfiles     = lt_doc_files2
            components        = lt_doc_comp.

        READ TABLE lt_doc_files2 INTO DATA(ls_filedoc) INDEX 1.

        IF sy-subrc = 0.
          lv_fdoc = ls_filedoc-docfile.
*         MESSAGE TEXT-014 TYPE 'I' DISPLAY LIKE 'S'. "Download de anexo feito com sucesso!
          CONCATENATE 'O arquivo foi baixado para a pasta:' lv_fdoc INTO DATA(caminho_arquivo) SEPARATED BY space.
          MESSAGE caminho_arquivo TYPE 'I' DISPLAY LIKE 'S'.
        ELSE.
          MESSAGE TEXT-015 TYPE 'I' DISPLAY LIKE 'E'. "
        ENDIF.

        CLEAR: gs_documentfiles,
               ls_return,
               ls_filedoc,
               lv_fdoc,
               lt_doc_struc,
               lt_doc_files2,
               lt_doc_comp.

      ENDLOOP.
*----------------------------------------------------------------------------

    WHEN exec.
      CLEAR: rows, wa_row.

      CALL METHOD lo_grid_anx->get_selected_rows
        IMPORTING
          et_index_rows = rows.

      LOOP AT rows INTO wa_row.
        READ TABLE gt_documentfiles INTO gs_documentfiles INDEX wa_row-index.

        "BAPI para fazer os checks para exibição
        CALL FUNCTION 'BAPI_DOCUMENT_CHECKOUTVIEW2'
          EXPORTING
            documenttype      = gv_dokar
            documentnumber    = gv_doknr
            documentpart      = gv_doktl
            documentversion   = gv_dokvr
            documentfile      = gs_documentfiles
            getstructure      = '1'
            getcomponents     = 'X'
          IMPORTING
            return            = ls_return
          TABLES
            documentstructure = lt_doc_struc
            documentfiles     = lt_doc_files2
            components        = lt_doc_comp.

        READ TABLE lt_doc_files2 INTO DATA(ls_filedoc2) INDEX 1.

        lv_fdoc = ls_filedoc2-docfile.

        "METODO para abrir o DOCUMENTO
        CALL METHOD cl_gui_frontend_services=>execute
          EXPORTING
            application            = lv_fdoc
            operation              = 'OPEN'
          EXCEPTIONS
            cntl_error             = 1
            error_no_gui           = 2
            bad_parameter          = 3
            file_not_found         = 4
            path_not_found         = 5
            file_extension_unknown = 6
            error_execute_failed   = 7
            synchronous_failed     = 8
            not_supported_by_gui   = 9
            OTHERS                 = 10.

        IF sy-subrc = 0.
          CONCATENATE 'O arquivo foi baixado para a pasta:' lv_fdoc INTO DATA(caminho_arquivo2) SEPARATED BY space.
          MESSAGE TEXT-016 TYPE 'I' DISPLAY LIKE 'S'. "Arquivo aberto com sucesso!
          MESSAGE caminho_arquivo2 TYPE 'I' DISPLAY LIKE 'S'.
        ELSE.
          MESSAGE TEXT-017 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao abrir o arquivo!
        ENDIF.

        CLEAR: gs_documentfiles,
               ls_return,
               ls_filedoc2,
               lv_fdoc,
               lt_doc_struc,
               lt_doc_files2,
               lt_doc_comp.

      ENDLOOP.
*----------------------------------------------------------------------------

    WHEN add.

      CLEAR: lt_files,
             gs_documentdata,
             gs_documentdatax,
             gs_return,
             lt_documentfiles.

      CLEAR: log_linha,
             gs_zbv_log_atuali.

      CALL METHOD cl_gui_frontend_services=>file_open_dialog
        EXPORTING
          multiselection          = abap_true
          window_title            = 'Selecione os arquivos'
        CHANGING
          file_table              = lt_files
          rc                      = v_rc
*         user_action             = lv_action
        EXCEPTIONS
          file_open_dialog_failed = 1
          cntl_error              = 2
          error_no_gui            = 3
          not_supported_by_gui    = 4
          OTHERS                  = 5.

      IF sy-subrc = 0.
        gs_documentdata-documentnumber = gv_doknr.
        gs_documentdata-documenttype = gv_dokar.
        gs_documentdata-description = gv_desc. "gv_desc
        gs_documentdata-username = sy-uname.
        gs_documentdata-statusintern = ' '. "gv_status
        gs_documentdata-validfromdate = sy-datum.

        DESCRIBE TABLE gt_documentfiles LINES DATA(lines).

        gs_documentdatax-wsapplication1 = abap_true.
        gs_documentdatax-docfile1 = abap_true.
        gs_documentdatax-datacarrier1 = abap_true.

        GET TIME STAMP FIELD DATA(time_stamp).

        LOOP AT lt_files ASSIGNING FIELD-SYMBOL(<fs_files>).
          ls_documentfiles-documenttype = gv_dokar.
          ls_documentfiles-originaltype = condense( CONV string( lines + 1 ) ).

          ls_documentfiles-created_by = sy-uname.
          ls_documentfiles-storagecategory = gv_dep_cat.
          ls_documentfiles-docfile = <fs_files>-filename.
          ls_documentfiles-created_at = time_stamp.

          CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
            EXPORTING
              full_name     = <fs_files>-filename
            IMPORTING
              stripped_name = ls_documentfiles-description.

          IF sy-subrc <> 0.
            MESSAGE 'Erro ao armazenar o nome do arquivo!' TYPE 'I' DISPLAY LIKE 'E'.
          ENDIF. "'TRINT_SPLIT_FILE_AND_PATH'

*Pega a extensão do arquivo
          CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
            EXPORTING
              filename  = <fs_files>-filename
              uppercase = 'X'
            IMPORTING
              extension = ls_documentfiles-wsapplication.

          INSERT ls_documentfiles INTO TABLE lt_documentfiles.

          lines += 1.

          CLEAR: ls_documentfiles.

        ENDLOOP.

        "BAPI para fazer a adição no documento do DMS
        CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
          EXPORTING
            documenttype    = gv_dokar
            documentnumber  = gv_doknr
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
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          "Obtem os dados da tabela de log
          PERFORM f_obtem_dados_logs.
          DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

          gs_zbv_chamados-data_atualizacao = sy-datum.
          gs_zbv_chamados-hora_atualizacao = sy-uzeit.

          MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

          MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
          gs_zbv_log_atuali-atualizacao = 'Adicionar'.
          gs_zbv_log_atuali-campo = 'Anexo'.
          gs_zbv_log_atuali-antes = ' '.

          LOOP AT lt_documentfiles ASSIGNING FIELD-SYMBOL(<fs_docfile_add>).

            gs_zbv_log_atuali-depois = <fs_docfile_add>-description.

            IF log_linha = 0.
              gs_zbv_log_atuali-n_registro = 1.
            ELSE.
              gs_zbv_log_atuali-n_registro = log_linha + 1.
            ENDIF.

            INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

            log_linha += 1.

            CLEAR: gs_zbv_log_atuali-depois.

          ENDLOOP.

          UNASSIGN <fs_docfile_add>.

          MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

          IF sy-subrc = 0.

*            INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

            COMMIT WORK.
            PERFORM f_obtem_dados_anexos.

            MESSAGE TEXT-018 TYPE 'I' DISPLAY LIKE 'S'. "Anexo adicionado com sucesso!
          ELSE.

            ROLLBACK WORK.
            MESSAGE TEXT-031 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao atualizar data e horário!

          ENDIF. "MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

        ENDIF. "gs_return-type

      ENDIF. "file_open_dialog

*----------------------------------------------------------------------------

    WHEN del.

      CLEAR: log_linha,
             gs_zbv_log_atuali.

      "Verificar se possível permissão para Eliminar(deletar)
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '06'.
      CASE sy-subrc.
        WHEN 0.
          DATA: ls_ans TYPE c,
                auth   TYPE c.

          CLEAR: rows, wa_row, auth.
          CLEAR: gs_documentdata,
                 gs_documentdatax,
                 lt_documentfiles.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Remoção de anexo' "Criar TEXT-000
              text_question         = 'Deseja remover o(s) anexo(s)?' "Criar TEXT-000
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
            CALL METHOD lo_grid_anx->get_selected_rows
              IMPORTING
                et_index_rows = rows.

            IF sy-subrc NE 0.
              MESSAGE TEXT-029 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao selecionar a linha da tabela!

            ELSE.

              gs_documentdata-documentnumber = gv_doknr.
              gs_documentdata-documenttype = gv_dokar.
              gs_documentdata-description = gv_desc. "gv_desc
              gs_documentdata-username = sy-uname.
              gs_documentdata-statusintern = ' '. "gv_status
              gs_documentdata-validfromdate = sy-datum.

              gs_documentdatax-wsapplication1 = abap_true.
              gs_documentdatax-docfile1 = abap_true.
              gs_documentdatax-datacarrier1 = abap_true.

              LOOP AT rows INTO wa_row.

                READ TABLE gt_documentfiles INTO gs_documentfiles INDEX wa_row-index.
*          READ TABLE gt_zbv_anexos INTO gs_zbv_anexos INDEX wa_row-index.

                "verificar se usuario que estava deletando é o mesmo que criou
                IF gs_documentfiles-created_by = sy-uname.
                  auth = abap_true.
                ENDIF.

                IF sy-subrc = 0.

                  MOVE-CORRESPONDING gs_documentfiles TO ls_documentfiles.
                  ls_documentfiles-deletevalue = abap_true.

                  INSERT ls_documentfiles INTO TABLE lt_documentfiles.

                  CLEAR: ls_documentfiles,
                         gs_documentfiles.

                ELSE.
                  MESSAGE TEXT-028 TYPE 'I' DISPLAY LIKE 'E'. "Dados não encontrados na tabela gt_zbv_anexos!
                ENDIF. "READ TABLE gt_documentfiles

              ENDLOOP.

              IF auth = abap_true.

                CALL FUNCTION 'BAPI_DOCUMENT_CHANGE2'
                  EXPORTING
                    documenttype    = gv_dokar
                    documentnumber  = gv_doknr
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

                  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                    EXPORTING
                      wait = abap_true.

                  "Obtem os dados da tabela de log
                  PERFORM f_obtem_dados_logs.
                  DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

                  gs_zbv_chamados-data_atualizacao = sy-datum.
                  gs_zbv_chamados-hora_atualizacao = sy-uzeit.

                  MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

                  MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
                  gs_zbv_log_atuali-atualizacao = 'Deletar'.
                  gs_zbv_log_atuali-campo = 'Anexo'.
                  gs_zbv_log_atuali-depois = ' '.

                  LOOP AT lt_documentfiles ASSIGNING FIELD-SYMBOL(<fs_docfile_del>).

                    gs_zbv_log_atuali-antes = <fs_docfile_del>-description.

                    IF log_linha = 0.
                      gs_zbv_log_atuali-n_registro = 1.
                    ELSE.
                      gs_zbv_log_atuali-n_registro = log_linha + 1.
                    ENDIF.

                    INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

                    log_linha += 1.

                    CLEAR: gs_zbv_log_atuali-antes.

                  ENDLOOP.

                  UNASSIGN <fs_docfile_del>.

                  MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

                  IF sy-subrc <> 0.
                    ROLLBACK WORK.
                    MESSAGE TEXT-027 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao atualizar data e horário!
                  ELSE.

*                    INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

                    COMMIT WORK.
                    MESSAGE TEXT-022 TYPE 'I' DISPLAY LIKE 'S'. "Anexo removido com sucesso!
                  ENDIF.

                ENDIF. "gs_return-type CA 'WEA'

              ELSE.
                MESSAGE 'Somente quem criou o comentário pode eliminá-lo!' TYPE 'W'.
              ENDIF. "sem_auth = abap_true.

            ENDIF. "sy-subrc NE 0
          ENDIF. "ls_ans = '1'

        WHEN OTHERS.
          MESSAGE 'Você não possui permissão' TYPE 'W'.
      ENDCASE.

*----------------------------------------------------------------------------

    WHEN back OR ok.
      LEAVE TO SCREEN 0.
    WHEN exit.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module STATUS_1006 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1006 OUTPUT.
  SET PF-STATUS 'STATUS_1006'.
  SET TITLEBAR 'TITLE_1006'.

  PERFORM textEditor_comentario.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1006  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1006 INPUT.
  CASE lv_okcode_1006.
    WHEN back OR ok. "OR 'VOLTAR'.
      LEAVE TO SCREEN 0.
      FREE: lo_editor_com, lo_editor_con_com.
    WHEN exit.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form textEditor_comentario
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM textEditor_comentario .
  IF lo_editor_com IS NOT BOUND. "IS INITIAL.
    "Criar Container (Necessário criar o container no Layout da tela)
    CREATE OBJECT lo_editor_con_com
      EXPORTING
        container_name              = 'COMENTARIO_DETALHADO' "container do comentario detalhado
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.

    "Instância obj to tipo textedit
    CREATE OBJECT lo_editor_com
      EXPORTING
        parent = lo_editor_con_com.
*        wordwrap_mode              = cl_gui_textedit=>wordwrap_at_fixed_position
*        wordwrap_position          = c_line_lenght
*        wordwrap_to_linebreak_mode = cl_gui_textedit=>true.

    "Passa as linhas de texto via tabela interna
    lo_editor_com->set_text_as_r3table( lt_table_com[] ).


    "Se houver conteúdo salvo, devemos somente mostrar o textarea sem edição.
    lo_editor_com->set_readonly_mode( ).


    "Remove os botões (toolbar)
*    lo_editor_cen_er->set_toolbar_mode( 0 ).

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1007 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1007 OUTPUT.
  SET PF-STATUS 'STATUS_1007'.
  SET TITLEBAR 'TITLE_1007'.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_1007 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_show_grid_1007 OUTPUT.
  FREE:lt_fieldcat_hrs.

  PERFORM f_obtem_dados_horas.

  IF gt_zbv_horas IS INITIAL.
    CLEAR: gs_zbv_horas.
    gs_zbv_horas-id_linha = 0.
    gs_zbv_horas-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_horas-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_horas-consultor_tecnico = gs_zbv_chamados-consultor_tecnico.
    gs_zbv_horas-solicitante = gs_zbv_chamados-solicitante.
    gs_zbv_horas-tecnologia = gs_zbv_chamados-tecnologia.
    APPEND gs_zbv_horas TO gt_zbv_horas.
  ENDIF.

*  ls_layout_com-cwidth_opt = 'X'.

  PERFORM f_eliminar_botoes_alv.

*  PERFORM f_build_fieldcat USING:
*    'ID_LINHA' 'ID_LINHA' 'ZBV_COMENTARIOS' 'ID Linha' '' 8 CHANGING lt_fieldcat_com[].

  PERFORM f_build_fieldcat USING:
    id_chamado       id_chamado       tab_horas 'ID Chamado'       '' 17 CHANGING lt_fieldcat_hrs[].
  PERFORM f_build_fieldcat USING:
    tipo_chamado     tipo_chamado     tab_horas 'Tipo Chamado'     '' 14 CHANGING lt_fieldcat_hrs[].
  PERFORM f_build_fieldcat USING:
    'CRIADO_POR'     'CRIADO_POR'     tab_horas 'Criado por'       '' 14 CHANGING lt_fieldcat_hrs[].
  PERFORM f_build_fieldcat USING:
    data_atualizacao data_atualizacao tab_horas 'Data Atualização' '' 18 CHANGING lt_fieldcat_hrs[].
  PERFORM f_build_fieldcat USING:
    hora_atualizacao hora_atualizacao tab_horas 'Hora Atualização' '' 18 CHANGING lt_fieldcat_hrs[].
  PERFORM f_build_fieldcat USING:
    horas_utilizadas horas_utilizadas tab_horas 'Horas Apontadas'  'X' 13 CHANGING lt_fieldcat_hrs[].

  IF lo_grid_hrs IS NOT BOUND. "lo_grid_hrs IS INITIAL.
    CREATE OBJECT lo_container_hrs
      EXPORTING
        container_name = 'CONTAINER_HORAS'. "container_horas.

    CREATE OBJECT lo_grid_hrs
      EXPORTING
        i_parent = lo_container_hrs.

    "Permite seleção múltipla de linhas
    lo_grid_hrs->set_ready_for_input( 1 ).

    "Permite alteração da célula
    lo_grid_hrs->register_edit_event(
      i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    lo_grid_hrs->set_table_for_first_display(
      EXPORTING
        it_toolbar_excluding = lt_tool_bar
*        is_variant      = ls_variant_com
*        is_layout       = ls_layout_com
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = lt_fieldcat_hrs[]
        it_outtab       = gt_zbv_horas
    ).

    lo_grid_hrs->set_gridtitle( TEXT-024 ). "Lista de Horas Apontadas

    SET HANDLER event_handler->data_changed_hora FOR lo_grid_hrs.
  ELSE.
    lo_grid_hrs->refresh_table_display( ).
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1007  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1007 INPUT.
  CASE lv_okcode_1007.
    WHEN save.
      PERFORM f_salvar_alteracoes_horas.
    WHEN add.

      CLEAR: log_linha, gs_zbv_log_atuali.

      "Validação de permissão de Modificar (Adicionar)
      AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '02'.
      CASE sy-subrc.
        WHEN 0.

          PERFORM f_obtem_dados_horas.

          READ TABLE gt_zbv_horas INTO gs_zbv_horas INDEX sy-dbcnt.
          IF sy-subrc = 0.

            CLEAR: gs_zbv_horas-horas_utilizadas.
            gs_zbv_horas-id_linha += 1.
            gs_zbv_horas-data_atualizacao = sy-datum.
            gs_zbv_horas-hora_atualizacao = sy-uzeit.
            gs_zbv_horas-consultor_tecnico = gs_zbv_chamados-consultor_tecnico.
            gs_zbv_horas-solicitante = gs_zbv_chamados-solicitante.
            gs_zbv_horas-tecnologia = gs_zbv_chamados-tecnologia.
            gs_zbv_horas-criado_por = sy-uname.
            APPEND gs_zbv_horas TO gt_zbv_horas.

            "Obtem os dados da tabela de log
            PERFORM f_obtem_dados_logs.
            DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

            MOVE-CORRESPONDING gs_zbv_horas TO gs_zbv_log_atuali.
            gs_zbv_log_atuali-atualizado_por = gs_zbv_horas-criado_por.
            gs_zbv_log_atuali-atualizacao = 'Criado'. "'Horas'.
            gs_zbv_log_atuali-campo = 'Linha - Horas'.

            IF log_linha = 0.
              gs_zbv_log_atuali-n_registro = 1.
            ELSE.
              gs_zbv_log_atuali-n_registro = log_linha + 1.
            ENDIF.

            MODIFY zbv_ger_horas FROM TABLE gt_zbv_horas.

            IF sy-subrc = 0.

              INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

              COMMIT WORK.
              PERFORM f_obtem_dados_horas.
              MESSAGE TEXT-004 TYPE 'I' DISPLAY LIKE 'S'. "Linha adicionada com sucesso

            ELSE.
              ROLLBACK WORK.
              MESSAGE TEXT-005 TYPE 'S' DISPLAY LIKE 'E'. "Erro ao adicionar linha!

            ENDIF.
          ELSE.
            MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'. "Erro no processo de adidionar linha!
          ENDIF.

        WHEN OTHERS.
          MESSAGE 'Você não tem autorização' TYPE 'W'.
      ENDCASE.

    WHEN del.

      CLEAR: rows, wa_row.
      CLEAR: log_linha,
             gs_zbv_log_atuali.

      CALL METHOD lo_grid_hrs->get_selected_rows
        IMPORTING
          et_index_rows = rows.

      IF sy-subrc NE 0.
        MESSAGE TEXT-029 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao selecionar a linha da tabela!

      ELSE.

        LOOP AT rows INTO wa_row.
          READ TABLE gt_zbv_horas INTO gs_zbv_horas INDEX wa_row-index.

          "Verificar se possível permissão para Eliminar(deletar)
          AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '06'.
          CASE sy-subrc.
            WHEN 0.

              IF gs_zbv_horas-criado_por = sy-uname.

                gs_zbv_chamados-horas_utilizadas -= gs_zbv_horas-horas_utilizadas.

                "Obtem os dados da tabela de log
                PERFORM f_obtem_dados_logs.
                DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

                DELETE FROM zbv_ger_horas WHERE id_linha   = gs_zbv_horas-id_linha
                                          AND   tipo_chamado = gs_zbv_horas-tipo_chamado
                                          AND   id_chamado   = gs_zbv_horas-id_chamado
                                          AND   cliente      = gs_zbv_horas-cliente
                                          AND   acao         = gs_zbv_horas-acao.

                IF sy-subrc = 0.
                  gs_zbv_chamados-data_atualizacao = sy-datum.
                  gs_zbv_chamados-hora_atualizacao = sy-uzeit.
                  gs_zbv_chamados-atualizado_por = sy-uname.

                  MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

                  MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
                  gs_zbv_log_atuali-atualizacao = 'Deletar'. "'Horas'.

                  IF log_linha = 0.
                    gs_zbv_log_atuali-n_registro = 1.
                  ELSE.
                    gs_zbv_log_atuali-n_registro = log_linha + 1.
                  ENDIF.


                  gs_zbv_log_atuali-campo = 'Linha - Horas'.

                  gs_zbv_log_atuali-antes = gs_zbv_horas-horas_utilizadas.
                  gs_zbv_log_atuali-depois = 0.

*                  LOOP AT gt_zbv_horas INTO gs_zbv_horas WHERE id_linha = gs_zbv_horas_antes-id_linha.
*                    gs_zbv_log_atuali-antes = gs_zbv_horas_antes-horas_utilizadas.
*                    gs_zbv_log_atuali-depois = gs_zbv_horas-horas_utilizadas.
*                  ENDLOOP.


                  MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

                  CHECK sy-subrc = 0.

                  INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

                  COMMIT WORK.
                  MESSAGE TEXT-025 TYPE 'I' DISPLAY LIKE 'S'. "Horas removidas com sucesso!

                  CLEAR: gs_zbv_horas.
                ELSE.
                  ROLLBACK WORK.
                  MESSAGE TEXT-026 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao remover as horas!

                ENDIF.

              ELSE.
                MESSAGE 'Somente quem criou o comentário pode eliminá-lo!' TYPE 'W'.
              ENDIF. "gs_zbv_horas-criado_por = sy-uname.

            WHEN OTHERS.
              MESSAGE 'Você não possui permissão' TYPE 'W'.
          ENDCASE.

        ENDLOOP.
      ENDIF.

    WHEN back OR ok.
      CLEAR: lv_salvou_item.
      LEAVE TO SCREEN 0.
    WHEN exit.
      CLEAR: lv_salvou_item.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form f_obtem_dados_horas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_obtem_dados_horas .
  SELECT * FROM zbv_ger_horas
              INTO TABLE gt_zbv_horas
                WHERE id_chamado   = gs_zbv_chamados-id_chamado
                AND   tipo_chamado = gs_zbv_chamados-tipo_chamado
                AND   cliente      = gs_zbv_chamados-cliente
                AND   acao         = gs_zbv_chamados-acao.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form f_salvar_alteracoes_horas
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_salvar_alteracoes_horas .
  CLEAR: lv_hora_total,
         gs_zbv_chamados-horas_utilizadas,
         log_linha,
         gs_zbv_log_atuali,
         gs_zbv_horas.

  MODIFY zbv_ger_horas FROM TABLE gt_zbv_horas.
  IF sy-subrc = 0.

    "Obtem os dados da tabela de log
    PERFORM f_obtem_dados_logs.
    DESCRIBE TABLE gt_zbv_log_atuali LINES log_linha.

    LOOP AT gt_zbv_horas INTO DATA(temp_zbv_horas).
      gs_zbv_chamados-horas_utilizadas += temp_zbv_horas-horas_utilizadas.
    ENDLOOP.

    MOVE-CORRESPONDING gs_zbv_chamados TO gs_zbv_chamados_sc.

    MOVE-CORRESPONDING gs_zbv_chamados_sc TO gs_zbv_log_atuali.
    gs_zbv_log_atuali-campo = 'Linha - Horas'.

    LOOP AT gt_zbv_horas INTO gs_zbv_horas WHERE id_linha = gs_zbv_horas_antes-id_linha.
      gs_zbv_log_atuali-antes = gs_zbv_horas_antes-horas_utilizadas.
      gs_zbv_log_atuali-depois = gs_zbv_horas-horas_utilizadas.
    ENDLOOP.

    IF gs_zbv_horas_antes-horas_utilizadas IS NOT INITIAL.
      gs_zbv_log_atuali-atualizacao = 'Alterar'.
    ELSE.
      gs_zbv_log_atuali-atualizacao = 'Criado'.
    ENDIF.

    IF log_linha = 0.
      gs_zbv_log_atuali-n_registro = 1.
    ELSE.
      gs_zbv_log_atuali-n_registro = log_linha + 1.
    ENDIF.

    MODIFY zbv_chamados FROM gs_zbv_chamados_sc.

    IF sy-subrc = 0.

      INSERT zbv_log_atuali FROM gs_zbv_log_atuali.

      COMMIT WORK.
      lv_salvou_item = 'X'.
      PERFORM f_obtem_dados_horas.
      MESSAGE TEXT-010 TYPE 'I' DISPLAY LIKE 'S'. "Dados salvos na tabela com sucesso!
      lo_grid_hrs->refresh_table_display( ).
*      CLEAR: lv_salvou_item.
*    LEAVE TO SCREEN 0.
    ELSE.
      ROLLBACK WORK.
      MESSAGE TEXT-011 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao salvar dados na tabela!

    ENDIF.
  ELSE.
    MESSAGE TEXT-012 TYPE 'I' DISPLAY LIKE 'E'. "Erro ao salvar comentario na tabela do banco!
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Module STATUS_1008 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_1008 OUTPUT.
  SET PF-STATUS 'STATUS_1008'.
  SET TITLEBAR 'TITLE_1008'.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Module M_SHOW_GRID_1008 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE m_show_grid_1008 OUTPUT.
  FREE:lt_fieldcat_log.

  PERFORM f_obtem_dados_logs.

*  ls_layout_com-cwidth_opt = 'X'.

  PERFORM f_eliminar_botoes_alv.

*  PERFORM f_build_fieldcat USING:
*    'ID_LINHA' 'ID_LINHA' 'ZBV_COMENTARIOS' 'ID Linha' '' 8 CHANGING lt_fieldcat_com[].

  PERFORM f_build_fieldcat USING:
    id_chamado       id_chamado       'ZBV_LOG_ATUALI'   'ID Chamado'       '' 15 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    tipo_chamado     tipo_chamado     'ZBV_LOG_ATUALI'   'Tipo Chamado'     '' 14 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'N_REGISTRO'     'N_REGISTRO'     'ZBV_LOG_ATUALI'   'Nº Registro'      '' 10 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'ATUALIZADO_POR' 'ATUALIZADO_POR' 'ZBV_LOG_ATUALI'   'Atualizado por'   '' 12 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    data_atualizacao data_atualizacao 'zbv_log_atuali'   'Data Atualização' '' 12 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    hora_atualizacao hora_atualizacao 'ZBV_LOG_ATUALI'   'Hora Atualização' '' 12 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'ATUALIZACAO'    'ATUALIZACAO'    'ZBV_LOG_ATUALI'   'Atualização'      '' 11 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'CAMPO'          'CAMPO'          'ZBV_LOG_ATUALI'   'Campo'            '' 17 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'ANTES'          'ANTES'          'ZBV_LOG_ATUALI'   'Antes'            '' 20 CHANGING lt_fieldcat_log[].
  PERFORM f_build_fieldcat USING:
    'DEPOIS'         'DEPOIS'         'ZBV_LOG_ATUALI'   'Depois'           '' 20 CHANGING lt_fieldcat_log[].

  IF lo_grid_log IS NOT BOUND. "lo_grid_com IS INITIAL.
    CREATE OBJECT lo_container_log
      EXPORTING
        container_name = 'CONTAINER_LOGS'. "container_comentario.

    CREATE OBJECT lo_grid_log
      EXPORTING
        i_parent = lo_container_log.

    "Permite seleção múltipla de linhas
*    lo_grid_log->set_ready_for_input( 1 ).

    "Permite alteração da célula
*    lo_grid_com->register_edit_event(
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified ).

    lo_grid_log->set_table_for_first_display(
      EXPORTING
        it_toolbar_excluding = lt_tool_bar
*        is_variant      = ls_variant_com
*        is_layout       = ls_layout_com
        i_save          = 'A'
      CHANGING
        it_fieldcatalog = lt_fieldcat_log[]
        it_outtab       = gt_zbv_log_atuali
    ).

    lo_grid_log->set_gridtitle( 'Lista de Logs de Atualização' ). "Lista de comentários

*    SET HANDLER event_handler->data_changed FOR lo_grid_com.
  ELSE.
    lo_grid_log->refresh_table_display( ).
  ENDIF.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1008  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_1008 INPUT.
  CASE lv_okcode_1008.
    WHEN back OR ok.
      LEAVE TO SCREEN 0.
    WHEN exit.
      LEAVE PROGRAM.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.


*&---------------------------------------------------------------------*
*& Form f_obtem_dados_logs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_obtem_dados_logs .
  SELECT * FROM zbv_log_atuali
              INTO TABLE gt_zbv_log_atuali
                WHERE id_chamado   = gs_zbv_chamados-id_chamado
                AND   tipo_chamado = gs_zbv_chamados-tipo_chamado
                AND   cliente      = gs_zbv_chamados-cliente
                AND   acao         = gs_zbv_chamados-acao.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form registra_chamado_parado
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM registra_contagem_parado .

  IF gs_zbv_tmp_parado-n_registro IS INITIAL.
    gs_zbv_tmp_parado-n_registro = 1.

    gs_zbv_tmp_parado-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_parado-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_parado-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_parado-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_parado-data_ini = sy-datum.
    gs_zbv_tmp_parado-hora_ini = sy-uzeit.

    INSERT zbv_tmp_parado FROM gs_zbv_tmp_parado.

  ELSEIF gs_zbv_tmp_parado-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_parado-data_fim IS INITIAL
         AND gs_zbv_tmp_parado-hora_fim IS INITIAL.

    gs_zbv_tmp_parado-data_fim = sy-datum.
    gs_zbv_tmp_parado-hora_fim = sy-uzeit.

    "Fazer o calculo do tempo parado
    CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
      EXPORTING
        date_from       = gs_zbv_tmp_parado-data_ini
        date_to         = gs_zbv_tmp_parado-data_fim
        time_from       = gs_zbv_tmp_parado-hora_ini
        time_to         = gs_zbv_tmp_parado-hora_fim
      IMPORTING
        delta_time      = tempo_minutos
*       DELTA_UNIT      =
      EXCEPTIONS
        from_greater_to = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      tempo_total = tempo_minutos / 60.
      gs_zbv_tmp_parado-tempo = tempo_total.

      MODIFY zbv_tmp_parado FROM gs_zbv_tmp_parado.

    ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE


  ELSEIF gs_zbv_tmp_parado-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_parado-data_fim IS NOT INITIAL
         AND gs_zbv_tmp_parado-hora_fim IS NOT INITIAL.

    gs_zbv_tmp_parado-n_registro += 1.

    gs_zbv_tmp_parado-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_parado-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_parado-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_parado-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_parado-data_ini = sy-datum.
    gs_zbv_tmp_parado-hora_ini = sy-uzeit.

    CLEAR: gs_zbv_tmp_parado-data_fim,
           gs_zbv_tmp_parado-hora_fim,
           gs_zbv_tmp_parado-tempo.

    INSERT zbv_tmp_parado FROM gs_zbv_tmp_parado.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerrado_contagem_tst_func
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_tst_func .

  SELECT * FROM zbv_tmp_tst_func
                INTO TABLE gt_zbv_tmp_tst_func
                WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                AND   id_chamado   = gs_zbv_chamados-id_chamado
                AND   cliente      = gs_zbv_chamados-cliente
                AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_tst_func INTO gs_zbv_tmp_tst_func INDEX sy-dbcnt.
  gs_zbv_tmp_tst_func-data_fim = sy-datum.
  gs_zbv_tmp_tst_func-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo teste funcional
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_tst_func-data_ini
      date_to         = gs_zbv_tmp_tst_func-data_fim
      time_from       = gs_zbv_tmp_tst_func-hora_ini
      time_to         = gs_zbv_tmp_tst_func-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_tst_func-tempo = tempo_total.

    MODIFY zbv_tmp_tst_func FROM gs_zbv_tmp_tst_func.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerra_contagem_tst_clnt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_tst_clnt .
  SELECT * FROM zbv_tmp_tst_clnt
                INTO TABLE gt_zbv_tmp_tst_clnt
                WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                AND   id_chamado   = gs_zbv_chamados-id_chamado
                AND   cliente      = gs_zbv_chamados-cliente
                AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_tst_clnt INTO gs_zbv_tmp_tst_clnt INDEX sy-dbcnt.
  gs_zbv_tmp_tst_clnt-data_fim = sy-datum.
  gs_zbv_tmp_tst_clnt-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo teste funcional
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_tst_clnt-data_ini
      date_to         = gs_zbv_tmp_tst_clnt-data_fim
      time_from       = gs_zbv_tmp_tst_clnt-hora_ini
      time_to         = gs_zbv_tmp_tst_clnt-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_tst_clnt-tempo = tempo_total.

    MODIFY zbv_tmp_tst_clnt FROM gs_zbv_tmp_tst_clnt.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

ENDFORM.

*&---------------------------------------------------------------------*
*& Form registra_contagem_tst_func
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM registra_contagem_tst_func .
  IF gs_zbv_tmp_tst_func-n_registro IS INITIAL.

    gs_zbv_tmp_tst_func-n_registro = 1.

    gs_zbv_tmp_tst_func-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_tst_func-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_tst_func-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_tst_func-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_tst_func-data_ini = sy-datum.
    gs_zbv_tmp_tst_func-hora_ini = sy-uzeit.

    INSERT zbv_tmp_tst_func FROM gs_zbv_tmp_tst_func.

  ELSEIF gs_zbv_tmp_tst_func-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_tst_func-data_fim IS INITIAL
         AND gs_zbv_tmp_tst_func-hora_fim IS INITIAL.

    gs_zbv_tmp_tst_func-data_fim = sy-datum.
    gs_zbv_tmp_tst_func-hora_fim = sy-uzeit.

    "Fazer o calculo do tempo teste funcional
    CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
      EXPORTING
        date_from       = gs_zbv_tmp_tst_func-data_ini
        date_to         = gs_zbv_tmp_tst_func-data_fim
        time_from       = gs_zbv_tmp_tst_func-hora_ini
        time_to         = gs_zbv_tmp_tst_func-hora_fim
      IMPORTING
        delta_time      = tempo_minutos
*       DELTA_UNIT      =
      EXCEPTIONS
        from_greater_to = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      tempo_total = tempo_minutos / 60.
      gs_zbv_tmp_tst_func-tempo = tempo_total.

      MODIFY zbv_tmp_tst_func FROM gs_zbv_tmp_tst_func.

    ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

  ELSEIF gs_zbv_tmp_tst_func-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_tst_func-data_fim IS NOT INITIAL
         AND gs_zbv_tmp_tst_func-hora_fim IS NOT INITIAL.

    gs_zbv_tmp_tst_func-n_registro += 1.

    gs_zbv_tmp_tst_func-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_tst_func-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_tst_func-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_tst_func-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_tst_func-data_ini = sy-datum.
    gs_zbv_tmp_tst_func-hora_ini = sy-uzeit.

    CLEAR: gs_zbv_tmp_tst_func-hora_fim,
           gs_zbv_tmp_tst_func-data_fim,
           gs_zbv_tmp_tst_func-tempo.

    INSERT zbv_tmp_tst_func FROM gs_zbv_tmp_tst_func.

  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerra_contagem_parado
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_parado .
  SELECT * FROM zbv_tmp_parado
                INTO TABLE gt_zbv_tmp_parado
                WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                AND   id_chamado   = gs_zbv_chamados-id_chamado
                AND   cliente      = gs_zbv_chamados-cliente
                AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_parado INTO gs_zbv_tmp_parado INDEX sy-dbcnt.
  gs_zbv_tmp_parado-data_fim = sy-datum.
  gs_zbv_tmp_parado-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo parado
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_parado-data_ini
      date_to         = gs_zbv_tmp_parado-data_fim
      time_from       = gs_zbv_tmp_parado-hora_ini
      time_to         = gs_zbv_tmp_parado-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_parado-tempo = tempo_total.

    MODIFY zbv_tmp_parado FROM gs_zbv_tmp_parado.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE
ENDFORM.


*&---------------------------------------------------------------------*
*& Form registra_contagem_tst_clnt
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM registra_contagem_tst_clnt .

  IF gs_zbv_tmp_tst_clnt-n_registro IS INITIAL.

    gs_zbv_tmp_tst_clnt-n_registro = 1.

    gs_zbv_tmp_tst_clnt-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_tst_clnt-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_tst_clnt-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_tst_clnt-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_tst_clnt-data_ini = sy-datum.
    gs_zbv_tmp_tst_clnt-hora_ini = sy-uzeit.

    INSERT zbv_tmp_tst_clnt FROM gs_zbv_tmp_tst_clnt.

  ELSEIF gs_zbv_tmp_tst_clnt-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_tst_clnt-data_fim IS INITIAL
         AND gs_zbv_tmp_tst_clnt-hora_fim IS INITIAL.

    gs_zbv_tmp_tst_clnt-data_fim = sy-datum.
    gs_zbv_tmp_tst_clnt-hora_fim = sy-uzeit.

    "Fazer o calculo do tempo parado
    CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
      EXPORTING
        date_from       = gs_zbv_tmp_tst_clnt-data_ini
        date_to         = gs_zbv_tmp_tst_clnt-data_fim
        time_from       = gs_zbv_tmp_tst_clnt-hora_ini
        time_to         = gs_zbv_tmp_tst_clnt-hora_fim
      IMPORTING
        delta_time      = tempo_minutos
*       DELTA_UNIT      =
      EXCEPTIONS
        from_greater_to = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      tempo_total = tempo_minutos / 60.
      gs_zbv_tmp_tst_clnt-tempo = tempo_total.

      MODIFY zbv_tmp_tst_clnt FROM gs_zbv_tmp_tst_clnt.

    ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

  ELSEIF gs_zbv_tmp_tst_clnt-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_tst_clnt-data_fim IS NOT INITIAL
         AND gs_zbv_tmp_tst_clnt-hora_fim IS NOT INITIAL.

    gs_zbv_tmp_tst_clnt-n_registro += 1.

    gs_zbv_tmp_tst_clnt-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_tst_clnt-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_tst_clnt-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_tst_clnt-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_tst_clnt-data_ini = sy-datum.
    gs_zbv_tmp_tst_clnt-hora_ini = sy-uzeit.

    CLEAR: gs_zbv_tmp_tst_clnt-data_fim,
           gs_zbv_tmp_tst_clnt-hora_fim,
           gs_zbv_tmp_tst_clnt-tempo.

    INSERT zbv_tmp_tst_clnt FROM gs_zbv_tmp_tst_clnt.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerra_contatem_em_atnd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_em_atnd .

  SELECT * FROM zbv_tmp_em_atnd
                  INTO TABLE gt_zbv_tmp_em_atnd
                  WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                  AND   id_chamado   = gs_zbv_chamados-id_chamado
                  AND   cliente      = gs_zbv_chamados-cliente
                  AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_em_atnd INTO gs_zbv_tmp_em_atnd INDEX sy-dbcnt.
  gs_zbv_tmp_em_atnd-data_fim = sy-datum.
  gs_zbv_tmp_em_atnd-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo parado
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_em_atnd-data_ini
      date_to         = gs_zbv_tmp_em_atnd-data_fim
      time_from       = gs_zbv_tmp_em_atnd-hora_ini
      time_to         = gs_zbv_tmp_em_atnd-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_em_atnd-tempo = tempo_total.

    MODIFY zbv_tmp_em_atnd FROM gs_zbv_tmp_em_atnd.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerra_contagem_dev
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_dev .

  SELECT * FROM zbv_tmp_dev
                    INTO TABLE gt_zbv_tmp_dev
                    WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                    AND   id_chamado   = gs_zbv_chamados-id_chamado
                    AND   cliente      = gs_zbv_chamados-cliente
                    AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_dev INTO gs_zbv_tmp_dev INDEX sy-dbcnt.
  gs_zbv_tmp_dev-data_fim = sy-datum.
  gs_zbv_tmp_dev-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo parado
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_dev-data_ini
      date_to         = gs_zbv_tmp_dev-data_fim
      time_from       = gs_zbv_tmp_dev-hora_ini
      time_to         = gs_zbv_tmp_dev-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_dev-tempo = tempo_total.

    MODIFY zbv_tmp_dev FROM gs_zbv_tmp_dev.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

ENDFORM.


*&---------------------------------------------------------------------*
*& Form registra_contagem_em_atnd
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM registra_contagem_em_atnd .

  IF gs_zbv_tmp_em_atnd-n_registro IS INITIAL.

    gs_zbv_tmp_em_atnd-n_registro = 1.

    gs_zbv_tmp_em_atnd-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_em_atnd-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_em_atnd-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_em_atnd-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_em_atnd-data_ini = sy-datum.
    gs_zbv_tmp_em_atnd-hora_ini = sy-uzeit.

    INSERT zbv_tmp_em_atnd FROM gs_zbv_tmp_em_atnd.

  ELSEIF gs_zbv_tmp_em_atnd-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_em_atnd-data_fim IS INITIAL
         AND gs_zbv_tmp_em_atnd-hora_fim IS INITIAL.

    gs_zbv_tmp_em_atnd-data_fim = sy-datum.
    gs_zbv_tmp_em_atnd-hora_fim = sy-uzeit.

    "Fazer o calculo do tempo parado
    CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
      EXPORTING
        date_from       = gs_zbv_tmp_em_atnd-data_ini
        date_to         = gs_zbv_tmp_em_atnd-data_fim
        time_from       = gs_zbv_tmp_em_atnd-hora_ini
        time_to         = gs_zbv_tmp_em_atnd-hora_fim
      IMPORTING
        delta_time      = tempo_minutos
*       DELTA_UNIT      =
      EXCEPTIONS
        from_greater_to = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      tempo_total = tempo_minutos / 60.
      gs_zbv_tmp_em_atnd-tempo = tempo_total.

      MODIFY zbv_tmp_em_atnd FROM gs_zbv_tmp_em_atnd.

    ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

  ELSEIF gs_zbv_tmp_em_atnd-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_em_atnd-data_fim IS NOT INITIAL
         AND gs_zbv_tmp_em_atnd-hora_fim IS NOT INITIAL.

    gs_zbv_tmp_em_atnd-n_registro += 1.

    gs_zbv_tmp_em_atnd-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_em_atnd-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_em_atnd-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_em_atnd-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_em_atnd-data_ini = sy-datum.
    gs_zbv_tmp_em_atnd-hora_ini = sy-uzeit.

    CLEAR: gs_zbv_tmp_em_atnd-data_fim,
           gs_zbv_tmp_em_atnd-hora_fim,
           gs_zbv_tmp_em_atnd-tempo.

    INSERT zbv_tmp_em_atnd FROM gs_zbv_tmp_em_atnd.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form registra_contagem_dev
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM registra_contagem_dev .

  IF gs_zbv_tmp_dev-n_registro IS INITIAL.

    gs_zbv_tmp_dev-n_registro = 1.

    gs_zbv_tmp_dev-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_dev-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_dev-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_dev-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_dev-data_ini = sy-datum.
    gs_zbv_tmp_dev-hora_ini = sy-uzeit.

    INSERT zbv_tmp_dev FROM gs_zbv_tmp_dev.

  ELSEIF gs_zbv_tmp_dev-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_dev-data_fim IS INITIAL
         AND gs_zbv_tmp_dev-hora_fim IS INITIAL.

    gs_zbv_tmp_dev-data_fim = sy-datum.
    gs_zbv_tmp_dev-hora_fim = sy-uzeit.

    "Fazer o calculo do tempo parado
    CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
      EXPORTING
        date_from       = gs_zbv_tmp_dev-data_ini
        date_to         = gs_zbv_tmp_dev-data_fim
        time_from       = gs_zbv_tmp_dev-hora_ini
        time_to         = gs_zbv_tmp_dev-hora_fim
      IMPORTING
        delta_time      = tempo_minutos
*       DELTA_UNIT      =
      EXCEPTIONS
        from_greater_to = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
    ELSE.
      tempo_total = tempo_minutos / 60.
      gs_zbv_tmp_dev-tempo = tempo_total.

      MODIFY zbv_tmp_dev FROM gs_zbv_tmp_dev.

    ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

  ELSEIF gs_zbv_tmp_dev-n_registro IS NOT INITIAL
         AND gs_zbv_tmp_dev-data_fim IS NOT INITIAL
         AND gs_zbv_tmp_dev-hora_fim IS NOT INITIAL.

    gs_zbv_tmp_dev-n_registro += 1.

    gs_zbv_tmp_dev-tipo_chamado = gs_zbv_chamados-tipo_chamado.
    gs_zbv_tmp_dev-id_chamado = gs_zbv_chamados-id_chamado.
    gs_zbv_tmp_dev-cliente = gs_zbv_chamados-cliente.
    gs_zbv_tmp_dev-acao = gs_zbv_chamados-acao. "x
    gs_zbv_tmp_dev-data_ini = sy-datum.
    gs_zbv_tmp_dev-hora_ini = sy-uzeit.

    CLEAR: gs_zbv_tmp_dev-data_fim,
           gs_zbv_tmp_dev-hora_fim,
           gs_zbv_tmp_dev-tempo.

    INSERT zbv_tmp_dev FROM gs_zbv_tmp_dev.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form encerra_contagem_em_aberto
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM encerra_contagem_em_aberto .

  SELECT * FROM zbv_tmp_em_abrt
                  INTO TABLE gt_zbv_tmp_em_abrt
                  WHERE tipo_chamado = gs_zbv_chamados-tipo_chamado
                  AND   id_chamado   = gs_zbv_chamados-id_chamado
                  AND   cliente      = gs_zbv_chamados-cliente
                  AND   acao         = gs_zbv_chamados-acao.

  READ TABLE gt_zbv_tmp_em_abrt INTO gs_zbv_tmp_em_abrt INDEX sy-dbcnt.
  gs_zbv_tmp_em_abrt-data_fim = sy-datum.
  gs_zbv_tmp_em_abrt-hora_fim = sy-uzeit.

  "Fazer o calculo do tempo parado
  CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
    EXPORTING
      date_from       = gs_zbv_tmp_em_abrt-data_ini
      date_to         = gs_zbv_tmp_em_abrt-data_fim
      time_from       = gs_zbv_tmp_em_abrt-hora_ini
      time_to         = gs_zbv_tmp_em_abrt-hora_fim
    IMPORTING
      delta_time      = tempo_minutos
*     DELTA_UNIT      =
    EXCEPTIONS
      from_greater_to = 1
      OTHERS          = 2.

  IF sy-subrc <> 0.
    MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
  ELSE.
    tempo_total = tempo_minutos / 60.
    gs_zbv_tmp_em_abrt-tempo = tempo_total.

    MODIFY zbv_tmp_em_abrt FROM gs_zbv_tmp_em_abrt.

  ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE

ENDFORM.
