*&---------------------------------------------------------------------*
*& Report ZRELATORIO_DISPONIBILIDADE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio_disponibilidade.

*TYPES:
*  BEGIN OF ty_qnt_chamados,
*    tipo_chamado      TYPE zbv_chamados-tipo_chamado,
*    id_chamado        TYPE zbv_chamados-id_chamado,
*    status            TYPE zbv_chamados-status,
*    acao              TYPE zbv_chamados-acao,
*    cliente           TYPE zbv_chamados-cliente,
*    ambiente          TYPE zbv_chamados-ambiente,
*    tecnologia        TYPE zbv_chamados-tecnologia,
*    complexidade      TYPE zbv_chamados-complexidade,
*    prioridade        TYPE zbv_chamados-prioridade,
*    solicitentate     TYPE zbv_chamados-solicitante,
*    data_abertura     TYPE zbv_chamados-data_abertura,
*    data_encerramento TYPE zbv_chamados-data_encerramento,
*    quantidade        TYPE i,
*  END OF ty_qnt_chamados.

DATA: wa_zbv_disponivel TYPE zbv_disponivel.

DATA: gt_zbv_disponivel TYPE TABLE OF zbv_disponivel,
      gs_zbv_disponivel TYPE zbv_disponivel.

*DATA:
*      quantidade TYPE i VALUE 1.

DATA: gr_table     TYPE REF TO cl_salv_table,
      lo_alv_error TYPE REF TO cx_salv_msg,
      gr_functions TYPE REF TO cl_salv_functions,
      gr_columns   TYPE REF TO cl_salv_columns_table,
      gr_column    TYPE REF TO cl_salv_column_table,
      gr_events    TYPE REF TO cl_salv_events_table.

*----- selection-screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "Tela de Seleção para lista de chamados

  SELECT-OPTIONS:
  so_co_te  FOR wa_zbv_disponivel-consultor_tecnico NO INTERVALS NO-EXTENSION,
  so_dt_rg    FOR wa_zbv_disponivel-data_registro.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  "Validação para verificar se o usuário possui permissão de execução
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
  CASE sy-subrc.
    WHEN 0.

      PERFORM get_data.
      CHECK gt_zbv_disponivel IS NOT INITIAL.

      SORT gt_zbv_disponivel ASCENDING BY consultor_tecnico data_registro.

*-- display the table itab with CL_SALV_TABLE
      PERFORM create_salv_table.

      PERFORM build_layout.

      PERFORM display_output.

    WHEN OTHERS.
      MESSAGE 'Sem permissão para executar a transação!' TYPE 'E'.
  ENDCASE.

FORM get_data.
  SELECT * FROM zbv_disponivel
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_disponivel
      WHERE consultor_tecnico IN so_co_te
      AND   data_registro     IN so_dt_rg.
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
          t_table      = gt_zbv_disponivel.
    CATCH cx_salv_msg.

  ENDTRY.

ENDFORM.


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
  gr_table->get_display_settings( )->set_list_header( TEXT-002 ). "Relatório de Chamados Fábrica

*-- column
  gr_columns = gr_table->get_columns( ).

  PERFORM exclude_column.
  PERFORM column_centered.

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
*& Form exclude_column
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM exclude_column.
  TRY.
      gr_column ?= gr_columns->get_column( 'MANDT' ).
      gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      gr_column ?= gr_columns->get_column( 'TEMPO_DISPONIVEL_TEMP' ).
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

      gr_column ?= gr_columns->get_column( 'DATA_REGISTRO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 13 ).

      gr_column ?= gr_columns->get_column( 'CONSULTOR_TECNICO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 25 ).

      gr_column ?= gr_columns->get_column( 'DISPONIVEL' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'DATA_INI_DISP' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'HORA_INI_DISP' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'DATA_FIM_DISP' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'HORA_FIM_DISP' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 8 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_DISPONIVEL' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 16 ).


    CATCH cx_salv_not_found.

  ENDTRY.

ENDFORM.


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
