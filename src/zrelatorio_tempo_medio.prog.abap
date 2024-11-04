*&---------------------------------------------------------------------*
*& Report ZRELATORIO_TEMPO_MEDIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio_tempo_medio.

TYPES:
  BEGIN OF ty_tempo_medio,
    tipo_chamado      TYPE zbv_chamados-tipo_chamado,
    id_chamado        TYPE zbv_chamados-id_chamado,
    status            TYPE zbv_chamados-status,
    cliente           TYPE zbv_chamados-cliente,
    tecnologia        TYPE zbv_chamados-tecnologia,
    complexidade      TYPE zbv_chamados-complexidade,
    prioridade        TYPE zbv_chamados-prioridade,
    data_abertura     TYPE zbv_chamados-data_abertura,
    hora_abertura     TYPE zbv_chamados-hora_abertura,
    data_encerramento TYPE zbv_chamados-data_encerramento,
    hora_encerramento TYPE zbv_chamados-hora_encerramento,
    tempo_atendimento TYPE zbv_disponivel-tempo_disponivel,
  END OF ty_tempo_medio.

DATA: wa_zbv_chamados TYPE zbv_chamados.

DATA: gt_zbv_chamados TYPE TABLE OF ty_tempo_medio.
*      <fs_zbv_chamados> TYPE ty_tempo_medio.

DATA:
      encerrado TYPE zbv_chamados-status VALUE 'Encerrado'.

DATA:
      tempo_minutos TYPE mcwmit-be_ae.

DATA: gr_table     TYPE REF TO cl_salv_table,
      lo_alv_error TYPE REF TO cx_salv_msg,
      gr_functions TYPE REF TO cl_salv_functions,
      gr_columns   TYPE REF TO cl_salv_columns_table,
      gr_column    TYPE REF TO cl_salv_column_table,
      gr_events    TYPE REF TO cl_salv_events_table.

*----- selection-screen
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001. "Tela de Seleção para lista de chamados

  SELECT-OPTIONS:
  so_tp_ch  FOR wa_zbv_chamados-tipo_chamado      NO INTERVALS NO-EXTENSION,
  so_id_ch  FOR wa_zbv_chamados-id_chamado        NO INTERVALS NO-EXTENSION,
  so_co_te  FOR wa_zbv_chamados-consultor_tecnico NO INTERVALS NO-EXTENSION,
  so_clnt   FOR wa_zbv_chamados-cliente           NO INTERVALS NO-EXTENSION,
  so_tecno  FOR wa_zbv_chamados-tecnologia        NO INTERVALS NO-EXTENSION,
  so_compl  FOR wa_zbv_chamados-complexidade      NO INTERVALS NO-EXTENSION,
  so_prior  FOR wa_zbv_chamados-prioridade        NO INTERVALS NO-EXTENSION.
*  so_dt_ab  FOR wa_zbv_chamados-data_abertura,
*  so_dt_en  FOR wa_zbv_chamados-data_encerramento.

SELECTION-SCREEN END OF BLOCK b1.

START-OF-SELECTION.

  "Validação para verificar se o usuário possui permissão de execução
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
  CASE sy-subrc.
    WHEN 0.

      PERFORM get_data.
      CHECK gt_zbv_chamados IS NOT INITIAL.

      LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_zbv_chamados>).

        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_chamados>-data_abertura
            date_to    = <fs_zbv_chamados>-data_encerramento
            time_from  = <fs_zbv_chamados>-hora_abertura
            time_to    = <fs_zbv_chamados>-hora_encerramento
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*   EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          <fs_zbv_chamados>-tempo_atendimento = tempo_minutos / 60.

        ENDIF. "L_MC_TIME_DIFFERENCE

      ENDLOOP.

*  PERFORM set_data.

*  SORT gt_zbv_chamados ASCENDING BY data_abertura.

*-- display the table itab with CL_SALV_TABLE
      PERFORM create_salv_table.

      PERFORM build_layout.

      PERFORM display_output.

    WHEN OTHERS.
      MESSAGE 'Sem permissão a transação' TYPE 'E'.
  ENDCASE.

FORM get_data.
  SELECT * FROM zbv_chamados
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_chamados
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
      AND   cliente           IN so_clnt
      AND   tecnologia        IN so_tecno
      AND   complexidade      IN so_compl
      AND   prioridade        IN so_prior
      AND   status            EQ encerrado.
*      AND   data_abertura     IN so_dt_ab
*      AND   data_encerramento IN so_dt_en.
ENDFORM.


*&---------------------------------------------------------------------*
*& Form set_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
*FORM set_data .
*  LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_zbv_chamados>).
*    <fs_zbv_chamados>-quantidade = quantidade.
*  ENDLOOP.
*ENDFORM.


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
*  gr_table->get_columns( )->set_column_position( columnname = 'QUANTIDADE' position = 1 ).
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

      gr_column ?= gr_columns->get_column( 'TIPO_CHAMADO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'ID_CHAMADO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'CLIENTE' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'TECNOLOGIA' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'COMPLEXIDADE' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 12 ).

      gr_column ?= gr_columns->get_column( 'PRIORIDADE' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'DATA_ABERTURA' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'DATA_ENCERRAMENTO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 10 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_ATENDIMENTO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo de Atendimento' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').

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
