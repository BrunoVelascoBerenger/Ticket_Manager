*&---------------------------------------------------------------------*
*& Report ZRELATORIO_TEMPO_MEDIO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrelatorio_tempo_medio.

TYPE-POOLS: vrm.

TYPES:
  BEGIN OF ty_tempo_medio,
    tipo_chamado      TYPE zbv_chamados-tipo_chamado,
    id_chamado        TYPE zbv_chamados-id_chamado,
    status            TYPE zbv_chamados-status,
    cliente           TYPE zbv_chamados-cliente,
    acao              TYPE zbv_chamados-acao,
    tecnologia        TYPE zbv_chamados-tecnologia,
    complexidade      TYPE zbv_chamados-complexidade,
    prioridade        TYPE zbv_chamados-prioridade,
    data_abertura     TYPE zbv_chamados-data_abertura,
    hora_abertura     TYPE zbv_chamados-hora_abertura,
    data_encerramento TYPE zbv_chamados-data_encerramento,
    hora_encerramento TYPE zbv_chamados-hora_encerramento,
    tempo_abrt        TYPE zbv_tmp_em_abrt-tempo,
    tempo_atnd        TYPE zbv_tmp_em_atnd-tempo,
    tempo_dev         TYPE zbv_tmp_dev-tempo,
    tempo_parado      TYPE zbv_tmp_parado-tempo,
    tempo_tst_func    TYPE zbv_tmp_tst_func-tempo,
    tempo_tst_clnt    TYPE zbv_tmp_tst_clnt-tempo,
    tempo_total       TYPE zbv_chamados-tempo_total,

  END OF ty_tempo_medio.

DATA: name  TYPE vrm_id,
      list  TYPE vrm_values,
      value LIKE LINE OF list.

DATA: wa_zbv_chamados TYPE zbv_chamados.

DATA: gt_zbv_chamados     TYPE TABLE OF ty_tempo_medio,
      gt_zbv_tmp_em_abrt  TYPE TABLE OF zbv_tmp_em_abrt,
      gt_zbv_tmp_em_atnd  TYPE TABLE OF zbv_tmp_em_atnd,
      gt_zbv_tmp_dev      TYPE TABLE of zbv_tmp_dev,
      gt_zbv_tmp_parado   TYPE TABLE OF zbv_tmp_parado,
      gt_zbv_tmp_tst_func TYPE TABLE OF zbv_tmp_tst_func,
      gt_zbv_tmp_tst_clnt TYPE TABLE OF zbv_tmp_tst_clnt.
*      <fs_zbv_chamados> TYPE ty_tempo_medio.

DATA:
      encerrado TYPE zbv_chamados-status VALUE 'Encerrado'.

DATA:
      tempo_minutos TYPE mcwmit-be_ae.

DATA: diferenca_horas LIKE tempo_minutos,
     valor_dias       LIKE tempo_minutos,
     valor_reduzir    LIKE tempo_minutos.

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

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003. "Selecione a fase que deseja visualizar

  PARAMETERS: p_fase AS LISTBOX VISIBLE LENGTH 20.

SELECTION-SCREEN END OF BLOCK b2.


AT SELECTION-SCREEN OUTPUT.

  CLEAR list.

  name = 'P_FASE'.

*  value-key = '1'.
*  value-text = 'Em Aberto'.
*  APPEND value TO list.
*
*  value-key = '2'.
*  value-text = 'Em Atendimento.'.
*  APPEND value TO list.

  PERFORM: listbox USING '1' 'Em Aberto',
           listbox USING '2' 'Em Atendimento',
           listbox USING '3' 'Em Desenvolvimento',
           listbox USING '4' 'Parado',
           listbox USING '5' 'Teste Funcional',
           listbox USING '6' 'Teste Cliente',
           listbox USING '7' 'Todas'.

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = name
      values = list.

START-OF-SELECTION.

  "Validação para verificar se o usuário possui permissão de execução
  AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
  CASE sy-subrc.
    WHEN 0.

      PERFORM get_data.
      CHECK gt_zbv_chamados IS NOT INITIAL.

*      LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_zbv_chamados>).
*
*        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
*          EXPORTING
*            date_from  = <fs_zbv_chamados>-data_abertura
*            date_to    = <fs_zbv_chamados>-data_encerramento
*            time_from  = <fs_zbv_chamados>-hora_abertura
*            time_to    = <fs_zbv_chamados>-hora_encerramento
*          IMPORTING
*            delta_time = tempo_minutos
**           DELTA_UNIT =
**   EXCEPTIONS
**           FROM_GREATER_TO       = 1
**           OTHERS     = 2
*          .
*        IF sy-subrc <> 0.
*          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
*        ELSE.
*          <fs_zbv_chamados>-tempo_atendimento_total = tempo_minutos / 60.
*
*        ENDIF. "L_MC_TIME_DIFFERENCE
*
*      ENDLOOP.

*  PERFORM set_data.

      SORT gt_zbv_chamados ASCENDING BY data_abertura.

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

  SELECT * FROM zbv_tmp_em_abrt
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_em_abrt
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
      AND   cliente           IN so_clnt.

  SELECT * FROM zbv_tmp_em_atnd
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_em_atnd
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
 	  AND   cliente           IN so_clnt.

  SELECT * from zbv_tmp_dev
    INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_dev
    WHERE tipo_chamado      IN so_tp_ch
    AND   id_chamado        IN so_id_ch
    AND   cliente           IN so_clnt.

  SELECT * FROM zbv_tmp_parado
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_parado
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
      AND   cliente           IN so_clnt.

  SELECT * FROM zbv_tmp_tst_func
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_tst_func
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
      AND   cliente           IN so_clnt.

  SELECT * FROM zbv_tmp_tst_clnt
      INTO CORRESPONDING FIELDS OF TABLE gt_zbv_tmp_tst_clnt
      WHERE tipo_chamado      IN so_tp_ch
      AND   id_chamado        IN so_id_ch
      AND   cliente           IN so_clnt.



  LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_zbv_chamados>).

    "Tempo Em Aberto
    LOOP AT gt_zbv_tmp_em_abrt ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_em_abrt>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_em_abrt>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_em_abrt>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_em_abrt>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_em_abrt>-acao.

        <fs_zbv_chamados>-tempo_abrt += <fs_zbv_tmp_em_abrt>-tempo.


        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_em_abrt>-data_ini
            date_to    = <fs_zbv_tmp_em_abrt>-data_fim
            time_from  = <fs_zbv_tmp_em_abrt>-hora_ini
            time_to    = <fs_zbv_tmp_em_abrt>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_abrt -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.

    "Tempo Em Atendimento
    LOOP AT gt_zbv_tmp_em_atnd ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_em_atnd>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_em_atnd>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_em_atnd>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_em_atnd>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_em_atnd>-acao.

        <fs_zbv_chamados>-tempo_atnd += <fs_zbv_tmp_em_atnd>-tempo.

        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_em_abrt>-data_ini
            date_to    = <fs_zbv_tmp_em_abrt>-data_fim
            time_from  = <fs_zbv_tmp_em_abrt>-hora_ini
            time_to    = <fs_zbv_tmp_em_abrt>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_atnd -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.


    "Tempo Em Desenvolvimento
    LOOP AT gt_zbv_tmp_dev ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_dev>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_dev>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_dev>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_dev>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_dev>-acao.

        <fs_zbv_chamados>-tempo_dev += <fs_zbv_tmp_dev>-tempo.

        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_dev>-data_ini
            date_to    = <fs_zbv_tmp_dev>-data_fim
            time_from  = <fs_zbv_tmp_dev>-hora_ini
            time_to    = <fs_zbv_tmp_dev>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_dev -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.


    "Tempo Parado
    LOOP AT gt_zbv_tmp_parado ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_parado>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_parado>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_parado>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_parado>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_parado>-acao.

        <fs_zbv_chamados>-tempo_parado += <fs_zbv_tmp_parado>-tempo.

        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_em_abrt>-data_ini
            date_to    = <fs_zbv_tmp_em_abrt>-data_fim
            time_from  = <fs_zbv_tmp_em_abrt>-hora_ini
            time_to    = <fs_zbv_tmp_em_abrt>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_parado -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.

    "Tempo em Teste Funcional
    LOOP AT gt_zbv_tmp_tst_func ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_tst_func>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_tst_func>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_tst_func>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_tst_func>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_tst_func>-acao.

        <fs_zbv_chamados>-tempo_tst_func += <fs_zbv_tmp_tst_func>-tempo.

        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_em_abrt>-data_ini
            date_to    = <fs_zbv_tmp_em_abrt>-data_fim
            time_from  = <fs_zbv_tmp_em_abrt>-hora_ini
            time_to    = <fs_zbv_tmp_em_abrt>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_tst_func -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.

    "Tempo em Teste Cliente
    LOOP AT gt_zbv_tmp_tst_clnt ASSIGNING FIELD-SYMBOL(<fs_zbv_tmp_tst_clnt>).

      IF        <fs_zbv_chamados>-tipo_chamado = <fs_zbv_tmp_tst_clnt>-tipo_chamado
         AND    <fs_zbv_chamados>-id_chamado   = <fs_zbv_tmp_tst_clnt>-id_chamado
         AND    <fs_zbv_chamados>-cliente      = <fs_zbv_tmp_tst_clnt>-cliente
         AND    <fs_zbv_chamados>-acao         = <fs_zbv_tmp_tst_clnt>-acao.

        <fs_zbv_chamados>-tempo_tst_clnt += <fs_zbv_tmp_tst_clnt>-tempo.

        "Verificando se a diferença entre o fim e o inicio do tempo é mais de 1 dia
        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = <fs_zbv_tmp_em_abrt>-data_ini
            date_to    = <fs_zbv_tmp_em_abrt>-data_fim
            time_from  = <fs_zbv_tmp_em_abrt>-hora_ini
            time_to    = <fs_zbv_tmp_em_abrt>-hora_fim
          IMPORTING
            delta_time = tempo_minutos
*           DELTA_UNIT =
*           EXCEPTIONS
*           FROM_GREATER_TO       = 1
*           OTHERS     = 2
          .
        IF sy-subrc <> 0.
          MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
        ELSE.
          diferenca_horas = tempo_minutos / 60.

        ENDIF.

        IF diferenca_horas >= 24.
          valor_dias = diferenca_horas / 24.
          valor_reduzir = 16 * valor_dias.
          <fs_zbv_chamados>-tempo_tst_clnt -= valor_reduzir.

          CLEAR: diferenca_horas,
          valor_dias,
          valor_reduzir.

        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDLOOP.

  "Fazer um loop na tabela de chamados e fazer com que o tempo total que consta ali seja a soma do tempo de todas as fases.

  LOOP AT gt_zbv_chamados ASSIGNING FIELD-SYMBOL(<fs_gt_zbv_chamados>).
    <fs_gt_zbv_chamados>-tempo_total = <fs_gt_zbv_chamados>-tempo_abrt +
                                       <fs_gt_zbv_chamados>-tempo_atnd +
                                       <fs_gt_zbv_chamados>-tempo_dev +
                                       <fs_gt_zbv_chamados>-tempo_parado +
                                       <fs_gt_zbv_chamados>-tempo_tst_func +
                                       <fs_gt_zbv_chamados>-tempo_tst_clnt.

  ENDLOOP.

*  FREE: <fs_zbv_chamados>,
*        <fs_zbv_tmp_em_abrt>,
*        <fs_zbv_tmp_em_atnd>,
*        <fs_zbv_tmp_parado>,
*        <fs_zbv_tmp_tst_func>,
*        <fs_zbv_tmp_tst_clnt>.

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
*      gr_column ?= gr_columns->get_column( 'MANDT' ).

*      gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      CASE p_fase.
        WHEN '1'. "Em Aberto

          gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

        WHEN '2'. "Em Atendimento

          gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

        WHEN '3'. "Em Desenvolvimento

          gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

        WHEN '4'. "Parado

          gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

        WHEN '5'. "Teste Funcional

          gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

        WHEN '6'. "Teste Cliente

          gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

          gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
          gr_column->set_technical( value = if_salv_c_bool_sap=>true ).

      ENDCASE.

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

      gr_column ?= gr_columns->get_column( 'STATUS' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'CLIENTE' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_output_length( 15 ).

      gr_column ?= gr_columns->get_column( 'ACAO' ).
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

      gr_column ?= gr_columns->get_column( 'TEMPO_ABRT' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo em Aberto' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').

      gr_column ?= gr_columns->get_column( 'TEMPO_ATND' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo em Atendimento' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').
      gr_column->set_output_length( 18 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_DEV' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo em Desenvolvimento' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').
      gr_column->set_output_length( 20 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_PARADO' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo - Parado' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').

      gr_column ?= gr_columns->get_column( 'TEMPO_TST_FUNC' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo - Teste Funcional' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').
      gr_column->set_output_length( 18 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_TST_CLNT' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo - Teste Cliente' ).
      gr_column->set_short_text('').
      gr_column->set_medium_text('').
      gr_column->set_output_length( 16 ).

      gr_column ?= gr_columns->get_column( 'TEMPO_TOTAL' ).
      gr_column->set_alignment( if_salv_c_alignment=>centered ).
      gr_column->set_long_text( 'Tempo Total' ).
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
*&---------------------------------------------------------------------*
*& Form listbox
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_KEY
*&      --> P_TEXT
*&---------------------------------------------------------------------*
FORM listbox  USING    p_key
                       p_text.

  value-key = p_key.
  value-text = p_text.
  APPEND value TO list.

ENDFORM.
