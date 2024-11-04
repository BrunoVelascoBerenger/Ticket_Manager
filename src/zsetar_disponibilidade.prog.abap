*&---------------------------------------------------------------------*
*& Report ZSETAR_DISPONIBILIDADE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsetar_disponibilidade.

DATA:
  gt_zbv_disponivel TYPE TABLE OF zbv_disponivel,
  gs_zbv_disponivel TYPE zbv_disponivel.

DATA:
  tempo_minutos    TYPE mcwmit-be_ae,
  tempo_disponivel TYPE p.

PARAMETERS:
  p_co_tec TYPE zbv_disponivel-consultor_tecnico AS LISTBOX VISIBLE LENGTH 20,
  p_dispon TYPE zbv_disponivel-disponivel AS CHECKBOX.

"Validação para verificar se o usuário possui permissão de execução
AUTHORITY-CHECK OBJECT 'ZFAB_AUT' ID 'ACTVT' FIELD '16'.
CASE sy-subrc.
  WHEN 0.

    SELECT * FROM zbv_disponivel
        INTO TABLE gt_zbv_disponivel
          WHERE consultor_tecnico = p_co_tec
          AND   data_registro = sy-datum.

    IF sy-subrc = 0.
      READ TABLE gt_zbv_disponivel ASSIGNING FIELD-SYMBOL(<fs_zbv_disponivel>) INDEX 1.

      IF <fs_zbv_disponivel>-disponivel = p_dispon.
        MESSAGE 'Não é possível repetir o mesmo status' TYPE 'W'.
      ENDIF.

      IF <fs_zbv_disponivel>-data_ini_disp IS NOT INITIAL AND <fs_zbv_disponivel>-data_fim_disp IS NOT INITIAL.
        CLEAR: <fs_zbv_disponivel>-data_ini_disp, <fs_zbv_disponivel>-hora_ini_disp.
        CLEAR: <fs_zbv_disponivel>-data_fim_disp, <fs_zbv_disponivel>-hora_fim_disp.

        <fs_zbv_disponivel>-tempo_disponivel_temp = <fs_zbv_disponivel>-tempo_disponivel.

        CLEAR: <fs_zbv_disponivel>-tempo_disponivel.

        <fs_zbv_disponivel>-disponivel = p_dispon.

        <fs_zbv_disponivel>-data_ini_disp = sy-datum.
        <fs_zbv_disponivel>-hora_ini_disp = sy-uzeit.

        MODIFY zbv_disponivel FROM <fs_zbv_disponivel>.
        FREE: <fs_zbv_disponivel>.

      ELSEIF <fs_zbv_disponivel>-data_registro IS NOT INITIAL AND <fs_zbv_disponivel>-tempo_disponivel_temp IS NOT INITIAL.

*    IF p_dispon = abap_true.
*      <fs_zbv_disponivel>-data_ini_disp = sy-datum.
*      <fs_zbv_disponivel>-hora_ini_disp = sy-uzeit.
*    ELSE.
        <fs_zbv_disponivel>-data_fim_disp = sy-datum.
        <fs_zbv_disponivel>-hora_fim_disp = sy-uzeit.
        <fs_zbv_disponivel>-disponivel = abap_false.

        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = '20240817' "gs_zbv_disponivel-data_ini_disp
            date_to    = '20240820' "gs_zbv_disponivel-data_fim_disp
            time_from  = '100000'   "gs_zbv_disponivel-hora_ini_disp
            time_to    = '110000'   "gs_zbv_disponivel-hora_fim_disp
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
          tempo_disponivel = tempo_minutos / 60.
          <fs_zbv_disponivel>-tempo_disponivel = tempo_disponivel + <fs_zbv_disponivel>-tempo_disponivel_temp.
          CLEAR: <fs_zbv_disponivel>-tempo_disponivel_temp.

          MODIFY zbv_disponivel FROM <fs_zbv_disponivel>.
          FREE: <fs_zbv_disponivel>.
        ENDIF. "L_MC_TIME_DIFFERENCE

*  ENDIF. "p_dispon = abap_true

      ELSEIF <fs_zbv_disponivel>-data_registro IS NOT INITIAL.
        <fs_zbv_disponivel>-data_fim_disp = sy-datum.
        <fs_zbv_disponivel>-hora_fim_disp = sy-uzeit.
        <fs_zbv_disponivel>-disponivel = abap_false.

*    gs_zbv_disponivel-data_fim_disp = sy-datum.
*    gs_zbv_disponivel-hora_fim_disp = sy-uzeit.

        CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
          EXPORTING
            date_from  = '20240818' "gs_zbv_disponivel-data_ini_disp
            date_to    = '20240820' "gs_zbv_disponivel-data_fim_disp
            time_from  = '100000'   "gs_zbv_disponivel-hora_ini_disp
            time_to    = '110000'   "gs_zbv_disponivel-hora_fim_disp
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
          tempo_disponivel = tempo_minutos / 60.
          <fs_zbv_disponivel>-tempo_disponivel = tempo_disponivel.
*      gs_zbv_disponivel-tempo_disponivel = tempo_disponivel.

          MODIFY zbv_disponivel FROM <fs_zbv_disponivel>.
          FREE: <fs_zbv_disponivel>.
        ENDIF.

      ENDIF.

    ELSE.

*  <fs_zbv_disponivel>-data_registro = sy-datum.
*  <fs_zbv_disponivel>-consultor_tecnico = p_co_tec.
*  <fs_zbv_disponivel>-disponivel = p_dispon.

      IF p_dispon = abap_false.
        MESSAGE 'Não é possível criar um novo registro de disponibilidade não estando disponível!' TYPE 'W'.
      ENDIF.

      gs_zbv_disponivel-data_registro = sy-datum.
      gs_zbv_disponivel-consultor_tecnico = p_co_tec.
      gs_zbv_disponivel-disponivel = p_dispon.

      IF p_dispon = abap_true.
*    <fs_zbv_disponivel>-data_ini_disp = sy-datum.
*    <fs_zbv_disponivel>-hora_ini_disp = sy-uzeit.

        gs_zbv_disponivel-data_ini_disp = sy-datum.
        gs_zbv_disponivel-hora_ini_disp = sy-uzeit.

        INSERT zbv_disponivel FROM gs_zbv_disponivel.
      ENDIF.

    ENDIF.
  WHEN OTHERS.
    MESSAGE 'Sem permissão a transação!' TYPE 'E'.
ENDCASE.

BREAK-POINT.
