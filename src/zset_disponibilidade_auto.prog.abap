*&---------------------------------------------------------------------*
*& Report ZSET_DISPONIBILIDADE_AUTO
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zset_disponibilidade_auto.

DATA: gt_zbv_disponivel TYPE TABLE OF zbv_disponivel,
      gs_zbv_disponivel TYPE zbv_disponivel.

DATA : gt_zbv_consul_tec TYPE TABLE OF zbv_consul_tec.

DATA:
  tempo_minutos    TYPE mcwmit-be_ae,
  tempo_disponivel TYPE p DECIMALS 3.

DATA: dia_anterior TYPE d VALUE IS INITIAL.

CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
  EXPORTING
    date      = sy-datum
    days      = '01'
    months    = '00'
    signum    = '-'
    years     = '00'
  IMPORTING
    calc_date = dia_anterior.

*dia_anterior = sy-datum - 1.

SELECT * FROM zbv_disponivel
  INTO TABLE gt_zbv_disponivel
    WHERE data_registro = dia_anterior.

SELECT * FROM zbv_consul_tec
  INTO TABLE gt_zbv_consul_tec.

IF sy-subrc = 0.

  "verificar se o sy-dbcnt de gt_zbv_disponivel é igual a quantidade
  "de registros na tabela zbv_disponivel
  "se não for igual, precisa verificar qual consultor está faltando e
  "informar que ele precisa fazer o primeiro registro na tabela

*  LOOP AT gt_zbv_disponivel ASSIGNING FIELD-SYMBOL(<fs_zbv_disponivel>).
**    CLEAR: <fs_zbv_disponivel>-data_fim_disp, <fs_zbv_disponivel>-hora_fim_disp,
**           <fs_zbv_disponivel>-tempo_disponivel, <fs_zbv_disponivel>-tempo_disponivel_temp.
*
**    <fs_zbv_disponivel>-data_ini_disp = sy-datum.
**    <fs_zbv_disponivel>-hora_ini_disp = '080000'.
*
*
*
**  INSERT zbv_disponivel FROM <fs_zbv_disponivel>.
*  ENDLOOP.

  LOOP AT gt_zbv_consul_tec ASSIGNING FIELD-SYMBOL(<fs_consul_tec>).

    "Loop para chegar na ultimo registro do consultor no dia anterior
    LOOP AT gt_zbv_disponivel ASSIGNING FIELD-SYMBOL(<fs_zbv_disponivel>) WHERE consultor_tecnico = <fs_consul_tec>-consultor_tecnico.

*      DATA(maior_n_registro) = 0.

*      IF maior_n_registro < <fs_zbv_disponivel>-n_registro.
*        maior_n_registro = <fs_zbv_disponivel>-n_registro.
*      ENDIF.

    ENDLOOP.

    IF <fs_zbv_disponivel> IS NOT ASSIGNED.
      CONTINUE.
    ENDIF.

    IF <fs_zbv_disponivel>-disponivel = abap_true.
      <fs_zbv_disponivel>-disponivel = abap_false.
      <fs_zbv_disponivel>-data_fim_disp = dia_anterior.
      <fs_zbv_disponivel>-hora_fim_disp = '180000'.

      "fazer o calculo para pegar as horas do inicio até as 18h
      CALL FUNCTION 'L_MC_TIME_DIFFERENCE'
        EXPORTING
          date_from       = <fs_zbv_disponivel>-data_ini_disp "gs_zbv_disponivel-data_ini_disp
          date_to         = <fs_zbv_disponivel>-data_fim_disp "gs_zbv_disponivel-data_fim_disp
          time_from       = <fs_zbv_disponivel>-hora_ini_disp   "gs_zbv_disponivel-hora_ini_disp
          time_to         = <fs_zbv_disponivel>-hora_fim_disp   "gs_zbv_disponivel-hora_fim_disp
        IMPORTING
          delta_time      = tempo_minutos
*         DELTA_UNIT      =
        EXCEPTIONS
          from_greater_to = 1
          OTHERS          = 2.

      "Verifica se a função pegou corretamento o tempo
      IF sy-subrc <> 0.
        MESSAGE 'Erro ao pegar a diferença de tempo!' TYPE 'I' DISPLAY LIKE 'E'.
      ELSE.
        tempo_disponivel = tempo_minutos / 60.
        <fs_zbv_disponivel>-tempo_disponivel = tempo_disponivel.

        "Modificar o ultimo registro para que ele seja finalizado
        MODIFY zbv_disponivel FROM <fs_zbv_disponivel>.

        "criar um novo registro de disponibilidade
        gs_zbv_disponivel-data_registro = sy-datum.
        gs_zbv_disponivel-n_registro = 1.
        gs_zbv_disponivel-consultor_tecnico = <fs_consul_tec>-consultor_tecnico.
        gs_zbv_disponivel-disponivel = abap_true.
        gs_zbv_disponivel-data_ini_disp = sy-datum.
        gs_zbv_disponivel-hora_ini_disp = '080000'.

        "INSERE o novo registro na tabela ZBV_DISPONIVEL
        INSERT zbv_disponivel FROM gs_zbv_disponivel.

        FREE: <fs_zbv_disponivel>.
      ENDIF. "sy-subrc <> 0. Validação da função L_MC_TIME_DIFFERENCE


      "Se terminar em INDISPONIVEL, ÑÃO PRECISA CRIAR UM NOVO REGISTRO
*    "Se o registro do dia anterior já tiver sido finalizado, somente criar um novo
*    ELSEIF <fs_zbv_disponivel>-disponivel = abap_false.
*      "criar um novo registro
*      gs_zbv_disponivel-data_registro = sy-datum.
*      gs_zbv_disponivel-n_registro = 1.
*      gs_zbv_disponivel-consultor_tecnico = <fs_consul_tec>-consultor_tecnico.
*      gs_zbv_disponivel-disponivel = abap_false.
*      gs_zbv_disponivel


    ENDIF. "<fs_zbv_disponivel>-disponivel = abap_true.

  ENDLOOP.

ENDIF.
