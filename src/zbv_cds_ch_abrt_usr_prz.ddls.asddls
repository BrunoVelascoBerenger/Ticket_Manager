@AbapCatalog.sqlViewName: 'ZCCHABRTUSR_PRZ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Monitorar chamados Abertos com Prioridade - Consultor'
@Metadata.ignorePropagatedAnnotations: true
@OData.publish: true
define view ZBV_CDS_CH_ABRT_USR_PRZ
  as select from zbv_chamados   as chamados
    inner join   zbv_consul_tec as consultores on  chamados.consultor_tecnico = consultores.consultor_tecnico
                                               and consultores.usuario_syst   = $session.user
{
  key chamados.tipo_chamado         as TipoChamado,
  key chamados.id_chamado           as IdChamado,
  key chamados.cliente              as Cliente,
  key chamados.acao                 as Acao,
  key consultores.consultor_tecnico as ConsultorTecnico,
      chamados.status               as Status,
      chamados.ambiente             as Ambiente,
      chamados.solicitante          as Solicitante,
      chamados.tecnologia           as Tecnologia,
      chamados.modulo               as Modulo,
      chamados.em_atendimento       as EmAtendimento,
      chamados.complexidade         as Complexidade,
      chamados.prioridade           as Prioridade,
      chamados.anexo                as Anexo,
      chamados.cenario_erro         as CenarioErro,
      chamados.cenario_sucesso      as CenarioSucesso,
      chamados.acesso_produtivo     as AcessoProdutivo,
      chamados.aplicacao_nota       as AplicacaoNota,
      chamados.id_estimativa        as IdEstimativa,
      chamados.data_abertura        as DataAbertura,
      chamados.hora_abertura        as HoraAbertura,
      chamados.data_atualizacao     as DataAtualizacao,
      chamados.hora_atualizacao     as HoraAtualizacao,
      chamados.data_encerramento    as DataEncerramento,
      chamados.hora_encerramento    as HoraEncerramento,
      chamados.horas_estimadas      as HorasEstimadas,
      chamados.horas_utilizadas     as HorasUtilizadas,
      chamados.atualizado_por       as AtualizadoPor,
      chamados.tempo_total          as TempoTotal,
      consultores.usuario_syst      as UsuarioSyst
}
where
      chamados.prioridade = 'P1'
  and chamados.status <> 'Encerrado'
  or chamados.prioridade = 'P2'
  and chamados.status <> 'Encerrado'
