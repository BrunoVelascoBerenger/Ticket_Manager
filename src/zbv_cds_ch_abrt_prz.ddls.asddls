@AbapCatalog.sqlViewName: 'ZBVCDSCHABRT_PRZ'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS para monitorar chamados Abertos com Prioridade'
@Metadata.ignorePropagatedAnnotations: true
@OData.publish: true
define view ZBV_CDS_CH_ABRT_PRZ
  as select from zbv_chamados
{
  key tipo_chamado      as TipoChamado,
  key id_chamado        as IdChamado,
  key cliente           as Cliente,
  key acao              as Acao,
      status            as Status,
      consultor_tecnico as ConsultorTecnico,
      ambiente          as Ambiente,
      solicitante       as Solicitante,
      tecnologia        as Tecnologia,
      modulo            as Modulo,
      em_atendimento    as EmAtendimento,
      complexidade      as Complexidade,
      prioridade        as Prioridade,
      anexo             as Anexo,
      cenario_erro      as CenarioErro,
      cenario_sucesso   as CenarioSucesso,
      acesso_produtivo  as AcessoProdutivo,
      aplicacao_nota    as AplicacaoNota,
      id_estimativa     as IdEstimativa,
      data_abertura     as DataAbertura,
      hora_abertura     as HoraAbertura,
      data_atualizacao  as DataAtualizacao,
      hora_atualizacao  as HoraAtualizacao,
      data_encerramento as DataEncerramento,
      hora_encerramento as HoraEncerramento,
      horas_estimadas   as HorasEstimadas,
      horas_utilizadas  as HorasUtilizadas,
      atualizado_por    as AtualizadoPor,
      tempo_total       as TempoTotal
}
where
      zbv_chamados.status     = 'Aberto'
  and zbv_chamados.prioridade = 'P1'
  or  zbv_chamados.status     = 'Aberto'
  and zbv_chamados.prioridade = 'P2'
