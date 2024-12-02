# Ticket_Manager
Projeto de Ticket Manager em ABAP.

#Funcionalidade
Com o projeto do Ticket Manager:
* É possível realizar a criação de chamados através da utilização de formulários dinâmicos, realizar a manutenção e a consulta dos chamados via relatórios, onde é possível realizar comentários, realizar o gerenciamento de anexos, gerenciamento de horas e o gerenciamento das atualizações do chamado.
* Monitorar as filas de atendimento através de aplicações de monitoramento.
* Realizar a visualização de relatórios dinâmicos com base no volume de chamados, na disponibilidade dos consultores e na medição do tempo gasto em cada fase do atendimento.
* Relatar disponibilidade e indisponibilidade de atendimento de forma simples e objetiva.

Programas Criados:
* ZMENU_FABRICA - Menu da Fábrica de Software
* ZFORMULARIO_FABRICA_OO - Formulário da Fábrica de Software
* ZRELATORIO_DISPONIBILIDADE - Relatório de Disponibilidade
* ZRELATORIO_FABRICA_V2 - Relatório de Consulta de Chamados
* ZRELATORIO_QNT_CHAMADOS - Relatório de Volume de Chamados
* ZRELATORIO_TEMPO_MEDIO - Relatório de Tempo Médio de Atendimento
* ZSETAR_DISPONIBILIDADE - Relatar Disponibilidade / Indisponibilidade
* ZSET_DISPONIBILIDADE_AUTO - Programa de atualização automática de disponibilidade
* ZSET_DISP_MULT_REG - Relatar Disponibilidade
* ZSET_INDISP_MULT_REG - Relatar Indisponibilidade

CDS Views Criadas:
* ZBV_CDS_CH_ABRT - CDS para monitorar chamados em aberto
* ZBV_CDS_CH_ABRT_PRZ - CDS para monitorar chamados Abertos com Prioridade
* ZBV_CDS_CH_ABRT_USR - CDS para monitorar chamados Abertos - Consultor
* ZBV_CDS_CH_ABRT_USR_PRZ - Monitorar chamados Abertos com Prioridade - Consultor
* ZBV_CDS_CH_ATND_USR - Monitorar chamados em Atendimento - Consultor
* ZBV_CDS_CH_PRD_USR - Monitorar chamados Parados - Consultor
* ZBV_CDS_CH_TCLNT_USR Monitorar chamados em Teste Cliente - Consultor
* ZBV_CDS_CH_TFUNC_USR - Monitorar chamados em Teste Funcional - Consultor

Tabelas Criadas:
* zbv_acao - Tabela para o tipo de ação dos chamados
* zbv_ambiente - Tabela para o campo ambiente da Tabela Chamados
* zbv_anexos - Tabela de anexos dos chamados
* zbv_chamados - Tabela para Chamados Fábrica de Software
* zbv_clientes - Chamados Fábrica de Software
* zbv_comentarios - Tabela de comentários
* zbv_consul_tec - Tabela de Consultores Técnicos
* zbv_disponivel - Tabela para controle diária de disponibilidade por consultor
* zbv_ger_horas - Tabela de gerenciamento de horas utilizadas nos chamados
* zbv_log_atuali - Tabela de log de atualização de chamado
* zbv_modulo - Tabela de Módulos
* zbv_solicitante - Tabela para Solicitantes
* zbv_status - Tabela para Status de chamados
* zbv_tecnologia - Tabela de Tecnologias
* zbv_tpm_dev - Tabela para contagem do tempo em Desenvolvimento
* zbv_tmp_em_abrt - Tabela para contagem do tempo em Aberto
* zbv_tmp_em_atnd - Tabela para contagem do tempo em Atendimento
* zbv_tmp_parado - Tabela para contagem do tempo parado
* zbv_tmp_tst_func - Tabela para contagem do tempo Teste Cliente
* zbv_tmp_tst_clnt - Tabela para contagem do tempo Teste Funcional
