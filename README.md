---
title: "DeclaraServ"
output: github_document
---

# 📄 DeclaraServ

### Emissão digital de declarações funcionais com segurança, agilidade e autonomia.

[![License: MIT](https://img.shields.io/badgen.svg](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D4.3-blue)://www.r-project.org/)
[![Shiny](https://img.shields.io/badge%20App-lightblue](https://shiny.posit.co/)
[![Status](https://img.shields.io/badge/Statusvolvimento-yellow]()
[![GitHub last commit](https://img.shields.io/github/last-commit/SEU-USUARIO/DeclaraServ)](https://githubServ)

<br>

<p align="center">
  <img src="img/logo_declaraserv.png" width="250r">
<b>Sistema de Emissão de Declarações Funcionais para Servidores Públicos</b>
</p>

---

# 📖 Sobre o Projeto

O **DeclaraServ** é uma aplicação desenvolvida para automatizar a emissão de declarações funcionais por meio do **Portal do Servidor**, proporcionando aos servidores públicos autonomia para obter documentos oficiais de forma rápida, segura e totalmente digital.

A solução visa eliminar procedimentos manuais de solicitação e emissão de declarações, reduzindo o tempo de atendimento e otimizando os processos administrativos relacionados à gestão de pessoas.

Inicialmente, a aplicação contempla a emissão dos seguintes documentos:

- Declaração de Afastamento;
- Declaração de Tempo de Serviço;
- Declaração de Vínculo Funcional.

Novos modelos poderão ser incorporados futuramente, de acordo com as necessidades institucionais e normativas do órgão.

---

# 🎯 Objetivos

- Automatizar a emissão de declarações funcionais;
- Reduzir a demanda de atendimento manual pelas áreas de Gestão de Pessoas;
- Disponibilizar documentos oficiais diretamente ao servidor;
- Garantir maior agilidade e transparência nos processos administrativos;
- Padronizar os modelos institucionais de declaração;
- Permitir expansão para novos documentos futuramente.

---

# ✨ Funcionalidades

- 🔐 Integração com o Portal do Servidor;
- 📄 Emissão automática de declarações funcionais;
- ⚡ Geração instantânea de documentos;
- 📥 Download em formato PDF;
- ✅ Validação automática dos dados cadastrais;
- 📋 Modelos padronizados institucionalmente;
- 🔎 Consulta de histórico de emissões;
- 📆 Registro de data e hora da emissão;
- 🔒 Controle de acesso baseado no usuário autenticado;
- 📈 Possibilidade de inclusão de novos modelos de declaração.

---

# 📑 Declarações Disponíveis

## Declaração de Afastamento

Documento destinado à comprovação dos afastamentos funcionais registrados para o servidor.

## Declaração de Tempo de Serviço

Documento contendo a informação do tempo de serviço reconhecido pela instituição.

## Declaração de Vínculo Funcional

Documento destinado à comprovação da existência de vínculo funcional do servidor com o órgão.

---

# 🚀 Evolução do Projeto

O DeclaraServ foi concebido para crescimento contínuo.

Exemplos de futuras declarações:

- Declaração de Lotação;
- Declaração de Remuneração;
- Declaração de Exercício;
- Declaração de Não Acumulação de Cargos;
- Declaração de Férias;
- Declaração de Frequência;
- Declaração de Participação em Comissões;
- Declaração de Dependentes;
- Declaração para Instituições Financeiras;
- Outras declarações específicas da gestão de pessoas.

---

# 👥 Público-Alvo

A solução destina-se a:

- Servidores efetivos;
- Servidores comissionados;
- Magistrados;
- Cedidos;
- Aposentados;
- Pensionistas (conforme regulamentação);
- Unidades de Gestão de Pessoas;
- Setores administrativos.

---

# ✅ Benefícios

- Redução do atendimento presencial;
- Maior autonomia para os servidores;
- Disponibilidade 24 horas por dia;
- Padronização documental;
- Diminuição do retrabalho administrativo;
- Agilidade na obtenção de documentos;
- Segurança das informações;
- Melhoria da experiência do usuário.

---

# 🛠 Tecnologias Utilizadas

```r
R
Shiny
shinydashboard
DT
dplyr
glue
lubridate
stringr
htmltools
bslib
rmarkdown
```

---

# 📂 Estrutura do Projeto

```text
DeclaraServ/
│
├── app/
│   ├── ui.R
│   ├── server.R
│   └── global.R
│
├── certificados/
├── modelos/
├── relatorios/
├── scripts/
├── data/
├── img/
│   └── logo_declaraserv.png
│
├── README.Rmd
├── README.md
├── LICENSE
└── .gitignore
```

---

# 🔄 Fluxo de Utilização

1. O servidor realiza autenticação no Portal do Servidor.
2. A aplicação identifica o usuário autenticado.
3. O servidor seleciona o tipo de declaração desejada.
4. O sistema consulta as informações funcionais necessárias.
5. O documento é gerado automaticamente.
6. A declaração é disponibilizada para download.
7. O histórico da emissão é registrado.

---

# 🔒 Segurança

A aplicação foi projetada observando princípios de segurança da informação e proteção de dados:

- Controle de acesso autenticado;
- Emissão apenas para o próprio usuário;
- Registro de auditoria das emissões;
- Integridade dos documentos gerados;
- Utilização de dados oficiais da instituição;
- Possibilidade de inclusão de mecanismos de validação eletrônica.

---

# 🏛 Governança e Transformação Digital

O DeclaraServ contribui para a modernização da administração pública por meio da digitalização dos serviços internos, promovendo:

- Eficiência administrativa;
- Desburocratização;
- Governança digital;
- Transparência;
- Sustentabilidade;
- Melhoria contínua dos serviços ao servidor.

---

# 👨‍💻 Desenvolvedores

## Kleberson Pinto

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)

## Edison Carvalho

**Técnico Judiciário - Programação de Sistemas**  
Tribunal de Justiça do Estado de Sergipe (TJSE)

---

# 🏛 Instituição

**Tribunal de Justiça do Estado de Sergipe (TJSE)**

---

# 📜 Licença

## Licença MIT

Copyright (c) 2026 Kleberson Pinto e Edison Carvalho

É concedida permissão, gratuitamente, a qualquer pessoa que obtenha uma cópia deste software e dos arquivos de documentação associados ("Software"), para utilizar o Software sem restrição, incluindo, sem limitação, os direitos de usar, copiar, modificar, mesclar, publicar, distribuir, sublicenciar e/ou vender cópias do Software, e permitir que as pessoas a quem o Software seja fornecido façam o mesmo, sujeito às seguintes condições:

O aviso de copyright acima e esta permissão deverão ser incluídos em todas as cópias ou partes substanciais do Software.

O SOFTWARE É FORNECIDO "NO ESTADO EM QUE SE ENCONTRA", SEM GARANTIA DE QUALQUER NATUREZA, EXPRESSA OU IMPLÍCITA, INCLUINDO, MAS NÃO SE LIMITANDO ÀS GARANTIAS DE COMERCIALIZAÇÃO, ADEQUAÇÃO A UM DETERMINADO PROPÓSITO E NÃO VIOLAÇÃO. EM NENHUMA HIPÓTESE OS AUTORES OU DETENTORES DOS DIREITOS AUTORAIS SERÃO RESPONSÁVEIS POR QUALQUER RECLAMAÇÃO, DANO OU OUTRA RESPONSABILIDADE, SEJA EM AÇÃO CONTRATUAL, ILÍCITO CIVIL OU DE OUTRA FORMA, DECORRENTE DE, OU EM CONEXÃO COM O SOFTWARE OU O USO OU OUTRAS NEGOCIAÇÕES NO SOFTWARE.

---

## 📚 Justificativa da Licença MIT

O DeclaraServ é uma solução voltada à transformação digital dos serviços de gestão de pessoas no setor público. A adoção da Licença MIT incentiva o compartilhamento de conhecimento, a reutilização por outras instituições e a evolução colaborativa do software.

A escolha desta licença permite que órgãos públicos adaptem e ampliem a solução conforme suas necessidades, preservando o reconhecimento dos autores e promovendo princípios fundamentais da administração pública moderna:

- Transformação Digital;
- Eficiência Administrativa;
- Compartilhamento de Conhecimento;
- Transparência Tecnológica;
- Cooperação Institucional;
- Inovação Aberta.

---

# 📌 Slogan

> **DeclaraServ**
>
> *Mais autonomia, menos burocracia.*
---

<p align="center">
Desenvolvido no Tribunal de Justiça do Estado de Sergipe (TJSE).
</p>
