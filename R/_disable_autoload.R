# =====================================================
# R/_disable_autoload.R
# -----------------------------------------------------
# A simples EXISTÊNCIA deste arquivo desliga o "autoload" do Shiny.
#
# Desde o Shiny 1.5, ao executar um app que tem uma pasta R/, o Shiny
# faz source() de TODOS os arquivos R/*.R automaticamente, ANTES de
# executar o app.R. Isso fazia o R/auth.R rodar import("ldap3") antes
# de o app.R carregar o .Renviron e chamar use_python() — ou seja, o
# reticulate escolhia um Python "padrão" (sem o ldap3), e o app quebrava
# com "ModuleNotFoundError: No module named 'ldap3'" antes mesmo de a
# verificação do app.R ser executada. Além disso, todos os arquivos de
# R/ eram carregados DUAS vezes (autoload + source() explícito no app.R).
#
# Com este arquivo presente, quem carrega R/*.R é apenas o app.R, na
# ordem correta. NÃO REMOVA este arquivo.
# =====================================================
