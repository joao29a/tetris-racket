PYTHON=python2.7 -B

.PHONY: default atualizar zip limpar limpar-log

default: enviar

atualizar:
	git pull

zip: src.zip

src.zip: $(shell find src/ -type f ! -wholename 'src/racket/compiled*' ! -name '*~')
	@if grep -q -P '\000' $?; then\
        echo "** Arquivos binários encontrados **";\
        grep -l -P '\000' $?;\
        echo "** Remova os arquivos binários antes de fazer o envio **";\
        echo "** Se você estiver usando um IDE, procure pela opção Clean **";\
        exit 1; fi
	@echo Criando arquivo src.zip.
	@zip --quiet src.zip -r $?
	@echo Arquivo src.zip criado.

enviar: src.zip
	@$(PYTHON) enviar.py src.zip

limpar-log:
	@echo Removendo arquivos de log
	@rm -f *.log

limpar: limpar-log
	@echo Removendo src.zip
	@rm -f src.zip

# TODO: criar make ajuda?
