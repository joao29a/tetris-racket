#!/usr/bin/python
# coding: utf-8

import os
import sys
import codecs
import re

sys.stdout = codecs.getwriter('utf-8')(sys.stdout)

VERSAO = '0.1'
MAX_TAMANHO_ARQUIVO = 100 * 1024
ESPACOS = re.compile(r"\s+")
URL_DESTINO = "http://malbarbo.pro.br/trabalhos"

def urlopen(url, data):
    import urllib
    import urllib2
    return urllib2.urlopen(url, urllib.urlencode(data))

def read_sim_nao(prompt):
    r = None
    while r not in ["s", "n"]:
        r = raw_input(prompt + " [s/n] ")
    return r == "s"

def read_ints(prompt, max = 10):
    values = None
    while not values:
        try:
            values = map(int, ESPACOS.split(raw_input(prompt)))
            if len(values) > max:
                print u"Digite no máximo %s números" % (max)
                values = None
        except:
            print u"Digite apenas números separados por espaços"
    return values
 
def md5digest(data):
    import hashlib
    m = hashlib.md5()
    m.update(data)
    return m.hexdigest()

def kb(valor):
    return valor / 1024

def enviar(arquivo, dis, ras):
    try:
        data = {'versao': VERSAO,
                'arquivo': arquivo,
                'dis': dis,
                'ras': " ".join(map(str, ras))}
        r = urlopen(URL_DESTINO, data)
        print unicode(r.read(), "utf-8")
    except Exception as e:
        print u"Erro ao enviar o arquivo:"
        print e
        print
        print u"Tente enviar novamente, se o problema persistir,"
        print u"envie um email para o professor com o seu trabalho"
        print u"e uma cópia desta mensagem de erro."

def exibir_usar_make():
    print u"Não execute o programa '%s' diretamente." % (sys.argv[0])
    print u"Para enviar o trabalho execute o comando 'make enviar'."

def ler_arquivo(arquivo):
    try:
        f = open(arquivo, "rb")
        data = f.read()
        f.close()
        return data
    except Exception as e:
        print u"Não foi possível ler o arquivo '%s'" % (arquivo)
        print e

def main():
    if len(sys.argv) < 2:
        print u"Poucos parâmetros. É necessário especificar o arquivo a ser enviado."
        exibir_usar_make()
        sys.exit(1)

    if len(sys.argv) > 2:
        print u"Muitos parâmetros. Apenas um arquivo deve ser especificado."
        exibir_usar_make()
        sys.exit(1)

    arquivo = sys.argv[1]
    if not os.path.exists(arquivo):
        print u"O arquivo '%s' não existe." % (arquivo)
        exibir_usar_make()
        sys.exit(1)

    conteudo = ler_arquivo(arquivo)
    if len(conteudo) > MAX_TAMANHO_ARQUIVO:
        print u"O arquivo a ser enviado deve ter no máximo de %dKb" % (kb(MAX_TAMANHO_ARQUIVO))
        print u"O arquivo '%s' tem %dKb" % (arquivo, kb(len(conteudo))) 
        print u"Certifique-se que o arquivo '%s' não contém arquivos compilados (binários)" % (arquivo)
        sys.exit(1)

    dis = read_ints(u"Código da disciplina: ", 1)[0]
    ras = read_ints(u"RA(s): ")
    print

    digest = md5digest(conteudo)

    print u"Antes de confirmar o envio, certifique-se que"
    print u"  1 - o conteúdo do arquivo '%s' está correto" % (arquivo)
    print u"  2 - o md5 do arquivo '%s' é %s" % (arquivo, digest)
    print u"  3 - o código da disciplina é %d" % (dis)
    if len(ras) == 1:
        print u"  4 - o RA é %s" % (" ".join(map(str, ras)))
    else:
        print u"  4 - os RA's são %s" % (" ".join(map(str, ras)))

    if read_sim_nao("Enviar o trabalho?"):
        print
        enviar(conteudo, dis, ras)

if __name__ == "__main__":
    main()
