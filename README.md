# chainword

[![Build Status](https://travis-ci.org/cmc-haskell-2015/chainword.svg?branch=master)](https://travis-ci.org/cmc-haskell-2015/chainword)

Головоломка чайнворд.

## Установка и запуск

Для установки клонируйте репозиторий и запустите `cabal install`:
Запуск- cabal run из корневой папки репозитория

```
$ git clone https://github.com/cmc-haskell-2015/chainword.git
$ cd chainword
$ cabal install
```

После установки запуск осуществляется командой `chainword`:

```
$ chainword
```

Для сборки и запуска текущей версии непосредственно из репозитория используйте `cabal run`:

```
$ cd chainword
$ cabal run
```

## Документация

Автоматическая документация кода сгенерирована при помощи [Haddock](https://www.haskell.org/haddock/).

Онлайн документация доступна здесь: http://cmc-haskell-2015.github.io/chainword/docs/

Локально документацию можно собрать, запустив простую команду:

```
$ cabal haddock
```

