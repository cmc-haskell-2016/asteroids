# asteroids

[![Build Status](https://travis-ci.org/cmc-haskell-2016/asteroids.svg?branch=master)](https://travis-ci.org/cmc-haskell-2016/asteroids)

## Установка и запуск

Для установки клонируйте репозиторий и соберите проект с помощью `stack`:

```
git clone https://github.com/cmc-haskell-2016/asteroids.git
cd asteroids
stack setup
stack build
```

После установки запуск осуществляется командой `stack exec`:

```
stack exec asteroids
```

Во время разработки инициировать повторную сборку проекта с последующим запуском рекомендуется
следующей командой:

```
stack build && stack exec asteroids
```
