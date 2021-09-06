# echoBot

## Table of contents
* [Introduction](#Introduction)
* [Setup](#setup)
* [Features](#features)

## Introduction
It's a simple telegram bot, which just echoes back everything you send it.

## Setup
At first, you have to build the project, using the stack tool:
```
$ stack build
```
Then, you have to create the file, called *config.yaml* with the following field:
- token - token of the telegram bot
- timeout - long polling timeout (some integer)
- mode - logger mode (Stdout | File | Both)
- priority - log level (None | Debug | Error | Warn | Info)
- file - log file name
- help - text for the /help command
- question - question for the /repeat command
- repeat - default repeat rate

![example](./example.png "example")
