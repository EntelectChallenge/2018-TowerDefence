# Haskell Sample Bot
Haskell is a purely functional programming language.  You can find out
more about Haskell [here](https://www.haskell.org/).

## Environment Requirements
Install the [Haskell Platform](https://www.haskell.org/platform/) and
ensure that the `stack` executable is on the path.

## Building
Simply run:

```
stack install --local-bin-path bin
```

to build the binary and put it into a folder in the root of the
project called `bin`.

## Running
Haskell creates native binaries so you can simply run:

```
./bin/EntelectChallenge2018-exe
```

from the command line to invoke the bot program.

## Running the Bot with Multiple Threads
In order to run the Haskell bot with multiple threads (and possibly
cores) you need to change the bot configuration to inform the runner
to supply the appropriate runtime arguments.  The following is an
example "bot.json" for launching the bot with 2 threads:

```
{
    "author": "John Doe",
    "email": "john.doe@email.com",
    "nickName": "a-bot",
    "botLocation": "/bin",
    "botFileName": "EntelectChallenge2018-exe",
    "botLanguage": "haskell",
    "arguments": {
        "coreCount": 2
    }
}
```

You should also ensure that the executable is built with the
"-rtsopts" and "-threaded" compile time flags.  These are supplied
under the "executables" section in "package.yaml".  e.g.

```
executables:
  EntelectChallenge2018-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    dependencies:
    - EntelectChallenge2018
```
