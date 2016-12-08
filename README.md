[![Build Status](https://travis-ci.org/jjst/elmtimate-tictactoe.svg?branch=master)](https://travis-ci.org/jjst/elmtimate-tictactoe)
<a href='http://www.recurse.com' title='Made with love at the Recurse Center'><img src='https://cloud.githubusercontent.com/assets/2883345/11325206/336ea5f4-9150-11e5-9e90-d86ad31993d8.png' height='20px'/></a>

[Play it online here!](https://jjst.github.io/elmtimate-tictactoe/)

# Elmtimate Tic-tac-toe
An implementation of [Ultimate Tic-tac-toe](https://mathwithbaddrawings.com/2013/06/16/ultimate-tic-tac-toe/) in your browser, in Elm.

![board](http://i.imgur.com/VbXU7vj.png)

## Install and run

If you haven't already done so, install [Elm](http://elm-lang.org/):
```bash
$ npm install -g elm
```

Then clone the repo:
```bash
$ git clone git@github.com:jjst/elmtimate-tictactoe.git
```

Then cd to the project folder and compile with:
```bash
$ cd elmtimate-tictactoe/
$ elm make src/Main.elm --output=tictactoe.html
```

Woohoo! If everything goes according to my evil instructions, you should be able to open tictactoe.html and play you some ultimate tic-tac-toe!

Note: for actual development, you probably want to use `elm reactor` instead of `elm make`: this will automatically rebuild the project on the fly when changes are made to the source (see the [getting started section](http://elm-lang.org/get-started) of the Elm website).

## Testing

To run the unit tests suite, run `runtests.sh`.
