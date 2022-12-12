# Functional Programming 1 - 1DL330
# Individual Assignment: An Artificial Intelligence for Reversi

Introduction
------------

Your task for this assignment is to implement a Reversi AI in Haskell.

This is an individual assignment. It is acceptable to discuss abstract solution
methods with others, and to get inspiration from the Internet or other sources,
provided

- you give proper credit (via an explicit comment in your sources) whenever you
  use someone else's ideas, and

- you have constructed your submission yourself. It is not acceptable to share
  or copy source code.

See the course's [Ethics Rules](https://uppsala.instructure.com/courses/71668/pages/ethics-rules)
for further guidelines.

Game Rules
----------

Reversi (also known as Othello) is a board game for two players. We will call
the players Black and White.

The game is played on an 8-by-8 board. Each field of the board is either empty,
marked black, or marked white.

We will number the fields of the board from 0 to 63, from top left to bottom
right:

![The fields of the board are numbered from 0 to 63](images/board.svg)

Initially, fields 28 and 35 are marked black, fields 27 and 36 are marked white,
and all other fields are empty:

![The initial board. Empty fields are shown in gray.](images/initial-board.svg)

Players take alternate turns. Black moves first.

A move consist of choosing an empty field. This field is marked with the moving
player's color. Then, all fields lying on a straight, contiguous (horizontal,
vertical or diagonal) *line of fields marked in the opponent's color* that
extends from the newly marked field to *another field marked in the moving
player's color* change their color (from the opponent's to the moving
player's). A move is valid only if at least one non-empty field changes its
color. See <http://en.wikipedia.org/wiki/Reversi#Rules> for diagrams and further
explanations.

If one player cannot make a valid move, that player must pass (skip the
move). Play passes back to the other player.

When neither player can move, the game ends. (This occurs when all fields of the
board have been marked, or when neither player can mark a field so that this
would cause a non-empty field to change color.)

The player with the most fields marked in his/her color at the end of the game
wins. If there are equally many black and white fields, the game is a draw.

Your Task
---------

Your task is to write a single file called `ReversiAI.hs` that declares a module
`ReversiAI`.

Your module `ReversiAI` must import the `Reversi` module. This exports types
`Player` and `Move` (with their constructors) that have been defined as follows:

```haskell
data Player = Black | White
data Move = Pass | Move Int  -- 0 ... 63
```

Do not define these types yourself!

Your module `ReversiAI` must export the following types, values, and functions:

```haskell
type State  -- the internal state of your AI
author :: String
nickname :: String
initial :: Reversi.Player -> State
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
```

These are specified as follows:

- `State` encodes the internal state of your AI, i.e., any information that your
  AI wants to keep track of between different invocations of `think`. (This
  information will likely include a representation of the board and current
  player.) Your module may define `State` any way you see fit.

- `author` is your (first and last) name.

- `nickname` is an arbitrary string (of at most 20 ASCII characters) chosen by
  you to identify your AI. It will be used to report the
  [evaluation](#evaluation) results pseudonymously.

- `initial` takes an argument that indicates whether your AI will be playing
  Black or White, and returns the initial state of your AI.

- `think` takes three arguments: the current state of your AI, your opponent's
  last move, and the time remaining for your AI (in seconds) to play the rest of
  the game.

  The `move` argument will be `Pass` when (i) your AI is playing Black and
  making the first move of the game, or (ii) your opponent had to skip their
  last move.

  `think` returns a pair `(m,s)`, where `m` is the move that your AI wants to
  take, and `s` is your AI's internal state after taking this move. (`s` will be
  passed to the *next* invocation of your AI's think function, along with your
  opponent's response to `m`.) `m` must be `Pass` if (and only if) your AI
  cannot otherwise make a valid move.

Near the top of your file must be a comment containing a brief (about 100-500
words) description of your implementation. All functions must be specified
according to our [Coding Convention](https://uppsala.instructure.com/courses/71668/pages/coding-convention).

Your file must compile with GHC, and pass the [automated test](#testing) on
GitHub.

Your implementation must not perform any kind of input or output (e.g., via the
console, file system or network) or any other side effects. Your implementation
must not raise any exceptions.

Your implementation must not spawn additional threads or processes.

Your implementation may use any types and values provided by the Prelude, or by
other Haskell packages that are available in the test environment, with the
exception of [unsafe](https://wiki.haskell.org/Unsafe_functions) functions. Your
implementation must not require the installation of additional packages.

Testing
-------

For your convenience, we provide an automated testing framework. When you push
to your GitHub repository, your AI plays *one* game against a computer
player—called `RandomAI`—that makes pseudo-random moves. Whether your AI plays
as Black or White during this test is determined (pseudo-)randomly.

Note that the color of the fields (black and white) may be inverted if you are
reading the test output with a light-on-dark color scheme.

You are encouraged to develop your own test harness if you want to test your AI
more thoroughly.

Submission
----------

Please submit your file `ReversiAI.hs` simply by pushing to your GitHub
repository. (In case of technical problems—and only then!—email your solution to
<tjark.weber@it.uu.se>.) The submission deadline is **Friday, 2022-10-21** at
18:00. (Completions are accepted in accordance with the deadlines that have been
announced on Studium.)

Evaluation
----------

Your AI will compete against a very simple computer player (`RandomAI`) that
chooses its moves (pseudo-)randomly. Each AI will play 20 games: 10 as Black and
10 as White.

For each game, your AI will receive a score between -64 and 64. First, if the
game is not a draw, all empty fields will be marked in the winner's color. Your
AI's game score is then defined as

    (number of fields marked in your color) - (number of fields marked in your opponent's color)

Your AI will be given 5 minutes per game. The time it takes to call your AI's
`initial` function is counted against this limit. If your AI returns an invalid
move or runs out of time, it will receive a score of -64 for that game (and your
opponent will receive a score of 64). All games will be played on reasonably
recent hardware.

If your AI causes stability issues due to excessive memory requirements during a
game, we may treat this similar to a timeout. (We do not expect memory usage to
be an issue in practice, and will not actively monitor it unless problems
arise.)

Your AI's total score is defined as the sum of its game scores.

Grading
-------

Grading for this assignment will be according to the [grading rubric](https://uppsala.instructure.com/courses/71668/assignments/140579)
that is published on Studium:

If your implementation conforms to the requirements stated [above](#your-task),
passes the [automated test](#testing) <i>and</i> follows the [Coding Convention](https://uppsala.instructure.com/courses/71668/pages/coding-convention),
your assignment grade will be (at least) 3.

If your implementation additionally is functionally correct and returns no
invalid move during <i>any</i> of the evaluation games, your assignment grade
will be (at least) 4.

If your implementation additionally achieves a total evaluation score of 500 or
more, your assignment grade will be 5.

With this grading scheme, you can *decide for yourself* whether you merely want
to implement the rules of the game, or whether you want to implement a
moderately strong AI—and hence, which grade you want to aim for.

Tournament
----------

We will also conduct a tournament between all AIs that were submitted by the
deadline. Every student AI will play against every other student AI, once as
Black and once as White. The tournament rules (e.g., timing, scoring) are as
detailed for the [evaluation](#evaluation).

We will report the tournament results on Studium, using your AI's nickname to
identify it. These results may bestow bragging rights, but will not affect the
grading.

Hints
-----

**For grades 3 and 4:**

To attain a grade of 4, it is sufficient to return *any* valid move (or `Pass`
when no valid move exists). This merely requires you to implement the rules of
the game. It can be done without game tree search (and without an evaluation
function for game positions).

Your implementation of `think` may ignore the argument that indicates the
remaining time.

The board can be represented in many different ways. A 64-tuple of fields is one
(but probably not the most convenient) possible solution. Consider using lists
or
[sequences](https://hackage.haskell.org/package/containers-0.6.5.1/docs/Data-Sequence.html)
instead.

Also consider using a 10-by-10 board internally, whose border fields stay
empty. This can greatly simplify the implementation of the straight-line color
change rule.

For grade 3, it is sufficient to play a single game correctly. For grade 4, test
your code thoroughly before you submit.

**For grade 5:**

Two key components in a strong Reversi AI are (i) the search algorithm, and
(ii) the evaluation function.

There are many strong Reversi programs. Do not try to reinvent the
wheel. Instead, research existing techniques. A good starting point is
<https://en.wikipedia.org/wiki/Computer_Othello>. Remember to give credit when
you use someone else's ideas.

Test your code thoroughly before you submit. Buggy code may easily be worse than
no code.

If you are aiming for a very strong AI, make sure it doesn't run out of
time. The evaluation hardware may be slower (or faster) than the testing
hardware.

Do not go overboard. We understand that one could spend a lifetime perfecting a
Reversi AI, while you only have a few days.
