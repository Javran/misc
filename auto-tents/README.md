# Automation of "tents & trees" Game

Note: whenever I say "private directory" or `private/`,
it means `auto-tents/private` that you will need to manually create if not existing.

Here are few steps to have a working program:

- Collect sample for different sizes.

  Each different puzzle size needs one.
  Collect and store them under private directory
  with naming pattern `sample-{x}-{x}.py` where `{x}` is
  the size of the puzzle (a number from `6` to `22`).

- Generate `private/preset.json`.

  This file allows us to quickly find cell positions
  for all different sizes of the puzzle board.

- Collect digit samples.

  We'll need to visit through those samples again
  in order to collect digit samples for recognition to work.

- Have a working [tents-demo](https://github.com/Javran/puzzle-solving-collection/tree/master/tents-solver) binary.

- Set environment variable `TENTS_DEMO_BIN` to the location of the binary. `cd py/` then `./solver.py` when the phone is at a game screen.
