# Acids and Bases Game

An educational drag-and-drop game written in Racket, where players sort everyday substances into **acid** and **base** buckets according to their pH values.

Built as the course project for **CMPE 100**.

## Gameplay

- Twelve substances move around the screen, including lemon, soda, stomach acid, bleach, baking soda and hand soap
- Drag each one into the correct bucket:
  - **Acid bucket:** pH < 7
  - **Base bucket:** pH ≥ 7
- A correct drop gives **+50 points**, plays a sound and respawns the item at a random position
- A wrong drop gives 0 points and plays an error sound
- You have **60 seconds**. Your final score is saved to the leaderboard.

| Substance | pH | | Substance | pH |
|---|---:|---|---|---:|
| Battery Liquid | 1 | | Baking Soda | 8 |
| Stomach Acid | 2 | | Hand Soap | 9 |
| Coke | 2.5 | | Detergent | 10 |
| Lemon | 3 | | Bleach | 12 |
| Soda | 4 | | Oven Cleaner | 13 |
| Milk | 6 | | Drain Cleaner | 14 |

## Features

- Scenes for the menu, game, game over and leaderboard
- Moving items that bounce off the screen edges
- Persistent leaderboard stored in `leaderboard.txt`
- Sound effects for correct and wrong drops
- Unit tests with `check-expect`

## Requirements

- [Racket](https://racket-lang.org/) (DrRacket recommended)
- Libraries used: `racket/gui`, `2htdp/image`, `2htdp/universe`, `2htdp/batch-io`, `test-engine/racket-tests` (all included with Racket)

## Running

Open `acids-and-bases-game.rkt` in DrRacket and press **Run**, or from the project folder:

```bash
racket acids-and-bases-game.rkt
```

> Run the game from the project folder: the images, sounds and `leaderboard.txt` are loaded with relative paths.

The unit tests run after the game window is closed.

## Project structure

```
.
├── acids-and-bases-game.rkt   # Game source + tests
├── *.png                      # Item, bucket and background images
├── T.mp3                      # "Correct" sound
└── F.mp3                      # "Wrong" sound
```

## Author

Yağız Efe Çiçekçisoy

## License

[MIT](LICENSE)
