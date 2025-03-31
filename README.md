# Cube Solver
## Before running
- Create a `scramble.in` file in the project directory containing a scramble using the template below. The orientation of the cube should be green in the front and white on the top. NOTE: This will not work with cubes that do not follow the standard color scheme.

## Scramble input example file
### Notes 
- The parser is case sensitive and will fail if the letters are not all upper case.
- The file should always end with a single newline character.
- The spacing for the white and yellow faces should be present.
### Face Color to Letter Mapping
| Color   | Letter |
|---------|--------|
| White   | W      |
| Orange  | O      |
| Green   | G      |
| Red     | R      |
| Blue    | B      |
| Yellow  | Y      |
### Solved Cube
```
   WWW
   WWW
   WWW
OOOGGGRRRBBB
OOOGGGRRRBBB
OOOGGGRRRBBB
   YYY
   YYY
   YYY

```
### Scrambled cube
```
   YBR
   RWR
   OYB
GYGWBOWGWGWR
OOGOGRWRGYBW
GYOYWWBOBYBO
   BGR
   OYB
   YRR

```

## How to run
```
cabal run
```

## Credit
* ["Rubik's Cube: Why are some cases impossible to solve?" - by Dylan Wang AKA "JPerm" on YouTube](https://youtu.be/o-RxLzRe2YE?si=PNoy7rsajMeGU8o2)
* [PLL: E perm and Z perm from SpeedCubeDB](https://speedcubedb.com/a/3x3/PLL)
* [Names of OLL cases from jperm.net](https://jperm.net/algs/2look/oll)