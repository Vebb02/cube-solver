# Cube Solver
## Before running
- Create a `scramble.in` file in the project directory containing a scramble using the template below. The orientation of the cube should be green in the front and white on the top. NOTE: This will not work with cubes that do not follow the standard color scheme.

## Scramble input example file
### Notes 
- The parser is case sensitive and will fail if the letters are not all upper case.
- The file should always end with a single newline character.
- The spacing for the white and yellow faces should be present.
### Face color to letter mapping
| Color   | Letter |
|---------|--------|
| White   | W      |
| Orange  | O      |
| Green   | G      |
| Red     | R      |
| Blue    | B      |
| Yellow  | Y      |
### Solved cube
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
### From `scramble.in`
```
cabal run
```
### With GAN bluetooth cube
```
cabal run -- cube-solver --bluetooth
```
### PDF API
NOTE: Using the server flag overides the bluetooth flag.
```
cabal run -- cube-solver --server
```
## API
To call the API, send a GET request to `https://voiestad.no/api/cubesolver` with a parameter named cube which should contain a scrambled cube in the same format as described [here](#scramble-input-example-file).<br>
You could also use the [web UI](https://voiestad.no/cube-solver) to interact with the API.
### Example API Call
```
https://voiestad.no/api/cubesolver?cube=%20YBR%0A%20RWR%0A%20OYB%0AGYGWBOWGWGWR%0AOOGOGRWRGYBW%0AGYOYWWBOBYBO%0A%20BGR%0A%20OYB%0A%20YRR%0A
```
## Credit
* ["Rubik's Cube: Why are some cases impossible to solve?" - by Dylan Wang AKA "JPerm" on YouTube](https://youtu.be/o-RxLzRe2YE?si=PNoy7rsajMeGU8o2)
* [PLL: E perm and Z perm from SpeedCubeDB](https://speedcubedb.com/a/3x3/PLL)
* [Names of OLL cases from jperm.net](https://jperm.net/algs/2look/oll)
* [GAN Bluetooth with Node.js](https://github.com/afedotov/gan-node-sample)
