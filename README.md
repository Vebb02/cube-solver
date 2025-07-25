# Cube Solver
The cube solver is a Haskell program that generates a PDF solving manual from a Rubik's cube scramble. There are currently three modes:
- Standard mode: Solves the scramble from an input file and creates a PDF solution.
- Server mode: Starts an API that takes a scramble and gives a PDF solution as Base64 response.
- Bluetooth mode: Connects to a GAN bluetooth cube and gives live update of the PDF solution.

## Compatibility
- Linux: Fully compatible.
- Windows: Fully compatible. (Bluetooth mode: see [how to run](#on-non-linux-systems) section)
- Mac: Standard and Server mode works. Bluetooth mode not yet supported.

## Before running
- Create a `scramble.in` file in the project directory containing a scramble using the template below. The orientation of the cube should be green in the front and white on the top. NOTE: This will not work with cubes that do not follow the standard color scheme.

## Scramble input example file
### Notes 
- The parser is not case sensitive and will accept both lower and upper case.
- The file should always end with either the last color or white space.
- The spacing for the white and yellow faces can be present and is recommended for manual input, but is not necessary.
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
#### On non-Linux systems
```
cabal run -- cube-solver --bluetooth --non-linux
```
### PDF API
NOTE: Using the server flag overrides the bluetooth flag.
```
cabal run -- cube-solver --server
```
## API
To interact with the API, send a GET request to `https://voiestad.no/api/cubesolver` with a parameter named `cube` containing a scrambled cube in the same format as described [here](#scramble-input-example-file).<br>
You can also use the [web UI](https://voiestad.no/cube-solver) to interact with the API.

### API endpoints
- **Validate**: Returns `true` if the provided scramble is valid, `false` otherwise.
- **Solve**: Returns the solution for the provided scramble as PDF.

### Example API call - Validate
```
https://voiestad.no/api/cubesolver/validate?cube=YBRRWROYBGYGWBOWGWGWROOGOGRWRGYBWGYOYWWBOBYBOBGROYBYRR
```

### Example API call - Solve
```
https://voiestad.no/api/cubesolver/solve?cube=YBRRWROYBGYGWBOWGWGWROOGOGRWRGYBWGYOYWWBOBYBOBGROYBYRR
```

## Credit
- ["Rubik's Cube: Why are some cases impossible to solve?" - by Dylan Wang AKA "JPerm" on YouTube](https://youtu.be/o-RxLzRe2YE?si=PNoy7rsajMeGU8o2)
- [PLL: E perm and Z perm from SpeedCubeDB](https://speedcubedb.com/a/3x3/PLL)
- [Names of OLL cases from jperm.net](https://jperm.net/algs/2look/oll)
- [OLL algorithms from jperm.net](https://jperm.net/algs/oll)
- [GAN Bluetooth with Node.js by Andy Fedotov](https://github.com/afedotov/gan-node-sample)
