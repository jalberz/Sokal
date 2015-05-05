# Sokal: Postmoder Text Generator
Lab 1 for CMSC 22311

A fill-text generator for Postmodern Text from the magazine Postmodern Culture.

### Installation & Running the Program
* First insure that you have both Text.HTML.TagSoup and Network.HTTP installed
* Clone this git repository: https://github.com/jalberz/Sokal.git
* Use `cabal build sokal`
* This will provide an executable `sokal` that can be run by 
  `./sokal <desired output length>`

### Notes
* I've noticed that reading all those URLs can take some amount of time... on the order of 20-30 seconds
sometimes
* Strange issue with creating proper Primitive Model
