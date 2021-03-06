# rslox

Bytecode virtual machine for the Lox programming language written in Rust.

## Development
While developing, if you would like auto-running check and tests you can install cargo-watch and then run `./scripts/tdd`.  
You can then also have an auto-restarting rslox repl by opening a second terminal and running `./scripts/repl`.  

## Acknowledgements

**Bob Nystrom** ([munificent](https://github.com/munificent@munificentbob)) - author of the fantastic [https://craftinginterpreters.com](https://craftinginterpreters.com)  
**John Chabot** ([johnnyboyC](https://github.com/jonnyboyC)) - implementor of [SpaceLox](https://github.com/jonnyboyC/spacelox) which has been immensely helpful. I was excited when some of my code was very similar, and grateful when I got stuck and was able to borrow.  
**Lenard Pratt** ([Lapz](https://github.com/Lapz)) - implementor of [tox](https://github.com/Lapz/tox), a statically typed version of lox. I'm excited to see what his implementation can teach me after I've completed following the book.  
[**string-interner**](https://github.com/Robbepop/string-interner) - implementation for string interning in Rust.  
