# Performance-aware programming in Rust

## Intro 
This is the repo I use to save my dabblings with [C.M.'s course](https://www.computerenhance.com/).
It is my first attempt at learning in public.
I will document my learning here to keep as a reference and look back at the path I have treaded.

## Dependencies
For testing purposes only, you will need :
  - the [Netwide Assembler : nasm](https://www.nasm.us/)

## TODO
I am currently writing a dissassembler for the Intel's 8086 architecture.
The following tasks need to be done :
  - implementing the ADD, SUB and CMP and JNZ mnemonics

## What I learnt
  - Ontop of cache friendliness, instruction level paralellism is a parameter for speed

## Reflections
I wanted to have the minimal amount of dependencies to external crates. 
I wanted a static way of doing the parsing (more because i'm not used to it so it was a good opportunity to try).
Sp I chose to implement my own state machine for the x86 instruction parser. The mental model is easy, you have transitional states that store data needed to emit the next instruction. The bytes come one by one.
There is quite a lot of code duplication for now (eg. when emmitting a new instruction, I have to manually reset the state to Start and emit), I am not sure which direction I could take to clean it up


