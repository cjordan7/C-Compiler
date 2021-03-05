//
//  main.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

// TODO: Consider using "MARK" For separating comments
// TODO: Implement better errors handling


// TODO: Pretty printing of assembly

// TODO: Write tests


// TODO: Refactor this so that the pipeline is done in
// two lines of code such as:
// var compiler = Compiler()
// compiler.run()

// TODO: According to 5.1.2.2.3 Program termination of C11-Standard
//       available here http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1570.pdf
// If the return type of the main function is a type compatible with int, a return from the initial call to the main function is equivalent to calling the exit function with the value returned by the main function as its argument; reaching the } that terminates the main function returns a value of 0.
// TODO: Take into account
// int main(void)
// int main(int argc, char **argv)


var test = TestCases()
test.run()

var lexer = Lexer()

// TODO: Let the user pass the file through the command line
lexer.read(filename: "/Users/cosmejordan/Documents/Programming/Swift/Projects/Swift C-Compiler/src/TestCases/C Programs/Succeed/Operations/test3.c")
lexer.run()

var lexerTokens = lexer.getTokens()
print(lexerTokens)

var parser = Parser(tokens: lexerTokens)
var parseAST = parser.parseAST()
print(parseAST)

// var codeGen = CodeGeneration(parseAST: parseAST)
// print(codeGen.generateCode())
