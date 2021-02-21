//
//  main.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

// FIXME: TODO: Consider using "MARK" For separating comments
// FIXME: TODO: Implement better errors handling


// FIXME: TODO: Pretty printing of assembly

// FIXME: TODO: Write tests


// FIXME: TODO: Refactor this so that the pipeline is done in
// two lines of code such as:
// var compiler = Compiler()
// compiler.run()

var test = TestCases()
test.run()

var lexer = Lexer()

// FIXME: TODO: Let the user pass the file through the command line
lexer.read(filename: "/Users/cosmejordan/Documents/Programming/Swift/Projects/Swift C-Compiler/src/TestCases/C Programs/Succeed/Operations/test3.c")
lexer.run()

var lexerTokens = lexer.getTokens()
print(lexerTokens)

var parser = Parser(tokens: lexerTokens)
var parseAST = parser.parseAST()
print(parseAST)

//var codeGen = CodeGeneration(parseAST: parseAST)
//print(codeGen.generateCode())
