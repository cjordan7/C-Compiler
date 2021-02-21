//
//  TestCases.swift
//  Swift C-Compiler
//
//  Created by Cosme Jordan on 01.10.20.
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

class TestCases {
    init() {
    }

    func run() {
        let packageRootPath = URL(fileURLWithPath: #file).pathComponents
            .prefix(while: { $0 != "TestCases.swift" }).joined(separator: "/").dropFirst()
        let path = packageRootPath + "/C Programs/Succeed/Operations/"
        let enumerator = FileManager.default.enumerator(atPath: String(path))

        while let filename = enumerator?.nextObject() as? String {
            runThroughFiles(filename: path+filename)
        }
    }

    func generateAssembly() {
    
    }
    
    func generateTokens() {
        let lexerTokens = lexer.getTokens()
        print(lexerTokens)
    }
    
    func runThroughFiles(filename: String) {
        var lexer = Lexer()
        print(filename)
//        print(filename == "/Users/cosmejordan/Documents/Programming/Swift/Projects/Swift C-Compiler/src/TestCases/C Programs/Succeed/Operations/test3.c")
        // FIXME: TODO: Let the user pass the file through the command line
        lexer.read(filename: filename)
        lexer.run()
//
        var lexerTokens = lexer.getTokens()
        print(lexerTokens)
//
//        var parser = Parser(tokens: lexerTokens)
//        var parseAST = parser.parseAST()
//        print(parseAST)

        //var codeGen = CodeGeneration(parseAST: parseAST)
        //print(codeGen.generateCode())
    }
//
//// FIXME: TODO: Let the user pass the file through the command line
//lexer.run()
//
//var lexerTokens = lexer.getTokens()
//print(lexerTokens)
//
//var parser = Parser(tokens: lexerTokens)
//var parseAST = parser.parseAST()
//print(parseAST)
//
//var codeGen = CodeGeneration(parseAST: parseAST)
//print(codeGen.generateCode())
}
