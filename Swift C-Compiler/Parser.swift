//
//  Parser.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

// FIXME: TODO: Refactor the """fatalError()""" so that
// FIXME: TODO: the compiler can send better errors and explanations
// FIXME: TODO: to the programmer
class Parser {
    private var tokens: [Token]
    private var pointer: Int
    
    init(tokens: [Token]) {
        // FIXME: TODO: Check size tokens
        self.tokens = tokens
        pointer = 0
    }
    
    // =================================================================
    // Utility functions
    
    private func eatToken() -> Token {
        let eaten = tokens[pointer]
        pointer += 1
        return eaten
        
    }
    
    private func checkToken() -> Token {
        return tokens[pointer]
    }
    
    // FIXME: TODO: Change name
    private func match(token: Token) -> Token {
        let eaten = eatToken()
        if(eaten != token) {
            fatalError()
        }

        return eaten
    }

    private func matchDelimiter(token: Token) {
        let _ = match(token: token)
    }
    
    // FIXME: TODO: Change name
    private func eatAndCheckValue(token: Token) {
//        let eaten = eatToken()
//        if case token(_) = eaten {} else {
//            fatalError()
//        }
    }
    
    // =================================================================
    // Functions for Recursive Descent Parsing
    
    func parseAST() -> TranslationUnit {
        return parseProgram()
    }
    
    private func parseProgram() -> TranslationUnit {
        let depth = 1
        let function = parseFunction(depth: depth + 1)
        return TranslationUnit(function: function, depth: depth)
    }
    
    private func parseFunction(depth: Int) -> Function {
        // FIXME: TODO: Refactor everything below
        let returnType = match(token: .INT)
        
        let name = eatToken()
        if case .ID(_) = name {} else {
            fatalError()
        }
        
        matchDelimiter(token: .LEFT_PAREN)
        matchDelimiter(token: .RIGHT_PAREN)
        matchDelimiter(token: .OPEN_BRACE)
        
        let statement = parseStatement(depth: depth + 1)
        
        matchDelimiter(token: .CLOSING_BRACE)

        return Function(returnType: returnType, name: name,
                        //leftParen: leftParen,
                        //rigthParen: rightParen,
                        //leftBrace: leftBrace,
                        statement: statement,
                        //rightBrace: rightBrace,
                        depth: depth)
    }
    
    private func parseStatement(depth: Int) -> Statement {
        let ret = match(token: .RETURN)
        let expression = parseExpression(depth: depth + 1)
        
        matchDelimiter(token: .SEMICOLON)

        return Statement(retu: ret,
                         expression: expression,
                         depth: depth)
    }
    
    private func parseExpression(depth: Int) -> Expression {
        let newToken = eatToken()
        if case .CONSTANT_INT(_) = newToken {
            return Expression(unaryOperation: newToken,
                              expression: nil,
                              depth: depth)
        } else if(checkUnaryOperation(token: newToken)) {
            return Expression(unaryOperation: newToken,
                              expression: parseExpression(depth: depth+1),
                              depth: depth)
        }

        fatalError()
    }

    private func parseTerm(depth: Int) {
    
    }

    private func parseFactor(depth: Int) {
        
    }

    private func checkUnaryOperation(token: Token) -> Bool {
        switch token {
        case .NOT, .TILDE, .MINUS:
            return true
        default:
        return false
        }
    }
}
