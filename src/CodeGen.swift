//
//  CodeGen.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

//
//  CodeGen.swift
//  Swift C-Compiler
//
//  Created by Cosme Jordan on 10.09.20.
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

class CodeGeneration {
    private var parseAST: TranslationUnit
    
    init(parseAST: TranslationUnit) {
        self.parseAST = parseAST
    }
    
    func generateCode() -> String {
        return generateTranslationUnit(parseAST)
    }
    
    func generateTranslationUnit(_ translationUnit: TranslationUnit) -> String {
        return generateFunction(translationUnit.function)
    }
    
    func generateFunction(_ function: Function) -> String {
        let nameFunc = function.name.value
        return """
        .globl _\(nameFunc)
        _\(nameFunc):
        \(generateStatement(function.statement))
        """
    }
    
    func generateStatement(_ statement: Statement) -> String {
        return """
        \(generateExpression(statement.expression))
        ret
        """
    }
    
    func generateExpression(_ expression: Expression) -> String {
        return generateUnaryOperation(token: expression.unaryOperation)
    }
    
    func generateUnaryOperation(token: Token) ->String {
        switch token {
        case .NOT:
            return """
            cmpl   $0, %eax
            movl   $0, %eax
            sete   %al
            """
        case .TILDE:
            return """
            movl    $\(token.value), %eax
            neg     %eax
            """
        case .MINUS:
            return """
            movl $\(token.value), %eax
            neg %eax
            """
        case .CONSTANT_INT(let value):
            return """
            mov $\(value) %eax
            """
        default:
        fatalError()
        }
    }
}
