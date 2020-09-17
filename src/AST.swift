//
//  AST.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

extension Optional where Wrapped == Expression {
    var value: String {
        guard let unwrapped = self else {
            return ""
        }

        return unwrapped.description
    }
}

extension String {
    func repeatString(n: Int) -> String {
        var returnString = ""
        
        for _ in 0..<n {
            returnString += self
        }
        
        return returnString
    }
}

protocol Node: CustomStringConvertible {
    var depth: Int {get set}
}

extension Node {
    func tabsDepth() -> String {
        return "\t".repeatString(n: depth)
    }
}

// FIXME: TODO: Set var and func to either private, public,...
class TranslationUnit: Node {
    var depth: Int
    
    var description: String {
        """
        PROGRAM:
        \(tabsDepth())\(function)
        """
    }
    
    var function: Function
    
    init(function: Function, depth: Int) {
        self.function = function
        self.depth = depth
    }
}

// <Function> ::= "int" <id> "(" ")" "{" <Statement> "}"
class Function: Node {
    var depth: Int
    var description: String {
        """
        FUNCTION \(returnType) \(name):
        \(tabsDepth())Params: ()
        \(tabsDepth())Body:
        \(statement)
        """
    }
    
    var returnType: Token
    var name: Token
    
    var statement: Statement
    
    init(returnType: Token, name: Token,
         statement: Statement,
         depth: Int) {
        
        self.returnType = returnType
        self.name = name
        self.statement = statement
        self.depth = depth
    }
}

// CustomStringConvertible
class Statement: Node {
    var depth: Int
    var description: String {
        """
        \(tabsDepth())\(retu) \(expression)
        """
    }
    
    //"return" <exp> ";"
    var retu: Token
    var expression: Expression

    init(retu: Token,
         expression: Expression,
         depth: Int) {

        self.retu = retu
        self.expression = expression
        self.depth = depth
    }
}

class Expression: Node {
    var depth: Int
    var description: String {
        """
        \(unaryOperation)\(expression.value)
        """
    }
    
    // TODO: Change name of unaryOperation
    var unaryOperation: Token
    var expression: Expression?
    
    init(unaryOperation: Token, expression: Expression?, depth: Int) {
        self.unaryOperation = unaryOperation
        self.expression = expression
        self.depth = depth
    }
}
