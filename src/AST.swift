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

enum Choice {
    case ONE
    case TWO
    case THREE
    case FOUR
    case FIVE
    case SIX
}

class Many<T> {
    var operation: Token
    var next: T

    init(operation: Token, next: T) {
        self.operation = operation
        self.next = next
    }
}

protocol ManyExpression {
    associatedtype T

    var begin: T {get set}
    var many: [Many<T>] {get set}

    init(begin: T, many: [Many<T>], depth: Int)
}

protocol BNFOrExpression { // | "" expression | "" | ...
    var orExpr: Choice {get set}
}

protocol Node: CustomStringConvertible {
    var depth: Int {get set}
}

extension Node {
    func tabsDepth() -> String {
        return "\t".repeatString(n: depth)
    }
}

// TODO: Set var and func to either private, public,...

// <TranslationUnit> ::= <Function>
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

//<Function> ::= "int" <id> "(" ")" <CompoundStatement>
class Function: Node {
    var depth: Int
    var description: String {
        """
        FUNCTION \(returnType) \(name):
        \(tabsDepth())Params: ()
        \(tabsDepth())Body:
        \\(statement) \\ TODO
        """
    }
    
    var returnType: Token
    var name: Token
    var compoundStatement: CompoundStatement

    init(returnType: Token,
         name: Token,
         compoundStatement: CompoundStatement,
         depth: Int) {
        
        self.returnType = returnType
        self.name = name
        self.compoundStatement = compoundStatement
        self.depth = depth
    }
}

// <CompoundStatement> ::= "{" {<Statement> | <Declaration>} "}"
class CompoundStatement: Node {
    var depth: Int

//    class StatementOrDeclaration {
//        enum WHO {
//            case Statement
//            case Declaration
//        }
//
//        var node: Node
//        var who: WHO
//
//        init(node: Node, who: WHO) {
//            self.node = node
//            self.who = who
//        }
//    }

    var description: String {
        "TODO"
    }

    var many: [Node]

    init(many: [Node], depth: Int) {
        self.many = many
        self.depth = depth
    }
}

// <Declaration> ::= "int" <id> [ = <Expression> ] ";"
class Declaration: Node {
    var depth: Int

    var description: String {
        ""
    }

    var id: Token
    var expression: Expression?

    init(id: Token, expression: Expression?, depth: Int) {
        self.id = id
        self.expression = expression
        self.depth = depth
    }

}

// <Statement> ::= <ExpressionStatement>
//               | <CompoundStatement>
//               | <SelectionStatement>
//               | <IterationStatemments>
//               | <JumpStatement>
//               | LabeledStatement
class Statement: Node, BNFOrExpression {
    var orExpr: Choice

    var depth: Int
    var description: String {
        "TODO"
    }

    var expressionStatement: ExpressionStatement? = nil
    var compoundStatement: CompoundStatement? = nil
    var selectionStatement: SelectionStatement? = nil
    var iterationStatement: IterationStatement? = nil
    var jumpStatement: JumpStatement? = nil
    var labeledStatement: LabeledStatement? = nil

    init(expressionStatement: ExpressionStatement,
         depth: Int) {
        self.expressionStatement = expressionStatement
        self.depth = depth
        self.orExpr = .ONE
    }

    init(compoundStatement: CompoundStatement,
         depth: Int) {
        self.compoundStatement = compoundStatement
        self.depth = depth
        self.orExpr = .TWO
    }

    init(selectionStatement: SelectionStatement,
         depth: Int) {
        self.selectionStatement = selectionStatement
        self.orExpr = .THREE
        self.depth = depth
    }

    init(iterationStatement: IterationStatement,
         depth: Int) {
        self.iterationStatement = iterationStatement
        self.orExpr = .FOUR
        self.depth = depth
    }

    init(jumpStatement: JumpStatement,
         depth: Int) {
        self.jumpStatement = jumpStatement
        self.orExpr = .FIVE
        self.depth = depth
    }

    init(labeledStatement: LabeledStatement,
         depth: Int) {
        self.labeledStatement = labeledStatement
        self.orExpr = .SIX
        self.depth = depth
    }
}

// <LabeledStatement> ::= <id> : <Statement>
//                       | case <ConditionalExpression> : <Statement>
//                       | default : <Statement>
class LabeledStatement: Node, BNFOrExpression {
    var depth: Int

    var orExpr: Choice

    var description: String {
        ""
    }

    var id: Token?
    var statement: Statement?
    var conditionalExpression: ConditionalExpression?

    init(id: Token, statement: Statement, depth: Int) {
        self.id = id
        self.statement = statement
        self.depth = depth
        self.orExpr = .ONE
    }

    init(conditionalExpression: ConditionalExpression, statement: Statement, depth: Int) {
        self.conditionalExpression = conditionalExpression
        self.statement = statement
        self.depth = depth
        self.orExpr = .TWO
    }

    init(statement: Statement, depth: Int) {
        self.statement = statement
        self.depth = depth
        self.orExpr = .THREE
    }

}

// <ExpressionStatement> ::= [<Expression>] ";"
class ExpressionStatement: Node {
    var depth: Int

    var description: String {
        ""
    }

    var expression: Expression? = nil

    init(depth: Int) {
        self.depth = depth
    }

    init(expression: Expression?, depth: Int) {
        self.expression = expression
        self.depth = depth
    }
}

// <JumpStatement> :: = "goto" <id> ";"
//                   | "continue" ";"
//                   | "break" ";"
//                   | "return" [<Expression>] ";"
class JumpStatement: Node, BNFOrExpression {
    var orExpr: Choice

    var depth: Int

    var description: String {
        ""
    }

    var id: Token? = nil
    var expression: Expression? = nil

    init(id: Token, depth: Int) {
        self.id = id
        self.orExpr = .ONE
        self.depth = depth
    }

    init(expression: Expression?, depth: Int) {
        self.orExpr = .FOUR
        self.expression = expression
        self.depth = depth
    }

    init(orExpr: Choice, depth: Int) {
        self.orExpr = orExpr
        self.depth = depth
    }
}

// <SelectionStatement> ::= "if" "(" <Expression> ")" <Statement>
//                        | "if" "(" <Expression> ")" <Statement> "else"
//                                                    <Statement>
//                        | "switch" "(" <Expression> ")" <Statement>
class SelectionStatement: Node, BNFOrExpression {
    var orExpr: Choice

    var depth: Int

    var description: String {
        ""
    }

    var expression: Expression? = nil
    var insideStatement: Statement? = nil
    var elseStatement: Statement? = nil

    init(expression: Expression,
         insideStatement: Statement,
         depth: Int, isSwitch: Bool) {
        self.expression = expression
        self.insideStatement = insideStatement
        self.depth = depth

        if(isSwitch) {
            self.orExpr = .THREE
        } else {
            self.orExpr = .ONE
        }
    }

    init(expression: Expression,
         insideStatement: Statement,
         elseStatement: Statement,
         depth: Int) {
        self.expression = expression
        self.insideStatement = insideStatement
        self.elseStatement = elseStatement
        self.depth = depth
        self.orExpr = .TWO
    }
}

// <IterationStatement> ::= "while" "(" <Expression> ")" <Statement>
//                        | "do" <Statement> "while" "(" <Expression> ) ";"
//                        | "for" ( [<Expression>] ";" [<Expression>] ";"
//                                  [<Expression>] ")" <Statement>
class IterationStatement: Node, BNFOrExpression {
    var orExpr: Choice

    var depth: Int

    var description: String {
        ""
    }

    var expression: Expression? = nil
    var expression2: Expression? = nil
    var expression3: Expression? = nil
    var insideStatement: Statement? = nil


    init(expression: Expression,
         insideStatement: Statement,
         isWhile: Bool, // Test if the expression
                        // is a while statement else it is a do while
         depth: Int) {
        self.depth = depth
        self.expression = expression
        self.insideStatement = insideStatement

        if(isWhile) {
            self.orExpr = .ONE
        } else {
            self.orExpr = .TWO
        }
    }

    init(expression: Expression?,
         expression2: Expression?,
         expression3: Expression?,
         insideStatement: Statement,
         depth: Int) {
        self.depth = depth
        self.expression = expression
        self.expression2 = expression2
        self.expression3 = expression3
        self.insideStatement = insideStatement
        self.orExpr = .THREE
    }
}

// <Expression> ::= <id> "=" <Expression> | <ConditionalExpression>
class Expression: Node, BNFOrExpression {
    var orExpr: Choice

    var depth: Int
    var description: String {
        """
        TODO
        """
    }
    
    var id: Token? = nil
    var expr: Expression? = nil

    var begin: ConditionalExpression? = nil

    init(begin: ConditionalExpression,
        depth: Int) {
        self.begin = begin
        self.depth = depth
        self.orExpr = .ONE
    }

    init(id: Token, expr: Expression, depth: Int) {
        self.id = id
        self.expr = expr
        self.depth = depth
        self.orExpr = .TWO
    }
}


// <ConditionalExpression> ::= <LogicalOrExpression> [ "?" <Expression> ":" <ConditionalExpression> ]
class ConditionalExpression: Node {
    var depth: Int

    var description: String {
        "TODO"
    }

    var logicalOrExpression: LogicalOrExpression
    var expression: Expression? = nil
    var conditionalExpression: ConditionalExpression? = nil

    init(logicalOrExpression: LogicalOrExpression,
         expression: Expression?,
         conditionalExpression: ConditionalExpression?,
         depth: Int) {
        self.conditionalExpression = conditionalExpression
        self.logicalOrExpression = logicalOrExpression
        self.expression = expression
        self.depth = depth
    }
}

// <LogicalOrExpression> ::= LogicalAndExpression {"||" LogicalAndExpression}
class LogicalOrExpression: Node, ManyExpression {
    typealias T = LogicalAndExpression
    
    var depth: Int
    var description: String {
        """
        TODO
        """
    }
    
    var begin: LogicalAndExpression
    var many: [Many<LogicalAndExpression>]

    required init(begin: LogicalAndExpression,
                  many: [Many<LogicalAndExpression>],
                  depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }
}

// <LogicalAndExpression ::= InclusiveOrExpression {"&&" InclusiveOrExpression}
class LogicalAndExpression: Node, ManyExpression {
    typealias T = InclusiveOrExpression

    var depth: Int
    var description: String {
        ""
    }

    var begin: InclusiveOrExpression
    var many: [Many<InclusiveOrExpression>]

    required init(begin: InclusiveOrExpression,
                  many: [Many<InclusiveOrExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }
}

// <InclusiveOrExpression> ::= ExclusiveOrExpression {"|" ExclusiveOrExpression}
class InclusiveOrExpression: Node, ManyExpression {
    typealias T = ExclusiveOrExpression

    var depth: Int
    var description: String {
        ""
    }

    var begin: ExclusiveOrExpression
    var many: [Many<ExclusiveOrExpression>]

    required init(begin: ExclusiveOrExpression, many: [Many<ExclusiveOrExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }

}

// <ExclusiveOrExpression> ::= AndExpression {"^" AndExpression}
class ExclusiveOrExpression: Node, ManyExpression {
    typealias T = AndExpression

    var depth: Int
    var description: String {
        ""
    }

    var begin: AndExpression
    var many: [Many<AndExpression>]

    required init(begin: AndExpression, many: [Many<AndExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }
}

//<AndExpression> ::= EqualityExpression {"&" EqualityExpression}
class AndExpression: Node, ManyExpression {
    typealias T = EqualityExpression

    var depth: Int
    var description: String {
        ""
    }

    var begin: EqualityExpression
    var many: [Many<EqualityExpression>]

    required init(begin: EqualityExpression, many: [Many<EqualityExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }

}

//<EqualityExpression> ::= RelationalExpression {('==' | '!=') RelationalExpression}
class EqualityExpression: Node, ManyExpression {
    typealias T = RelationalExpression

    var depth: Int

    var description: String {
        ""
    }

    var begin: RelationalExpression
    var many: [Many<RelationalExpression>]

    required init(begin: RelationalExpression,
         many: [Many<RelationalExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }
}



//<RelationalExpression> ::= ShiftExpression {("<" | ">" | "<=" | ">=") ShiftExpression}
class RelationalExpression: Node, ManyExpression {
    typealias T = ShiftExpression

    var depth: Int

    var description: String {
        ""
    }

    var begin: ShiftExpression
    var many: [Many<ShiftExpression>]

    required init(begin: ShiftExpression,
         many: [Many<ShiftExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }

}


//<ShiftExpression> ::= AdditiveExpression {"<<" | ">>" AdditiveExpression}
class ShiftExpression: Node, ManyExpression {
    typealias T = AdditiveExpression

    var depth: Int
    var description: String {
        ""
    }

    var begin: AdditiveExpression
    var many: [Many<AdditiveExpression>]

    required init(begin: AdditiveExpression, many: [Many<AdditiveExpression>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }

}

// <AdditiveExpression> ::= <Term> { ("+" | "-") <Term> }
class AdditiveExpression: Node, ManyExpression {
    typealias T = Term
    
    var depth: Int
    
    var begin: Term
    var many: [Many<Term>]
    
    var description: String {
        ""
    }
    
    required init(begin: Term, many: [Many<Term>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth
    }
}

// <Term> ::= <Factor> { ("*" | "/" | "%") <Factor> }
class Term: Node, ManyExpression {
    typealias T = Factor
    
    var depth: Int
    var description: String {
        """
        
        """
    }
    
    var begin: Factor
    var many: [Many<Factor>]

    required init(begin: Factor, many: [Many<Factor>], depth: Int) {
        self.begin = begin
        self.many = many
        self.depth = depth;
    }
}

// <Factor> ::= "(" <Expression> ")" | <unaryOp> <Factor> | <int> | <id>
class Factor: Node, BNFOrExpression {
    var orExpr: Choice
    
    var depth: Int
    var description: String {
        """
        
        """
    }
    
    var expression: Expression?
    
    var unaryOp: Token?
    var factor: Factor?
    
    var id: Token?

    init(expression: Expression, depth: Int) {
        self.expression = expression
        self.unaryOp = nil
        self.factor = nil
        self.id = nil
        self.depth = depth;
        
        self.orExpr = .ONE
    }
    
    init(unaryOp: Token, factor: Factor, depth: Int) {
        self.expression = nil
        self.unaryOp = unaryOp
        self.factor = factor
        self.id = nil
        self.depth = depth;
        
        self.orExpr = .TWO
    }
    
    init(id: Token, depth: Int) {
        self.expression = nil
        self.unaryOp = nil
        self.factor = nil
        self.id = id
        self.depth = depth;
        
        self.orExpr = .THREE
    }
}
