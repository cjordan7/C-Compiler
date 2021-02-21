//
//  Parser.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

// TODO: Refactor the """fatalError()""" so that
// TODO: the compiler can send better errors and explanations
// TODO: to the programmer
class Parser {
    private var tokens: [Token]
    private var pointer: Int
    
    private let plusMinus = [Token.PLUS, Token.MINUS]
    private let mulDiv = [Token.TIMES, Token.DIVIDE]
    
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

    private func checkNextToken() -> Token {
        return tokens[pointer+1]
    }


    private func match(token: Token) -> Token {
        let eaten = eatToken()

        if(eaten != token) {
            fatalError()
        }

        return eaten
    }

    private func matchOneOf(oneOf: [Token]) -> Token {
        let eaten = eatToken()

        for token in oneOf {
            if(eaten == token) {
                return eaten
            }
        }

        fatalError()
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

    // <Function> ::= "int" <id> "(" ")" <CompoundStatement>
    private func parseFunction(depth: Int) -> Function {
        // FIXME: TODO: Refactor everything below
        let returnType = match(token: .INT)

        let name = eatToken()
        // TODO refactorize
        if case .ID(_) = name {} else {
            fatalError()
        }

        matchDelimiter(token: .LEFT_PAREN)
        matchDelimiter(token: .RIGHT_PAREN)

        let compoundStatement = parseCompoundStatement(depth: depth + 1)

        return Function(returnType: returnType, name: name,
                        compoundStatement: compoundStatement,
                        depth: depth + 1)
    }

    // <CompoundStatement> ::= "{" {<Statement> | <Declaration>} "}"
    private func parseCompoundStatement(depth: Int) -> CompoundStatement {
        matchDelimiter(token: .OPEN_BRACE)

        var many: [Node] = []

        while(checkToken() != .CLOSING_BRACE) {
            if(checkToken() == .INT) {
                let node: Node = parseDeclaration(depth: depth + 1) as Node
                many.append(node)
            } else {
                let node: Node = parseStatement(depth: depth + 1) as Node
                many.append(node)
            }
        }

        matchDelimiter(token: .CLOSING_BRACE)

        return CompoundStatement(many: many, depth: depth + 1)
    }

    // <Declaration> ::= "int" <id> [ = <Expression> ] ";"
    private func parseDeclaration(depth: Int) -> Declaration {
        matchDelimiter(token: .INT)

        let id = eatToken()

        if case .ID(_) = id {} else {
            fatalError()
        }

        var expr: Expression? = nil
        print(id)
        print(checkToken())
        if(checkToken() == .EQUAL) {
            matchDelimiter(token: .EQUAL)
            expr = parseExpression(depth: depth + 1)
        }

        matchDelimiter(token: .SEMICOLON)

        return Declaration(id: id, expression: expr, depth: depth + 1)
    }

    // <Statement> ::= <ExpressionStatement>
    //              | <CompoundStatement>
    //              | <SelectionStatement>
    //              | <IterationStatemments>
    //              | <JumpStatement>
    //              | <LabeledStatement>
    private func parseStatement(depth: Int) -> Statement {
        let next = checkToken()


        // <Statement> ::= <ExpressionStatement>
        //              | <JumpStatement>
        if(next == .OPEN_BRACE) {
            let compound = parseCompoundStatement(depth: depth + 1)
            return Statement(compoundStatement: compound, depth: depth + 1)
        } else if([Token.IF, Token.SWITCH].contains(next)) {
            let selection = parseSelectionStatement(depth: depth + 1)
            return Statement(selectionStatement: selection, depth: depth + 1)
        } else if([Token.WHILE, Token.DO, Token.FOR].contains(next)) {
            let iteration = parseIterationStatement(depth: depth + 1)
            return Statement(iterationStatement: iteration, depth: depth + 1)
        } else if([Token.GOTO, Token.CONTINUE,
                   Token.BREAK, Token.RETURN].contains(next)) {
            let jump = parseJumpStatement(depth: depth + 1)
            return Statement(jumpStatement: jump, depth: depth + 1)
        } else {
            if case .ID(_) = checkToken(){
                if(checkNextToken() == .COLON) {
                    let labeled = parseLabeledStatement(depth: depth + 1)
                    return Statement(labeledStatement: labeled, depth: depth + 1)
                }
            }

            let expression = parseExpressionStatement(depth: depth + 1)
            return Statement(expressionStatement: expression, depth: depth + 1)
        }
    }

    // <LabeledStatement> ::= <id> : <Statement>
    //                       | case <ConditionalExpression> : <Statement>
    //                       | default : <Statement>
    private func parseLabeledStatement(depth: Int) -> LabeledStatement {
        let eaten = eatToken()

        if(eaten == .CASE) {
            let cond = parseConditionalExpression(depth: depth + 1)
            matchDelimiter(token: .COLON)
            let statement = parseStatement(depth: depth + 1)

            return LabeledStatement(conditionalExpression: cond,
                                    statement: statement, depth: depth + 1)
        } else if(eaten == .DEFAULT) {
            matchDelimiter(token: .COLON)
            let statement = parseStatement(depth: depth + 1)
            return LabeledStatement(statement: statement, depth: depth + 1)
        } else if case .ID(_) = eaten {
            matchDelimiter(token: .COLON)
            let statement = parseStatement(depth: depth + 1)
            return LabeledStatement(id: eaten,
                                    statement: statement,
                                    depth: depth + 1)
        } else {
            fatalError()
        }
    }

    // <ExpressionStatement> ::= [<Expression>] ";"
    private func parseExpressionStatement(depth: Int) -> ExpressionStatement {
        let next = checkToken()

        if(next == .SEMICOLON) {
            matchDelimiter(token: .SEMICOLON)
            return ExpressionStatement(depth: depth + 1)
        }

        let expression = parseExpression(depth: depth + 1)
        matchDelimiter(token: .SEMICOLON)

        return ExpressionStatement(expression: expression, depth: depth + 1)
    }

    // <JumpStatement> :: = "goto" <id> ";"
    //                   | "continue" ";"
    //                   | "break" ";"
    //                   | "return" [<Expression>] ";"
    private func parseJumpStatement(depth: Int) -> JumpStatement {
        let next = eatToken()

        if(next == .GOTO) {
            let id = eatToken()
            matchDelimiter(token: .SEMICOLON)
            return JumpStatement(id: id, depth: depth + 1)
        } else if(next == .CONTINUE) {
            matchDelimiter(token: .SEMICOLON)
            return JumpStatement(orExpr: .TWO, depth: depth + 1)
        } else if(next == .BREAK) {
            matchDelimiter(token: .SEMICOLON)
            return JumpStatement(orExpr: .THREE, depth: depth + 1)
        } else if(next == .RETURN) {
            var expression: Expression? = nil

            if(checkToken() != .SEMICOLON) {
                expression = parseExpression(depth: depth + 1)
            }
            matchDelimiter(token: .SEMICOLON)

            return JumpStatement(expression: expression, depth: depth + 1)
        } else {
            fatalError()
        }
    }

    // <SelectionStatement> ::= "if" "(" <Expression> ")" <Statement>
    //                        | "if" "(" <Expression> ")" <Statement> "else"
    //                                                    <Statement>
    //                        | "switch" "(" <Expression> ")" <Statement>
    private func parseSelectionStatement(depth: Int) -> SelectionStatement {
        let next = matchOneOf(oneOf: [.IF, .SWITCH])

        matchDelimiter(token: .LEFT_PAREN)
        let expression = parseExpression(depth: depth + 1)
        matchDelimiter(token: .RIGHT_PAREN)

        let statement = parseStatement(depth: depth + 1)

        if(next == .IF) {
            let elze = checkToken()

            if(elze == .ELSE) {
                matchDelimiter(token: .ELSE)
                let statement2 = parseStatement(depth: depth + 1)

                return SelectionStatement(expression: expression,
                                          insideStatement: statement,
                                          elseStatement: statement2,
                                          depth: depth + 1)
            }

            return SelectionStatement(expression: expression,
                                      insideStatement: statement,
                                      depth: depth + 1, isSwitch: false)
        } else if(next == .SWITCH) {
            return SelectionStatement(expression: expression,
                                      insideStatement: statement,
                                      depth: depth + 1, isSwitch: true)
        } else {
            fatalError()
        }
    }

    // <IterationStatement> ::= "while" "(" <Expression> ")" <Statement>
    //                        | "do" <Statement> "while" "(" <Expression> ) ";"
    //                        | "for" ( [<Expression>] ";" [<Expression>] ";"
    //                                  [<Expression>] ")" <Statement>
    private func parseIterationStatement(depth: Int) -> IterationStatement {
        let next = matchOneOf(oneOf: [.WHILE, .DO, .FOR])

        let next2 = checkToken()

        if(next2 == .LEFT_PAREN) {
            matchDelimiter(token: .LEFT_PAREN)
            if(next == .WHILE) {
//                "while" "(" <Expression> ")" <Statement>
                let expression = parseExpression(depth: depth + 1)
                matchDelimiter(token: .RIGHT_PAREN)
                let statement = parseStatement(depth: depth + 1)
                return IterationStatement(expression: expression,
                                          insideStatement: statement,
                                          isWhile: true, depth: depth + 1)
            } else if(next == .FOR) {
//                "for" ( [<Expression>] ";" [<Expression>] ";"
//                        [<Expression>] ")" <Statement>
                var expression: Expression? = nil
                var expression2: Expression? = nil
                var expression3: Expression? = nil

                if(checkToken() != .SEMICOLON) {
                    expression = parseExpression(depth: depth + 1)
                }
                matchDelimiter(token: .SEMICOLON)

                if(checkToken() != .SEMICOLON) {
                    expression2 = parseExpression(depth: depth + 1)
                }
                matchDelimiter(token: .SEMICOLON)

                if(checkToken() != .SEMICOLON) {
                    expression3 = parseExpression(depth: depth + 1)
                }

                matchDelimiter(token: .RIGHT_PAREN)
                let statement = parseStatement(depth: depth + 1)

                return IterationStatement(expression: expression,
                                          expression2: expression2,
                                          expression3: expression3,
                                          insideStatement: statement,
                                          depth: depth + 1)
            } else {
                // TODO: SANITY CHECK
                fatalError()
            }

        } else {
//            "do" <Statement> "while" "(" <Expression> ) ";"
            let statement = parseStatement(depth: depth + 1)
            matchDelimiter(token: .WHILE)
            matchDelimiter(token: .LEFT_PAREN)
            let expression = parseExpression(depth: depth + 1)
            matchDelimiter(token: .RIGHT_PAREN)
            matchDelimiter(token: .SEMICOLON)

            return IterationStatement(expression: expression,
                                      insideStatement: statement,
                                      isWhile: false, depth: depth + 1)
        }
    }

    // <Expression> ::= <id> "=" <Expression> | <ConditionalExpression>
    private func parseExpression(depth: Int) -> Expression {
        var id = checkToken()
        if case .ID(_) = id {
            id = eatToken()
            matchDelimiter(token: .EQUAL)
            let parsed = parseExpression(depth: depth + 1)

            return Expression(id: id, expr: parsed, depth: depth + 1)
        }

        let begin = parseConditionalExpression(depth: depth + 1)
        return Expression(begin: begin, depth: depth)
    }

    // <ConditionalExpression> ::= <LogicalOrExpression> [ "?" <Expression> ":" <ConditionalExpression> ]
    private func parseConditionalExpression(depth: Int) -> ConditionalExpression {
        let logicalOrExpression = parseLogicalOrExpression(depth: depth + 1)

        var expression: Expression? = nil
        var conditional: ConditionalExpression? = nil

        if(checkToken() == .QUESTION_MARK) {
            matchDelimiter(token: .QUESTION_MARK)
            expression = parseExpression(depth: depth + 1)
            conditional = parseConditionalExpression(depth: depth + 1)
        }

        return ConditionalExpression(logicalOrExpression: logicalOrExpression,
                                     expression: expression,
                                     conditionalExpression: conditional,
                                     depth: depth + 1)
    }

    // <LogicalOrExpression> ::= LogicalAndExpression {"||" LogicalAndExpression}
    private func parseLogicalOrExpression(depth: Int) -> LogicalOrExpression {
        let begin = parseLogicalAndExpression(depth: depth + 1)

        var many: [Many<LogicalAndExpression>] = []
        while checkToken() == .LOGICAL_OR {
            many.append(Many<LogicalAndExpression>(operation: eatToken(), next: parseLogicalAndExpression(depth: depth + 1)))
        }

        return LogicalOrExpression(begin: begin, many: many, depth: depth)
    }

    // <LogicalAndExpression> ::= InclusiveOrExpression {"&&" InclusiveOrExpression}
    private func parseLogicalAndExpression(depth: Int) -> LogicalAndExpression {
        let begin = parseInclusiveOrExpression(depth: depth + 1)

        var many: [Many<InclusiveOrExpression>] = []
        while checkToken() == .LOGICAL_AND {
            many.append(Many<InclusiveOrExpression>(operation: eatToken(), next: parseInclusiveOrExpression(depth: depth + 1)))
        }

        return LogicalAndExpression(begin: begin, many: many, depth: depth)
    }

    // <InclusiveOrExpression> ::= ExclusiveOrExpression {"|" ExclusiveOrExpression}
    private func parseInclusiveOrExpression(depth: Int) -> InclusiveOrExpression {
        let begin = parseExclusiveOrExpression(depth: depth + 1)

        var many: [Many<ExclusiveOrExpression>] = []
        while checkToken() == .BITWISE_OR {
            many.append(Many<ExclusiveOrExpression>(operation: eatToken(), next: parseExclusiveOrExpression(depth: depth + 1)))
        }

        return InclusiveOrExpression(begin: begin, many: many, depth: depth)
    }

    // <ExclusiveOrExpression> ::= AndExpression {"^" AndExpression}
    private func parseExclusiveOrExpression(depth: Int) -> ExclusiveOrExpression {
        let begin = parseAndExpression(depth: depth + 1)

        var many: [Many<AndExpression>] = []
        while checkToken() == .XOR {
            many.append(Many<AndExpression>(operation: eatToken(), next: parseAndExpression(depth: depth + 1)))
        }

        return ExclusiveOrExpression(begin: begin, many: many, depth: depth)
    }

    // <AndExpression> ::= EqualityExpression {"&" EqualityExpression}
    private func parseAndExpression(depth: Int) -> AndExpression {
        let begin = parseEqualityExpression(depth: depth + 1)

        var many: [Many<EqualityExpression>] = []
        while checkToken() == .BITWISE_AND {
            many.append(Many<EqualityExpression>(operation: eatToken(), next: parseEqualityExpression(depth: depth + 1)))
        }

        return AndExpression(begin: begin, many: many, depth: depth)
    }

    // <EqualityExpression> ::= <RelationalExpression> { ("==" | "!=") <RelationalExpression> }
    private func parseEqualityExpression(depth: Int) -> EqualityExpression {
        let begin = parseRelationalExpression(depth: depth + 1)

        var many: [Many<RelationalExpression>] = []
        while checkToken() == .RELATIONAL_EQUAL ||
            checkToken() == .RELATIONAL_NOT_EQUAL {
                many.append(Many<RelationalExpression>(operation: eatToken(), next: parseRelationalExpression(depth: depth + 1)))
        }

        return EqualityExpression(begin: begin, many: many, depth: depth + 1)
    }

    // <RelationalExpression> ::= ShiftExpression {("<" | ">" | "<=" | ">=") ShiftExpression}
    private func parseRelationalExpression(depth: Int) -> RelationalExpression {
        let begin = parseShiftExpression(depth: depth + 1)

        var many: [Many<ShiftExpression>] = []
        while checkToken() == .SMALLER || checkToken() == .BIGGER ||
            checkToken() == .SMALLER_EQUAL || checkToken() == .BIGGER_EQUAL {
                many.append(Many<ShiftExpression>(operation: eatToken(), next: parseShiftExpression(depth: depth + 1)))
        }

        return RelationalExpression(begin: begin, many: many, depth: depth + 1)
    }

    // <ShiftExpression> ::= AdditiveExpression {"<<" | ">>" AdditiveExpression}
    private func parseShiftExpression(depth: Int) -> ShiftExpression {
        let begin = parseAdditiveExpression(depth: depth + 1)

        var many: [Many<AdditiveExpression>] = []
        while checkToken() == .SHIFT_RIGHT || checkToken() == .SHIFT_LEFT {
            many.append(Many<AdditiveExpression>(operation: eatToken(),
                                   next: parseAdditiveExpression(depth: depth + 1)))
        }

        return ShiftExpression(begin: begin, many: many, depth: depth + 1)
    }

    // <AdditivieExpression> ::= <Term> { ("+" | "-") <Term> }
    private func parseAdditiveExpression(depth: Int) -> AdditiveExpression {
        let term = parseTerm(depth: depth + 1)

        var many: [Many<Term>] = []
        while checkToken() == .PLUS || checkToken() == .MINUS {
            many.append(Many<Term>(operation: eatToken(),
                                   next: parseTerm(depth: depth + 1)))
        }

        return AdditiveExpression(begin: term, many: many, depth: depth + 1)
    }

    // <Term> ::= <Factor> { ("*" | "/" | "%") <Factor> }
    private func parseTerm(depth: Int) -> Term {
        let begin = parseFactor(depth: depth + 1)

        var many: [Many<Factor>] = []
        while checkToken() == .TIMES ||
            checkToken() == .DIVIDE ||
            checkToken() == .MODULO {
            many.append(Many<Factor>(operation: eatToken(),
                                     next: parseFactor(depth: depth + 1)))
        }

        return Term(begin: begin, many: many, depth: depth + 1)
    }

    // <Factor> ::= "(" <Expression> ")" | <unaryOp> <Factor> | <int> | <id>
    private func parseFactor(depth: Int) -> Factor {
        let eaten = eatToken()
        if(isCorrectToken(token: eaten, isTok: .LEFT_PAREN)) {

            let expression = parseExpression(depth: depth + 1)
            let eaten2 = eatToken()

            if(isCorrectToken(token: eaten2, isTok: .RIGHT_PAREN)) {
                return Factor(expression: expression, depth: depth)
            } else {
                fatalError()
            }
        } else if(checkUnaryOperation(token: eaten)) {
            return Factor(unaryOp: eaten,
                          factor: parseFactor(depth: depth + 1),
                          depth: depth)
        }

        if case .ID(_) = eaten {
            return Factor(id: eaten, depth: depth)
        }
        print(eaten)
        if case .CONSTANT_INT(_) = eaten {
            return Factor(id: eaten, depth: depth)
        } else {
            fatalError()
        }
    }

    private func isCorrectToken(token: Token, isTok: Token) -> Bool {
        return token == isTok
    }

    private func checkUnaryOperation(token: Token) -> Bool {
        switch token {
        case .NOT, .TILDE, .MINUS, .INCREMENT, .DECREMENT:
            return true
        default:
            return false
        }
    }
}
