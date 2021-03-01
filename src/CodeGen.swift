//
//  CodeGen.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

// TODO: Comment assembly code
// TODO: Refactor, change names


extension String {
    var tabify: String {
        let lines = self.components(separatedBy: "\n")
        var newString = ""
        for s in lines {
            if(!s.isEmpty) {
                newString += "\t\(s)\n"
            }
        }

        return newString
    }
}

class CodeGeneration {
    private let PUSH_RAX = """
            push %rax
            """.tabify
    private let POP_RCX = """
            pop %rcx
            """.tabify
    private let COMPARE = """
            cmp %rax, %rcx
            mov $0, %rax
            """.tabify

    private var labelsMap = [String: Int]()
    private var parseAST: TranslationUnit

    init(parseAST: TranslationUnit) {
        self.parseAST = parseAST
        labelsMap["JUMP_CLAUSE_OR"] = 0
        labelsMap["JUMP_CLAUSE_AND"] = 0
        labelsMap["CONDITIONAL_LABEL"] = 0
        labelsMap["CASE_LABEL"] = 0
        labelsMap["LOOP_LABEL"] = 0
    }

    private func generateNewLabel(labelName: String) -> (String, String) {
        labelsMap[labelName]! += 1
        return ("__\(labelName)\(labelsMap[labelName]!)",
            "__\(labelName)__end\(labelsMap[labelName]!)__")
    }

    public func generateCode() -> String {
        return generateTranslationUnit(parseAST, SymbolTable())
    }

    // <TranslationUnit> ::= <Function>
    private func generateTranslationUnit(_ tUnit: TranslationUnit,
                                         _ sT: SymbolTable) -> String {
        return generateFunction(tUnit.function, sT)
    }

    // <Function> ::= "int" <id> "(" ")" <CompoundStatement>
    private func generateFunction(_ function: Function,
                                  _ sT: SymbolTable) -> String {
        let nameFunc = function.name.value

        // TODO: Maybe not unique with several files
        sT.currentFunction = nameFunc
        // Add functions to Symbol Table to it
        sT.functions[nameFunc] = "int"

        // TODO: Change this and put it in functions
        sT.jumpEndFunction = "__FUNCTION__END__\(nameFunc)__"

        // TODO Generate variables
        return """

        .global _\(nameFunc)
        _\(nameFunc):
        \(generateCompoundStatement(function.compoundStatement, sT))
        \(sT.jumpEndFunction)
        """
    }

    // <CompoundStatement> ::= "{" {<Statement> | <Declaration>} "}"
    private func generateCompoundStatement(_ cS: CompoundStatement,
                                           _ sT: SymbolTable) -> String {
        var currentVars = [String: String]()
        var string = ""
        for node in cS.many {
            string += "\n"
            switch node {
            case let decl as Declaration:
                let (code, variable, returnType) = generateDeclaration(decl)

                if let _ = currentVars[variable] {
                    fatalError()
                } else {
                    currentVars[variable] = returnType
                }

                string += code
            case let stat as Statement:
                string += generateStatement(stat, sT.copyVariables(with: currentVars))
            default:
                // TODO: Should not happen
                fatalError()
            }
        }

        return string.tabify
    }

    // <Declaration> ::= "int" <id> [ = <Expression> ] ";"
    private func generateDeclaration(_ declaration: Declaration) -> (String, String, String) {
//        declaration.
//        return generateExpression(declaration.expression)
        // TODO...
    }

    // <Statement> ::= <ExpressionStatement>
    //              | <CompoundStatement>
    //              | <SelectionStatement>
    //              | <IterationStatements>
    //              | <JumpStatement>
    //              | <LabeledStatement>
    private func generateStatement(_ statement: Statement,
                                   _ sT: SymbolTable) -> String {
        var string = ""
        if let expreStat = statement.expressionStatement {
            string = generateExpressionStatement(expreStat, sT)
        } else if let compState = statement.compoundStatement {
            string = generateCompoundStatement(compState, sT)
        } else if let selStat = statement.selectionStatement {
            string = generateSelectionStatement(selStat, sT)
        } else if let itStat = statement.iterationStatement {
            string = generateIterationStatement(itStat, sT)
        } else if let jumStat = statement.jumpStatement {
            string = generateJumpStatement(jumStat, sT)
        } else if let labelStat = statement.labeledStatement {
            string = generateLabeledStatement(labelStat, sT)
        }

        return string.tabify
//        return "\(generateExpression(statement.expression))" +
//            "ret".tabify
    }

    // <LabeledStatement> ::= <id> : <Statement>
    //                       | case <ConditionalExpression> : <Statement>
    //                       | default : <Statement>
    private func generateLabeledStatement(_ lS: LabeledStatement,
                                          _ sT: SymbolTable) -> String {
        var string = ""
        if let id = lS.id {
            // id.value
            // TODO: This is not exactly correct, should be unique for each functions
            sT.jumps[id.value+sT.currentFunction] = id.value+sT.currentFunction

            //TODO: Refactor
            string += "\n"
            if let sat = lS.statement {
                string += generateStatement(sat, sT)
            }
        } else if let condExpre = lS.conditionalExpression {
            // Switch was never defined, so no case can be defined
            if(sT.endJumpLabel == nil) {
                fatalError()
            }

            string = generateConditionalExpression(condExpre, sT)
            string += POP_RCX
            string += "cmpl %rcx, %rax"

            string += "push %rcx"
            let (var1, _) = generateNewLabel(labelName: "CASE_LABEL")
            string += "jne \(var1)"

            //TODO: Refactor
            string += "\n"
            if let sat = lS.statement {
                string += generateStatement(sat, sT)
                string += "jmp \(sT.endJumpLabel!)"
            }

            string += "\(var1)"
        } else if let stat = lS.statement { // Default...
            string = generateStatement(stat, sT)
        }

        return string.tabify
    }

    // <ExpressionStatement> ::= [<Expression>] ";"
    private func generateExpressionStatement(_ exStat: ExpressionStatement,
                                            _ sT: SymbolTable) -> String {
        var string = ""
        if let expression = exStat.expression {
            string = generateExpression(expression, sT)
        }

        return string.tabify
    }

    // <JumpStatement> :: = "goto" <id> ";"
    //                   | "continue" ";"
    //                   | "break" ";"
    //                   | "return" [<Expression>] ";"
    private func generateJumpStatement(_ jumStat: JumpStatement,
                                       _ sT: SymbolTable) -> String {

        var string = ""

        switch jumStat.orExpr {
        case .ONE: // goto
            string = "jmp \(jumStat.id!)"
            break
        case .TWO: // continue
            string = "jmp \(sT.continueLabel)"
            break
        case .THREE: // break
            if let endJump = sT.endJumpLabel {
                string = "jmp \(endJump)"
            } else {
                // Break should be inside, loop or switch
                fatalError()
            }
            break
        case .FOUR: // Return expression
            // TODO: ...
            break
        default:
            fatalError()
        }

        return string.tabify
    }

    // <SelectionStatement> ::= "if" "(" <Expression> ")" <Statement>
    //                        | "if" "(" <Expression> ")" <Statement> "else"
    //                                                    <Statement>
    //                        | "switch" "(" <Expression> ")" <Statement>
    private func generateSelectionStatement(_ selStat: SelectionStatement,
                                            _ sT: SymbolTable) -> String {
        var string = generateExpression(selStat.expression, sT)
        if(selStat.orExpr == .ONE || selStat.orExpr == .TWO) {
            string += "\n"
            string += "cmpl $0, %rax"
            let (var1, var2) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            string += "je \(var1)"
            string += generateStatement(selStat.insideStatement!, sT)

            string += "jmp \(var2)"

            string += "\(var1)"

            if(selStat.orExpr == .TWO) {
                string += generateStatement(selStat.elseStatement!, sT)
            }

            string += "\(var2)"
        } else {
            string += "\n"
            string += PUSH_RAX
            let (var1, _) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            sT.endJumpLabel = var1
            string += generateStatement(selStat.insideStatement!, sT)
            string += "\(var1)"
        }

        return string.tabify
    }

    // <IterationStatement> ::= "while" "(" <Expression> ")" <Statement>
    //                        | "do" <Statement> "while" "(" <Expression> ) ";"
    //                        | "for" ( [<Expression>] ";" [<Expression>] ";"
    //                                  [<Expression>] ")" <Statement>
    //                        | "for" "(" [<Declaration>] [Expression] ";"
    //                                     [<Expression>] ")" <Statement>
    private func generateIterationStatement(_ itStat: IterationStatement,
                                            _ sT: SymbolTable) -> String {
        var string = ""
        let (var1, var2) = generateNewLabel(labelName: "LOOP_LABEL")
        switch itStat.orExpr {
        case .ONE:
            string += "\(var2)"
            if let exprOne = itStat.expression {
                string += generateExpression(exprOne, sT)
            }

            string += "cmpl $0, %rax"
            string += "je \(var1)"

            string += generateStatement(itStat.insideStatement!, sT)

            string += "jmp \(var2)"
            string += "\(var1)"
            break
        case .TWO:

            string += "\(var2)"
            string += generateStatement(itStat.insideStatement!, sT)

            if let exprOne = itStat.expression {
                string += generateExpression(exprOne, sT)
            }

            string += "cmpl $0, %rax"
            string += "je \(var1)"

            string += "jmp \(var2)"
            string += "\(var1)"
            break
        case .THREE:
            if let exprOne = itStat.expression {
                string += generateExpression(exprOne, sT)
            }

            if let exprTwo = itStat.expression2 {
                string += generateExpression(exprTwo, sT)
            }

            if let exprThree = itStat.expression3 {
                string += generateExpression(exprThree, sT)
            }

            string += generateStatement(itStat.insideStatement!, sT)

            break
        case .FOUR:
            // TODO: Refactor with above
            // TODO: I believe it is possible to declare a lot of variables, there
//            var currentVariables
            let sT2 = sT.copyVariables(with: [:])
            if let declar = itStat.declaration {
                let (var1, var2, var3) = generateDeclaration(declar)
                sT2.variables[var2] = var3
                string += var1
            }

            if let exprOne = itStat.expression {
                string += generateExpression(exprOne, sT2)
            }

            if let exprTwo = itStat.expression2 {
                string += generateExpression(exprTwo, sT2)
            }

            break

        default:
            fatalError()
        }
    }



    // <Expression> ::= <id> "=" <Expression> | <ConditionalExpression>
    private func generateExpression(_ expression: Expression,
                                    _ sT: SymbolTable) -> String {
        return generateLogicalOrExpression(expression.begin)
    }

    // <ConditionalExpression> ::= <LogicalOrExpression> [ "?" <Expression> ":" <ConditionalExpression> ]
    private func generateConditionalExpression(_ cE: ConditionalExpression,
                                               _ sT: SymbolTable) -> String {
        var string = generateLogicalOrExpression(cE.logicalOrExpression)
        string += "\n"

        if let expression = cE.expression {
            string += "cmpl $0, %rax"
            let (var1, var2) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            string += "je \(var1)"

            string += generateExpression(expression, sT)

            string += "jmp \(var2)"

            string += "\(var1)"
            string += generateConditionalExpression(cE.conditionalExpression!, sT)

            string += "\(var2)"
        }

        return string.tabify
    }

    // <LogicalOrExpression> ::= LogicalAndExpression {"||" LogicalAndExpression}
    func generateLogicalOrExpression(_ lExpr: LogicalOrExpression) -> String {
        var logExpr = generateLogicalAndExpression(lExpr.begin)

        for next in lExpr.many {
            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_OR")

            logExpr += """
                cmpl $0, %rax
                je \(labelJump)
                movl $1, %rax
                jmp \(labelEnd)
                \(labelJump):
                """.tabify

            logExpr += generateLogicalAndExpression(next.next)

            logExpr += """
                cmp $0, %rax
                mov $0, %rax
                setne %al
                \(labelEnd):
                """.tabify

            // Generate unique labels
        }

        return logExpr
    }

    // <LogicalAndExpression> ::= InclusiveOrExpression {"&&" InclusiveOrExpression}
    func generateLogicalAndExpression(_ logAndExpr: LogicalAndExpression) -> String {
        var lAndExpr = generateInclusiveOrExpression(logAndExpr.begin)

        for next in logAndExpr.many {
            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_AND")
            lAndExpr += """
                cmp $0, %rax
                jne \(labelJump)
                jmp \(labelEnd)
                \(labelJump):
                """.tabify

            lAndExpr += generateInclusiveOrExpression(next.next)

            lAndExpr += """
                cmp $0, %rax
                mov $0, %rax
                setne %al
                \(labelEnd):
                """.tabify
        }

        return lAndExpr
    }

    // <InclusiveOrExpression> ::= ExclusiveOrExpression {"|" ExclusiveOrExpression}
    func generateInclusiveOrExpression(_ inclExpr: InclusiveOrExpression) -> String {
        var gen = generateExclusiveOrExpression(inclExpr.begin)

        for next in inclExpr.many {
            gen += PUSH_RAX

            gen += generateExclusiveOrExpression(next.next)
            gen += POP_RCX

            gen += """
                or %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <ExclusiveOrExpression> ::= AndExpression {"^" AndExpression}
    func generateExclusiveOrExpression(_ xorExpr: ExclusiveOrExpression) -> String {
        var gen = generateAndExpression(xorExpr.begin)

        for next in xorExpr.many {
            gen += PUSH_RAX

            gen += generateAndExpression(next.next)
            gen += POP_RCX

            gen += """
                xor %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <AndExpression> ::= EqualityExpression {"&" EqualityExpression}
    func generateAndExpression(_ andExpr: AndExpression) -> String {
        var gen = generateEqualityExpression(andExpr.begin)

        for next in andExpr.many {
            gen += PUSH_RAX

            gen += generateEqualityExpression(next.next)
            gen += POP_RCX

            gen += """
                and %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <EqualityExpression> ::= <RelationalExpression> { ("!=" | "==") <RelationalExpression> }
    func generateEqualityExpression(_ eExpr: EqualityExpression) -> String {
        var relExpr = generateRelationalExpression(eExpr.begin)

        for next in eExpr.many {
            // TODO
            relExpr += PUSH_RAX
            relExpr += generateRelationalExpression(next.next)
            relExpr += POP_RCX

            relExpr += COMPARE

            if(next.operation == .RELATIONAL_EQUAL) {
                relExpr += """
                sete   %al
                """.tabify
            } else if(next.operation == .RELATIONAL_NOT_EQUAL) {
                relExpr += """
                setne   %al
                """.tabify
            } else {
                fatalError()
            }
        }

        return relExpr
    }

    // <RelationalExpression> ::= ShiftExpression {("<" | ">" | "<=" | ">=") ShiftExpression}
    func generateRelationalExpression(_ rExpr: RelationalExpression) -> String {
        var gen = generateShiftExpression(rExpr.begin)

        for next in rExpr.many {
            gen += PUSH_RAX
            gen += generateShiftExpression(next.next)
            gen += POP_RCX

            gen += COMPARE

            if(next.operation == .SMALLER) {
                gen += """
                setl   %al
                """.tabify
            } else if(next.operation == .SMALLER_EQUAL) {
                gen += """
                setle   %al
                """.tabify
            } else if(next.operation == .BIGGER) {
                gen += """
                setg   %al
                """.tabify
            } else if(next.operation == .BIGGER_EQUAL) {
                gen += """
                setge   %al
                """.tabify
            } else {
                fatalError()
            }

        }

        return gen
    }

    // <ShiftExpression> ::= AdditiveExpression {"<<" | ">>" AdditiveExpression}
    private func generateShiftExpression(_ shiftExpr: ShiftExpression) -> String {
        var gen = generateAdditiveExpression(shiftExpr.begin)

        for next in shiftExpr.many {
            // TODO: Unsigned type: do logical shift
            // TODO: Signed type: do arithmetic shift
            gen += PUSH_RAX
            gen += generateAdditiveExpression(next.next)
            gen += POP_RCX

            if(next.operation == .SHIFT_RIGHT) {
                gen += """
                shr %rcx, %rax
                """.tabify
            } else if(next.operation == .SHIFT_LEFT) {
                gen += """
                shl %rcx, %rax
                """.tabify
            } else {
                fatalError()
            }
        }

        return gen
    }

    // <AdditiveExpression> ::= <Term> { ("+" | "-") <Term> }
    private func generateAdditiveExpression(_ addExpr: AdditiveExpression) -> String {
        var gen = generateTerm(addExpr.begin)

        for next in addExpr.many {
            gen += PUSH_RAX

            gen += generateTerm(next.next)
            gen += POP_RCX

            if(next.operation == .PLUS) {
                // Add expression
                gen +=
                    """
                    add %rcx, %rax
                    """.tabify
            } else if(next.operation == .MINUS) {
                // Calculate sub expression
                gen +=
                    """
                    sub %rax, %rcx
                    mov %rcx, %rax
                    """.tabify
            } else {
                fatalError()
            }
        }

        return gen
    }

    // Term ::= <Factor> { ("*" | "/" | "%") <Factor> }
    private func generateTerm(_ term: Term) -> String {
        var gen = generateFactor(term.begin)

        for next in term.many {
            gen += PUSH_RAX
            gen += generateFactor(next.next)
            gen += POP_RCX

            if(next.operation == .TIMES) {
                gen +=
                    """
                    imul %rcx, %rax
                    """.tabify
            } else if(next.operation == .DIVIDE) {
                // TODO: REwrite division, because it is wrong
                gen +=
                    """
                    idiv %rcx, %rax
                    """.tabify
            } else if(next.operation == .MODULO) {
                // TODO: Check that out
                gen +=
                    """
                    idiv %rcx, %rax
                    mov %rdx, %rax
                    """.tabify
            } else {
                fatalError()
            }

        }

        return gen
    }

    // <Factor> ::= "(" <Expression> ")" | <unaryOp> <Factor> | <int> | <id>
    private func generateFactor(_ factor: Factor) -> String {
        switch factor.orExpr {
        case .ONE:
            return generateExpression(factor.expression!)
        case .TWO:
            // TODO: THis feels odd...
            let gFactor = generateFactor(factor.factor!)
            return gFactor + generateUnaryOperation(token: factor.unaryOp!)
        case .THREE:
            return generateUnaryOperation(token: factor.intT!)
        }
    }

    // <unaryOp> ::= "!" | "~" | "-"
    private func generateUnaryOperation(token: Token) ->String {
        switch token {
        case .NOT:
            return """
            cmp   $0, %rax
            mov   $0, %rax
            sete   %al
            """.tabify
        case .TILDE:
            return """
                mov    $\(token.value), %rax
                neg     %rax
                """.tabify
        case .MINUS:
            return """
                mov $\(token.value), %rax
                neg %rax
                """.tabify
        case .CONSTANT_INT(let value):
            return """
                mov $\(value), %rax
                """.tabify
        default:
            fatalError()
        }
    }
}
