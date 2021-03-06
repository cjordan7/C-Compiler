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

    // TODO: Refactor
    private func generateNewLabel(labelName: String) -> (String, String) {
        labelsMap[labelName]! += 1
        return ("__\(labelName)__\(labelsMap[labelName]!)__:",
            "__\(labelName)__SECOND__\(labelsMap[labelName]!)__:")
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
        // Add functions to Symbol Table
        sT.functions[nameFunc] = "int"

        // TODO: Change this and put it in functions
        sT.jumpEndFunction = "__FUNCTION__END__\(nameFunc)__"

        // TODO: Generate variables
        // TODO: global should only be used
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
        var currentVars = [String: VariablesRepresentation]()
        var code = ""
        for node in cS.many {
            code += "\n"
            switch node {
            case let decl as Declaration:
                let (varCode, variable, varRepr, rbp) = generateDeclaration(decl, sT)

                if let _ = currentVars[variable] {
                    fatalError("Variable \(variable) has already been declared")
                } else {
                    currentVars[variable] = varRepr
                }

                sT.rbpValue = rbp

                code += varCode
            case let stat as Statement:
                code += generateStatement(stat, sT.copyVariables(with: currentVars))
            default:
                // TODO: Should not happen
                fatalError("This should not happen")
            }
        }

        code += "add $\(currentVars.count*8), %rsp"

        return code.tabify
    }

    // <Declaration> ::= "int" <id> [ = <Expression> ] ";"
    private func generateDeclaration(_ declaration: Declaration,
                                     _ sT: SymbolTable) -> (String, String, VariablesRepresentation, Int) {
        var code = ""
        if let expr = declaration.expression {
            code += generateExpression(expr, sT)
            code += PUSH_RAX
        }

        let varRepr = VariablesRepresentation()

        // TODO "int", other types...
        var rbpValue = sT.rbpValue
        rbpValue -= 8
        varRepr.offset = 0 - rbpValue

        // TODO: Assign a type to declaration
        varRepr.type = .INT

        return (code, declaration.id.value, varRepr, rbpValue)
    }

    // <Statement> ::= <ExpressionStatement>
    //              | <CompoundStatement>
    //              | <SelectionStatement>
    //              | <IterationStatements>
    //              | <JumpStatement>
    //              | <LabeledStatement>
    private func generateStatement(_ statement: Statement,
                                   _ sT: SymbolTable) -> String {
        var code = ""
        if let expreStat = statement.expressionStatement {
            code = generateExpressionStatement(expreStat, sT)
        } else if let compState = statement.compoundStatement {
            code = generateCompoundStatement(compState, sT)
        } else if let selStat = statement.selectionStatement {
            code = generateSelectionStatement(selStat, sT)
        } else if let itStat = statement.iterationStatement {
            code = generateIterationStatement(itStat, sT)
        } else if let jumStat = statement.jumpStatement {
            code = generateJumpStatement(jumStat, sT)
        } else if let labelStat = statement.labeledStatement {
            code = generateLabeledStatement(labelStat, sT)
        }

        return code.tabify
    }

    // <LabeledStatement> ::= <id> : <Statement>
    //                       | case <ConditionalExpression> : <Statement>
    //                       | default : <Statement>
    private func generateLabeledStatement(_ lS: LabeledStatement,
                                          _ sT: SymbolTable) -> String {
        var code = ""
        if let id = lS.id {
            // id.value
            // TODO: This is not exactly correct, should be unique for each functions
            sT.jumps[id.value+sT.currentFunction] = id.value+sT.currentFunction

            //TODO: Refactor
            code += "\n"
            if let sat = lS.statement {
                code += generateStatement(sat, sT)
            }
        } else if let condExpre = lS.conditionalExpression {
            // Switch was never defined, so no case can be defined
            if(sT.endJumpLabel == nil) {
                fatalError("Found a \"case\" without a switch")
            }

            code = generateConditionalExpression(condExpre, sT)
            code += POP_RCX
            code += "cmpl %rcx, %rax"

            code += "push %rcx"
            let (var1, _) = generateNewLabel(labelName: "CASE_LABEL")
            code += "jne \(var1)"

            //TODO: Refactor
            code += "\n"
            if let sat = lS.statement {
                code += generateStatement(sat, sT)
                code += "jmp \(sT.endJumpLabel!)"
            }

            code += "\(var1)"
        } else if let stat = lS.statement { // Default...
            code = generateStatement(stat, sT)
        }

        return code.tabify
    }

    // <ExpressionStatement> ::= [<Expression>] ";"
    private func generateExpressionStatement(_ exStat: ExpressionStatement,
                                            _ sT: SymbolTable) -> String {
        var code = ""
        if let expression = exStat.expression {
            code = generateExpression(expression, sT)
        }

        return code.tabify
    }

    // <JumpStatement> :: = "goto" <id> ";"
    //                   | "continue" ";"
    //                   | "break" ";"
    //                   | "return" [<Expression>] ";"
    private func generateJumpStatement(_ jumStat: JumpStatement,
                                       _ sT: SymbolTable) -> String {

        var code = ""

        switch jumStat.orExpr {
        case .ONE: // goto
            code = "jmp \(jumStat.id!)"
            break
        case .TWO: // continue
            code = "jmp \(sT.continueLabel)"
            break
        case .THREE: // break
            if let endJump = sT.endJumpLabel {
                code = "jmp \(endJump)"
            } else {
                fatalError("break should be inside a loop or a switch")
            }
            break
        case .FOUR: // Return expression

            if let expres = jumStat.expression {
                code += generateExpression(expres, sT)
            }

            code += "ret"
            break
        default:
            fatalError("This should not happen")
        }

        return code.tabify
    }

    // <SelectionStatement> ::= "if" "(" <Expression> ")" <Statement>
    //                        | "if" "(" <Expression> ")" <Statement> "else"
    //                                                    <Statement>
    //                        | "switch" "(" <Expression> ")" <Statement>
    private func generateSelectionStatement(_ selStat: SelectionStatement,
                                            _ sT: SymbolTable) -> String {
        var code = generateExpression(selStat.expression, sT)
        if(selStat.orExpr == .ONE || selStat.orExpr == .TWO) {
            code += "\n"
            code += "cmpl $0, %rax"
            let (var1, var2) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            code += "je \(var1)"
            code += generateStatement(selStat.insideStatement!, sT)

            code += "jmp \(var2)"

            code += "\(var1)"

            if(selStat.orExpr == .TWO) {
                code += generateStatement(selStat.elseStatement!, sT)
            }

            code += "\(var2)"
        } else {
            code += "\n"
            code += PUSH_RAX
            let (var1, _) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            sT.endJumpLabel = var1
            code += generateStatement(selStat.insideStatement!, sT)
            code += "\(var1)"
        }

        return code.tabify
    }

    // <IterationStatement> ::= "while" "(" <Expression> ")" <Statement>
    //                        | "do" <Statement> "while" "(" <Expression> ) ";"
    //                        | "for" ( [<Expression>] ";" [<Expression>] ";"
    //                                  [<Expression>] ")" <Statement>
    //                        | "for" "(" [<Declaration>] [Expression] ";"
    //                                     [<Expression>] ")" <Statement>
    private func generateIterationStatement(_ itStat: IterationStatement,
                                            _ sT: SymbolTable) -> String {
        var code = ""
        let (var1, var2) = generateNewLabel(labelName: "LOOP_LABEL")
        switch itStat.orExpr {
        case .ONE:
            code += "\(var2)"
            if let exprOne = itStat.expression {
                code += generateExpression(exprOne, sT)
            } else {
                fatalError("Empty while")
            }

            code += "cmpl $0, %rax"
            code += "je \(var1)"

            code += generateStatement(itStat.insideStatement!, sT)

            code += "jmp \(var2)"
            code += "\(var1)"
            break
        case .TWO:

            code += "\(var2)"
            code += generateStatement(itStat.insideStatement!, sT)

            if let exprOne = itStat.expression {
                code += generateExpression(exprOne, sT)
            } else {
                fatalError("Empty while.")
            }

            code += "cmpl $0, %rax"
            code += "je \(var1)"

            code += "jmp \(var2)"
            code += "\(var1)"
            break
        case .THREE:
            if let exprOne = itStat.expression {
                code += generateExpression(exprOne, sT)
            }

            code += "\(var2)"

            if let condition = itStat.expression2 {
                code += generateExpression(condition, sT)
            } else {
                // Making sure it loop forever
                code += "mov $1, %rax"
            }

            code += "cmpl $0, %rax"
            code += "je \(var1)"

            if let exprThree = itStat.expression3 {
                code += generateExpression(exprThree, sT)
            }

            code += generateStatement(itStat.insideStatement!, sT)

            code += "jmp \(var2)"
            code += "\(var1)"
            break
        case .FOUR:
            // TODO: Refactor with above
            // TODO: I believe it is possible to declare a lot of variables, there
            let sT2 = sT.copyVariables(with: [:])
            if let declar = itStat.declaration {
                let (codeVar, variable, varRepr, rbp) = generateDeclaration(declar, sT)
                sT.rbpValue = rbp
                sT2.variables[variable] = varRepr
                code += codeVar
            }

            code += "\(var2)"
            if let exprOne = itStat.expression {
                code += generateExpression(exprOne, sT2)
            } else {
                // Making sure it loop forever
                code += "mov $1, %rax"
            }

            code += "cmpl $0, %rax"
            code += "je \(var1)"

            if let exprTwo = itStat.expression2 {
                code += generateExpression(exprTwo, sT2)
            }

            code += "jmp \(var2)"
            code += "\(var1)"

            // Delete the initialized variable
            // TODO: Maybe more than one...
            code += POP_RCX
            break
        default:
            fatalError("This should not happen")
        }
        return code.tabify
    }



    // <Expression> ::= <id> "=" <Expression> | <ConditionalExpression>
    private func generateExpression(_ expression: Expression,
                                    _ sT: SymbolTable) -> String {
        var code = ""
        if let id = expression.id {
            if let variable = sT.variables[id.value] {
                code += "mov %rax, \(variable.offset)(%rbp)"
            } else {
                fatalError("Variable \(id.value) hasn't been initialized yet")
            }
        } else if let cond = expression.begin {
            code += generateConditionalExpression(cond, sT)
        }

        return code.tabify
    }

    // <ConditionalExpression> ::= <LogicalOrExpression> [ "?" <Expression> ":" <ConditionalExpression> ]
    private func generateConditionalExpression(_ cE: ConditionalExpression,
                                               _ sT: SymbolTable) -> String {
        var code = generateLogicalOrExpression(cE.logicalOrExpression, sT)
        code += "\n"

        if let expression = cE.expression {
            code += "cmpl $0, %rax"
            let (var1, var2) = generateNewLabel(labelName: "CONDITIONAL_LABEL")
            code += "je \(var1)"

            code += generateExpression(expression, sT)

            code += "jmp \(var2)"

            code += "\(var1)"
            code += generateConditionalExpression(cE.conditionalExpression!, sT)

            code += "\(var2)"
        }

        return code.tabify
    }

    // <LogicalOrExpression> ::= LogicalAndExpression {"||" LogicalAndExpression}
    func generateLogicalOrExpression(_ lExpr: LogicalOrExpression,
                                     _ sT: SymbolTable) -> String {
        var logExpr = generateLogicalAndExpression(lExpr.begin, sT)

        for next in lExpr.many {
            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_OR")

            logExpr += """
                cmpl $0, %rax
                je \(labelJump)
                movl $1, %rax
                jmp \(labelEnd)
                \(labelJump):
                """.tabify

            logExpr += generateLogicalAndExpression(next.next, sT)

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
    func generateLogicalAndExpression(_ logAndExpr: LogicalAndExpression,
                                      _ sT: SymbolTable) -> String {
        var lAndExpr = generateInclusiveOrExpression(logAndExpr.begin, sT)

        for next in logAndExpr.many {
            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_AND")
            lAndExpr += """
                cmp $0, %rax
                jne \(labelJump)
                jmp \(labelEnd)
                \(labelJump):
                """.tabify

            lAndExpr += generateInclusiveOrExpression(next.next, sT)

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
    func generateInclusiveOrExpression(_ inclExpr: InclusiveOrExpression,
                                        _ sT: SymbolTable) -> String {
        var gen = generateExclusiveOrExpression(inclExpr.begin, sT)

        for next in inclExpr.many {
            gen += PUSH_RAX

            gen += generateExclusiveOrExpression(next.next, sT)
            gen += POP_RCX

            gen += """
                or %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <ExclusiveOrExpression> ::= AndExpression {"^" AndExpression}
    func generateExclusiveOrExpression(_ xorExpr: ExclusiveOrExpression,
                                        _ sT: SymbolTable) -> String {
        var gen = generateAndExpression(xorExpr.begin, sT)

        for next in xorExpr.many {
            gen += PUSH_RAX

            gen += generateAndExpression(next.next, sT)
            gen += POP_RCX

            gen += """
                xor %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <AndExpression> ::= EqualityExpression {"&" EqualityExpression}
    func generateAndExpression(_ andExpr: AndExpression,
                               _ sT: SymbolTable) -> String {
        var gen = generateEqualityExpression(andExpr.begin, sT)

        for next in andExpr.many {
            gen += PUSH_RAX

            gen += generateEqualityExpression(next.next, sT)
            gen += POP_RCX

            gen += """
                and %rcx, %rax
                """.tabify
        }

        return gen
    }

    // <EqualityExpression> ::= <RelationalExpression> { ("!=" | "==") <RelationalExpression> }
    func generateEqualityExpression(_ eExpr: EqualityExpression,
                                     _ sT: SymbolTable) -> String {
        var relExpr = generateRelationalExpression(eExpr.begin, sT)

        for next in eExpr.many {
            // TODO
            relExpr += PUSH_RAX
            relExpr += generateRelationalExpression(next.next, sT)
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
                fatalError("This should not happen")
            }
        }

        return relExpr
    }

    // <RelationalExpression> ::= ShiftExpression {("<" | ">" | "<=" | ">=") ShiftExpression}
    func generateRelationalExpression(_ rExpr: RelationalExpression,
                                       _ sT: SymbolTable) -> String {
        var gen = generateShiftExpression(rExpr.begin, sT)

        for next in rExpr.many {
            gen += PUSH_RAX
            gen += generateShiftExpression(next.next, sT)
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
                fatalError("This should not happen")
            }

        }

        return gen
    }

    // <ShiftExpression> ::= AdditiveExpression {"<<" | ">>" AdditiveExpression}
    private func generateShiftExpression(_ shiftExpr: ShiftExpression,
                                         _ sT: SymbolTable) -> String {
        var gen = generateAdditiveExpression(shiftExpr.begin, sT)

        for next in shiftExpr.many {
            // TODO: Unsigned type: do logical shift
            // TODO: Signed type: do arithmetic shift
            gen += PUSH_RAX
            gen += generateAdditiveExpression(next.next, sT)
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
                fatalError("This should not happen")
            }
        }

        return gen
    }

    // <AdditiveExpression> ::= <Term> { ("+" | "-") <Term> }
    private func generateAdditiveExpression(_ addExpr: AdditiveExpression,
                                             _ sT: SymbolTable) -> String {
        var gen = generateTerm(addExpr.begin, sT)

        for next in addExpr.many {
            gen += PUSH_RAX

            gen += generateTerm(next.next, sT)
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
                fatalError("This should not happen")
            }
        }

        return gen
    }

    // Term ::= <Factor> { ("*" | "/" | "%") <Factor> }
    private func generateTerm(_ term: Term, _ sT: SymbolTable) -> String {
        var gen = generateFactor(term.begin, sT)

        for next in term.many {
            gen += PUSH_RAX
            gen += generateFactor(next.next, sT)
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
                fatalError("This should not happen")
            }

        }

        return gen
    }

    // <Factor> ::= "(" <Expression> ")" | <unaryOp> <Factor> | <int> | <id>
    private func generateFactor(_ factor: Factor, _ sT: SymbolTable) -> String {
        switch factor.orExpr {
        case .ONE:
            return generateExpression(factor.expression!, sT)
        case .TWO:
            // TODO: THis feels odd...
            let gFactor = generateFactor(factor.factor!, sT)
            return gFactor + generateUnaryOperation(token: factor.unaryOp!)
        case .THREE:
            return generateUnaryOperation(token: factor.id!)
        case .FOUR:
            if let variable = sT.variables[factor.id!.value] {
                switch variable.type {
                case .INT:
                    return "movq \(variable.offset)(%rbp), %eax".tabify
                case .NONE:
                    fatalError("This should not happen")
                }
            } else {
                fatalError("This should not happen")
            }
        default:
            fatalError("This should not happen")
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
                movq $\(value), %eax ; Push a constant double word to eax
                """.tabify
        default:
            fatalError("This should not happen")
        }
    }
}
