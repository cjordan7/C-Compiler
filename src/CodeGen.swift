////
////  CodeGen.swift
////  Swift C-Compiler
////
////  Copyright © 2020 Cosme Jordan. All rights reserved.
////
//
//import Foundation
//
//// TODO: Comment assembly code
//
//extension String {
//    var tabify: String {
//        let lines = self.components(separatedBy: "\n")
//        var newString = ""
//        for s in lines {
//            if(!s.isEmpty) {
//                newString += "\t\(s)\n"
//            }
//        }
//
//        return newString
//    }
//}
//
//class CodeGeneration {
//    private let PUSH_RAX = """
//            push %rax
//            """.tabify
//    private let POP_RCX = """
//            pop %rcx
//            """.tabify
//    private let COMPARE = """
//            cmp %rax, %rcx
//            mov $0, %rax
//            """.tabify
//
//    private var labelsMap = [String: Int]()
//    private var parseAST: TranslationUnit
//
//    init(parseAST: TranslationUnit) {
//        self.parseAST = parseAST
//        labelsMap["JUMP_CLAUSE_OR"] = 0
//        labelsMap["JUMP_CLAUSE_AND"] = 0
//    }
//
//    private func generateNewLabel(labelName: String) -> (String, String) {
//        labelsMap[labelName]! += 1
//        return ("_\(labelName)\(labelsMap[labelName]!)",
//            "_\(labelName)_end\(labelsMap[labelName]!)")
//    }
//
//    public func generateCode() -> String {
//        return generateTranslationUnit(parseAST)
//    }
//
//    // <TranslationUnit> ::= <Function>
//    private func generateTranslationUnit(_ tUnit: TranslationUnit) -> String {
//        return generateFunction(tUnit.function)
//    }
//
//    // <Function> ::= "int" <id> "(" ")" "{" <Statement> "}"
//    private func generateFunction(_ function: Function) -> String {
//        let nameFunc = function.name.value
//        return """
//
//        .global _\(nameFunc)
//        _\(nameFunc):
//        \(generateStatement(function.statement))
//        """
//    }
//
//    // <Statement> ::= "return" <Expression> ";"
//    private func generateStatement(_ statement: Statement) -> String {
//        return "\(generateExpression(statement.expression))" +
//            "ret".tabify
//    }
//
//    // <Expression> ::= <LogicalOrExpression>
//    private func generateExpression(_ expression: Expression) -> String {
//        return generateLogicalOrExpression(expression.begin)
//    }
//
//    // <LogicalOrExpression> ::= LogicalAndExpression {"||" LogicalAndExpression}
//    func generateLogicalOrExpression(_ lExpr: LogicalOrExpression) -> String {
//        var logExpr = generateLogicalAndExpression(lExpr.begin)
//
//        for next in lExpr.many {
//            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_OR")
//
//            logExpr += """
//                cmpl $0, %eax
//                je \(labelJump)
//                movl $1, %eax
//                jmp \(labelEnd)
//                \(labelJump):
//                """.tabify
//
//            logExpr += generateLogicalAndExpression(next.next)
//
//            logExpr += """
//                cmp $0, %rax
//                mov $0, %rax
//                setne %al
//                \(labelEnd):
//                """.tabify
//
//            // Generate unique labels
//        }
//
//        return logExpr
//    }
//
//    // <LogicalAndExpression> ::= InclusiveOrExpression {"&&" InclusiveOrExpression}
//    func generateLogicalAndExpression(_ logAndExpr: LogicalAndExpression) -> String {
//        var lAndExpr = generateInclusiveOrExpression(logAndExpr.begin)
//
//        for next in logAndExpr.many {
//            let (labelJump, labelEnd) = generateNewLabel(labelName: "JUMP_CLAUSE_AND")
//            lAndExpr += """
//                cmp $0, %rax
//                jne \(labelJump)
//                jmp \(labelEnd)
//                \(labelJump):
//                """.tabify
//
//            lAndExpr += generateInclusiveOrExpression(next.next)
//
//            lAndExpr += """
//                cmp $0, %rax
//                mov $0, %rax
//                setne %al
//                \(labelEnd):
//                """.tabify
//        }
//
//        return lAndExpr
//    }
//
//    // <InclusiveOrExpression> ::= ExclusiveOrExpression {"|" ExclusiveOrExpression}
//    func generateInclusiveOrExpression(_ inclExpr: InclusiveOrExpression) -> String {
//        var gen = generateExclusiveOrExpression(inclExpr.begin)
//
//        for next in inclExpr.many {
//            gen += PUSH_RAX
//
//            gen += generateExclusiveOrExpression(next.next)
//            gen += POP_RCX
//
//            gen += """
//                or %rcx, %rax
//                """.tabify
//        }
//
//        return gen
//    }
//
//    // <ExclusiveOrExpression> ::= AndExpression {"^" AndExpression}
//    func generateExclusiveOrExpression(_ xorExpr: ExclusiveOrExpression) -> String {
//        var gen = generateAndExpression(xorExpr.begin)
//
//        for next in xorExpr.many {
//            gen += PUSH_RAX
//
//            gen += generateAndExpression(next.next)
//            gen += POP_RCX
//
//            gen += """
//                xor %rcx, %rax
//                """.tabify
//        }
//
//        return gen
//    }
//
//    // <AndExpression> ::= EqualityExpression {"&" EqualityExpression}
//    func generateAndExpression(_ andExpr: AndExpression) -> String {
//        var gen = generateEqualityExpression(andExpr.begin)
//
//        for next in andExpr.many {
//            gen += PUSH_RAX
//
//            gen += generateEqualityExpression(next.next)
//            gen += POP_RCX
//
//            gen += """
//                and %rcx, %rax
//                """.tabify
//        }
//
//        return gen
//    }
//
//    // <EqualityExpression> ::= <RelationalExpression> { ("!=" | "==") <RelationalExpression> }
//    func generateEqualityExpression(_ eExpr: EqualityExpression) -> String {
//        var relExpr = generateRelationalExpression(eExpr.begin)
//
//        for next in eExpr.many {
//            // TODO
//            relExpr += PUSH_RAX
//            relExpr += generateRelationalExpression(next.next)
//            relExpr += POP_RCX
//
//            relExpr += COMPARE
//
//            if(next.operation == .RELATIONAL_EQUAL) {
//                relExpr += """
//                sete   %al
//                """.tabify
//            } else if(next.operation == .RELATIONAL_NOT_EQUAL) {
//                relExpr += """
//                setne   %al
//                """.tabify
//            } else {
//                fatalError()
//            }
//        }
//
//        return relExpr
//    }
//
//    // <RelationalExpression> ::= ShiftExpression {("<" | ">" | "<=" | ">=") ShiftExpression}
//    func generateRelationalExpression(_ rExpr: RelationalExpression) -> String {
//        var gen = generateShiftExpression(rExpr.begin)
//
//        for next in rExpr.many {
//            gen += PUSH_RAX
//            gen += generateShiftExpression(next.next)
//            gen += POP_RCX
//
//            gen += COMPARE
//
//            if(next.operation == .SMALLER) {
//                gen += """
//                setl   %al
//                """.tabify
//            } else if(next.operation == .SMALLER_EQUAL) {
//                gen += """
//                setle   %al
//                """.tabify
//            } else if(next.operation == .BIGGER) {
//                gen += """
//                setg   %al
//                """.tabify
//            } else if(next.operation == .BIGGER_EQUAL) {
//                gen += """
//                setge   %al
//                """.tabify
//            } else {
//                fatalError()
//            }
//
//        }
//
//        return gen
//    }
//
//    // <ShiftExpression> ::= AdditiveExpression {"<<" | ">>" AdditiveExpression}
//    private func generateShiftExpression(_ shiftExpr: ShiftExpression) -> String {
//        var gen = generateAdditiveExpression(shiftExpr.begin)
//
//        for next in shiftExpr.many {
//            // TODO: Unsigned type: do logical shift
//            // TODO: Signed type: do arithmetic shift
//            gen += PUSH_RAX
//            gen += generateAdditiveExpression(next.next)
//            gen += POP_RCX
//
//            if(next.operation == .SHIFT_RIGHT) {
//                gen += """
//                shr %rcx, %rax
//                """.tabify
//            } else if(next.operation == .SHIFT_LEFT) {
//                gen += """
//                shl %rcx, %rax
//                """.tabify
//            } else {
//                fatalError()
//            }
//        }
//
//        return gen
//    }
//
//    // <AdditiveExpression> ::= <Term> { ("+" | "-") <Term> }
//    private func generateAdditiveExpression(_ addExpr: AdditiveExpression) -> String {
//        var gen = generateTerm(addExpr.begin)
//
//        for next in addExpr.many {
//            gen += PUSH_RAX
//
//            gen += generateTerm(next.next)
//            gen += POP_RCX
//
//            if(next.operation == .PLUS) {
//                // Add expression
//                gen +=
//                    """
//                    add %rcx, %rax
//                    """.tabify
//            } else if(next.operation == .MINUS) {
//                // Calculate sub expression
//                gen +=
//                    """
//                    sub %rax, %rcx
//                    mov %rcx, %rax
//                    """.tabify
//            } else {
//                fatalError()
//            }
//        }
//
//        return gen
//    }
//
//    // Term ::= <Factor> { ("*" | "/" | "%") <Factor> }
//    private func generateTerm(_ term: Term) -> String {
//        var gen = generateFactor(term.begin)
//
//        for next in term.many {
//            gen += PUSH_RAX
//            gen += generateFactor(next.next)
//            gen += POP_RCX
//
//            if(next.operation == .TIMES) {
//                gen +=
//                    """
//                    imul %rcx, %rax
//                    """.tabify
//            } else if(next.operation == .DIVIDE) {
//                // TODO: REwrite division, because it is wrong
//                gen +=
//                    """
//                    idiv %rcx, %rax
//                    """.tabify
//            } else if(next.operation == .MODULO) {
//                // TODO: Check that out
//                gen +=
//                    """
//                    idiv %rcx, %rax
//                    mov %rdx, %rax
//                    """.tabify
//            } else {
//                fatalError()
//            }
//
//        }
//
//        return gen
//    }
//
//    // <Factor> ::= "(" <Expression> ")" | <UnaryOp> <Factor> | <int>
//    private func generateFactor(_ factor: Factor) -> String {
//        switch factor.orExpr {
//        case .ONE:
//            return generateExpression(factor.expression!)
//        case .TWO:
//            // TODO: THis feels odd...
//            let gFactor = generateFactor(factor.factor!)
//            return gFactor + generateUnaryOperation(token: factor.unaryOp!)
//        case .THREE:
//            return generateUnaryOperation(token: factor.intT!)
//        }
//    }
//
//    // <unaryOp> ::= "!" | "~" | "-"
//    private func generateUnaryOperation(token: Token) ->String {
//        switch token {
//        case .NOT:
//            return """
//            cmp   $0, %rax
//            mov   $0, %rax
//            sete   %al
//            """.tabify
//        case .TILDE:
//            return """
//                mov    $\(token.value), %rax
//                neg     %rax
//                """.tabify
//        case .MINUS:
//            return """
//                mov $\(token.value), %rax
//                neg %rax
//                """.tabify
//        case .CONSTANT_INT(let value):
//            return """
//                mov $\(value), %rax
//                """.tabify
//        default:
//            fatalError()
//        }
//    }
//}
