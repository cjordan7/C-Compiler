//
//  SymbolTable.swift
//  Swift C-Compiler
//
//  Created by Cosme Jordan on 01.03.21.
//  Copyright © 2021 Cosme Jordan. All rights reserved.
//

import Foundation

enum Types {
    case INT
    case NONE
}

class VariablesRepresentation {
    var offset = 0
//    var alignment
    // TODO: Change that into array of types ?
    var type = Types.NONE
    // TODO: setSize as well ?
//    var
}

class SymbolTable {
    var functions = [String: String]()
    var variables = [String: VariablesRepresentation]()

    // For goto, continue, break
    var jumps = [String: String]()

    var rbpValue = 0
    var currentFunction: String = ""
    var jumpEndFunction: String = ""

    var continueLabel: String = ""
    var endJumpLabel: String? = nil

//    func addVar

    func copyVariables(with: [String: VariablesRepresentation]) -> SymbolTable {
        let newSymbolTable = SymbolTable()
        newSymbolTable.functions = self.functions
        newSymbolTable.endJumpLabel = endJumpLabel
        newSymbolTable.currentFunction = currentFunction
        newSymbolTable.continueLabel = continueLabel

        // Copy elements doesn't exist
        for (varString, varDef) in self.variables {
            if let _ = with[varString] {
            } else {
                newSymbolTable.variables[varString] = varDef
            }
        }

        for (varString, varDef) in with {
            newSymbolTable.variables[varString] = varDef
        }

        return newSymbolTable
    }
}
