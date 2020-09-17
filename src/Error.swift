//
//  Error.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

enum ErrorType {
    case None
    case LexicalError
}

struct ErrorCache {
    // Position of error
    var line: Int
    var position: Int
    
    // Type of error
    var errorType: ErrorType
}

class ErrorHandling {
    static func handleErrors(errorArray: [ErrorCache]) {
        
        exit(EXIT_FAILURE)
    }
}
