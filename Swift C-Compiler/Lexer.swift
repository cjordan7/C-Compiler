//
//  Lexer.swift
//  Swift C-Compiler
//
//  Copyright © 2020 Cosme Jordan. All rights reserved.
//

import Foundation

public enum Token: Equatable {
    // FIXME: TODO: Create string representable for each of them
    // FIXME: TODO: Use either CustomStringConvertible or
    // FIXME: TODO: a switch so that I can call the representable
    // FIXME: TODO: of everything else.
    // FIXME: TODO: For example, create a function with a switch
    // FIXME: TODO: switch(self)
    // FIXME: TODO: {case EVEYRTHINWITHValue(let value): return string
    // FIXME: TODO: default: String(SELF)}
    typealias Lexing = (String) -> Token?
    
    // =================================================================
    // List of Tokens
    
    // Keywords
    case RETURN // return
    case INT    // int
    
    // Unary Operators
    case MINUS // -
    case NOT // !
    case TIMES // *
    case PLUS // +
    case BITWISE_AND // &
    case BITWISE_OR // |
    case TILDE // ~
    
    
    // Logical Operators
    case LOGICAL_AND // &&
    case LOGICAL_OR // ||
    
    // Variables
    case ID(String) // [a-z-A-Z]([a-z-A-Z]|[0-9])*
    case CONSTANT_INT(Int) // 2, 3, 4, 5,...
    
    // Terminator Symbols
    case LEFT_PAREN    // "("
    case RIGHT_PAREN   // ")"
    case SEMICOLON     // ";"
    case OPEN_BRACE    // "{"
    case CLOSING_BRACE // "}"
    
    // Comments
    case SINGLE_LINE_COMMENT // "//"
        
    // =================================================================
    // Definitions of regexes
    static var lexing: [(String, Lexing)] {
        return [
            // Some explanations about pattern matching:
            // 1. \b: matches a word boundary between
            //    a word character and non word character
            //    Notice that we have to use \\b because
            //    it is a specificty of the language
        
            // ---------------------------------------------------------
            // Keywords
            ("\\breturn\\b", {_ in .RETURN}),
            ("\\bint\\b", {_ in .INT}),

            // Unary Operators
            ("-", {_ in .MINUS}),
            ("!", {_ in .NOT}),
            ("\\|", {_ in BITWISE_OR}),
            ("&", {_ in .BITWISE_AND}), // Or address
            ("~", {_ in .TILDE}),
            ("\\*", {_ in .TIMES}), // Or pointer
            ("\\+", {_ in .PLUS}),

            // Logical Operators
            ("&&", {_ in .LOGICAL_AND}),
            ("\\|\\|", {_ in LOGICAL_OR}),

            // ---------------------------------------------------------
            // Variables

            ("([a-z]|[A-Z])+([a-z]|[A-Z]|[0-9])*", {.ID($0)}),

            // [0-9] match a number between 0 and 9
            // [0-9]+ matches one or more numbers between 0 and 9
            ("\\b[0-9]+\\b", {.CONSTANT_INT(Int($0)!)}),


            // ---------------------------------------------------------
            // Terminator Symbols
            ("\\(", {_ in .LEFT_PAREN}),
            ("\\)", {_ in .RIGHT_PAREN}),
            (";", {_ in .SEMICOLON}),
            ("\\{", {_ in .OPEN_BRACE}),
            ("\\}", {_ in .CLOSING_BRACE}),
            
            // Comments
            ("\\/\\/.*", {_ in .SINGLE_LINE_COMMENT})
        ]
    }
    
    // =================================================================
    // Operator overloading
    public static func ==(lhs: Token, rhs: Token) -> Bool {
        // It is possible to write only one case for all the
        // cases that return true, but I think it is clearer like this.
        switch (lhs, rhs) {

        // Keyword
        case (RETURN, RETURN), (INT, INT):
            return true

        // Operators
        case (MINUS, MINUS),
             (NOT, NOT),
             (LOGICAL_OR, LOGICAL_OR),
             (LOGICAL_AND, LOGICAL_AND),
             (BITWISE_OR, BITWISE_OR),
             (BITWISE_AND, BITWISE_AND),
             (TIMES, TIMES),
             (PLUS, PLUS),
             (TILDE, TILDE):
            return true

        // Variables
        case (ID(_), ID(_)), (CONSTANT_INT(_), CONSTANT_INT(_)):
            return true

        // Terminator Symbols
        case (LEFT_PAREN, LEFT_PAREN),
             (RIGHT_PAREN, RIGHT_PAREN),
             (SEMICOLON, SEMICOLON),
             (OPEN_BRACE, OPEN_BRACE),
             (CLOSING_BRACE, CLOSING_BRACE):
            return true
            
        // Comments
        case (SINGLE_LINE_COMMENT, SINGLE_LINE_COMMENT):
            return true
        default:
            return false
        }
    }
    
    var value: String {
        switch self {
        case .ID(let value):
            return value
        case .CONSTANT_INT(let value):
            return "\(value)"
        default:
            return "\(self)"
        }
    }
    
}

// FIXME: TODO: Set var and func to either private, public,...
class Lexer {
    // FIXME: TODO: MAke private below
    var errorArray: [ErrorCache] = []
    var fileInput: String = ""
    private var tokens: [Token] = []
    
    init() {
    }

    func read(nameFile: String) {
        do {
            let nSString = try NSString(contentsOfFile: nameFile, encoding: String.Encoding.utf8.rawValue)
            fileInput = nSString as String
        } catch{
            // FIXME: TODO: Handle Error
        }
    }
    
    func getFirstMatch(code: String, regex: String) -> String? {
        let range = NSRange(location: 0, length: code.utf16.count)
        let regex = try! NSRegularExpression(pattern: "\(regex)")
        let firstMatch = regex.rangeOfFirstMatch(in: code, options: [], range: range)

        if(firstMatch.location == 0) {
            let index = code.index(code.startIndex, offsetBy: firstMatch.length)
            
            return String(code[..<index])
        } else {
            return nil
        }
    }
    
    func getNextMatch(code: String) -> (Token.Lexing, String)? {
        let matchedOptio = Token.lexing.first(where: {
            regex, lex in
            lexer.getFirstMatch(code: code, regex: regex) != nil
        })
        
        if let matched = matchedOptio {
            // This guarantees that matched is not nil so tokenFunction must exist
    
            let (regex, tokenFunction) = matched
            let firstMatch = lexer.getFirstMatch(code: code, regex: regex)!
        
           return (tokenFunction, firstMatch)
        } else {
            // Add error and keep going
            // FIXME: TODO Eat invalid stuff
            //
            
            // Use: "(.+?)(?=(int|txt))"
            // It eats everything until it encounters either "int" or "txt"
            // not included. It is not greedy
            // Create static function for all valid lexer such as
            // "(.+?)(?=(regex1|regex2|regex3|regex4|regex5|regex6|...))"
            
            // Get current line and save it to array + token
            // Use: var errorArray: [ErrorCache] = []
            
            // Delete fatalerror
            
            fatalError("Error")
            // FIXME: TODO: Decomment
            // return nil
        }
    }
    
    func run() {
        var code = fileInput.trimmingCharacters(in: .whitespacesAndNewlines)

        while code != "" {
            if let next = getNextMatch(code: code) {
                let (tokenFunction, firstMatch) = next
                
                let token = tokenFunction(firstMatch)!
                if(token != Token.SINGLE_LINE_COMMENT) {
                    // tokenFunction is guaranted to be non nil
                    tokens.append(tokenFunction(firstMatch)!)
                }
                
                code = String(code[firstMatch.endIndex...])
                code = code.trimmingCharacters(in: .whitespacesAndNewlines)
            } else {
                
            }
        }
    }
    
    func getTokens() -> [Token] {
        return tokens
    }
}
