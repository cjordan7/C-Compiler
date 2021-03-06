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

    case FOR
    case WHILE
    case DO
    case SWITCH
    case GOTO
    case CONTINUE
    case BREAK
    case IF
    case ELSE
    case CASE
    case DEFAULT

    // TODO: not really a keyword
    case QUESTION_MARK

    // Unary Operators
    case MINUS // -
    case NOT // !
    case TIMES // *
    case DIVIDE // /
    case PLUS // +
    case BITWISE_AND // &
    case BITWISE_OR // |
    case TILDE // ~
    case MODULO // %

    case INCREMENT // ++
    case DECREMENT // --
    
    
    // Logical Operators
    case LOGICAL_AND // &&
    case LOGICAL_OR // ||

    case SHIFT_LEFT // >>
    case SHIFT_RIGHT // <<

    case XOR // ^
    
    // Relational operators
    case RELATIONAL_EQUAL // ==
    case RELATIONAL_NOT_EQUAL // !=
    case SMALLER // <
    case SMALLER_EQUAL // <=
    case BIGGER // >
    case BIGGER_EQUAL // >=

    // Assignment Operators
    case EQUAL // =
    case EQUAL_TIMES // *=
    case EQUAL_DIVIDE // /=
    case EQUAL_MODULO // %=
    case EQUAL_PLUS // +=
    case EQUAL_MINUS // -=
    case EQUAL_SHIFT_LEFT // <<=
    case EQUAL_SHIFT_RIGHT // >>=
    case EQUAL_AND // &=
    case EQUAL_XOR // ^=
    case EQUAL_OR // |=

    // Variables
    case ID(String) // [a-z-A-Z]([a-z-A-Z]|[0-9])*
    case CONSTANT_INT(Int) // 2, 3, 4, 5,...
    
    // Terminator Symbols
    case LEFT_PAREN    // "("
    case RIGHT_PAREN   // ")"
    case COLON     // ":"
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
        
            // Comments
            ("\\/\\/.*", {_ in .SINGLE_LINE_COMMENT}),
        
            // ---------------------------------------------------------
            // Keywords
            ("\\breturn\\b", {_ in .RETURN}),
            ("\\bint\\b", {_ in .INT}),
            ("\\bfor\\b", {_ in .FOR}),
            ("\\bwhile\\b", {_ in .WHILE}),
            ("\\bdo\\b", {_ in .DO}),
            ("\\bswitch\\b", {_ in .SWITCH}),
            ("\\bgoto\\b", {_ in .GOTO}),
            ("\\bcontinue\\b", {_ in .CONTINUE}),
            ("\\bbreak\\b", {_ in .BREAK}),
            ("\\bif\\b", {_ in .IF}),
            ("\\belse\\b", {_ in .ELSE}),
            ("\\bcase\\b", {_ in .CASE}),
            ("\\default\\b", {_ in .DEFAULT}),

            // TODO: not really a keyword
            ("\\?", {_ in .QUESTION_MARK}),

            ("\\+\\+", {_ in .INCREMENT}),
            ("--", {_ in .DECREMENT}),


            // TODO Check this
            // Relational operator
            ("==", {_ in .RELATIONAL_EQUAL}),
            ("!=", {_ in .RELATIONAL_NOT_EQUAL}),
            
            ("=", {_ in .EQUAL}),
            ("\\*=", {_ in .EQUAL_TIMES}),
            ("\\/=", {_ in .EQUAL_DIVIDE}),
            ("%=", {_ in .EQUAL_MODULO}),
            ("\\+=", {_ in .EQUAL_PLUS}),
            ("-=", {_ in .EQUAL_MINUS}),
            (">>=", {_ in .EQUAL_SHIFT_RIGHT}),
            ("<<=", {_ in .EQUAL_SHIFT_LEFT}),
            ("&=", {_ in .EQUAL_AND}),
            ("^=", {_ in .EQUAL_XOR}),
            ("\\|=", {_ in .EQUAL_OR}),

            ("\\^", {_ in .XOR}),

            (">>", {_ in .SHIFT_RIGHT}),
            ("<<", {_ in .SHIFT_LEFT}),

            ("<=", {_ in .SMALLER_EQUAL}),
            ("<", {_ in .SMALLER}),
            (">=", {_ in .BIGGER_EQUAL}),
            (">", {_ in .BIGGER}),

            // Logical Operators
            ("&&", {_ in .LOGICAL_AND}),
            ("\\|\\|", {_ in LOGICAL_OR}),

            // Unary Operators
            ("-", {_ in .MINUS}),
            ("!", {_ in .NOT}),
            ("\\|", {_ in BITWISE_OR}),
            ("&", {_ in .BITWISE_AND}), // Or address
            ("~", {_ in .TILDE}),
            ("%", {_ in .MODULO}),
            ("\\+", {_ in .PLUS}),
            ("\\*", {_ in .TIMES}), // Or pointer
            ("\\/", {_ in .DIVIDE}),

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
            (":", {_ in .COLON}),
            (";", {_ in .SEMICOLON}),
            ("\\{", {_ in .OPEN_BRACE}),
            ("\\}", {_ in .CLOSING_BRACE})
        ]
    }
    
    // =================================================================
    // Operator overloading
    public static func ==(lhs: Token, rhs: Token) -> Bool {
        // It is possible to write only one case for all the
        // cases that return true, but I think it is clearer like this.
        switch (lhs, rhs) {

        // Keyword
        case (RETURN, RETURN),
             (INT, INT),
             (FOR, FOR),
             (WHILE, WHILE),
             (DO, DO),
             (SWITCH, SWITCH),
             (GOTO, GOTO),
             (CONTINUE, CONTINUE),
             (BREAK, BREAK),
             (IF, IF),
             (ELSE, ELSE),

            // TODO: not really a keyword
            (QUESTION_MARK, QUESTION_MARK):
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
             (TILDE, TILDE),
             (DIVIDE, DIVIDE),
             (RELATIONAL_EQUAL, RELATIONAL_EQUAL),
             (RELATIONAL_NOT_EQUAL, RELATIONAL_NOT_EQUAL),
             (SMALLER, SMALLER),
             (SMALLER_EQUAL, SMALLER_EQUAL),
             (BIGGER, BIGGER),
             (BIGGER_EQUAL, BIGGER_EQUAL),
             (EQUAL, EQUAL),
             (EQUAL_TIMES, EQUAL_TIMES),
             (EQUAL_DIVIDE, EQUAL_DIVIDE),
             (EQUAL_MODULO, EQUAL_MODULO),
             (EQUAL_PLUS, EQUAL_PLUS),
             (EQUAL_MINUS, EQUAL_MINUS),
             (EQUAL_SHIFT_LEFT, EQUAL_SHIFT_LEFT),
             (EQUAL_SHIFT_RIGHT, EQUAL_SHIFT_RIGHT),
             (EQUAL_AND, EQUAL_AND),
             (EQUAL_XOR, EQUAL_XOR),
             (EQUAL_OR, EQUAL_OR),
             (MODULO, MODULO),
             (SHIFT_LEFT, SHIFT_LEFT),
             (SHIFT_RIGHT, SHIFT_RIGHT),
             (XOR, XOR),
             (INCREMENT, INCREMENT),
             (DECREMENT, DECREMENT):

            return true

        // Variables
        case (ID(_), ID(_)), (CONSTANT_INT(_), CONSTANT_INT(_)):
            return true

        // Terminator Symbols
        case (LEFT_PAREN, LEFT_PAREN),
             (RIGHT_PAREN, RIGHT_PAREN),
             (COLON, COLON),
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

    func read(filename: String) {
        do {
            let nSString = try NSString(contentsOfFile: filename, encoding: String.Encoding.utf8.rawValue)
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
            getFirstMatch(code: code, regex: regex) != nil
        })
        
        if let matched = matchedOptio {
            // This guarantees that matched is not nil so tokenFunction must exist

            let (regex, tokenFunction) = matched
            let firstMatch = getFirstMatch(code: code, regex: regex)!
        
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
        print(code)
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
