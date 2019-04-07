// node.js dependencies
const fs = require('fs');
const URL = require('url').URL;
const config = require('./config');

// check arguments count
if (process.argv.length != 3) {
    console.log(`Usage: node compiler.js [program_name]`)
    console.log(`The program should be called in the directory, where Fmilan source file is located.`)
    console.log(`The file [program_name].mil is the input file for the compiler.\
Files [program_name].asm, [program_name].o and [program_name].exe are generated \
during compilation process`)
    process.exit(0)
}

// project structure variables
const folder_path = process.cwd() + '\\';
const file_name = process.argv[2];
// other options get from config file
const src_ext = config.extensions.src_ext
const asm_ext = config.extensions.asm_ext
const exe_ext = config.extensions.exe_ext
const assembly_snippet_path = config.assembly_snippet_path
const linker_path = config.linker_path

// language keywords description
var types = ["BYTE", "WORD", "INT", "FLOAT", "STRING"]
// check that type idx2 can be cast to idx1
// now, BYTE, WORD and INT are compatible, else must be equal
// ideally: add FLOAT to the listed below, and that all could be cast to STRING
function can_be_cast(type1, type2) {
    let idx1 = types.indexOf(type1)
    let idx2 = types.indexOf(type2)
    return idx2 <= 2 && idx1 <= 2 || idx1 == idx2
}
var keywords = ["BEGIN", "END", "IF", "THEN", "ELSE",  "DO", "WHILE", "FOR", "CASE", "OF", "FUNCTION"
                , "FI", "OD", "ROF", "ESAC", "TO", "DOWNTO", "STEP"]
var ops = [
    {op: "*", priority: 0, assoc: 0, unary: 0},
    {op: "\\", priority: 0, assoc: 1, unary: 0},
    {op: "%", priority: 0, assoc: 1, unary: 0},
    {op: "+", priority: 1, assoc: 0, unary: 1},
    {op: "-", priority: 1, assoc: 1, unary: 1},
    {op: "==", priority: 2, assoc: 0, unary: 0},
    {op: "!=", priority: 2, assoc: 0, unary: 0},
    {op: ">", priority: 2, assoc: 0, unary: 0},
    {op: "<", priority: 2, assoc: 0, unary: 0},
    {op: ">=", priority: 2, assoc: 0, unary: 0},
    {op: "<=", priority: 2, assoc: 0, unary: 0},
    {op: "NOT", priority: 3, assoc: 0, unary: 1},
    {op: "AND", priority: 4, assoc: 0, unary: 0},
    {op: "XOR", priority: 4, assoc: 0, unary: 0},
    {op: "OR", priority: 5, assoc: 0, unary: 0},
    {op: ":=", priority: 6, assoc: 2, unary: 0}
]
var brackets = ["(", ")"]
var delimiter = ";"
var allowed_varname_first = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
var allowed_varname_chars = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

// table of extern functions definition
// if local equivalent cannot be found, search in the external functions table
var funcs = {
    n : 0,
    // func, types, asm_alias
    ext_table: [
        ["PRINT", [types[2]], null, "@print_int32"],
        ["PRINT", [types[4]], null, "@print_str"],
        ["PRINTLN", [types[2]], null, "@println_int32"],
        ["PRINTLN", [types[4]], null, "@println_str"],
        ["READ", [], types[2], "@scan_int32"],
        ["HALT", [], null, "@quit_program"],
        ["HALT", [types[2]], null, "@quit_program"]
    ],
    table : [],
    next: function(ast) { // pass function call to typecheck parameters
        if (! ast instanceof func_call)
            throw "Typechecking function call of not function is unacceptable"
        if (ast.ref != null) {
            // implement as variable style
            return this.register(ast.ref)
        }
        if (ast.ext_ref != null) {
            return ast.ext_ref[3] // already searched, see below
        }
        // else search in externals
        let funcIndex = this.ext_table.findIndex(row=>{
            if (row[0]!=ast.func_name) return false; // name didn't match
            let func_param_count = ast.params.length
            let actual_param_count = row[1].length
            if (func_param_count != actual_param_count) return false; // length didn't match
            for (var k = 0; k < func_param_count; k++) {
                let type = (ast.params[k] instanceof leaf_var)
                    ? ast.params[k].ref.type
                    : ast.params[k].type
                // we shoud use here can_we_cast in my opinion
                let cwc = can_be_cast(type, row[1][k])
                //if (type != row[1][k])
                if (!cwc)
                    return false;
            }
            return true;
        })
        if (funcIndex == -1)
            throw "No suitable prototype for function '" + ast.func_name + "' with such parameters."
        ast.ext_ref = this.ext_table[funcIndex]
        return this.ext_table[funcIndex][3]
    },
    register: function(ast) { // should be instance of func_def
        if (!(ast instanceof func_def))
            throw "Not function definition statement registered as var"
        let idx = this.table.findIndex(item=> {
            if (item.parent!=ast.parent) return false // didn't match parent
            if (item.func_name != ast.func_name) return false // didn't match name
            if (item.params.length != ast.params.length) return false // didn't match parameters count
            // check parameters compatibility
            for (let k = 0; k < item.params.length; k++) {
                if (item.params[k].type != ast.params[k].type)
                    return false;
            }
            return true; // matches by all criteria
        })
        if (idx != -1) {
            // this is not an error anymore, in semantic graph we checked explicity of the definitions
            return ast.asm_name // it's recomended to do this directly :)
        }
        // register function definition here
        ast.asm_name = "func" + this.n
        this.n++
        this.table.push(ast)
        return ast.asm_name
    }
}

// lexer
// TODO add semantic analysis (keyword, variable, constant (INT, FLOAT, STRING), operation, delimiter)
function lexer(src) {
    var i = 0;
    var j = 0;
    var lex_arr = [];
    function addLexem() {
        // check out lexem
        let lex = src.substr(i, j - i)
        // add lexem (it can be variable or keyword)
        lex_arr.push(lex);
        // refresh index
        i = j;
    }
    while (i < src.length) {
        let chr = src.charAt(i).toLowerCase()
        if (chr >= 'a' && chr <= 'z')  { // variable or keyword
            // find next not char and not digit symbol
            j = i + 1
            while (true) {
                let chr = src.charAt(j).toLowerCase()
                if ((chr < 'a' || chr > 'z') && (chr < '0' || chr > '9'))
                    break
                j = j + 1
            }
            addLexem()
        } else if (chr >= '0' && chr <= '9'/* || chr == '-' || chr == '+'*/) { // digit of fp constant
            // all digits required. One dot acceptable
            var fpu = 0 // floating point identifier for constant
            j = i + 1
            while (true) {
                let chr = src.charAt(j).toLowerCase()
                if (chr < '0' || chr > '9')
                    break
                j = j + 1
            }
            if (src.charAt(j).toLowerCase() == '.') {
                fpu = 1
                j = j + 1
                while (true) {
                    let chr = src.charAt(j).toLowerCase()
                    if (chr < '0' || chr > '9')
                        break
                    j = j + 1
                }
            }
            addLexem()
        } else if (chr == '\'') { // string type 1
            j = i + 1
            while (true) {
                let chr = src.charAt(j)
                let prev_chr = src.charAt(j-1)
                if (chr == '\'' && prev_chr != '\\')
                    break
                j = j + 1
                if (j == src.length || chr == '\n')
                    throw "No closing bracket found for string expression!"
            }
            j = j + 1
            addLexem()
        } else if (chr == '\"') { // string type 2
            j = i + 1
            while (true) {
                let chr = src.charAt(j)
                let prev_chr = src.charAt(j - 1)
                if (chr == '\"' && prev_chr != '\\')
                    break
                j = j + 1
                if (j == src.length || chr == '\n')
                    throw "No closing bracket found for string expression!"
            }
            j = j + 1
            addLexem()
        } else if (chr == "/") { // possible comment
            j = i + 1
            let chr = src.charAt(j).toLowerCase()
            switch (chr) {
            case "/": // single lined comment ends till newline
                while (true) {
                    let chr = src.charAt(j).toLowerCase()
                    if (j == src.length || chr == '\n') {
                        i = j
                        break
                    }
                    j = j + 1
                }
                break;
            case "*": // multiline comment ends till */ combination
                let idx = src.indexOf("*/", j)
                if (idx == -1) throw "No closing '*/' for multiline comment found!"
                i = idx + 2;
                break;
            default:
                throw "Token error: single or multiline comment expected" // no comment here found
            }
            // lexem is not added here :D 
        } else if (' \t\n\r\v'.indexOf(chr) > -1) {
            // whitespaces ignored completely
            i++
        } else if (":=!<>".indexOf(chr) > -1) {
            j = i + 1
            let chr = src.charAt(j).toLowerCase()
            if (chr == "=") {
                j = j + 1 // for :=, !=, ==, <=, >=
            }
            addLexem()
        } else { // single lexem
            j = i + 1
            addLexem()
        }
    }
    return lex_arr;
}

// Parser
// IDEA: use stack of ranges for parsing lexems
// also, there is such nodes of tree (operators) as:
//  1. SEQ (responsible for BEGIN ... END)
//  2. IF ... THEN .. ELSE
//  3. DO ... WHILE
//  4. WHILE ... DO
//  5. expr (the most basic)
// without lexem index range stack we cannot decompose lexems
// For example: IF (expr) THEN (op) ELSE (op)
// first of all -- we find IF, and create at current tree position node ITE
// then, the property of this node "if" must be evaluated AS expr (type 5).
// thus we enter [if = new expr()] and begin to parse expression till THEN keyword
// with new range lexem indexes: (idx(IF), idx(THEN))
// we find new indexes, but should return to old one.
// Solutions:
//  1. recursion
//  2. stack
// The second one seems stable and legit, the first one seems functional

// Defining nodes
// BEGIN END
function seq(parent) {
    this.parent = parent;
    this.seq = []
}
// IF THEN ELSE
function ite(parent) {
    this.parent = parent
    this.if = null
    this.then = null
    this.else = null
}
// DO WHILE
function dw(parent, precondition = false) {
    this.parent = parent
    this.precondition = precondition
    this.condition = null
    this.op = null
}
// declaration
function decl(parent) {
    this.parent = parent
    this.varname = null
    this.type = null
    this.value = null
}
// operators
// assign
function assign(parent) {
    this.parent = parent
    this.constant = false
    this.varname = null
    this.expr = null
}
// plus, minus, mul, div, rem
function op(parent, optype) {
    this.parent = parent
    this.optype = optype
    this.lop = null
    this.rop = null
    this.constant = null
}
function leaf_const(parent, type) {
    this.parent = parent
    this.type = type
    this.constant = true
    this.value = null
}
function leaf_var(parent) {
    this.parent = parent
    this.constant = false
    this.varname = null
}
function func_call(parent) {
    this.parent = parent
    this.constant = false // TODO if function is isolated, maybe it can be calculated from consts
    this.func_name = null // function name to call
    this.params = [] // array of parameters
}
function func_def(parent) {
    this.parent = parent
    this.constant = false
    this.func_name = null // function name
    this.type = null // function return type
    this.params = [] // array of decl's
    this.op // function operator sequence
}

// list of node types:
// seq, ite, dw, decl, assign, op, leaf_const, leaf_var, func_call, func_def

// building with that abstract syntax tree
function astBuilder(lexems) {

    function parseOp(lexems, i) { // decides, how to parse this type of operator
        let lexem = lexems[i].toUpperCase()
        switch (lexem) {
            case keywords[0]: // begin
                return parseSeq(lexems, i+1)
                break;
            case keywords[2]: // if
                return parseIf(lexems, i+1)
                break;
            case keywords[5]: // do 
                return parseDoWhile(lexems, i+1)
                break;
            case keywords[6]: // while
                return parseWhileDo(lexems, i+1)
                break;
            case keywords[7]: // for
                return parseFor(lexems, i+1);
                break;
            case keywords[10]: // function
                return parseFunction(lexems, i+1)
                break;
            case keywords[8]: // case
            case keywords[1]: // end
            case keywords[3]: // then 
            case keywords[4]: // else
            case keywords[9]: // of
            case keywords[11]: // fi
            case keywords[12]: // od
            case keywords[13]: // rof
            case keywords[14]: // esac
                throw "Unexpected keyword " + lexem;
                break;
            default: // expr or decl
                // TODO: expecting this is a variable (NOT constant!)
                if (lexems[i+1] == ":")
                    return parseDecl(lexems, i); // parse declaration
                else
                    // search for a range to parse expression
                    var k = i
                    while(k < lexems.length) {
                        k++
                        if (keywords.indexOf(lexems[k]) != -1
                            || types.indexOf(lexems[k]) != -1
                            || lexems[k] == delimiter)
                            break;
                    }
                    var res = parseExpr(lexems, i, k - 1) // parse expression
                    var ret = {
                        node: res, // parse expression
                        last_index: k
                    }
                    res.parent = ret
                    return ret
        }
    }

    function checkVarname(name) {
        if (allowed_varname_first.indexOf(name[0]) == -1)
            return false
        for (var cv in name)
            if (allowed_varname_chars.indexOf(cv) == -1)
                return false
        return true
    }

    function parseConstant(lexem) {
        var len = lexem.length
        var res
        // string constant
        if (lexem[0] == '\'' && lexem[len-1] == '\''
            || lexem[0] == '\"' && lexem[len-1] == '\"') {
            var ret = new leaf_const(null, types[4]) // STRING
            ret.value = lexem.substr(1, lexem.length - 2)
            return ret;
        } else {
            var filterFloat = function (value) {
                if(/^(\-|\+)?([0-9]+(\.[0-9]+)?)$/
                    .test(value))
                    return Number(value);
                return NaN;
            }
            var filterInt = function (value) {
                if(/^(\-|\+)?([0-9]+)$/
                    .test(value))
                    return parseInt(value);
                return NaN;
            }
            // int constant
            res = filterInt(lexem)
            if (isNaN(res)) {
                // float constant
                res = filterFloat(lexem)
                if (isNaN(res))
                    throw "Constant parse error: " + lexem;
                var ret = new leaf_const(null, types[3]) // FLOAT
                ret.value = res
                return ret;
            } else {
                var ret = new leaf_const(null, types[2]) // INT
                ret.value = res
                return ret;
            }
        }
        return res
    }

    // end -- parse till string passed as a parameter "end" (';' by default)
    // type -- expected type of declaration (needed in FOR iterator)
    function parseDecl(lexems, i, end = delimiter, type=null) {
        ret = new decl(null)
        ret.varname = lexems[i] // check it is variable
        if (!checkVarname(ret.varname))
            throw "Illegal variable name '" + ret.varname + "' in declaration."
        ret.type = lexems[i + 2].toUpperCase() // check it is type
        var j = i + 3 // next index to check
        if (types.indexOf(ret.type) == -1) {
            if (!type || types.indexOf(type) == -1)
                throw "Unknown type '" + ret.type + "' for variable '" + ret.varname + "' in declaration."
            ret.type = type; // remember type
            // check there is eq between them
            if (lexems[i + 1] != "=")
                throw "Illegal reduced variable declaration!"
            else
                j = i + 1
        }
        switch (lexems[j]) {
        case "=":
             // TODO make constant evaluation of expression till ";"!
            // search for a range to parse expression
            var k = j + 1
            while(k < lexems.length) {
                k++
                if (keywords.indexOf(lexems[k]) != -1
                    || types.indexOf(lexems[k]) != -1
                    || lexems[k] == end)
                    break;
            }
            ret.value = parseExpr(lexems, j + 1, k - 1);
            ret.value.parent = ret
            return {
                node: ret,
                last_index: k
            }
            break;
        case end:
            return {
                node: ret,
                last_index: j
            }
            break;  
        default:
            throw "Unexpected token " + lexems[j];
            break;
        }
    }

    function findBalancingBracketIndex(lexems, i, brOpen = brackets[0], brClose = brackets[1]) {
        if (lexems[i] != brOpen)
            return i
        var bracketCount = 1;
        while (bracketCount > 0) {
            i++
            if (i >= lexems.length) return -1; // not found
            switch (lexems[i]) {
            case brOpen:
                bracketCount++
                break
            case brClose:
                bracketCount--
                break
            }
        }
        return i
    }

    function parseFuncCall(lexems, i, j) {
        
        let isvar = checkVarname(lexems[i]) // check first lexem to be variable
        if ( lexems [i + 1] != brackets[0] || lexems[j] != brackets[1]) {
            throw "Function call " + lexems[i] + " expected";
        }
        if (!isvar) {
            throw "Function name " + lexems[i] + " is invalid"
        }

        var ret = new func_call(null);
        ret.func_name = lexems[i];
        var index1 = i + 2
        var index2 = index1
        while (index2 <= j) {
            switch (lexems[index2]) {
            case brackets[0]:
                index2 = findBalancingBracketIndex(lexems, index2);
                break;
            case brackets[1]:
                // check this is the end
                if (index2 != j) // but maybe we can do currying? :)
                    throw "Error parsing function call!"
                // no break here! let it parse the last arg
                // actually, there can be no arguments in function call
                if (index1 == index2)
                    break;
            case ",":
                var param = parseExpr(lexems, index1, index2 - 1)
                ret.params.push(param)
                param.parent = ret
                index1 = index2 + 1
                break;  
            }
            index2++;
        }
        return ret;
    }

    // returns node explicitly
    function parseExpr(lexems, i, j) {
        // parsing expression ends with:
        // delimiter ;
        // keyword (ANY keyword)
        var best_op_index = -1
        var best_op_priority = -1
        var tmp = i
        while (tmp <= j) {
            switch (lexems[tmp]) {
            case brackets[0]:
                tmp = findBalancingBracketIndex(lexems, tmp) + 1
                break
            case brackets[1]:
                throw "Brackets unbalanced in expression"
                break
            default:
                let opdef = ops.find((val)=>val.op == lexems[tmp])
                if (!opdef) {
                    tmp++;
                    break;
                }
                let op_prior = opdef.priority
                let op_assoc = opdef.assoc
                if (op_prior >= best_op_priority) {
                    if (op_prior > best_op_priority && op_assoc == 2) {
                        best_op_index = tmp
                        best_op_priority = op_prior
                    } else if (op_assoc < 2) {
                        best_op_index = tmp
                        best_op_priority = op_prior
                    }
                }
                tmp++
            }
        }
        if (best_op_priority != -1) { // op found
            // if there is operators with the same priority on the lhs, move earlier
            var idx = best_op_index
            while (idx > i) {
                idx--
                let opdef = ops.find((val)=>val.op == lexems[idx])
                if (!opdef) break; // there is no operators further
                let op_prior = opdef.priority
                let op_assoc = opdef.assoc
                if (op_prior == best_op_priority) {
                    best_op_index = idx
                } else break;
            }

            var resL = null
            if (best_op_index > i) // not unary operator
                var resL = parseExpr(lexems, i, best_op_index - 1)
            else {
                let opi =  ops.findIndex(elm=>elm.op == lexems[best_op_index])
                if (opi == -1 || !ops[opi].unary)
                    throw "Operator " + lexems[best_op_index] + " is a binary operator, not unary."
            }

            var resR = parseExpr(lexems, best_op_index + 1, j)
            var op_type = lexems[best_op_index]
            var ret;
            if (op_type == ops[15].op) { // := assign
                ret = new assign(null)
                ret.varname = resL // maybe error here if not lvalue
                ret.expr = resR
                // parentizing
                resL.parent = ret
                resR.parent = ret
            } else {
                ret = new op(null, lexems[best_op_index])
                ret.lop = resL
                ret.rop = resR
                ret.constant = (resL && resR) ? resL.constant && resR.constant :
                                resL ? resL.constant :
                                resR ? resR.constant : true
                // parentizing
                if (resL) resL.parent = ret
                if (resR) resR.parent = ret
            }
            return ret
        } else { // op not found
            if (lexems[i] == brackets[0] && lexems[j] == brackets[1]) { // unfold brackets and keep going
                return parseExpr(lexems, i + 1, j - 1)
            }
            // then, there should be function call, const or var expression
            let isvar = checkVarname(lexems[i]) // check first lexem to be variable
            if (i == j) {
                if (isvar) { // if this single lexem is variable, return variable node
                    var ret = new leaf_var(null)
                    ret.varname = lexems[i]
                    return ret
                } else {
                    return parseConstant(lexems[i])
                }
            } else if ( lexems [i + 1] == brackets[0] && lexems[j] == brackets[1]) {
                // parse function call here
                return parseFuncCall(lexems, i, j);
            } else {
                throw "Expression parse error: unexpected lexem " + lexems[i]
            }
        }
    }

    function parseSeq(lexems, i, stop_word = keywords[1]) { // parse begin-end
        var ret = new seq(null)

        while (i < lexems.length) {
            let lexem = lexems[i].toUpperCase()
            switch (lexem) {
            case delimiter: // op divider
                i++;
                break;
            case stop_word: // end by default
                // end found, return accumulated node and next index
                if (ret.seq.length != 1)
                    return {
                        node: ret,
                        last_index: i + 1
                    }
                else (ret.seq.length == 1) // one operator detected here
                    return {
                        node: ret.seq[0],
                        last_index: i + 1
                    }
            default: // parse the operator
                var rs = parseOp(lexems, i)
                i = rs.last_index
                ret.seq.push(rs.node)
                rs.node.parent = ret
                break;
            }
        }
        throw "Lexem "+stop_word+" expected, but not found."
    }

    // service function to find next keyword from index i
    function findNextKeyword(lexems, i, keyword) {
        while (i < lexems.length) {
            if (lexems[i].toUpperCase() == keyword)
                break
            i++
        }
        return i >= lexems.length ? null : i
    }

    // assume begins not from "IF"
    function parseIf(lexems, i) {
        var ret = new ite(null)
        // find THEN
        var j = findNextKeyword(lexems, i, keywords[3])
        var rs = parseExpr(lexems, i, j - 1)
        ret.if = rs
        ret.if.parent = ret
        // call parseSeq (even if there is no begin-end)
        // rs = parseOp(lexems, j + 1) // one operator version
        var needsSecondParse = false
        try {
            rs = parseSeq(lexems, j + 1, keywords[1])
        } catch (e) { // mostly occurs if ELSE found earlier, than END
            rs = parseSeq(lexems, j + 1, keywords[4])
            needsSecondParse = true
        }
        ret.then = rs.node
        ret.then.parent = ret
        i = rs.last_index
        // if (lexems[i] == keywords[4]) { // ELSE (one operator version)
        if (needsSecondParse) { // next generation version
            // rs = parseOp(lexems, i + 1) // one operator version
            rs = parseSeq(lexems, i, keywords[11]) // parse till fi
            ret.else = rs.node
            ret.else.parent = ret
            i = rs.last_index
        }
        return {
            node: ret,
            last_index: i
        }
    }
    
    // assume begins not from "DO"
    function parseDoWhile(lexems, i) {
        var ret = new dw(null)
        // parse till WHILE
        var res = parseSeq(lexems, i, keywords[6])
        ret.op = res.node
        ret.op.parent = ret
        i = res.last_index
        // find OD
        j = findNextKeyword(lexems, i, keywords[12])
        if (j == null) throw "No closing '" + keywords[12] +"' keyword for DO WHILE statement"
        ret.condition = parseExpr(lexems, i, j - 1)
        ret.condition.parent = ret
        return {
            node: ret,
            last_index: j + 1 // OD must be ignored!
        }
    }

    // assume begins not from "WHILE"
    function parseWhileDo(lexems, i) {
        var ret = new dw(null, true)
        // find DO
        var j = findNextKeyword(lexems, i, keywords[5])
        if (j == null) throw "No '" + keywords[5] + "' keyword found after '" + keywords[6] + "' statement."
        ret.condition = parseExpr(lexems, i, j - 1)
        ret.condition.parent = ret
        i = j + 1
        // parse till OD
        let res = parseSeq(lexems, i, keywords[12])
        ret.op = res.node
        ret.op.parent = ret
        return {
            node: ret,
            last_index: res.last_index
        }
    }

    // assume begins not from "FOR"
    function parseFor(lexems, i) {
        //throw "NOT IMPLEMENTED YET"
        // parse as decl till endword "TO"
        let to_idx = findNextKeyword(lexems, i, keywords[15]) // TO
        let downto_idx = findNextKeyword(lexems, i, keywords[16]) // or DOWNTO
        var downto = (downto_idx == null) ? false : (to_idx > downto_idx) ? false : true
        var result; // the declaration parse result
        if (downto) {
            result = parseDecl(lexems, i, keywords[16], types[2]) // parse till DOWNTO
        } else {
            result = parseDecl(lexems, i, keywords[15], types[2]) // parse till TO
        }

        var decl = result.node
        var j = result.last_index
        // parse as expr till "STEP" (if present)
        var j_step = findNextKeyword(lexems, j, keywords[17])
        var j_do = findNextKeyword(lexems, j, keywords[5])
        if (j_do == null) throw "Expected "+keywords[5]+" expression in " + keywords[7] + " expression."
        var to_expr = null
        var step_expr = null
        if (j_step != null && j_step < j_do) {
            // parse TO
            to_expr = parseExpr(lexems, j + 1, j_step - 1)
            j = j_step
            // parse STEP
            step_expr = parseExpr(lexems, j + 1, j_do - 1)
            j = j_do
        } else {
            // parse TO
            to_expr = parseExpr(lexems, j + 1, j_do - 1)
            j = j_do
        }
        // from here parse sequence till "ROF"
        var result = parseSeq(lexems, j + 1, keywords[13])
        var op_seq = result.node
        j = result.last_index
        // building return result

        var ret = new seq()
        ret.seq.push(decl)
        decl.parent = ret
        
        var do_while = new dw(ret, true) // interpret as while-do

        // ops[9].op -> ">="; ops[10].op -> "<="
        let comparison_op = downto ? ops[9].op : ops[10].op
        do_while.condition = new op(do_while, comparison_op)
        let getIterVar = function(parent) {
            let iter_var = new leaf_var(parent)
            iter_var.varname = decl.varname
            return iter_var
        }
        do_while.condition.lop = getIterVar(do_while.condition)
        do_while.condition.rop = to_expr
        do_while.condition.rop.parent = do_while.condition

        if (op_seq instanceof seq) {
            do_while.op = op_seq
            do_while.op.parent = do_while
        } else {
            do_while.op = new seq(do_while)
            do_while.op.seq.push(op_seq)
        }

        var ass = new assign(do_while.op)
        ass.varname = getIterVar(ass)
        ass.expr = new op(do_while.op, downto ? ops[4].op : ops[3].op)
        ass.expr.lop = getIterVar(ass.expr)
        if (step_expr) {
            ass.expr.rop = step_expr
        } else {
            ass.expr.rop = new leaf_const(ass.expr, type[2])
            ass.expr.rop.value = 1
        }

        do_while.op.seq.push(ass)
        
        ret.seq.push(do_while)
        return {
            node: ret,
            last_index: j
        }
    }
    
    // parsing function call
    // expecting parsing behind "FUNCTION" keyword
    function parseFunction(lexems, i) {
        var ret = new func_def(null)
        let isvar = checkVarname(lexems[i]) // check first lexem to be function name
        if (!isvar) {
            throw "Function name " + lexems[i] + " is invalid."
        }
        ret.func_name = lexems[i]
        if ( lexems [i + 1] != brackets[0]) {
            throw "Parameter list in function definition " + lexems[i] + " expected!";
        }
        let right_bracket = findBalancingBracketIndex(lexems, i + 1)
        if (right_bracket == -1)
            throw "No balancing bracket found for function declaration parameters"
        i = i + 2;
        var res;
        for (j = i; j <= right_bracket; j++) {
            switch (lexems[j]) {
            case '(': // skip bracketing
                j = findBalancingBracketIndex(lexems, j); // plus one during for!
                break;
            case ',':
                res = parseDecl(lexems, i, ',')
                ret.params.push(res.node)
                res.node.parent = ret
                j = res.last_index // plus one during for!
                i = j + 1
                break;
            case ')':
                res = parseDecl(lexems, i, ')')
                ret.params.push(res.node)
                res.node.parent = ret
                j = res.last_index // plus one during for!
                i = j + 1
                break;
            }
        }
        // check if there is defined return type of function
        ret.type = null
        if (lexems[i] == ":") {
            let type_id = types.indexOf(lexems[i + 1])
            if (type_id == -1)
                throw "Unknown return type of function declaration " + ret.func_name + "!"
            ret.type = lexems[i + 1]
            i = i + 2
        }
        // parse function body as usual -- as operator
        res = parseOp(lexems, i)
        ret.op = res.node
        ret.op.parent = ret
        return {
            node: ret,
            last_index: res.last_index
        }
        
    }

    // parsing program as a single operator
    return parseOp(lexems, 0) 
}

// returns the type of op result of two types
function type_collider(type1, type2) {
    return types[Math.max(types.indexOf(type1), types.indexOf(type2))]
}

// AST optimization (AST -- abstract syntax tree)
// builds semantic tree upon AST tree
// functions:
//  1. Constants pre-evaluations and automatic casting
//  2. Variable scope check and type inferrence according to declarations
//  3. Type inferrence of expressions and function calls
//  4. Tree nodes parentizing
// TODO: transefer typecasting to AST tree from assembly build
function semanticTreeBuilder(ast) {
    if (ast instanceof seq) {
        for (var k = 0; k < ast.seq.length; k++) {
            ast.seq[k] = semanticTreeBuilder(ast.seq[k])
            ast.seq[k].parent = ast
        }
        return ast
    } else if (ast instanceof ite) {
        ast.if = semanticTreeBuilder(ast.if)
        ast.then = semanticTreeBuilder(ast.then)
        ast.else = semanticTreeBuilder(ast.else)
        ast.if.parent = ast.then.parent = ast.else.parent = ast
        return ast
    } else if (ast instanceof dw) {
        ast.op = semanticTreeBuilder(ast.op)
        ast.condition = semanticTreeBuilder(ast.condition)
        ast.op.parent = ast.condition.parent = ast
        return ast
    } else if (ast instanceof decl) {
        ast.value = semanticTreeBuilder(ast.value)
        if (ast.value) ast.value.parent = ast
        // by the way -- check parent has type seq (this is the only allowed place to declare variables)
        if (!(ast.parent instanceof seq) && !(ast.parent instanceof func_def))
            throw "Variable declaration must appear in begin ... end statement, or as a function parameter!"
        return ast
    } else if (ast instanceof assign) {
        ast.varname = semanticTreeBuilder(ast.varname)
        ast.varname.parent = ast
        ast.expr = semanticTreeBuilder(ast.expr)
        ast.expr.parent = ast
        ast.type = ast.expr.type
        return ast
    } else if (ast instanceof op) {
        // here we need typecheck
        ast.lop = semanticTreeBuilder(ast.lop)
        ast.rop = semanticTreeBuilder(ast.rop)
        if (ast.lop == null && ast.rop == null)
            throw "Error: operand with no operands."
        // typechecking
        if (ast.lop == null)
            ast.type = ast.rop.type
        else if (ast.rop == null)
            ast.type = ast.lop.type
        else
            ast.type = type_collider(ast.lop.type, ast.rop.type)
        // no operation futher for no const
        if (!ast.constant) return ast;
        if (ast.lop == null || ast.rop == null ||
            !(ast.lop instanceof leaf_const) || !(ast.rop instanceof leaf_const))
            return ast; // these are not constants
        if (ast.rop == null)
            return ast.lop // actually, no action required
        if (ast.lop == null) {
            // unary calculations
            switch (ast.op_type) {
            case ops[4].op:
                if (ast.rop.type != "STRING") {
                    ast.rop.value = -ast.rop.value
                } else throw "Unary minus is not applicable to 'STRING' constant"
            case ops[3].op:
                return ast.rop
                break;
            case ops[11].op:
                if (ast.rop.type != "STRING") {
                    ast.rop.value = ast.rop.value == 0 ? 1 : 0
                } else throw "Unary 'NOT' is not applicable to 'STRING' constant"
                return ast.rop
                break;
            }
        }
        // from here starts binary check
        var resL = ast.lop.value
        var resR = ast.rop.value
        var resT = type_collider(ast.lop.type, ast.rop.type);
        var result;
        if (resT != "STRING") {
            switch (ops.findIndex(val=>val.op==ast.optype)) {
            case 0: result = resL*resR; break;
            case 1: result = resL/resR; break;
            case 2: result = resL%resR; break;
            case 3: result = resL+resR; break;
            case 4: result = resL-resR; break;
            case 5: result = resL==resR ? 1 : 0; break;
            case 6: result = resL!=resR ? 1 : 0; break;
            case 7: result = resL>resR ? 1 : 0; break;
            case 8: result = resL<resR ? 1 : 0; break;
            case 9: result = resL>=resR ? 1 : 0; break;
            case 10: result = resL<=resR ? 1 : 0; break;
            case 11: result = !resR ? 1 : 0; break;
            case 12: result = resL & resR; break;
            case 13: result = resL ^ resR; break;
            case 14: result = resL | resR; break;
            case 15: throw "Assign is not constant evaluation"
            }
            var ret = new leaf_const(ast.parent, resT);
            ret.value = (resT == "FLOAT") ? result : Math.round(result)
            return ret
        } else {
            var realResType = "INT" // or BYTE?
            switch (ops.findIndex(val=>val.op==ast.optype)) {
            case 0:
            case 1:
            case 2:
            case 4:
            case 11:
            case 12:
            case 13:
            case 14: throw "Illegal operator '" + ast.optype + "' on type 'STRING'"
            case 15: throw "Assign is not constant evaluation"
            
            case 3: result = String(resL)+String(resR); realResType = resT; break;
            case 5: result = String(resL)==String(resR) ? 1 : 0; break;
            case 6: result = String(resL)!=String(resR) ? 1 : 0; break;
            case 7: result = String(resL)>String(resR) ? 1 : 0; break;
            case 8: result = String(resL)<String(resR) ? 1 : 0; break;
            case 9: result = String(resL)>=String(resR) ? 1 : 0; break;
            case 10: result = String(resL)<=String(resR) ? 1 : 0; break;
            }
            var ret = new leaf_const(ast.parent, realResType);
            ret.value = result
            return ret
        }

    } else if (ast instanceof leaf_const) {
        return ast
    } else if (ast instanceof leaf_var) {
        // strategy: find nearest decl within begin ... end block (parent = seq)
        // decls also can be found in the function definition parameters!
        var parent = ast
        var child
        ast.call_stack_count = 0
        while (true) {
            var seq_to_check;
            child = parent // save old parent for order analysis
            parent = parent.parent
            while (true) {
                if (parent == null)
                    throw "No declaration found for variable '" + ast.varname + "'!"
                else if (parent instanceof seq) {
                    seq_to_check = parent.seq;
                    ast.local = false // variable declared in seq is not local
                    break;
                } else if (parent instanceof func_def) {
                    seq_to_check = parent.params;
                    ast.local = true // variable declared as func param resides in stack
                    ast.call_stack_count++ // this counts number of base pointer dereferences
                    break;
                }
                child = parent
                parent = parent.parent
            }
            // parent found, try to find in this scope declaration
            var idx = -1
            var repeat_idx = -1
            var child_index = seq_to_check.indexOf(child)
            if (parent instanceof func_def) // for function definition the arguments is always
                child_index = seq_to_check.length // goes afterwards the reference
            for (var k = 0; k < seq_to_check.length; k++) {
                if (seq_to_check[k] instanceof decl) {
                    let var_decl = seq_to_check[k]
                    if (var_decl.varname == ast.varname) {
                        if (repeat_idx == -1) {
                            repeat_idx = k
                            if (k < child_index)
                                idx = k // if it appeared earlier than the reference request, it's OK
                        }
                        else {
                            throw "Multiple declaration of variable " + ast.varname + " detected!"
                            // maybe we can be less strict and allow redefinitions?
                        }
                    }
                }
            }
            if (idx != -1) {
                ast.type = seq_to_check[idx].type // copy type property
                ast.ref = seq_to_check[idx] // also copy the reference to the found decl
                return ast // finally, found closest one
            }
        }
    } else if (ast instanceof func_call) {
        // basic one -- the first step is to semantically traverse parameters
        for (var k = 0; k < ast.params.length; k++) {
            ast.params[k] = semanticTreeBuilder(ast.params[k]);
            ast.params[k].parent = ast
        }
        // the same story as with leaf_var. Searching for local function definitions
        var parent = ast
        var child
        while (true) {
            child = parent // save old parent for order analysis
            parent = parent.parent
            if (parent == null) {
                // well, check the function call in externals instead
                // found this in assembly part. If no implementation found, throws error
                var asm_func = funcs.next(ast)
                break;
            }
            while (!(parent instanceof seq)) {
                child = parent
                parent = parent.parent
            }
            // parent found, try to find in this scope function definition
            var idx = -1
            var repeat_idx = -1
            var child_index = parent.seq.indexOf(child)
            for (var k = 0; k < parent.seq.length; k++) {
                if (parent.seq[k] instanceof func_def) {
                    // ast -> func_call, fdef -> func_def
                    let fdef = parent.seq[k]
                    let name_match = fdef.func_name == ast.func_name // check name is matching
                    // if name didn't match, just go further
                    if (!name_match) continue
                    var parameter_match = true // with parameters it could be much worse D:
                    // assume there WILL be parameters by default as in C language
                    var w = 0
                    for (;w < ast.params.length; w++) {
                        // assuming type was inferred earlier
                        let cbc = can_be_cast(fdef.params[w].type, ast.params[w].type)
                        if (!cbc) { // if cannot cast:
                            parameter_match = false
                            break;
                        }
                    }
                    // if parameter didn't match, just go further
                    if (!parameter_match) continue
                    if (repeat_idx == -1) {
                        repeat_idx = k
                        if (k <= child_index) { // also allow recursion!
                            idx = k // if it appeared earlier than the reference request, it's OK
                            // copy the rest of by-default parameters
                            for(;w < fdef.params.length;w++) {
                                if (fdef.params[w].value != null) {
                                    ast.params.push(fdef.params[w].value)
                                } else {
                                    throw "No default value for parameter" + fdef.params[w].varname + " in function " + fdef.func_name + " during function call!"
                                }
                            }
                        }
                    }
                    else {
                        throw "Multiple declaration of function " + ast.varname + " detected!"
                        // maybe we can be less strict and allow redefinitions?
                    }
                }
            }
            if (idx != -1) {
                ast.ref = parent.seq[idx] // copy the reference to the found func_def
                return ast // finally, found closest one
            }
        }
        return ast
    } else if (ast instanceof func_def) {
        // this is a combo of "dw", "decl" and "func_call"
        // decl part (function can be declared only within BEGIN...END statement)
        if (!(ast.parent instanceof seq))
            throw "Variable declaration must appear in begin ... end statement!"
        // func_call part

        // if there are nonzero params count, add them to the "op" scope
        // TODO is that really indeed needed? what about value passing!
        // with dispersed semantics it would be hard to recover context, what declaration
        // belongs to body, and what declaration belongs to passing parameters

        // make an extra seq to make variables visible
        for (var k = 0; k < ast.params.length; k++) {
            ast.params[k] = semanticTreeBuilder(ast.params[k]);
            ast.params[k].parent = ast
            ast.params[k].local =true // interpret as local parameter
            ast.params[k].call_stack_count = 1 // with stack depth 1
            if (!(ast.params[k] instanceof decl))
                throw "All passing parameters to function should be parameter declaration!"
        }
        // dw part (has body)
        ast.op = semanticTreeBuilder(ast.op)
        ast.op.parent = ast
        return ast
    }
    return ast
}
function astAssembly(ast) {
    // get explicit jump labels here
    var jump = {
        n: 0,
        next: function() {
            ret = this.n
            this.n++
            return "jmp" + ret
        }
    }
    // string consts can be also asquired here (int and float consts not needed)
    // needed to generate .data assembly section
    var consts = {
        n: 0,
        table: [],
        next: function(value) {
            let idx = this.table.findIndex(row=>row[1]==value) // search by value
            if (idx != -1) // table has already registered that constant
                return this.table[idx][0]
            // otherwize create one
            let key = "const" + this.n
            this.table.push([key, value])
            this.n++
            return key
        }
    }
    // variable table could be complicated to maintain due to scopes, et cetera...
    var vars = {
        n: 0,
        table: [],
        register: function(ast) { // should be instance of decl
            if (!(ast instanceof decl))
                throw "Nondeclarational statement registered as var"
            let idx = this.table.findIndex(item=>item.parent==ast.parent && item.varname== ast.varname)
            if (idx != -1)
                throw "Multiple declaration of variable " + ast.varname + " of the same code block."
            // register variable here
            ast.asm_name = "var" + this.n
            this.n++
            this.table.push(ast)
            return ast.asm_name
        }
    }

    // from here, services to use:
    //  1. jump.next() -- get unique jump label every call
    //  2. consts.next(const_value) -- registers "const_value" and returns assembly symbol name for constant
    //  3. vars -- managing variable table
    //    3.1. vars.register(decl) -- registers variable declaration and returns assembly alias
    //    3.2. vars.find(leaf_var) -- finds closest symbolic declaration and returns assembly alias
    //         !not used anymore! the reference saved during semantic analysis

    // assembly accumulator
    var assembly = []
    function pushStr(str) {assembly.push(str)}
    // traverse AST nodes to convert them into assembly code parts (the sacred moment of compilation)
    function traverseAST(ast) {
        // needed for casting!! (maybe needed in AST?)
        function cast(expr, type, register = "eax") { // cast expr to type
            let var_type = types.indexOf(type)
            let val_type = types.indexOf(expr.type)
            if (expr instanceof leaf_var)
                val_type = types.indexOf(expr.ref.type)
            if (var_type == val_type)
                return; // no cast operations required
            switch (var_type) {
            case 0:
                if (val_type == 1 || val_type == 2) {
                    pushStr("and " + register + ", 000000FFh") // WARNING
                } else if (val_type == 3) {
                    throw "Can't convert 'FLOAT' to 'BYTE'"
                } else if (val_type == 4) {
                    throw "Can't convert 'STRING' to 'BYTE'"
                }
                break;
            case 1:
                if (val_type == 2) {
                    pushStr("and " + register + ", 0000FFFFh") // WARNING
                } else if (val_type == 3) {
                    throw "Can't convert 'FLOAT' to 'WORD'"
                } else if (val_type == 4) {
                    throw "Can't convert 'STRING' to 'WORD'"
                }
                break;
            case 2:
                if (val_type == 3) {
                    throw "Can't convert 'FLOAT' to 'INT'"
                } else if (val_type == 4) {
                    throw "Can't convert 'STRING' to 'INT'"
                }
                break;
            case 3: 
                throw "FLOATS NOT SUPPORTED YET!!"
                break;
            case 4:
                if (val_type != 4) {
                    if (expr.constant && expr instanceof leaf_const) {
                        pushStr("mov " + register + "," + consts.next(String(expr.value)))
                    } else throw "Can't implicitly convert expression to 'STRING'"
                }
                break;
            default:
                throw "Unknown type detected"
            }
        }

        // seq, ite, dw, decl, assign, op, leaf_const, leaf_var, func_call
        if (ast instanceof seq) {
            for (var k = 0; k < ast.seq.length; k++) {
                traverseAST(ast.seq[k]) // ez
            }
        } else if (ast instanceof ite) {
            traverseAST(ast.if);
            pushStr("cmp eax,0")
            if (ast.else == null) {
                let jmp = jump.next()
                pushStr("jz " + jmp)
                traverseAST(ast.then)
                pushStr(jmp + ":")
            } else {
                let jmp1 = jump.next()
                let jmp2 = jump.next()
                pushStr("jz " + jmp1)
                traverseAST(ast.then)
                pushStr("jmp " + jmp2)
                pushStr(jmp1 + ":")
                traverseAST(ast.else)
                pushStr(jmp2 + ":")
            }
        } else if (ast instanceof dw) {
            let begin_jmp = jump.next()
            var cond_jmp
            if (ast.precondition) {
                cond_jmp = jump.next()
                pushStr("jmp " + cond_jmp)
            }
            pushStr(begin_jmp + ":")
            traverseAST(ast.op)
            if (ast.precondition)
                pushStr(cond_jmp + ":")
            traverseAST(ast.condition)
            //pushStr("pop eax")
            pushStr("cmp eax,0")
            pushStr("jnz " + begin_jmp)
        } else if (ast instanceof decl) {
            let asm_varname = vars.register(ast)
            if (ast.value == null)
                return // no value compilation needed
            let var_type =  types.indexOf(ast.type)
            // TODO check type needed in semantic tree! here is to late!
            traverseAST(ast.value)
            cast(ast.value, ast.type)
            switch (var_type) {
            case 0: pushStr("mov byte [" + asm_varname + "], al"); break;
            case 1: pushStr("mov word [" + asm_varname + "], ax"); break;
            case 2: pushStr("mov dword [" + asm_varname + "], eax"); break;
            case 3: throw "FLOATS NOT SUPPORTED YET!!"; break;
            case 4: pushStr("mov dword [" + asm_varname + "], eax"); break;
            default: throw "Unknown type detected"
            }
        } else if (ast instanceof assign) {
            let var_decl = ast.varname.ref
            let var_type =  types.indexOf(var_decl.type)
            // !!! earlier it was "ast.type", and it was fatal error
            // after moving double word expression into BYTE variable, it affected
            // neibourhood variables. Critical error found by accident

            // TODO there is no check in AST for types!
            traverseAST(ast.expr)
            cast(ast.expr, ast.type)
            if (!var_decl.local) {
                let asm_varname = var_decl.asm_name
                switch (var_type) {
                case 0: pushStr("mov byte [" + asm_varname + "], al"); break;
                case 1: pushStr("mov word [" + asm_varname + "], ax"); break;
                case 2: pushStr("mov dword [" + asm_varname + "], eax"); break;
                case 3: throw "FLOATS NOT SUPPORTED YET!!"; break;
                case 4: pushStr("mov dword [" + asm_varname + "], eax"); break;
                default: throw "Unknown type detected"
                }
            } else {
                let param_index = var_decl.parent.params.indexOf(var_decl) // weird
                let param_offset = 4*(param_index + 2)
                if (var_decl.call_stack_count == 1) {
                    pushStr("mov [ebp + " + param_offset + "],eax")
                } else if (var_decl.call_stack_count == 2) {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov [ebx + " + param_offset + "],eax")
                } else if (var_decl.call_stack_count == 3) {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov ebx,[ebx]")
                    pushStr("mov [ebx + " + param_offset + "],eax")
                } else {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov ecx, " + String(var_decl.call_stack_count - 2))
                    let jmp = jump.next()
                    pushStr(jmp + ":")
                    pushStr("mov ebx,[ebx]")
                    pushStr("loop " + jmp)
                    pushStr("mov [ebx + " + param_offset + "],eax")
                }
            }
        } else if (ast instanceof op) { // here we end x_x, rip
            let type = types.indexOf(ast.type)
            let op_type = ops.findIndex(row=>row.op==ast.optype)
            if (ast.lop == null) { // unary operator
                switch (ast.optype) {
                case ops[4].op:
                    if (type <= 2) {
                        traverseAST(ast.rop)
                        cast(ast.rop, ast.type)
                        pushStr("mov ebx,0")
                        pushStr("sub ebx,eax")
                        pushStr("xchg eax,ebx")
                    } else throw "FLOAT IS NOT SUPPORTED"
                    break;
                case ops[3].op: // needs to be here to generate assembly code
                    traverseAST(ast.rop)
                    cast(ast.rop, ast.type)
                    break;
                case ops[11].op:
                    if (type <= 2) {
                        traverseAST(ast.rop)
                        cast(ast.rop, ast.type)
                        pushStr("xor eax, 1")
                    } else throw "FLOAT IS NOT SUPPORTED"
                    break;
                }
                return;
            }
            // TODO this is bad, typecasting needs separate node + extra analysis in AST
            traverseAST(ast.rop)
            cast(ast.rop, ast.type)
            pushStr("push eax")
            traverseAST(ast.lop)
            cast(ast.lop, ast.type)
            pushStr("pop ebx")
            if (type == 3)
                throw "FLOAT IS NOT SUPPORTED"
            else if (type >= 4)
                throw "ASSEMBLY ERROR ('STRING' multiplication)"
            
            switch (op_type) {
            case 0: // *
                switch(type) {
                    case 0: pushStr("mul bl"); break;
                    case 1: pushStr("mul bx"); break;
                    case 2: pushStr("mul ebx"); break;
                }
                break;
            case 1: // /
                pushStr("mov edx,0")
                switch(type) {
                    case 0:
                        pushStr("div bl"); 
                        pushStr("mov ah,0");
                    break;
                    case 1: pushStr("div bx"); break;
                    case 2: pushStr("div ebx"); break;
                }
                break;
            case 2: // %
                pushStr("mov edx,0")
                switch(type) {
                    case 0:
                        pushStr("div bl"); 
                        pushStr("mov al,0");
                        pushStr("shr eax, 8");
                    break;
                    case 1:
                        pushStr("div bx");
                        pushStr("mov eax,edx");
                    break;
                    case 2:
                        pushStr("div ebx");
                        pushStr("mov eax,edx");
                    break;
                }
                break;
            case 3: // +
                switch(type) {
                    case 0: pushStr("add al,bl"); break;
                    case 1: pushStr("add ax,bx"); break;
                    case 2: pushStr("add eax,ebx"); break;
                }
                break;
            case 4: // -
                switch(type) {
                    case 0: pushStr("sub al,bl"); break;
                    case 1: pushStr("sub ax,bx"); break;
                    case 2: pushStr("sub eax,ebx"); break;
                }
                break;
            case 5: // ==
            case 6: // !=
            case 7: // >
            case 8: // <
            case 9: // >=
            case 10: // <=
                pushStr("cmp eax, ebx")
                let jmp1 = jump.next()
                let jmp2 = jump.next()
                switch (op_type) {
                case 5: pushStr("jz " + jmp1); break;
                case 6: pushStr("jnz " + jmp1); break;
                case 7: pushStr("jg " + jmp1); break;
                case 8: pushStr("jl " + jmp1); break;
                case 9: pushStr("jnl " + jmp1); break;
                case 10: pushStr("jng " + jmp1); break;
                }
                pushStr("mov eax,0")
                pushStr("jmp " + jmp2)
                pushStr(jmp1 + ":")
                pushStr("mov eax,1")
                pushStr(jmp2 + ":")
                break;
            //case 11: // NOT
            case 12: // AND
            switch(type) {
                case 0: pushStr("and al,bl"); break;
                case 1: pushStr("and ax,bx"); break;
                case 2: pushStr("and eax,ebx"); break;
            }
            break;
            case 13:  // OR
            switch(type) {
                case 0: pushStr("or al,bl"); break;
                case 1: pushStr("or ax,bx"); break;
                case 2: pushStr("or eax,ebx"); break;
            }
            break;
            case 14: // XOR
            switch(type) {
                case 0: pushStr("xor al,bl"); break;
                case 1: pushStr("xor ax,bx"); break;
                case 2: pushStr("xor eax,ebx"); break;
            }
            break;
            case 15: // :=
            throw "ASSEMBLY ERROR! WRONG ASSIGN APPEAR"
            default: throw "UNKNOWN ASSEMBLY OPERATION!!!"
            }

        } else if (ast instanceof leaf_const) {
            let type = types.indexOf(ast.type)
            switch (type) {
            case 0:
            case 1: 
            case 2: pushStr("mov eax, " + ast.value); break;
            case 3:  throw "FLOAT OPERATIONS NOT IMPLEMENTED YET"; //pushStr("mov dd eax," + ast.value); break;
            case 4:  pushStr("mov eax," + consts.next(ast.value)); break; // STRING
            }
        } else if (ast instanceof leaf_var) {
            let var_decl = ast.ref
            let type = types.indexOf(var_decl.type)
            if (!ast.local) {
                let asm_varname = var_decl.asm_name
                switch (type) {
                case 0:
                    pushStr("mov eax,0")
                    pushStr("mov byte al,["+asm_varname+"]")
                    break;
                case 1: 
                    pushStr("mov eax,0")
                    pushStr("mov word ax,["+asm_varname+"]")
                    break;
                case 2:
                    pushStr("mov dword eax,["+asm_varname+"]")
                    break;
                case 3: throw "FLOATS NOT SUPPORTED YET!!"; break;
                case 4: 
                    pushStr("mov dword eax,"+asm_varname+"")
                    break; // STRING (it's just pointer to string)
                }
            } else {
                let param_index = ast.ref.parent.params.indexOf(ast.ref) // weird
                let param_offset = 4*(param_index + 2)
                if (ast.call_stack_count == 1) {
                    pushStr("mov eax,[ebp + " + param_offset + "]")
                } else if (ast.call_stack_count == 2) {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov eax,[ebx + " + param_offset + "]")
                } else if (ast.call_stack_count == 3) {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov ebx,[ebx]")
                    pushStr("mov eax,[ebx + " + param_offset + "]")
                } else {
                    pushStr("mov ebx,[ebp]")
                    pushStr("mov ecx, " + String(ast.call_stack_count - 2))
                    let jmp = jump.next()
                    pushStr(jmp + ":")
                    pushStr("mov ebx,[ebx]")
                    pushStr("loop " + jmp)
                    pushStr("mov eax,[ebx + " + param_offset + "]")
                }
                //throw "Local variables dereferences are not implemented yet!"
            }

        } else if (ast instanceof func_call) {
            // if there is reference to defined function, register it
            if (ast.ref && ast.ref.asm_name == null)
                funcs.register(ast.ref)
            if (!ast.ref)
                funcs.next(ast)
            // traverse arguments in reverse, then call function
            for (k = ast.params.length - 1; k >= 0; k--) {
                traverseAST(ast.params[k])
                // we can add cast here!
                // TODO make the same for external symbol!!
                if (ast.ref) {
                    cast(ast.params[k], ast.ref.params[k].type)
                } else if (ast.ext_ref) {
                    cast(ast.params[k], ast.ext_ref[1][k])
                }
                pushStr("push eax")
            }
            // problem -- we need to somehow cast variables to suitable variables!
            // because of multiple pushes, the only way -- AST modification
            var asm_func = ast.ref ? ast.ref.asm_name:ast.ext_ref[3];
            pushStr("call " + asm_func) // call function with pushed parameters
        }
    }

    traverseAST(ast) // begin with traversing root node
    pushStr("push dword 0") 
    pushStr("call @quit_program") // add program finish
    // then traverse all funcs
    var new_traversables
    do {
        new_traversables = false
        //funcs.table // array of func_def
        for (let i = 0; i < funcs.table.length; i++) {
            var fd = funcs.table[i]
            if (!fd.traversed) {
                fd.traversed = true
                new_traversables = true
                pushStr(fd.asm_name + ":") 
                pushStr("push ebp")
                pushStr("mov ebp,esp")
                traverseAST(fd.op)
                pushStr("pop ebp")
                let offset = 4*(fd.params.length + 1)
                pushStr("add esp, " + offset)
                pushStr("jmp [esp - " + offset + "]")
            }
        }
    } while (new_traversables)
    return [assembly, consts, vars, funcs]
}

// the very last step
function assembleCode(snippet_path, asm_path, assembly, consts, vars) {
    var snippet = String(fs.readFileSync(new URL('file:///' + snippet_path))).split('\n');
    const const_table_tag = "<<[ASSEMBLY_CONST_SECTION]>>"
    const var_table_tag = "<<[ASSEMBLY_VAR_SECTION]>>"
    const main_table_tag = "<<[ASSEMBLY_MAIN_SECTION]>>"
    var const_section = []
    var var_section = []
    // consts.table // [[key0, value0], [key1, value1], ...]
    // now consts are only string (and maybe it always be the same)
    for (var k = 0; k < consts.table.length; k++) {
        let key = consts.table[k][0]
        let value = consts.table[k][1]
        const_section.push(key + " db '" + value + "', 0x00")
    }
    // vars.table // [decl1, decl2, ...]
    for (var k = 0; k < vars.table.length; k++) {
        let ast = vars.table[k]
        if (!(ast instanceof decl))
            throw "Nondeclarational statement registered as var"
        switch(types.findIndex(t=>t==ast.type)) {
        case 0: var_section.push(ast.asm_name + ": RESB 1"); break;
        case 1: var_section.push(ast.asm_name + ": RESW 1"); break;
        case 2: var_section.push(ast.asm_name + ": RESD 1"); break;
        case 3: var_section.push(ast.asm_name + ": RESD 1"); break;
        case 4: var_section.push(ast.asm_name + ": RESB 5"); break; // LUL
        }
    }
    // sections built, time to merge them
    function modify (str_src_array, tag, str_replace_array) {
        let idx = str_src_array.findIndex(row=>row.includes(tag))
        str_src_array.splice(idx, 1) // remove
        for (var k = str_replace_array.length-1; k >= 0; k--)
            str_src_array.splice(idx, 0, str_replace_array[k]) // insert one by one
    }
    modify (snippet, const_table_tag, const_section)
    modify (snippet, var_table_tag, var_section)
    modify (snippet, main_table_tag, assembly)
    // then save to file!
    fs.writeFile(asm_path, snippet.join('\n'), 'utf8', (err) => {
        if (err) throw err;
        console.log('The .asm file has been saved!');
      });
}

// read source file
var source = String(fs.readFileSync(new URL('file:///' + folder_path + file_name + src_ext)));
// find lexems
lexems = lexer(source) // works pretty well
// building abstract syntax tree
var wtf = astBuilder(lexems);
// reducing constants + type infer
wtf = semanticTreeBuilder(wtf.node);
// variable table, scopes, constant table (+ errors)
// TODO
// compilation of semantic tree into assembly code (vartable + consttable + program)
var assembly = astAssembly(wtf)
// then finally IO operations on snippet and so on
assembleCode(assembly_snippet_path,
             folder_path + file_name + asm_ext
    , assembly[0], assembly[1], assembly[2])
// call linker to link up
// https://stackoverflow.com/questions/5775088/how-to-execute-an-external-program-from-within-node-js
var spawn = require('child_process').spawn;
var path = require('path')
var prc = spawn(path.basename(linker_path), [folder_path, file_name], {cwd: path.dirname(linker_path)});
//noinspection JSUnresolvedFunction
prc.stdout.setEncoding('utf8');
var printer = function (data) {
    var str = data.toString()
    var lines = str.split(/(\r?\n)/g);
    console.log(lines.join(""));
};
prc.stdout.on('data', printer);
prc.stderr.on('data', printer);

prc.on('close', function (code) {
    // check linker return code
    if (code == 1) {
        console.log('Error during linking program!')
    } else {
        console.log('Program build success!')
    }
});


var nop = 0
