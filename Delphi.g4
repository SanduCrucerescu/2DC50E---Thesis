grammar Delphi;

/*
 * Sonar Delphi Plugin
 * Copyright (C) 2010 SonarSource
 * dev@sonar.codehaus.org
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02

 * Updated from above original ANTLR3 to ANTLR4 by Jens Gotthardsen
 * https://github.com/gotthardsen/Delphi-ANTRL4-Grammar
 */

@lexer::namespace{DelphiGrammar}
@parser::namespace{DelphiGrammar}
//****************************
//section start
//****************************
file                         : program | library | unit | packageE
                             ;
//****************************
//section fileDefinition
//****************************

program                      : (programHead)? (usesFileClause)? block '.'
                             ;
programHead                  : 'program' namespaceName (programParmSeq)? ';'
                             ;
programParmSeq               : '(' (ident (',' ident)* )? ')'
                             ;

// ---------------

library                      : libraryHead (usesFileClause)? block '.'
                             ;
libraryHead                  : 'library' namespaceName (hintingDirective)* ';'
                             ;

// ---------------

packageE                     : packageHead requiresClause (containsClause)? 'end' '.'
                             ;
packageHead                  : 'package' namespaceName ';'
                             ;
// ---------------

unit                         : unitHead unitInterface unitImplementation unitBlock '.'
                             ;
unitHead                     : 'unit' namespaceName (hintingDirective)* ';'
                             ;
unitInterface                : 'interface' (usesClause)? (interfaceDecl)*
                             ;
unitImplementation           : 'implementation' (usesClause)? (declSection)*
                             ;
unitBlock                    : unitInitialization 'end'
                             | compoundStatement
                             | 'end'
                             ;
unitInitialization           : 'initialization' statementList (unitFinalization)?
                             ;
unitFinalization             : 'finalization' statementList
                             ;
//****************************
//section fileUsage
//****************************
containsClause               : 'contains' namespaceFileNameList
                             ;
requiresClause               : 'requires' namespaceNameList
                             ;
usesClause                   : 'uses' namespaceNameList
                             ;
usesFileClause               : 'uses' namespaceFileNameList
                             ;
namespaceFileNameList        : namespaceFileName (',' namespaceFileName)* ';'
                             ;
namespaceFileName            : namespaceName ('in' QuotedString)?
                             ;
namespaceNameList            : namespaceName (',' namespaceName)* ';'
                             ;
//****************************
//section declaration
//****************************
block                        : (declSection)* (blockBody)?
                             ;
blockBody                    : compoundStatement
                             | assemblerStatement
                             ;
declSection                  : labelDeclSection
                             | constSection
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | methodDecl
                             | procDecl
                             | exportsSection
                             ;
interfaceDecl                : procDecl
                             | methodDecl
                             | typeSection
                             | varSection
                             | exportedProcHeading
                             | exportsSection
                             | constSection
                             ;
labelDeclSection             : 'label' label (',' label)* ';'
                             ;
constSection                 : constKey (constDeclaration)*
                             ;
constKey                     : 'const'
                             | 'resourcestring'
                             ;
constDeclaration             : (customAttribute)? ident (':' typeExpr )? '=' constExpression (hintingDirective)* ';'
                             ;
typeSection                  : 'type' typeDeclaration (typeDeclaration)*
                             ;
typeDeclaration              : (customAttribute)? ident (genericDefinition)? '=' typeExpr  (hintingDirective)* ';'
                             ;
varSection                   : varKey varDeclaration (varDeclaration)*
                             ;
varKey                       : 'var'
                             | 'threadvar'
                             ;
varDeclaration               : (customAttribute)? identList ':' typeExpr  (varValueSpec)? (hintingDirective)* ';'
                             ;
varValueSpec                 : 'absolute' ident
                             | 'absolute' constExpression
                             | '=' constExpression
                             ;
exportsSection               : 'exports' ident exportItem (',' ident exportItem)* ';'
                             ;
exportItem                   : ('(' (formalParameterList)? ')')? (INDEX expression)? (NAME expression)? ('resident')?
                             ;
//****************************
//section type
//****************************
typeExpr                     : structuredType
                             | pointerType
                             | stringType
                             | procedureType
                             | variantType
                             | ('type')? genericType
                             | simpleType
                             ;
genericType                  : typeId (genericPostfix)?
                             ;
structuredType               : ('packed')? structuredTypePart
                             ;
structuredTypePart           : arrayType
                             | setType
                             | fileType
                             | classDecl
                             ;

arrayType                    :  'array' ('[' (arrayIndex)? (',' (arrayIndex)?)* ']')? 'of' arraySubType
                             ;

arrayIndex                   : typeId
                             | expression '..' expression
                             ;

arraySubType                 : 'const'
                             | typeExpr 
                             ;
setType                      : 'set' 'of' typeExpr 
                             ;
fileType                     : 'file' ('of' typeExpr )?
                             ;
pointerType                  : '^' typeExpr 
                             | 'pointer'
                             ;
stringType                   : 'string' ('[' expression ']')?
                             | ('type')? ANSISTRING (codePageNumber)?
                             ;
codePageNumber               : '(' intNum ')'
                             ;
procedureType                : methodType
                             | simpleProcedureType
                             | procedureReference
                             ;
methodType                   : procedureTypeHeading 'of' 'object'
                             ;
simpleProcedureType          : procedureTypeHeading ( (';')? callConventionNoSemi)?
                             ;
procedureReference           : 'reference' 'to' procedureTypeHeading
                             ;
procedureTypeHeading         : 'function' (formalParameterSection)? ':' (customAttribute)? typeExpr 
                             | 'procedure' (formalParameterSection)?
                             ;
variantType                  : 'variant' // SzJ TODO TEMP
                             ;
simpleType                   : ident
                             | subRangeType
                             | enumType
                             ;
subRangeType                 : constExpression ('..' constExpression)?
                             ;
enumType                     : '(' enumVariant (',' enumVariant )* ')'
                             ;
enumVariant                  : ident ('=' expression)?
                             ;
//typeId                       : namespacedQualifiedIdent
//                             ;
//****************************
//section generics
//****************************
genericTypeIdent             : qualifiedIdent (genericDefinition)?
                             ;
genericDefinition            : simpleGenericDefinition
                             | constrainedGenericDefinition
                             ;
simpleGenericDefinition      : '<' ident (',' ident)* '>'
                             ;
constrainedGenericDefinition : '<' constrainedGeneric (';' constrainedGeneric)* '>'
                             ;
constrainedGeneric           : ident (':' genericConstraint (',' genericConstraint)*)?
                             ;
genericConstraint            : ident
                             | ( 'record' | 'class' | 'constructor' )
                             ;
genericPostfix               : '<' typeExpr  (',' typeExpr )* '>'
                             ;
//****************************
//section class
//****************************
classDecl                    : classTypeTypeDecl
                             | classTypeDecl
                             | classHelperDecl
                             | interfaceTypeDecl
                             | objectDecl
                             | recordDecl
                             | recordHelperDecl
                             ;
classTypeTypeDecl            : 'class' 'of' typeId
                             ;
classTypeDecl                : 'class' (classState)? (classParent)? (classItem)* 'end'
                             | 'class' (classParent)?
                             ;
classState                   : 'sealed'
                             | 'abstract'
                             ;
classParent                  : '(' genericType (',' genericType)* ')'    //CHANGEd from typeId to classParentId
                             ;
classItem                    : visibility
                             | classMethod
                             | classProperty
                             | constSection
                             | typeSection
                             | classField
                             | ('class')? varSection
                             ;
classHelperDecl              : 'class' 'helper' (classParent)? 'for' typeId (classHelperItem)* 'end' //CHANGED, we only need "for" class name
                             ;
classHelperItem              : visibility
                             | classMethod
                             | classProperty
                             | ('class')? varSection
                             ;
interfaceTypeDecl            : interfaceKey (classParent)? (interfaceGuid)? (interfaceItem)* 'end'
                             | interfaceKey (classParent)?
                             ;
interfaceKey                 : 'interface'
                             | 'dispinterface'
                             ;
interfaceGuid                : '[' QuotedString ']'
                             ;
interfaceItem                : classMethod
                             | ('class')? classProperty
                             ;
objectDecl                   : 'object' (classParent)? (objectItem)* 'end'
                             ;
objectItem                   : visibility
                             | classMethod
                             | classField
                             ;
recordDecl                   : simpleRecord
                             | variantRecord
                             ;
simpleRecord                 : 'record' (recordField)* (recordItem)* 'end'
                             ;
variantRecord                : 'record' (recordField)* recordVariantSection 'end'
                             ;
recordItem                   : classMethod
//                             | visibility
                             | classProperty
                             | constSection
                             | typeSection
                             | recordField
                             | ('class')? varSection
                             ;
recordField                  : identList ':' typeExpr  (hintingDirective)* (';')?
                             ;
recordVariantSection         : 'case' (ident ':')? typeExpr  'of' (recordVariant | ';')+
                             ;
recordVariant                : constExpression (',' constExpression)* ':' '(' (recordVariantField)* ')'
                             ;
recordVariantField           : identList ':' typeExpr  (hintingDirective)* (';') ?
                             ;
recordHelperDecl             : 'record' 'helper' 'for' typeId (recordHelperItem)* 'end'
                             ;
recordHelperItem             : classMethod
//                             | visibility
                             | classProperty
                             ;
classMethod                  : (customAttribute)? ('class')? methodKey genericTypeIdent (formalParameterSection)? ';' (methodDirective)*
                             | (customAttribute)? ('class')? 'function' genericTypeIdent (formalParameterSection)? ':' (customAttribute)? typeExpr  ';' (methodDirective)*
                             | (customAttribute)? ('class')? 'operator' genericTypeIdent (formalParameterSection)? ':' (customAttribute)? typeExpr  ';'
                             ;
classField                   : (customAttribute)? identList ':' typeExpr  ';' (hintingDirective)*
                             ;
classProperty                : (customAttribute)? ('class')? 'property' ident (classPropertyArray)? (':' genericTypeIdent) (classPropertyIndex)? (classPropertyReadWrite)* ';'
//                             (classPropertyEndSpecifier)* NOT SUPPORTED
                             ;
classPropertyArray           : '[' formalParameterList ']'
                             ;
classPropertyIndex           : 'index' expression (';')?
                             ;
classPropertyReadWrite       : 'read' ident ('[' expression ']')?
                             | 'write' ident ('[' expression ']')?
                             ;
classPropertyDispInterface   : 'readonly' ';'
                             | 'writeonly' ';'
                             | dispIDDirective
                             ;
visibility                   : (STRICT)? 'protected'
                             | (STRICT)? 'private'
                             | ( 'public'
                             | 'published'
                             | 'automated' )     // win32 deprecated
                             ;
                               // NOT SUPPORTED
//classPropertySpecifier       : classPropertyReadWrite
//                             | classPropertyDispInterface
//                             | STORED expression 
//                             | 'default' expression
//                             | ( 'default'
//                             | 'nodefault' )
//                             | IMPLEMENTS typeId
//                             ;
//classPropertyEndSpecifier    : STORED expression ';'
//                             | 'default' expression ';'
//                             | 'default' ';'
//                             | 'nodefault' ';'
//                             ;
//****************************
//section procedure
//****************************
exportedProcHeading          : 'procedure' ident (formalParameterSection)? ':' (customAttribute)? typeExpr  ';' (functionDirective)*
                             | 'function' ident (formalParameterSection)? ';' (functionDirective)*
                             ;
methodDecl                   : methodDeclHeading (methodBody)?
                             ;
methodDeclHeading            : (customAttribute)? ('class')?  methodKey methodName (formalParameterSection)? ';' (methodDirective)* 
                             | (customAttribute)? ('class')? 'function' methodName (formalParameterSection)? (':' (customAttribute)? typeExpr )? ';' (methodDirective)* 
                             | (customAttribute)? 'class' 'operator' methodName (formalParameterSection)? (':' (customAttribute)? typeExpr )? ';' 
                             ;
methodKey                    : 'procedure'
                             | 'constructor'
                             | 'destructor'
                             ;
//methodName                   : ident (genericDefinition)? ('.' ident (genericDefinition)?)? '.' ident (genericDefinition)?
methodName                   : genericTypeIdent ('.' genericTypeIdent)*
                             ;
procDecl                     : procDeclHeading (procBody)?
                             ;
procDeclHeading              : (customAttribute)? 'procedure' ident (formalParameterSection)? ';' (functionDirective)* 
                             | (customAttribute)? 'function' ident (formalParameterSection)? ':' typeExpr ';' (functionDirective)* 
                             ;
formalParameterSection       : '(' (formalParameterList)? ')'
                             ;
formalParameterList          : formalParameter (';' formalParameter)*
                             ;
formalParameter              : //(customAttribute)?
                               (parmType)? identList (':' typeExpr )? ('=' expression)?
                             ;
parmType                     : 'const'
                             | 'var'
                             | 'out'
                             ;
methodBody                   : block ';'
                             ;
procBody                     : 'forward' ';' (functionDirective)* 
                             | 'external' ('name' expression | 'index' expression)* (functionDirective)*
                             | block ';'
                             ;
//****************************
//section customAttributes
//****************************
customAttribute              : 'abekat' //customAttributeList
                             ;
customAttributeList          : (customAttributeDecl)*
                             ;
customAttributeDecl          : '[' typeId ('(' (expressionList)? ')')? ']'
                             ;

//****************************
//section expression
//****************************
expression                   : anonymousExpression
                             | simpleExpression (relOp simpleExpression)*
                             ;
anonymousExpression          : 'procedure' (formalParameterSection)? block
                             | 'function' (formalParameterSection)? ':' typeExpr  block
                             ;
simpleExpression             : factor (operator factor)*
                             ;
factor                       : '@' factor
                             | DOUBLEAT factor
                             | 'not' factor
                             | '+' factor
                             | '-' factor
                             | '^' ident
                             | intNum
                             | realNum
                             | ( TkAsmHexNum 
                             | 'true'
                             | 'false'
                             | 'nil' )
                             | '(' expression ')' ('^')? ('.' expression)?
                             | stringFactor
                             | setSection
                             | designator
                             | typeId '(' expression ')' 
                             ;
                             
stringFactor                 : ControlString (QuotedString ControlString)* (QuotedString)?
                             | QuotedString (ControlString QuotedString)* (ControlString)?
                             ;
setSection                   : '[' (expression ((',' | '..') expression)*)? ']'
                             ;

designator                   : ('inherited')? typeId? (designatorItem)*
                             ;
designatorItem               : '^'
                             | ('.' | '@') ident
                             | ('<' genericTypeIdent (',' genericTypeIdent)* '>')
                             | '[' expressionList ']'
                             | '(' (expression (colonConstruct)? (',' expression (colonConstruct)?)*)? ')'
                             ;
expressionList               : expression (',' expression)*
                             ;
colonConstruct               : ':' expression 
                             // (':' expression)? 
                             // Removed the second part and just have `: expression`
                             // Have to dive deeper into Delphi syntax and find out what this is actually for
                             // Furthermore, I do not see anything handling named parameters
                             ;
operator                     : '+'
                             | '-'
                             | 'or'
                             | 'xor'
                             | '*'
                             | '/'
                             | 'div'
                             | 'mod'
                             | 'and'
                             | 'shl'
                             | 'shr'
                             | 'as'
                             ;
relOp                        : '<'
                             | '>'
                             | '<='
                             | '>='
                             | '<>'
                             | '='
                             | 'in'
                             | 'is'
                             ;
//****************************
//section statement
//****************************

statement                    : ifStatement
                             | caseStatement
                             | repeatStatement
                             | whileStatement
                             | forStatement
                             | withStatement
                             | tryStatement
                             | raiseStatement
                             | assemblerStatement
                             | compoundStatement
                             | label ':' statement
                             | simpleStatement
                             ;
ifStatement                  : 'if' expression 'then' statement ('else' statement)?
                             ;
caseStatement                : 'case' expression 'of' (caseItem)* ('else' statementList (';')?)? 'end'
                             ;
caseItem                     : caseLabel (',' caseLabel)* ':' statement (';')? 
                             ;
caseLabel                    : expression ('..' expression)?
                             ;
repeatStatement              : 'repeat' (statementList)? 'until' expression
                             ;
whileStatement               : 'while' expression 'do' statement
                             ;
forStatement                 : 'for' ident ':=' expression 'to' expression 'do' statement
                             | 'for' ident ':=' expression 'downto' expression 'do' statement
                             | 'for' ident 'in' expression 'do' statement
                             ;
withStatement                : 'with' withItem 'do' statement
                             ;
withItem                     : designator 'as' designator
                             | designator (',' designator)*
                             ;
compoundStatement            : 'begin' (statementList)? 'end'
                             ;
//statementList                : (statement)? (';' (statement)?)*
statementList                : (statement ';')+
                             ;
simpleStatement              : designator ':=' expression
                             | designator // call
                             | gotoStatement
                             ;
gotoStatement                : 'goto' label
                             | 'exit' ('(' expression ')')?
                             | ( 'break'
                             | 'continue' )
                             ;
//****************************
//section constExpression
//****************************
constExpression              : '(' recordConstExpression (';' recordConstExpression)* ')'
                             | '(' constExpression (',' constExpression)* ')'
                             | expression
                             ;
recordConstExpression        : ident ':' constExpression
                             ;
//****************************
//section exceptionStatement
//****************************
tryStatement                 : 'try' (statementList)? 'except' handlerList 'end'
                             | 'try' (statementList)? 'finally' (statementList)? 'end'
                             ;
handlerList                  : (handler)* ('else' statementList)?
                             | statementList
                             ;
handler                      : 'on' (handlerIdent)? typeId 'do' handlerStatement
                             ;
handlerIdent                 : ident ':'
                             ;
handlerStatement             : statement (';')?
                             | ';'
                             ;
raiseStatement               : 'raise' (designator)? (AT designator)? 
                             ;
//****************************
//section AssemblerStatement
//****************************
assemblerStatement           : 'asm' ~('end')* 'end'
                             ;
//****************************
//section directive
//****************************
methodDirective              : reintroduceDirective         // 1
                             | overloadDirective            // 2
                             | bindingDirective             // 3
                             | abstractDirective            // 3 virtual;
                             | inlineDirective              // 4 
                             | callConvention               // 4
                             | hintingDirective ';'         // 4 
                             | oldCallConventionDirective   // 1
                             | dispIDDirective
                             ;
functionDirective            : overloadDirective          // 1
                             | inlineDirective            // 1
                             | callConvention             // 1
                             | oldCallConventionDirective // 1
                             | hintingDirective ';'      // 1
                             | (callConventionNoSemi)? externalDirective          // 1
                             | 'unsafe' ';'              // 1
                             ;
reintroduceDirective         : 'reintroduce' ';'
                             ;
overloadDirective            : 'overload' (';')?
                             ;
bindingDirective             : 'message' expression ';'
                             | 'static' ';'
                             | 'dynamic' ';'
                             | 'override' ';'
                             | 'virtual' ';'
                             ;
abstractDirective            : 'abstract' ';'
                             | 'final' ';'
                             ;
inlineDirective              : 'inline' ';'
                             | 'assembler' ';' // deprecated
                             ;
callConvention               : 'cdecl' ';'    //
                             | 'pascal' ';'   //
                             | 'register' ';' //
                             | 'safecall' ';' //
                             | 'stdcall' ';'  //
                             | 'export' ';'   // deprecated
                             ;
callConventionNoSemi         : 'cdecl'    //    //ADDED for procedureType error fixing, without ';' at the end
                             | 'pascal'   //
                             | 'register' //
                             | 'safecall' //
                             | 'stdcall'  //
                             | 'export'   // deprecated
                             ;
oldCallConventionDirective   : 'far' ';'      // deprecated
                             | 'local' ';'    // niet in windows maakt functie niet exporteerbaar
                             | 'near' ';'     // deprecated
                             ;
hintingDirective             : 'deprecated' (stringFactor)?
                             | ( 'experimental'  // added 2006
                             | 'platform'
                             | 'library' )
                             ;
externalDirective            : 'varargs' ';'
                             | 'external' ';'
                             | 'external' constExpression (externalSpecifier)* ';' // expression : dll name
                             ;
externalSpecifier            : 'name' constExpression
                             | 'index' constExpression   // specific to a platform
                             ;
dispIDDirective              : 'dispid' expression ';'
                             ;
//****************************
////section general
//****************************
ident                        : TkIdentifier
                             | AMBER TkIdentifier
                             | usedKeywordsAsNames
                             ;
usedKeywordsAsNames          : (NAME | READONLY | ADD | AT | MESSAGE | POINTER | INDEX | DEFAULT | STRING | CONTINUE)
                             | (READ | WRITE | REGISTER | VARIANT | OPERATOR | REMOVE | LOCAL | REFERENCE | CONTAINS | FINAL)
                             | (BREAK | EXIT | STRICT | OUT | OBJECT | EXPORT | ANSISTRING | IMPLEMENTS | STORED)
                             ;
identList                    : ident (',' ident)*
                             ;
//identListFlat                : ident (',' ident)*    //ADDED used in formalParemeter
//                             ;
label                        : ( TkIdentifier | TkIntNum | TkHexNum ) | usedKeywordsAsNames
                             ;
intNum                       : TkIntNum
                             | TkHexNum
                             ;
realNum                      : TkRealNum
                             ;//namespacedQualifiedIdent     : (namespaceName qualifiedIdent
typeId                       : (namespaceName '.')? ident
                             ;
namespaceName                : ident ('.' ident)*
                             ;
qualifiedIdent               :  (ident '.')*  ident   //must stay the way it is, with '.' for proper class method identyfication
                             ;

// KEYWORDS
ABSOLUTE          : 'absolute'       ;
ABSTRACT          : 'abstract'       ;
ADD               : 'add'            ;
AND               : 'and'            ;
ANSISTRING        : 'ansistring'     ;
ARRAY             : 'array'          ;
AS                : 'as'             ;
ASM               : 'asm'            ;
ASSEMBLER         : 'assembler'      ;
ASSEMBLY          : 'assembly'       ;
AT                : 'at'             ;
AUTOMATED         : 'automated'      ;
BEGIN             : 'begin'          ;
BREAK             : 'break'          ;
CASE              : 'case'           ;
CDECL             : 'cdecl'          ;
CLASS             : 'class'          ;
CONST             : 'const'          ;
CONSTRUCTOR       : 'constructor'    ;
CONTAINS          : 'contains'       ;
CONTINUE          : 'continue'       ;
DEFAULT           : 'default'        ;
DEPRECATED        : 'deprecated'     ;
DESTRUCTOR        : 'destructor'     ;
DISPID            : 'dispid'         ;
DISPINTERFACE     : 'dispinterface'  ;
DIV               : 'div'            ;
DO                : 'do'             ;
DOWNTO            : 'downto'         ;
DQ                : 'dq'             ;
DW                : 'dw'             ;
DYNAMIC           : 'dynamic'        ;
ELSE              : 'else'           ;
END               : 'end'            ;
EXCEPT            : 'except'         ;
EXIT              : 'exit'           ;
EXPERIMENTAL      : 'experimental'   ;
EXPORT            : 'export'         ;
EXPORTS           : 'exports'        ;
EXTERNAL          : 'external'       ;
FAR               : 'far'            ;
FILE              : 'file'           ;
FINAL             : 'final'          ;
FINALIZATION      : 'finalization'   ;
FINALLY           : 'finally'        ;
FOR               : 'for'            ;
FORWARD           : 'forward'        ;
FUNCTION          : 'function'       ;
GOTO              : 'goto'           ;
HELPER            : 'helper'         ;
IF                : 'if'             ;
IMPLEMENTATION    : 'implementation' ;
IMPLEMENTS        : 'implements'     ;
IN                : 'in'             ;
INDEX             : 'index'          ;
INHERITED         : 'inherited'      ;
INITIALIZATION    : 'initialization' ;
INLINE            : 'inline'         ;
INTERFACE         : 'interface'      ;
IS                : 'is'             ;
LABEL             : 'label'          ;
LIBRARY           : 'library'        ;
LOCAL             : 'local'          ;
MESSAGE           : 'message'        ;
MOD               : 'mod'            ;
NAME              : 'name'           ;
NEAR              : 'near'           ;
NIL               : 'nil'            ;
NODEFAULT         : 'nodefault'      ;
NOT               : 'not'            ;
OBJECT            : 'object'         ;
OF                : 'of'             ;
ON                : 'on'             ;
OPERATOR          : 'operator'       ;
OR                : 'or'             ;
OUT               : 'out'            ;
OVERLOAD          : 'overload'       ;
OVERRIDE          : 'override'       ;
PACKAGE           : 'package'        ;
PACKED            : 'packed'         ;
PASCAL            : 'pascal'         ;
PLATFORM          : 'platform'       ;
POINTER           : 'pointer'        ;
PRIVATE           : 'private'        ;
PROCEDURE         : 'procedure'      ;
PROGRAM           : 'program'        ;
PROPERTY          : 'property'       ;
PROTECTED         : 'protected'      ;
PUBLIC            : 'public'         ;
PUBLISHED         : 'published'      ;
RAISE             : 'raise'          ;
READ              : 'read'           ;
READONLY          : 'readonly'       ;
RECORD            : 'record'         ;
REFERENCE         : 'reference'      ;
REGISTER          : 'register'       ;
REINTRODUCE       : 'reintroduce'    ;
REMOVE            : 'remove'         ;
REPEAT            : 'repeat'         ;
REQUIRES          : 'requires'       ;
RESIDENT          : 'resident'       ;
RESOURCESTRING    : 'resourcestring' ;
SAFECALL          : 'safecall'       ;
SEALED            : 'sealed'         ;
SET               : 'set'            ;
SHL               : 'shl'            ;
SHR               : 'shr'            ;
STATIC            : 'static'         ;
STDCALL           : 'stdcall'        ;
STORED            : 'stored'         ;
STRICT            : 'strict'         ;
STRING            : 'string'         ;
THEN              : 'then'           ;
THREADVAR         : 'threadvar'      ;
TO                : 'to'             ;
TRY               : 'try'            ;
TYPE              : 'type'           ;
UNIT              : 'unit'           ;
UNSAFE            : 'unsafe'         ;
UNTIL             : 'until'          ;
USES              : 'uses'           ;
VAR               : 'var'            ;
VARARGS           : 'varargs'        ;
VARIANT           : 'variant'        ;
VIRTUAL           : 'virtual'        ;
WHILE             : 'while'          ;
WITH              : 'with'           ;
WRITE             : 'write'          ;
WRITEONLY         : 'writeonly'      ;
XOR               : 'xor'            ;
FALSE             : 'false'          ;
TRUE              : 'true'           ;

//----------------------------------------------------------------------------
// OPERATORS
//----------------------------------------------------------------------------
PLUS              : '+'   ;
MINUS             : '-'   ;
STAR              : '*'   ;
SLASH             : '/'   ;
ASSIGN            : ':='  ;
COMMA             : ','   ;
SEMI              : ';'   ;
COLON             : ':'   ;
EQUAL             : '='   ;
NOT_EQUAL         : '<>'  ;
LT                : '<'   ;
LE                : '<='  ;
GE                : '>='  ;
GT                : '>'   ;
LPAREN            : '('   ;
RPAREN            : ')'   ;
LBRACK            : '['   ; // line_tab[line]
LBRACK2           : '(.'  ; // line_tab(.line.)
RBRACK            : ']'   ;
RBRACK2           : '.)'  ;
POINTER2          : '^'   ;
AT2               : '@'   ;
DOT               : '.'   ;// ('.' {$setType(DOTDOT);})?  ;
DOTDOT            : '..'  ;
LCURLY            : '{'   ;
RCURLY            : '}'   ;

AMBER             : '&'   ;
DOUBLEAT          : '@@'  ;

//****************************
//section token
//****************************
TkGlobalFunction        : 'FUNCTION_GLOBAL'
                        ;
TkFunctionName          : 'FUNCTION_NAME'
                        ;
TkFunctionArgs          : 'FUNCTION_ARGS'
                        ;
TkFunctionBody          : 'FUNCTION_BODY'
                        ;
TkFunctionReturn        : 'FUNCTION_RETURN'
                        ;
TkCustomAttribute       : 'CUSTOM_ATTRIBUTE '
                        ;
TkCustomAttributeArgs   : 'CUSTOM_ATTRIBUTE_ARGS'
                        ;
TkNewType               : 'NEW_TYPE'
                        ;
TkClass                 : 'CLASS'
                        ;
TkRecord                : 'RECORD_TYPE'
                        ;
TkRecordHelper          : 'RECORD_HELPER'
                        ;
TkInterface             : 'INTERFACE_TYPE'
                        ;
TkObject                : 'OBJECT_TYPE'
                        ;
TkClassOfType           : 'CLASS_OF_TYPE'
                        ;
TkVariableType          : 'VARIABLE_TYPE'
                        ;
TkVariableIdents        : 'VARIABLE_IDENTS'
                        ;
TkVariableParam         : 'VARIABLE_PARAM'
                        ;
TkGuid                  : 'INTERFACE_GUID'
                        ;
TkClassParents          : 'CLASS_PARENTS'
                        ;
TkClassField            : 'CLASS_FIELD'
                        ;
TkAnonymousExpression   : 'ANONYMOUS_EXPRESSION'
                        ;
TkIdentifier            : (Alpha | '_') (Alpha | Digit | '_')*
                        ;
TkIntNum                : Digitseq
                        ;
TkRealNum               : Digitseq ('.' Digitseq)? (('e'|'E') ('+'|'-')? Digitseq)?  //CHANGED
                        ;
TkHexNum                : '$' Hexdigitseq
                        ;
TkAsmHexNum             : Hexdigitseq ('h'|'H')
                        ;
//TkAsmHexLabel           : Hexdigitseq ':'
//                        ;
QuotedString            : '\'' ('\'\'' | ~('\''))* '\''   //taken from PASCAL grammar
                        ;
ControlString           : Controlchar+
//                        (Controlchar)*
                        ;

fragment
Controlchar             : '#' Digitseq
                        | '#' '$' Hexdigitseq
                        ;
fragment
Alpha                   : 'a'..'z'
                        | 'A'..'Z'
                        | '\u0080'..'\uFFFE' ~('\uFEFF') //ADDED unicode support
                        ;
fragment
Digit                   : '0'..'9'
                        ;
fragment
Digitseq                : Digit (Digit)*
                        ;
fragment
Hexdigit                : Digit | 'a'..'f' | 'A'..'F'
                        ;
Hexdigitseq             : Hexdigit (Hexdigit)*
                        ;
COMMENT                 :  ( '//' ~('\n'|'\r')* '\r'? '\n'
                        |  '(*' .*? '*)'
                        |  '{' .*? '}')    -> skip
                        ;
WS                      : (' '|'\t'|'\r'|'\n'|'\f')+ -> skip
                        ;
UnicodeBOM              : '\uFEFF' -> skip
                        ;
