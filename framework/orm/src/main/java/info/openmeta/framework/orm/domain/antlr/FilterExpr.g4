grammar FilterExpr;

expr:   expr AND expr               # AndExpr
    |   expr OR expr                # OrExpr
    |   '(' expr ')'                # ParenExpr
    |   unit                        # UnitExpr
    ;

AND:    'AND';
OR:     'OR';

unit:   FIELD OPERATOR value        # FilterUnitExpr
    ;

value: singleValue                  # SingleValueExpr
     | listValue                    # ListValueExpr
     ;

singleValue: NUMBER
           | BOOLEAN
           | QUOTED_STRING
           ;

listValue: '[' singleValue (',' singleValue)* ']'
          ;

FIELD:  [a-z][a-zA-Z0-9]*;
OPERATOR: '='
        | '!='
        | '>'
        | '>='
        | '<'
        | '<='
        | 'CONTAINS'
        | 'NOT CONTAINS'
        | 'START WITH'
        | 'NOT START WITH'
        | 'IN'
        | 'NOT IN'
        | 'BETWEEN'
        | 'NOT BETWEEN'
        | 'IS SET'
        | 'IS NOT SET'
        | 'PARENT OF'
        | 'CHILD OF';

NUMBER: [0-9]+ ('.' [0-9]+)?;
BOOLEAN: 'true' | 'false';
QUOTED_STRING: '"' (~["\\] | '\\' .)* '"';  // Double-quoted string, supports escape characters

WS: [ \t\r\n]+ -> skip;                     // Ignore whitespace