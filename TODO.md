TODO:
- quote block
    - quote block syntax
    - unquote syntax (`$ident`)
    - quote block evaluation =
        interpolate unquotes
        (keep as quote block)
    - quote block escape
        - can_escape(&Expr) -> bool ==> escape(Expr) -> Option < Expr >
        - if block contains zero/one expression,
            escape as single expression
        - if block contains statements,
            escape as tagged scope
            (add flatten tag to ED::Block)
            and flatten tagged block expressions
            if evaluated as statement

- add match
    - int, string, constructors patterns
    - binding patterns
    - or patterns
- lazily parse paren token trees at function calls
- allow passing tokens in const functions
- parse tree patterns
- hygiene ?
