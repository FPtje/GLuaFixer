uuagc --haskellsyntax --data src/GLua/AG/AST.ag
uuagc --haskellsyntax --data --strictdata src/GLua/AG/Token.ag
uuagc --catas --haskellsyntax --semfuns --wrappers --signatures src/GLua/AG/PrettyPrint.ag
uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/LexLint.ag
uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/ASTLint.ag
