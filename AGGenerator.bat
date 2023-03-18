cabal exec -- uuagc --haskellsyntax --data src/GLua/AG/AST.ag
cabal exec -- uuagc --haskellsyntax --data --strictdata src/GLua/AG/Token.ag
cabal exec -- uuagc --catas --haskellsyntax --semfuns --wrappers --signatures src/GLua/AG/PrettyPrint.ag
cabal exec -- uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/LexLint.ag
cabal exec -- uuagc --catas --haskellsyntax --semfuns --wrappers --signatures --optimize src/GLuaFixer/AG/ASTLint.ag
