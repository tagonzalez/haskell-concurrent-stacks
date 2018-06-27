all: LockFreeStackCAS.spec LockFreeExchangerCAS.spec EliminationBackoffStackCAS.spec

FLAGS = -O2 -threaded -rtsopts -i../src/

LockFreeStackCAS.spec: LockFreeStack/LockFreeStackCAS.spec.hs
	ghc $(FLAGS) LockFreeStack/LockFreeStackCAS.spec.hs -o bin/LockFreeStackCASTest

LockFreeExchangerCAS.spec: LockFreeExchanger/LockFreeExchangerCAS.spec.hs
	ghc $(FLAGS) LockFreeExchanger/LockFreeExchangerCAS.spec.hs -o bin/LockFreeExchangerCASTest

EliminationBackoffStackCAS.spec: EliminationBackoffStack/EliminationBackoffStackCAS.spec.hs
	ghc $(FLAGS) EliminationBackoffStack/EliminationBackoffStackCAS.spec.hs -o bin/EliminationBackoffStackCASTest