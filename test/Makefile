all: LockFreeStackCAS.spec LockFreeExchangerCAS.spec LockFreeExchangerCASusingSTM.spec LockFreeStackCASusingSTM.spec EliminationBackoffStackCAS.spec EliminationBackoffStackCASusingSTM.spec

FLAGS = -O2 -threaded -rtsopts -i../src/

LockFreeStackCAS.spec: LockFreeStack/LockFreeStackCAS.spec.hs
	ghc $(FLAGS) LockFreeStack/LockFreeStackCAS.spec.hs -o bin/LockFreeStackCASTest

LockFreeStackCASusingSTM.spec: LockFreeStack/LockFreeStackCAS.spec.hs
	ghc $(FLAGS) LockFreeStack/LockFreeStackCASusingSTM.spec.hs -o bin/LockFreeStackCASusingSTMTest

LockFreeExchangerCAS.spec: LockFreeExchanger/LockFreeExchangerCAS.spec.hs
	ghc $(FLAGS) LockFreeExchanger/LockFreeExchangerCAS.spec.hs -o bin/LockFreeExchangerCASTest

LockFreeExchangerCASusingSTM.spec: LockFreeExchanger/LockFreeExchangerCASusingSTM.spec.hs
	ghc $(FLAGS) LockFreeExchanger/LockFreeExchangerCASusingSTM.spec.hs -o bin/LockFreeExchangerCASusingSTMTest

EliminationBackoffStackCAS.spec: EliminationBackoffStack/EliminationBackoffStackCAS.spec.hs
	ghc $(FLAGS) EliminationBackoffStack/EliminationBackoffStackCAS.spec.hs -o bin/EliminationBackoffStackCASTest

EliminationBackoffStackCASusingSTM.spec: EliminationBackoffStack/EliminationBackoffStackCASusingSTM.spec.hs
	ghc $(FLAGS) EliminationBackoffStack/EliminationBackoffStackCASusingSTM.spec.hs -o bin/EliminationBackoffStackCASusingSTMTest