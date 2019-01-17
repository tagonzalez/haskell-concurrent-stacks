all: LockFreeStackIO.spec LockFreeExchangerIO.spec LockFreeExchangerSTM.spec LockFreeStackSTM.spec EliminationBackoffStackIO.spec EliminationBackoffStackSTM.spec StackSTM.spec

FLAGS = -O2 -threaded -rtsopts -i../src/

LockFreeStackIO.spec: LockFreeStack/LockFreeStackIO.spec.hs
	ghc $(FLAGS) LockFreeStack/LockFreeStackIO.spec.hs -o bin/LockFreeStackIOTest

LockFreeStackSTM.spec: LockFreeStack/LockFreeStackIO.spec.hs
	ghc $(FLAGS) LockFreeStack/LockFreeStackSTM.spec.hs -o bin/LockFreeStackSTMTest

LockFreeExchangerIO.spec: LockFreeExchanger/LockFreeExchangerIO.spec.hs
	ghc $(FLAGS) LockFreeExchanger/LockFreeExchangerIO.spec.hs -o bin/LockFreeExchangerCASTest

LockFreeExchangerSTM.spec: LockFreeExchanger/LockFreeExchangerSTM.spec.hs
	ghc $(FLAGS) LockFreeExchanger/LockFreeExchangerSTM.spec.hs -o bin/LockFreeExchangerSTMTest

EliminationBackoffStackIO.spec: EliminationBackoffStack/EliminationBackoffStackIO.spec.hs
	ghc $(FLAGS) EliminationBackoffStack/EliminationBackoffStackIO.spec.hs -o bin/EliminationBackoffStackIOTest

EliminationBackoffStackSTM.spec: EliminationBackoffStack/EliminationBackoffStackSTM.spec.hs
	ghc $(FLAGS) EliminationBackoffStack/EliminationBackoffStackSTM.spec.hs -o bin/EliminationBackoffStackSTMTest

StackSTM.spec: StackSTM/StackSTM.spec.hs
	ghc $(FLAGS) StackSTM/StackSTM.spec.hs -o bin/StackSTMTest

test:
	sh runTests.sh

.PHONY: clean

clean:
	rm -f bin/* ./**/*.hi
