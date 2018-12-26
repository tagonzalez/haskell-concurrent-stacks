all: expLFSIO expLFSSTM expEBSIO expEBSSTM expStackSTM

FLAGS = --make -threaded -rtsopts -i../src/ -I0 -prof -fprof-auto
DATE=`date '+%Y-%m-%d-%H%M%S'`

expLFSIO: src/LockFreeStack/expLFSIO.hs
	ghc $(FLAGS) src/LockFreeStack/expLFSIO.hs -o bin/expLFSIO

expLFSSTM: src/LockFreeStack/expLFSSTM.hs
	ghc $(FLAGS) src/LockFreeStack/expLFSSTM.hs -o bin/expLFSSTM

expEBSIO: src/EliminationBackoffStack/expEBSIO.hs
	ghc $(FLAGS) src/EliminationBackoffStack/expEBSIO.hs -o bin/expEBSIO

expEBSSTM: src/EliminationBackoffStack/expEBSSTM.hs
	ghc $(FLAGS) src/EliminationBackoffStack/expEBSSTM.hs -o bin/expEBSSTM

expStackSTM: src/StackSTM/expStackSTM.hs
	ghc $(FLAGS) src/StackSTM/expStackSTM.hs -o bin/expStackSTM

numberOfThreads:
	sh numberOfThreads.sh

numberOfThreadsDist:
	sh numberOfThreadsDist.sh

pushPercentages:
	sh pushPercentages.sh

.PHONY: clean

clean:
	rm -f ../src/**/*.hi ../src/**/*.o src/**/*.hi src/**/*.o bin/*
