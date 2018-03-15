all: expLFSCAS expLFSCASusingSTM expLFSSTM

expLFSCAS: expLFSCAS.hs
	ghc -O2 -threaded -rtsopts expLFSCAS.hs -o ../bin/expLFSCAS

expLFSCASusingSTM: expLFSCASusingSTM.hs
	ghc -O2 -threaded -rtsopts expLFSCASusingSTM.hs -o ../bin/expLFSCASusingSTM

expLFSSTM: expLFSSTM.hs
	ghc -O2 -threaded -rtsopts expLFSSTM.hs -o ../bin/expLFSSTM

clean:
	rm -f *.o *.hi *.csv expLFSCAS expLFSCASusingSTM expLFSSTM
