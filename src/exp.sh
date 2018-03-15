exps=(expLFSCAS expLFSCASusingSTM)

numberOfCores=$1
threadCounts=(2 4 6 8 10 12 14)
iterations=100000
outFile="exp-${numberOfCores}.csv"
echo "Number of cores used: $numberOfCores" > $outFile
echo "Number of threads,CAS time, CAS using STM time, STM time" >> $outFile
for threadCount in ${threadCounts[@]}; do
    echo "Running experiments for $threadCount threads"

    printf "CAS..."
    CAStime=$(./expLFSCAS 1 1 6 ${threadCount} ${iterations} +RTS -N${numberOfCores} | awk '{print $NF}' | sed 's/.$//')
    printf "done\n"

    printf "CAS with STM..."
    CASwSTMtime=$(./expLFSCASusingSTM 1 1 6 ${threadCount} ${iterations} +RTS -N${numberOfCores} | awk '{print $NF}' | sed 's/.$//')
    printf "done\n"

    printf "STM..."
    STMtime=$(./expLFSSTM 1 1 6 ${threadCount} ${iterations} +RTS -N${numberOfCores} | awk '{print $NF}' | sed 's/.$//')
    printf "done\n\n"

    echo "${threadCount},${CAStime},${CASwSTMtime},${STMtime}" >> $outFile
done

echo "Results available in $outFile"