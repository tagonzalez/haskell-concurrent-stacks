startTime=$(date +%s)

# Make output directory
scriptName="pushPercentages"
date=$(date '+%Y-%m-%d-%H%M%S')
outDir="results/${scriptName}/${scriptName}-${date}"
mkdir $outDir
mkdir ${outDir}/plots
mkdir ${outDir}/csv

# Meta-variable: number of cores
# coreNumbers=(2 4 6 8)
coreNumbers=(1 2 4)

# Independent variable: number of threads
pushPercentages=(0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90)

# Controlled variables
operations=10000
min=100
max=1000
capacity=10
duration=100
threadCount=14
distributeOperations=False

# Number of times to replicate the experiment: iterations
iterations=10

function runExperiment(){
    experiment=$1
    param1=$2
    param2=$3
    printf "pushPercentage"

    for (( i = 1; i <= $iterations; i++ ))
    do
        printf ", $i"
    done
    for pushPercentage in ${pushPercentages[@]}
    do
        printf "\n$pushPercentage"
        execArgs="$param1 $param2 $operations $pushPercentage $threadCount $distributeOperations +RTS -N$coreNumber"
        for (( i = 0; i < $iterations; i++ ))
        do
            execTime=$(printf "%8.6f" $(./bin/$experiment $execArgs))
            printf ", $execTime"
        done
    done
    printf "\n"
}

printf "pushPercentages
threadCount, $threadCount
operations, $operations
min, $min
max, $max
capacity, $capacity
duration, $duration
iterations, $iterations
distributeOperations, $distributeOperations" > ${outDir}/params.txt

for coreNumber in ${coreNumbers[@]}; do
    printf "Number of cores: ${coreNumber}\n"

    printf "Running expEBSIO..."
    runExperiment expEBSIO $capacity $duration > ${outDir}/csv/expEBSIO-${coreNumber}.csv
    printf "done\n"

    printf "Running expEBSSTM..."
    runExperiment expEBSSTM $capacity $duration > ${outDir}/csv/expEBSSTM-${coreNumber}.csv
    printf "done\n"

    printf "Running expLFSIO..."
    runExperiment expLFSIO $min $max > ${outDir}/csv/expLFSIO-${coreNumber}.csv
    printf "done\n"

    printf "Running expLFSSTM..."
    runExperiment expLFSSTM $min $max > ${outDir}/csv/expLFSSTM-${coreNumber}.csv
    printf "done\n"

    printf "Running expStackSTM..."
    runExperiment expStackSTM $min $max > ${outDir}/csv/expStackSTM-${coreNumber}.csv
    printf "done\n\n"
done

printf "Plotting results..."
python graph.py ${outDir}/
printf "done\n\n"

endTime=$(date +%s)

echo "Results available in $outDir"
echo "Time elapsed: $((${endTime} - ${startTime})) seconds"
echo "Time elapsed: $((${endTime} - ${startTime})) seconds" >> ${outDir}/params.txt
