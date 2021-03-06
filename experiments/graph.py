import matplotlib.pyplot as plt
import sys
import csv
import numpy as np

def calcAverage(row):
    execTimeSum = 0.0
    for i in xrange(1,len(row)):
        execTimeSum += float(row[i])

    return execTimeSum / (len(row) - 1)

emptyResults = {
    "expEBSIO": [],
    "expEBSSTM": [],
    "expLFSIO": [],
    "expLFSSTM": [],
    "expStackSTM": []
}

implementationsPerExperiment = {
    "expEBSIO": "EliminationBackoffStackIO",
    "expEBSSTM": "EliminationBackoffStackSTM",
    "expLFSIO": "LockFreeStackIO",
    "expLFSSTM": "LockFreeStackSTM",
    "expStackSTM": "StackSTM"
}

maxValuesPerCoreNumber = {
    '1':0,
    '2':0,
    '4':0
}

resultsDir = sys.argv[1]
csvDir = resultsDir + 'csv/'
plotsDir = resultsDir + 'plots/'

with open(resultsDir + 'params.txt', 'r') as file:
    experimentName = file.readline().split('-')[0]

if experimentName == 'pushPercentages':
    xLabel = 'Proporcion de hilos escritores'
else:
    xLabel = 'Cantidad de hilos'

coreNumbers = ['1', '2', '4']


# Get max value from results per core number to set the y-limits of the plots
for coreNumber in coreNumbers:
    results = emptyResults
    for experiment in results:
        with open(csvDir + experiment + '-' + coreNumber + '.csv','r') as csvfile:
            plots = csv.reader(csvfile, delimiter=',', quoting=csv.QUOTE_NONNUMERIC)
            csvfile.readline() #First line has labels
            for row in plots:
                if np.amax(row[1:])> maxValuesPerCoreNumber[coreNumber]:
                    maxValuesPerCoreNumber[coreNumber] = np.amax(row[1:])

# Plot results: individual graphs per implementation with boxplots
for coreNumber in coreNumbers:
    results = emptyResults
    for experiment in results:
        with open(csvDir + experiment + '-' + coreNumber + '.csv','r') as csvfile:
            plots = csv.reader(csvfile, delimiter=',', quoting=csv.QUOTE_NONNUMERIC)
            results[experiment] = []
            x = []
            boxplotData = []
            csvfile.readline() #First line has labels
            for row in plots:
                boxplotData.append(row[1:])
                results[experiment].append(calcAverage(row))
                if float(row[0]) < 1:
                    x_val = float(row[0])
                    manageX = False
                else:
                    x_val = int(row[0])
                    manageX = True
                x.append(x_val)
        step = x[1] - x[0]
        plt.plot(x,results[experiment])
        plt.boxplot(boxplotData, manage_xticks=manageX, widths=step*0.7, positions=x)
        plt.title('Implementacion: ' + implementationsPerExperiment[experiment] + ', Cantidad de nucleos: '+ coreNumber)
        plt.ylim(0,maxValuesPerCoreNumber[coreNumber] + 1)
        plt.xlabel(xLabel)
        plt.ylabel('Tiempo de ejecucion (segundos)')
        if not manageX:
            plt.xticks(np.arange(min(x), max(x)+ step, step))
        plt.savefig(plotsDir + experiment + '-' + coreNumber + '.png')
        plt.gcf().clear()
    # Plot overall graph showing trendlines for the average values
    for experiment in results:
        plt.plot(x,results[experiment],label=implementationsPerExperiment[experiment])
        plt.legend()
    # plt.legend(results.keys()p,loc="upper left")
    plt.title('Cantidad de nucleos: '+ coreNumber)
    plt.xlabel(xLabel)
    plt.ylabel('Tiempo de ejecucion (segundos)')
    if not manageX:
        plt.xticks(np.arange(min(x), max(x)+ step, step))
    plt.savefig(plotsDir + coreNumber + '.png')
    plt.gcf().clear()