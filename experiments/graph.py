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

resultsDir = sys.argv[1]
# resultsDir = './results/numberOfThreads/numberOfThreads-2018-09-03-080407/'
csvDir = resultsDir + 'csv/'
plotsDir = resultsDir + 'plots/'

coreNumbers = ['1', '2', '4']

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
        plt.title('Experiment: ' + experiment + ', Number of cores: '+ coreNumber)
        plt.xlabel('Number of threads')
        plt.ylabel('Execution time (seconds)')
        if not manageX:
            plt.xticks(np.arange(min(x), max(x)+ step, step))
        plt.savefig(plotsDir + experiment + '-' + coreNumber + '.png')
        plt.gcf().clear()
    for experiment in results:
        plt.plot(x,results[experiment])
    plt.legend(results.keys(),loc="upper left")
    plt.title('Number of cores: '+ coreNumber)
    plt.xlabel('Number of threads')
    plt.ylabel('Execution time (seconds)')
    if not manageX:
        plt.xticks(np.arange(min(x), max(x)+ step, step))
    plt.savefig(plotsDir + coreNumber + '.png')
    plt.gcf().clear()