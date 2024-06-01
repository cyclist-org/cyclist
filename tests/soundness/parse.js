const { readdirSync, readFileSync } = require('fs');
const { join } = require('path');
const parseWorseCase = (str) => {
    const durations =
        str
            .match(/(?<=real\t)(\d)m(.+)s/g)
            .map(s => s.match(/(\d)m(.+)s/)
                .slice(1, 3)
                .map(Number)
            )
            .map(([m, s]) => (m * 60 + s) * 1000);

    const times = str
        .match(/(?<=dune.*) (\d+)/g)
        .map(Number)
        .flatMap((size, index) => [size, durations[index]]);
    return times;
};

const toTestSummary = (filePath) => {
    const criterionName = getMethod(filePath);
    const testTypeName = filePath.split("_")[0];

    const fileContents = readFileSync(`${proverLogsDir}/${filePath}`).toString();

    const sequentName = fileContents.match(/(?<=test case: )(\S+)/g);
    const modelChecksPercentageOfElapsedTime = fileContents.match(/(?<=MODCHECK: Percentage of process time spent model checking: )(\d+)/g).map(Number);
    const modelChecksRejection = fileContents
        .match(/(?<=MODCHECK: Rejected )(\d+) out of (\d+) calls/g)
        .map(match => match.match(/(\d+) out of (\d+) calls/).slice(1, 3).map(Number))
        .map(([amountOfRejectedChecks, amountOfChecks]) => ({ amountOfRejectedChecks, amountOfChecks }));
    const modelChecksRejectionRatio = modelChecksRejection.map(({ amountOfChecks, amountOfRejectedChecks }) => amountOfRejectedChecks / amountOfChecks);
    const elapsedTimes = fileContents.match(/(?<=GENERAL: Elapsed process time: )(\d+)/g).map(Number);

    return {
        criterionName,
        testTypeName,
        sequentName,
        modelChecksPercentageOfElapsedTime,
        modelChecksAmount: modelChecksRejection.map(x => x.amountOfChecks),
        modelChecksRejectionAmount: modelChecksRejection.map(x => x.amountOfRejectedChecks),
        modelChecksRejectionRatio,
        elapsedTimes
    };
};


const groupByMethod = (groups, currResult) => ({
    ...groups,
    [currResult.criterionName]: (groups[currResult.criterionName] ?? []).concat(currResult)
});

/**
 * 
 * @param {Record<string,ReturnType<typeof toTestSummary>[]} groups 
 * @param {ReturnType<typeof toTestSummary>} currResult 
 */
const groupByTestType = (groups, currResult) => ({
    ...groups,
    [currResult.testTypeName]: (groups[currResult.testTypeName] ?? []).concat(currResult)
});

/**
 * 
 * @param {ReturnType<typeof toTestSummary>} accAverage 
 * @param {ReturnType<typeof toTestSummary>} currResult 
 * @returns {ReturnType<typeof toTestSummary>}
 */
const averageEveryColumn = (accAverage, currResult, _, results) => ({
    ...accAverage,
    elapsedTimes: accAverage.elapsedTimes
        .map((time, idx) => time + currResult.elapsedTimes[idx])
        .map(x => x / results.length),
    modelChecksPercentageOfElapsedTime: accAverage.modelChecksPercentageOfElapsedTime
        .map((time, idx) => time + currResult.modelChecksPercentageOfElapsedTime[idx])
        .map(x => x / results.length),
    modelChecksRejectionRatio: accAverage.modelChecksRejectionRatio
        .map((time, idx) => time + currResult.modelChecksRejectionRatio[idx])
        .map(x => x / results.length)
});

/**
 * 
 * @param {[string, ReturnType<typeof toTestSummary>[]]} param0 
 */
const averageOverTestTypes = ([methodName, results]) => [
    methodName,
    Object.entries(
        results.reduce(groupByTestType, {})
    )
        .map(([testType, results]) => [
            testType,
            results.reduce(averageEveryColumn)
        ])
];

const concatTestTypes = (acc, curr) => acc.concat(curr[1]);


const proverLogsToGroupOfProperty = property => resultsGroupedByType => ({
    label: resultsGroupedByType[0],
    x: Array.from({ length: resultsGroupedByType[1].length }).map((_, i) => i + 1),
    y: resultsGroupedByType[1].map(x => x[property])
});

/**
 * 
 * @param {*} results 
 * @param {"OR"|"SH"|"SLA|"VLA"} methodName 
 * @param {"elapsedTimes"|"modelChecksPercentageOfElapsedTime"|"modelChecksAmount"|"modelChecksRejectionAmount"|"modelChecksRejectionRatio"|"sequentName"} property 
 * @returns 
 */
const sortResultsBy = (results, methodName, property) => {
    const resultsToSortBy = results.find(methodResults => methodResults[0] == methodName)[1];
    const enumeratedResultsToSortBy = resultsToSortBy.map((result, idx) => ({ idx, result }));
    const sortedEnumeratedResultsToSortBy = enumeratedResultsToSortBy.sort((a, b) => a.result[property] - b.result[property]);

    return results.map(([methodName, methodResults]) => [
        methodName,
        sortedEnumeratedResultsToSortBy.map(sortedResult => methodResults[sortedResult.idx])
    ]);
};

/**
 * 
 * @param {*} sortedRunsResults 
 * @param {"OR"|"SH"|"SLA|"VLA"} methodName 
 */
const calculateOverheadsPercentage = (results, methodName) => {
    const baseline = results.find(methodResults => methodResults[0] == methodName)[1];
    return results.map(([methodName, methodResults]) => [
        methodName,
        methodResults.map((result, idx) => ({ overheadPercentage: ((result.elapsedTimes - baseline[idx].elapsedTimes) / baseline[idx].elapsedTimes) * 100 }))
    ]);
};

/**
 * 
 * @param {*} results 
 * @param {"OR"|"SH"|"SLA|"VLA"} methodName 
 */
const filterOutTimeouts = (results, methodName) => {
    const resultsToFilterBy = results.find(methodResults => methodResults[0] == methodName)[1];
    const enumeratedResultsToFilterBy = resultsToFilterBy.map((result, idx) => ({ idx, result }));
    const filteredEnumeratedResultsToFilterBy = enumeratedResultsToFilterBy.filter(x => /*x.result.elapsedTimes < 28000 &&*/ x.result.elapsedTimes > 200);

    return results.map(([methodName, methodResults]) => [
        methodName,
        methodResults.filter((_, idx) => filteredEnumeratedResultsToFilterBy.find(x => x.idx == idx))
    ]);
};

const proverLogsDir = "./tests/prover_logs_with_SH3";
const parseProverTests = () => {
    const testFileNames = readdirSync(proverLogsDir);
    const runs = Object.entries(testFileNames
        .map(toTestSummary)
        .reduce(groupByMethod, {})
    )
        .map(averageOverTestTypes);

    const runsResults = runs.map(([method, runs]) => [
        method,
        runs.map(([testType, runs]) => [
            testType,
            runs.elapsedTimes.map((_, idx) => ({
                elapsedTimes: runs.elapsedTimes[idx],
                modelChecksPercentageOfElapsedTime: runs.modelChecksPercentageOfElapsedTime[idx],
                modelChecksAmount: runs.modelChecksAmount[idx],
                modelChecksRejectionAmount: runs.modelChecksRejectionAmount[idx],
                modelChecksRejectionRatio: runs.modelChecksRejectionRatio[idx],
                sequentName: runs.sequentName[idx],
            }))
        ])
            .reduce(concatTestTypes, [])
    ]);

    const filteredRunsResults = filterOutTimeouts(runsResults, "OR");
    const sortedRunsResults = sortResultsBy(filteredRunsResults, "OR", "elapsedTimes");

    const overheads = calculateOverheadsPercentage(sortedRunsResults, "OR").map(proverLogsToGroupOfProperty("overheadPercentage"));

    const durationsGroups = sortedRunsResults.map(proverLogsToGroupOfProperty("elapsedTimes"));
    const modelCheckPercentageOfElapsedTimeGroups = sortedRunsResults.map(proverLogsToGroupOfProperty("modelChecksPercentageOfElapsedTime"));
    const modelChecksAmount = sortedRunsResults.map(proverLogsToGroupOfProperty("modelChecksAmount"));

    console.log(JSON.stringify([
        { name: "prover durations", y_label: "ms", groups: durationsGroups },
        { name: "% Overhead", groups: overheads },
        { name: "model check time percentage", y_label: "%", groups: modelCheckPercentageOfElapsedTimeGroups },
        { name: "model check amount", groups: modelChecksAmount }
    ]));
};

const getMethod = str =>
    str.match(/(VLA|SLA|OR|FWK|SH)/)[1];

const parseWorstCaseFile = filePath => {
    const fileContents = readFileSync(filePath).toString();

    const durations = fileContents
        .match(/(?<=real\t)(\d)m(.+)s/g)
        .map(s => s.match(/(\d)m(.+)s/)
            .slice(1, 3)
            .map(Number)
        )
        .map(([m, s]) => (m * 60 + s) * 1000);

    const inputSizes = fileContents
        .match(/(?<=dune.*) (\d+)/g)
        .map(Number);

    return {
        durations, inputSizes
    };
};


const worstcaseToGroup = ([label, runs]) => ({
    label,
    x: runs.inputSizes,
    y: runs.durations
});

const worstcaseToGraph = ([name, runsPerMethod]) => ({
    name,
    groups: runsPerMethod.map(worstcaseToGroup)
});

function parseTestsBenchmarks() {
    const logsDirPath = "./tests/logs4";
    const fileNamesGroupedByTestName = readdirSync(logsDirPath).reduce((acc, curr) => {
        const testName = curr.split("_")[0];
        const testPath = join(logsDirPath, curr);
        if (testName in acc) {
            acc[testName].push(testPath);
        } else {
            acc[testName] = [testPath];
        }
        return acc;
    }, {});

    const durationsGroupedByTestNameAndMethod = Object.entries(fileNamesGroupedByTestName)
        .map(([testName, testFilesNames]) => [
            testName,
            Object.entries(testFilesNames.reduce((acc, curr) => ({
                ...acc,
                [getMethod(curr)]: (acc[getMethod(curr)] ?? []).concat([curr])
            }), {}))
                .map(([methodName, fileNames]) => [
                    methodName,
                    fileNames.map(parseWorstCaseFile).reduce((acc, curr) => ({
                        inputSizes: curr.inputSizes,
                        durations: (acc.durations ?? curr.durations.map(() => 0))
                            .map((duration, idx) => duration + curr.durations[idx] / fileNames.length)
                    }), {})
                ])
        ]);

    console.log(JSON.stringify(durationsGroupedByTestNameAndMethod.map(worstcaseToGraph)));
}

/**
 * @typedef {{label: string, x: number[], y: number[]}} Group
 * @typedef {{name: string, x_label: string?, y_label:string?, y_scale?: "log", groups: Group[]}} Graph
 * @typedef {Group[][]} Result
 */

/**
 * @type Result
 */
const result = [[
    {
        label: "",
        x: [Number],
        y: [Number]
    }
]];

parseProverTests();
// parseTestsBenchmarks();

