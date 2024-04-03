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
    const modelCheckPercentageOfElapsedTime = fileContents.match(/(?<=MODCHECK: Percentage of process time spent model checking: )(\d+)/g).map(Number);
    const modelCheckRejectionRatio = fileContents
        .match(/(?<=MODCHECK: Rejected )(\d+) out of (\d+) calls/g)
        .map(match => match.match(/(\d+) out of (\d+) calls/).slice(1, 3).map(Number))
        .map(([amount, outOf]) => amount / outOf);
    const elapsedTimes = fileContents.match(/(?<=GENERAL: Elapsed process time: )(\d+)/g).map(Number);

    return {
        criterionName,
        testTypeName,
        sequentName,
        modelCheckPercentageOfElapsedTime,
        modelCheckRejectionRatio,
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
    modelCheckPercentageOfElapsedTime: accAverage.modelCheckPercentageOfElapsedTime
        .map((time, idx) => time + currResult.modelCheckPercentageOfElapsedTime[idx])
        .map(x => x / results.length),
    modelCheckRejectionRatio: accAverage.modelCheckRejectionRatio
        .map((time, idx) => time + currResult.modelCheckRejectionRatio[idx])
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

/**
 * 
 * @param {[string,[string,ReturnType<typeof toTestSummary>][]]} resultsGroupedByMethodAndTestType 
 * @returns {Group}
 */
const proverLogsToDurationsGroup = resultsGroupedByMethodAndTestType => ({
    label: resultsGroupedByMethodAndTestType[0],
    x: Array.from({ length: resultsGroupedByMethodAndTestType[1].reduce((acc, curr) => acc.concat(curr[1].elapsedTimes), []).length }).map((_, i) => i + 1),
    y: resultsGroupedByMethodAndTestType[1].reduce((acc, curr) => acc.concat(curr[1].elapsedTimes), [])
    // x: Array.from({ length: resultsGroupedByMethodAndTestType[1][0][1].elapsedTimes.length }).map((_, i) => i + 1),
    // y: resultsGroupedByMethodAndTestType[1][0][1].elapsedTimes
});

/**
 * 
 * @param {[string,[string,ReturnType<typeof toTestSummary>][]]} resultsGroupedByMethodAndTestType 
 * @returns {Group}
 */
const proverLogsToModelCheckPercentageGroup = resultsGroupedByMethodAndTestType => ({
    label: resultsGroupedByMethodAndTestType[0],
    x: Array.from({ length: resultsGroupedByMethodAndTestType[1].reduce((acc, curr) => acc.concat(curr[1].modelCheckPercentageOfElapsedTime), []).length }).map((_, i) => i + 1),
    y: resultsGroupedByMethodAndTestType[1].reduce((acc, curr) => acc.concat(curr[1].modelCheckPercentageOfElapsedTime), [])
    // x: Array.from({ length: resultsGroupedByMethodAndTestType[1][0][1].modelCheckPercentageOfElapsedTime.length }).map((_, i) => i + 1),
    // y: resultsGroupedByMethodAndTestType[1][0][1].modelCheckPercentageOfElapsedTime
});

const proverLogsDir = "./tests/prover_logs_with_SH2";
const parseProverTests = () => {
    const testFileNames = readdirSync(proverLogsDir);
    const runs = Object.entries(testFileNames
        .map(toTestSummary)
        .reduce(groupByMethod, {})
    )
        .map(averageOverTestTypes);
    const durationsGroups = runs.map(proverLogsToDurationsGroup).map(x=>{x.y.sort((a,b)=>a-b); return x;});
    const modelCheckPercentageOfElapsedTimeGroups = runs.map(proverLogsToModelCheckPercentageGroup).map(x => { x.y.sort((a, b) => a - b); return x; });

    console.log(JSON.stringify([
        { name: "prover durations", x_label: "test number", y_label: "ms", groups: durationsGroups },
        { name: "model check time percentage", x_label: "test number", y_label: "%", groups: modelCheckPercentageOfElapsedTimeGroups }
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
    const logsDirPath = "./tests/logs3";
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
 * @typedef {{name: string, x_label: string?, y_label:string?, groups: Group[]}} Graph
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

// parseProverTests();
parseTestsBenchmarks();

