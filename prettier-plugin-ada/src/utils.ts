import prettier, { Doc } from "prettier";

const docBuilders = prettier.doc.builders;
export const {
    align,
    join,
    hardline,
    line,
    softline,
    group,
    indent,
    dedent,
    indentIfBreak,
    fill,
    ifBreak,
} = docBuilders;

const docDebug = prettier.doc.debug;
export const printDocToDebug = docDebug.printDocToDebug;

export const fillBackwards3 = (
    doc0: Doc,
    doc1: Doc,
    doc2: Doc,
    group0: symbol,
    group1: symbol
): Doc => {
    return group(
        [
            group([doc0, align(2, fill(["", line, doc1]))], {
                id: group1,
            }),
            ifBreak(
                align(
                    2,
                    ifBreak(
                        align(2, fill(["", line, doc2])),
                        fill(["", line, doc2]),
                        { groupId: group1 }
                    )
                ),
                ifBreak(
                    align(2, fill(["", line, doc2])),
                    fill(["", line, doc2]),
                    { groupId: group1 }
                ),
                { groupId: group0 }
            ),
        ],
        {
            id: group0,
        }
    );
};

export const fillBackwards4 = (
    doc1: Doc,
    doc2: Doc,
    doc3: Doc,
    doc4: Doc
): Doc => {
    const group1 = Symbol();
    const group2 = Symbol();
    const group3 = Symbol();
    return group(
        [
            group(
                [
                    group([doc1, align(2, fill(["", line, doc2]))], {
                        id: group1,
                    }),
                    ifBreak(
                        align(
                            2,
                            ifBreak(
                                align(2, fill(["", line, doc3])),
                                fill(["", line, doc3]),
                                { groupId: group1 }
                            )
                        ),
                        ifBreak(
                            align(2, fill(["", line, doc3])),
                            fill(["", line, doc3]),
                            { groupId: group1 }
                        )
                    ),
                ],
                {
                    id: group2,
                }
            ),
            ifBreak(
                align(
                    2,
                    ifBreak(
                        align(
                            2,
                            ifBreak(
                                align(2, fill(["", line, doc4])),
                                fill(["", line, doc4]),
                                {
                                    groupId: group1,
                                }
                            )
                        ),
                        ifBreak(
                            align(2, fill(["", line, doc4])),
                            fill(["", line, doc4]),
                            {
                                groupId: group1,
                            }
                        ),
                        { groupId: group2 }
                    )
                ),
                ifBreak(
                    align(
                        2,
                        ifBreak(
                            align(2, fill(["", line, doc4])),
                            fill(["", line, doc4]),
                            {
                                groupId: group1,
                            }
                        )
                    ),
                    ifBreak(
                        align(2, fill(["", line, doc4])),
                        fill(["", line, doc4]),
                        {
                            groupId: group1,
                        }
                    ),
                    { groupId: group2 }
                ),
                { groupId: group3 }
            ),
        ],
        { id: group3 }
    );
};

export const fillBackwards = (docs: Doc[], groupIds: symbol[]): Doc => {
    if (docs.length == 0) {
        return "";
    } else if (docs.length == 1) {
        return docs[0];
    } else if (docs.length == 2) {
        return group([docs[0], align(2, fill(["", line, docs[1]]))]);
    }

    const cascadeStepBuilder: (step: number) => Doc = (step: number) => {
        console.log(`cascadeStepBuilder step: ${step}`);
        if (step == groupIds.length - 1) {
            return group([docs[0], align(2, fill(["", line, docs[1]]))], {
                id: groupIds[step],
            });
        }

        const ifBreakHelper: (step: number, docIdx: number) => Doc = (
            step: number,
            docIdx: number
        ) => {
            console.log(`ifBreakHelper step ${step} docIdx ${docIdx}`);
            if (step == docs.length - 1) {
                const result = fill(["", line, docs[docIdx]]);
                console.log(`ifBreakHelper step ${step} docIdx ${docIdx} out`);
                return result;
            } else {
                const result = ifBreak(
                    align(2, ifBreakHelper(step + 1, docIdx)),
                    ifBreakHelper(step + 1, docIdx),
                    {
                        groupId: groupIds[step],
                    }
                );
                console.log(`ifBreakHelper step ${step} docIdx ${docIdx} out`);
                return result;
            }
        };

        const result = group(
            [
                cascadeStepBuilder(step + 1),
                ifBreakHelper(step, docs.length - 1 - step),
            ],
            {
                id: groupIds[step],
            }
        );
        console.log(`cascadeStepBuilder step: ${step} out`);
        return result;
    };

    const result = cascadeStepBuilder(0);
    console.log(printDocToDebug(result));
    return result;
};
