"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.fillBackwards = exports.fillBackwards4 = exports.fillBackwards3 = exports.printDocToDebug = exports.ifBreak = exports.fill = exports.indentIfBreak = exports.dedent = exports.indent = exports.group = exports.softline = exports.line = exports.hardline = exports.join = exports.align = void 0;
const prettier_1 = __importDefault(require("prettier"));
const docBuilders = prettier_1.default.doc.builders;
exports.align = docBuilders.align, exports.join = docBuilders.join, exports.hardline = docBuilders.hardline, exports.line = docBuilders.line, exports.softline = docBuilders.softline, exports.group = docBuilders.group, exports.indent = docBuilders.indent, exports.dedent = docBuilders.dedent, exports.indentIfBreak = docBuilders.indentIfBreak, exports.fill = docBuilders.fill, exports.ifBreak = docBuilders.ifBreak;
const docDebug = prettier_1.default.doc.debug;
exports.printDocToDebug = docDebug.printDocToDebug;
const fillBackwards3 = (doc0, doc1, doc2, group0, group1) => {
    return (0, exports.group)([
        (0, exports.group)([doc0, (0, exports.align)(2, (0, exports.fill)(["", exports.line, doc1]))], {
            id: group1,
        }),
        (0, exports.ifBreak)((0, exports.align)(2, (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc2])), (0, exports.fill)(["", exports.line, doc2]), { groupId: group1 })), (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc2])), (0, exports.fill)(["", exports.line, doc2]), { groupId: group1 }), { groupId: group0 }),
    ], {
        id: group0,
    });
};
exports.fillBackwards3 = fillBackwards3;
const fillBackwards4 = (doc1, doc2, doc3, doc4) => {
    const group1 = Symbol();
    const group2 = Symbol();
    const group3 = Symbol();
    return (0, exports.group)([
        (0, exports.group)([
            (0, exports.group)([doc1, (0, exports.align)(2, (0, exports.fill)(["", exports.line, doc2]))], {
                id: group1,
            }),
            (0, exports.ifBreak)((0, exports.align)(2, (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc3])), (0, exports.fill)(["", exports.line, doc3]), { groupId: group1 })), (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc3])), (0, exports.fill)(["", exports.line, doc3]), { groupId: group1 })),
        ], {
            id: group2,
        }),
        (0, exports.ifBreak)((0, exports.align)(2, (0, exports.ifBreak)((0, exports.align)(2, (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc4])), (0, exports.fill)(["", exports.line, doc4]), {
            groupId: group1,
        })), (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc4])), (0, exports.fill)(["", exports.line, doc4]), {
            groupId: group1,
        }), { groupId: group2 })), (0, exports.ifBreak)((0, exports.align)(2, (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc4])), (0, exports.fill)(["", exports.line, doc4]), {
            groupId: group1,
        })), (0, exports.ifBreak)((0, exports.align)(2, (0, exports.fill)(["", exports.line, doc4])), (0, exports.fill)(["", exports.line, doc4]), {
            groupId: group1,
        }), { groupId: group2 }), { groupId: group3 }),
    ], { id: group3 });
};
exports.fillBackwards4 = fillBackwards4;
const fillBackwards = (docs, groupIds) => {
    if (docs.length == 0) {
        return "";
    }
    else if (docs.length == 1) {
        return docs[0];
    }
    else if (docs.length == 2) {
        return (0, exports.group)([docs[0], (0, exports.align)(2, (0, exports.fill)(["", exports.line, docs[1]]))]);
    }
    const cascadeStepBuilder = (step) => {
        console.log(`cascadeStepBuilder step: ${step}`);
        if (step == groupIds.length - 1) {
            return (0, exports.group)([docs[0], (0, exports.align)(2, (0, exports.fill)(["", exports.line, docs[1]]))], {
                id: groupIds[step],
            });
        }
        const ifBreakHelper = (step, docIdx) => {
            console.log(`ifBreakHelper step ${step} docIdx ${docIdx}`);
            if (step == docs.length - 1) {
                const result = (0, exports.fill)(["", exports.line, docs[docIdx]]);
                console.log(`ifBreakHelper step ${step} docIdx ${docIdx} out`);
                return result;
            }
            else {
                const result = (0, exports.ifBreak)((0, exports.align)(2, ifBreakHelper(step + 1, docIdx)), ifBreakHelper(step + 1, docIdx), {
                    groupId: groupIds[step],
                });
                console.log(`ifBreakHelper step ${step} docIdx ${docIdx} out`);
                return result;
            }
        };
        const result = (0, exports.group)([
            cascadeStepBuilder(step + 1),
            ifBreakHelper(step, docs.length - 1 - step),
        ], {
            id: groupIds[step],
        });
        console.log(`cascadeStepBuilder step: ${step} out`);
        return result;
    };
    const result = cascadeStepBuilder(0);
    console.log((0, exports.printDocToDebug)(result));
    return result;
};
exports.fillBackwards = fillBackwards;
