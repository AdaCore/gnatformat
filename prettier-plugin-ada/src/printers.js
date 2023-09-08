"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.printSubpSpec = exports.printSubpDecl = exports.printPackageDecl = exports.printPrivatePart = exports.printPublicPart = exports.printIdentifier = void 0;
const utils_1 = require("./utils");
const printIdentifier = (idenfier) => {
    return idenfier.name;
};
exports.printIdentifier = printIdentifier;
const printPublicPart = (publicPart) => {
    return (0, utils_1.join)(utils_1.hardline, publicPart);
};
exports.printPublicPart = printPublicPart;
const printPrivatePart = (privatePart) => {
    return [
        "private",
        (0, utils_1.indent)([
            utils_1.hardline,
            (0, utils_1.join)(utils_1.hardline, privatePart.map((packageDecl) => (0, exports.printPackageDecl)(packageDecl))),
        ]),
    ];
};
exports.printPrivatePart = printPrivatePart;
const printPackageDecl = (packageDecl) => {
    return [
        "package ",
        packageDecl.packageName,
        " is",
        (0, utils_1.indent)([utils_1.hardline, (0, exports.printPublicPart)(packageDecl.publicPart)]),
        packageDecl.privatePart
            ? [utils_1.hardline, (0, exports.printPrivatePart)(packageDecl.privatePart)]
            : "",
        utils_1.hardline,
        "end ",
        packageDecl.packageName,
        ";",
    ];
};
exports.printPackageDecl = printPackageDecl;
const printParamSpec = (paramSpec, options) => {
    const paramLength = paramSpec.ids.length;
    const paramAlign = (options === null || options === void 0 ? void 0 : options.paramsMaxLength)
        ? " ".repeat(options.paramsMaxLength - paramLength)
        : "";
    const paramModeLength = paramSpec.mode ? paramSpec.mode.length : -1;
    const paramModeAlign = " ".repeat((options === null || options === void 0 ? void 0 : options.paramsModeMaxLength)
        ? options.paramsModeMaxLength - paramModeLength
        : 0);
    const groupIds = Array(2).map(() => Symbol());
    const paramIdPart = (0, utils_1.group)([
        paramSpec.ids,
        (0, utils_1.ifBreak)(paramAlign, "", { groupId: options === null || options === void 0 ? void 0 : options.parentGroup }),
        " :",
    ]);
    const paramTypePart = [
        (0, utils_1.ifBreak)("", paramModeAlign, { groupId: groupIds[0] }),
        paramSpec.mode ? paramSpec.mode.concat(" ") : "",
        paramSpec.type,
        paramSpec.default_expr ? " :=" : "",
    ];
    const paramDefaultExprPart = [
        paramSpec.default_expr ? paramSpec.default_expr : "",
    ];
    // return fillBackwards3(
    //     "AAAAAAAAAAAAAAAA",
    //     "BBBBBBBBBBBBBBBBBB",
    //     "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
    // );
    // return fillBackwards(
    //     [paramIdPart, paramTypePart, paramDefaultExprPart],
    //     groupIds
    // );
    return (0, utils_1.fillBackwards)(["doc0", "doc1", "doc2"], groupIds);
    return (0, utils_1.fillBackwards3)(paramIdPart, paramTypePart, paramDefaultExprPart, groupIds[0], groupIds[1]);
    // const paramSpecGroupId = Symbol();
    // return group([
    //     group(
    //         [
    //             [
    //                 paramSpec.ids,
    //                 ifBreak(paramAlign, "", { groupId: options?.parentGroup }),
    //                 " :",
    //             ],
    //             align(2, [
    //                 line,
    //                 ifBreak("", paramModeAlign, { groupId: paramSpecGroupId }),
    //                 paramSpec.mode ? paramSpec.mode.concat(" ") : "",
    //                 paramSpec.type,
    //                 paramSpec.default_expr ? " :=" : "",
    //             ]),
    //         ],
    //         { id: paramSpecGroupId }
    //     ),
    //     ifBreak(
    //         align(
    //             4,
    //             fill([
    //                 "",
    //                 paramSpec.default_expr ? line : "",
    //                 paramSpec.default_expr ? paramSpec.default_expr : "",
    //             ])
    //         ),
    //         align(
    //             2,
    //             fill([
    //                 "",
    //                 paramSpec.default_expr ? line : "",
    //                 paramSpec.default_expr ? paramSpec.default_expr : "",
    //             ])
    //         ),
    //         { groupId: paramSpecGroupId }
    //     ),
    // ]);
};
function printSubpDecl(subpDecl, subpDeclOptions) {
    function printSubpDeclTall(subpDecl) {
        console.log("tall");
        return subpDecl.overriding
            ? [subpDecl.overriding, utils_1.hardline, printSubpSpec(subpDecl.spec)]
            : printSubpSpec(subpDecl.spec);
    }
    function printSubpDeclCompact(subpDecl) {
        console.log("compact");
        return subpDecl.overriding
            ? (0, utils_1.fill)([subpDecl.overriding, utils_1.line, printSubpSpec(subpDecl.spec)])
            : printSubpSpec(subpDecl.spec);
    }
    console.debug(subpDeclOptions.layout);
    switch (subpDeclOptions.layout) {
        case "tall":
            return printSubpDeclTall(subpDecl);
        case "compact":
            return printSubpDeclCompact(subpDecl);
    }
}
exports.printSubpDecl = printSubpDecl;
function printSubpSpec(subpSpec) {
    console.debug("printSubpSpec");
    let return_part = [];
    if (subpSpec.returns) {
        return_part = [utils_1.line, "return ", subpSpec.returns];
    }
    const groupId = Symbol();
    const printParamSpecOptions = {
        parentGroup: groupId,
        paramsMaxLength: Math.max(...subpSpec.parameters.map((paramSpec) => paramSpec.ids.length)),
        paramsModeMaxLength: Math.max(...subpSpec.parameters.map((paramSpec) => paramSpec.mode ? paramSpec.mode.length : 0)),
    };
    return (0, utils_1.group)([
        subpSpec.subp_kind,
        " ",
        subpSpec.name,
        (0, utils_1.align)(2, [
            utils_1.line,
            "(",
            (0, utils_1.align)(1, [
                (0, utils_1.join)([";", utils_1.line], subpSpec.parameters.map((param) => printParamSpec(param, printParamSpecOptions))),
                ")",
                return_part,
            ].flat()),
        ]),
        ";",
    ], { id: groupId });
}
exports.printSubpSpec = printSubpSpec;
