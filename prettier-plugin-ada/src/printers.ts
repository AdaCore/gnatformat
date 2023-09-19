import { Doc } from "prettier";
import {
    DottedName,
    Identifier,
    PackageDecl,
    ParamSpec,
    SubpDecl,
    SubpSpec,
} from "./ast";
import {
    align,
    fill,
    fillBackwards3,
    group,
    hardline,
    ifBreak,
    indent,
    join,
    line,
} from "./utils";

export const printIdentifier: (Identifier: Identifier) => Doc = (
    idenfier: Identifier
) => {
    return idenfier.name;
};

export function printDottedName(dottedName: DottedName): Doc {
    return group([
        printIdentifier(dottedName.prefix),
        ".",
        indent([hardline, printIdentifier(dottedName.suffix)]),
    ]);
}

export const printPublicPart = (publicPart: string[]): Doc => {
    return join(hardline, publicPart);
};

export const printPrivatePart = (privatePart: PackageDecl[]): Doc => {
    return [
        "private",
        indent([
            hardline,
            join(
                hardline,
                privatePart.map((packageDecl) => printPackageDecl(packageDecl))
            ),
        ]),
    ];
};

export const printPackageDecl = (packageDecl: PackageDecl): Doc => {
    return [
        "package ",
        packageDecl.packageName,
        " is",
        indent([hardline, printPublicPart(packageDecl.publicPart)]),
        packageDecl.privatePart
            ? [hardline, printPrivatePart(packageDecl.privatePart)]
            : "",
        hardline,
        "end ",
        packageDecl.packageName,
        ";",
    ];
};

type PrintParamSpecOptions = {
    parentGroup: symbol;
    paramsMaxLength: number;
    paramsModeMaxLength: number;
};

const printParamSpec = (
    paramSpec: ParamSpec,
    options?: PrintParamSpecOptions
): Doc => {
    const paramLength = paramSpec.ids.length;
    const paramAlign = options?.paramsMaxLength
        ? " ".repeat(options.paramsMaxLength - paramLength)
        : "";
    const paramModeLength = paramSpec.mode ? paramSpec.mode.length : -1;
    const paramModeAlign = " ".repeat(
        options?.paramsModeMaxLength
            ? options.paramsModeMaxLength - paramModeLength
            : 0
    );

    const groupIds = Array(2).map(() => Symbol());
    const paramIdPart = group([
        paramSpec.ids,
        ifBreak(paramAlign, "", { groupId: options?.parentGroup }),
        " :",
    ]);
    const paramTypePart = [
        ifBreak("", paramModeAlign, { groupId: groupIds[0] }),
        paramSpec.mode ? paramSpec.mode.concat(" ") : "",
        paramSpec.type,
        paramSpec.defaultExpr ? " :=" : "",
    ];
    const paramDefaultExprPart = [
        paramSpec.defaultExpr ? paramSpec.defaultExpr : "",
    ];

    return fillBackwards3(
        paramIdPart,
        paramTypePart,
        paramDefaultExprPart,
        groupIds[0],
        groupIds[1]
    );
};

type SubpDeclOptions = {
    layout: "tall" | "compact";
};

export function printSubpDecl(
    subpDecl: SubpDecl,
    subpDeclOptions: SubpDeclOptions
): Doc {
    function printSubpDeclTall(subpDecl: SubpDecl): Doc {
        return subpDecl.overriding
            ? [subpDecl.overriding, hardline, printSubpSpec(subpDecl.spec)]
            : printSubpSpec(subpDecl.spec);
    }

    function printSubpDeclCompact(subpDecl: SubpDecl): Doc {
        return subpDecl.overriding
            ? fill([subpDecl.overriding, line, printSubpSpec(subpDecl.spec)])
            : printSubpSpec(subpDecl.spec);
    }

    switch (subpDeclOptions.layout) {
        case "tall":
            return printSubpDeclTall(subpDecl);
        case "compact":
            return printSubpDeclCompact(subpDecl);
    }
}

export function printSubpSpec(subpSpec: SubpSpec): Doc {
    let return_part: Doc[] = [];
    if (subpSpec.returns) {
        return_part = [line, "return ", subpSpec.returns];
    }
    const groupId = Symbol();
    const printParamSpecOptions = {
        parentGroup: groupId,
        paramsMaxLength: Math.max(
            ...subpSpec.parameters.map((paramSpec) => paramSpec.ids.length)
        ),
        paramsModeMaxLength: Math.max(
            ...subpSpec.parameters.map((paramSpec) =>
                paramSpec.mode ? paramSpec.mode.length : 0
            )
        ),
    };

    return group(
        [
            subpSpec.subpKind,
            " ",
            subpSpec.name,
            align(2, [
                line,
                "(",
                align(
                    1,
                    [
                        join(
                            [";", line],
                            subpSpec.parameters.map((param) =>
                                printParamSpec(param, printParamSpecOptions)
                            )
                        ),
                        ")",
                        return_part,
                    ].flat()
                ),
            ]),
            ";",
        ],
        { id: groupId }
    );
}
