import { spawnSync } from "child_process";

import { type Doc, type AstPath } from "prettier";
import { printIdentifier, printSubpDecl, printSubpSpec } from "./printers";
import { AdaNode, Identifier, SubpDecl, SubpSpec } from "./ast";

type parseOptions = {
    rule: string;
};

function parseAda(
    text: string,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _parsers: object,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _options: parseOptions
): AdaNode {
    const child = spawnSync("./obj/ada_ast", ["Subp_Decl_Rule"], {
        input: text,
    });

    const error = child.stderr.toString();

    if (error) {
        throw new Error(error);
    }

    const response = child.stdout.toString();

    return JSON.parse(response) as AdaNode;
}

type printOptions = {
    layout: "tall" | "compact";
};

function printAda(
    path: AstPath,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _options: printOptions,
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    _print: (
        selector?: string | number | Array<string | number> | AstPath
    ) => Doc
) {
    const node = path.getValue();

    switch (node.kind) {
        case "Identifier": {
            return printIdentifier(node as Identifier);
        }
        case "SubpDecl": {
            return printSubpDecl(node as SubpDecl, { layout: "tall" });
        }
        case "SubpSpec": {
            return printSubpSpec(node as SubpSpec);
        }
        default:
            break;
    }
}

export const parsers = {
    "ada-parse": {
        parse: parseAda,
        astFormat: "ada-ast",
    },
};

export const languages = [
    {
        extensions: [".adb", ".ads"],
        name: "Ada",
        parsers: ["ada-parse"],
    },
];

export const printers = {
    "ada-ast": {
        print: printAda,
    },
};
