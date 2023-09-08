"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.printers = exports.languages = exports.parsers = void 0;
const child_process_1 = require("child_process");
const printers_1 = require("./printers");
function parseAda(text, 
// eslint-disable-next-line @typescript-eslint/no-unused-vars
_parsers, 
// eslint-disable-next-line @typescript-eslint/no-unused-vars
_options) {
    const child = (0, child_process_1.spawnSync)("./obj/ada_ast", ["Subp_Decl_Rule"], {
        input: text,
    });
    const error = child.stderr.toString();
    if (error) {
        throw new Error(error);
    }
    const response = child.stdout.toString();
    return JSON.parse(response);
}
function printAda(path, options, 
// eslint-disable-next-line @typescript-eslint/no-unused-vars
_print) {
    const node = path.getValue();
    switch (node.kind) {
        case "Identifier": {
            return (0, printers_1.printIdentifier)(node);
        }
        case "SubpDecl": {
            return (0, printers_1.printSubpDecl)(node, { layout: "tall" });
        }
        case "SubpSpec": {
            return (0, printers_1.printSubpSpec)(node);
        }
        default:
            break;
    }
}
exports.parsers = {
    "ada-parse": {
        parse: parseAda,
        astFormat: "ada-ast",
    },
};
exports.languages = [
    {
        extensions: [".adb", ".ads"],
        name: "Ada",
        parsers: ["ada-parse"],
    },
];
exports.printers = {
    "ada-ast": {
        print: printAda,
    },
};
