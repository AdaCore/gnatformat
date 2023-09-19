export type AdaNode = {
    kind: string;
    commentBefore?: string;
    commentEndOfLine?: string;
    commentAfter?: string;
};

export type Identifier = {
    kind: "Identifier";
    name: string;
};

export type DottedName = {
    kind: "DottedName";
    suffix: Identifier;
    prefix: Identifier;
};

export type SubpDecl = AdaNode & {
    kind: "SubpDecl";
    overriding?: "overriding" | "not overriding";
    spec: SubpSpec;
    aspects?: string; // Aspect_Spec
};

export type SubpSpec = AdaNode & {
    kind: "SubpSpec";
    subpKind: "procedure" | "function";
    name: string; // Dotted_Name
    parameters: ParamSpec[];
    returns?: string; // Type_Expr
};

export type PackageDecl = {
    packageName: string;
    publicPart: string[];
    privatePart?: PackageDecl[];
};

export type ParamSpec = AdaNode & {
    kind: "ParamSpec";
    ids: string; // Defining_Name_List
    mode?: "in" | "out" | "in out";
    aliased: boolean;
    type: string; // Type_Expr
    defaultExpr?: string; // Expr
    aspects?: string; // Aspect_Spec
};
