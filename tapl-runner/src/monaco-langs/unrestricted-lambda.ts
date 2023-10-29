import type * as monaco from "monaco-editor";

const id = "unrestricted-lambda";
const conf: monaco.languages.LanguageConfiguration = {
  comments: {
    lineComment: "//",
  },
  brackets: [
    ["{", "}"],
    ["[", "]"],
    ["(", ")"],
  ],
  autoClosingPairs: [
    { open: "{", close: "}" },
    { open: "[", close: "]" },
    { open: "(", close: ")" },
  ],
};
const def: monaco.languages.IMonarchLanguage = {
  defaultToken: "invalid",
  tokenPostfix: ".unrestricted-lambda",
  keywords: ["def", "type", "axiom", "all", "ex", "pi", "let", "true", "false"],

  identifier: /[a-zA-Z][\w]*/,
  numberInteger: /0(?![box])|[1-9]\d*|0b[01]+|0o[0-7]+|0x[\da-fA-F]+/,

  tokenizer: {
    root: [
      { include: "@whitespace" },

      ["Prop", "type"],
      ["Prf", "type"],

      [/#@numberInteger/, "debug-token"],
      [/\$@identifier/, "variable"],
      { include: "@number" },

      { include: "@identifier" },
      [/[{}()[\]]/, "@brackets"],

      ["->", "operators"],
      ["::", "operators"],
      [":", "operators"],
      [/\*/, "operators"],
      ["=>", "operators"],
      [/\./, "operators"],
      ["=", "operators"],
      [";", "operators"],
      ["_", "operators"],
      [/\\/, "keyword"],
    ],

    whitespace: [
      [/[ \t\r\n]+/, "white"],
      [/\/\/.*$/, "comment"],
    ],

    number: [[/@numberInteger/, "number"]],

    identifier: [
      [
        /@identifier/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "variable",
          },
        },
      ],
    ],
  },
};

export const idUnrestrictedLambda = id;
export const registerUnrestrictedLambda = (
  monaco: typeof import("monaco-editor"),
) => {
  monaco.languages.register({ id });
  monaco.languages.setMonarchTokensProvider(id, def);
  monaco.languages.setLanguageConfiguration(id, conf);
};
// export const useUnrestrictedLambda = () => {
//   useEffect(() => {
//     if (monaco != null) {
//       monaco.languages.register({ id });
//       monaco.languages.setMonarchTokensProvider(id, def);
//       monaco.languages.setLanguageConfiguration(id, conf);
//     }
//   }, []);
//   return id;
// };
