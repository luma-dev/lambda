import type * as monaco from "monaco-editor";

const id = "untyped-arith";
const conf: monaco.languages.LanguageConfiguration = {};
const def: monaco.languages.IMonarchLanguage = {
  tokenPostfix: ".untyped-arith",
  keywords: ["if", "then", "else", "true", "false"],

  identifier: /[a-zA-Z$][\w$]*/,

  numberInteger: /0/,

  tokenizer: {
    root: [
      { include: "@whitespace" },

      { include: "@number" },

      { include: "@identifier" },

      ["succ", "operators"],
      ["pred", "operators"],
      ["iszero", "operators"],
    ],

    whitespace: [[/[ \t\r\n]+/, "white"]],

    number: [[/@numberInteger/, "number"]],

    identifier: [
      [
        /@identifier/,
        {
          cases: {
            "@keywords": "keyword",
            "@default": "",
          },
        },
      ],
    ],
  },
};

export const idUntypedArith = id;
export const registerUntypedArithLanguage = (
  monaco: typeof import("monaco-editor"),
) => {
  monaco.languages.register({ id });
  monaco.languages.setMonarchTokensProvider(id, def);
  monaco.languages.setLanguageConfiguration(id, conf);
};
// export const useUntypedArithLanguage = () => {
//   const monaco = useMonaco();
//   useEffect(() => {
//     if (monaco != null) {
//       monaco.languages.register({ id });
//       monaco.languages.setMonarchTokensProvider(id, def);
//       monaco.languages.setLanguageConfiguration(id, conf);
//     }
//   }, [monaco]);
//   return id;
// };
