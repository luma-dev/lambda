"use client";
import CodeEditor from "@/components/code-editor/CodeEditor";
import { useCursoredControlCustom } from "@/lib/cursored/custom";
import {
  idUntypedArith,
  registerUntypedArithLanguage,
} from "@/monaco-langs/untyped-arith";
import { Box, Button, Stack, Typography } from "@mui/material";
import { dedent } from "@qnighy/dedent";
import { useSuspenseQuery } from "@tanstack/react-query";
import katex from "katex";
import { useCallback, useMemo, useState } from "react";
import type { EditorDidMount } from "react-monaco-editor";

const examples = [
  "if iszero 0 then true else false",
  dedent`\
    if iszero succ 0
        then false
        else iszero 0
  `,
  dedent`\
    if iszero pred succ pred 0
        then if iszero succ pred pred 0
          then iszero true
          else succ pred 0
        else iszero 0
  `,
];

type Props = {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
};
export default function Main({ monacoVim, vimMode }: Props) {
  const system = useSuspenseQuery({
    queryKey: ["import:@/lib/systems/untyped-arith"],
    queryFn: () => import("@/lib/systems/untyped-arith"),
  }).data;
  const defaultCode = examples[1];
  const [code, setCode] = useState(defaultCode);

  const handleDidMount: EditorDidMount = useCallback((_editor, monaco) => {
    registerUntypedArithLanguage(monaco);
  }, []);

  const handleCodeChange = useCallback((newValue: string | undefined) => {
    setCode(newValue ?? "");
  }, []);

  const cursored = useMemo(() => {
    // TODO: memory leak
    return new system.UntypedArithCursored(system.Evaluator.parse(code));
  }, [system.Evaluator, system.UntypedArithCursored, code]);

  const {
    next,
    nextDisabled,
    prev,
    prevDisabled,
    reset,
    resetDisabled,
    step,
    valWrapped,
    prev100,
    next100,
  } = useCursoredControlCustom({
    cursored,
    smallStepMemoQueueSize: 50,
  });

  const formattedLatex = useMemo(() => {
    return katex.renderToString(valWrapped.val.formattedLatex());
  }, [valWrapped]);
  const formattedCode = useMemo(() => {
    return valWrapped.val.formattedCode();
  }, [valWrapped]);
  const isParseOk = useMemo(() => {
    return valWrapped.val.isParseOk();
  }, [valWrapped]);

  return (
    <>
      <Box>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          defaultValue={defaultCode}
          height="400px"
          language={idUntypedArith}
          onMount={handleDidMount}
          onChange={handleCodeChange}
          theme="vs-dark"
        />
      </Box>
      <Stack direction="row" gap={3}>
        <Button disabled={resetDisabled} onClick={reset}>
          reset
        </Button>
        <Button disabled={prevDisabled} onClick={prev}>
          prev
        </Button>
        <Button disabled={nextDisabled} onClick={next}>
          next
        </Button>
        <Button disabled={prevDisabled} onClick={prev100}>
          prev100
        </Button>
        <Button disabled={nextDisabled} onClick={next100}>
          next100
        </Button>
        {isParseOk === false && (
          <Typography variant="caption" my={2} color="error">
            Parse Error
          </Typography>
        )}
      </Stack>
      <Box>STEP: {step.toString()}</Box>
      <Box maxWidth="100%" sx={{ overflowX: "scroll" }}>
        <div dangerouslySetInnerHTML={{ __html: formattedLatex }} />
      </Box>
      <Box>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          height="400px"
          language={idUntypedArith}
          options={{ readOnly: true }}
          theme="vs-dark"
          value={formattedCode}
        />
      </Box>
    </>
  );
}
