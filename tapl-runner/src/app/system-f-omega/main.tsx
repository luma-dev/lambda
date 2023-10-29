"use client";
import CodeEditor from "@/components/code-editor/CodeEditor";
import ExampleSelectModal from "@/components/example-select/ExampleSelectModal";
import { useCursoredControlCustom } from "@/lib/cursored/custom";
import {
  idUnrestrictedLambda,
  registerUnrestrictedLambda,
} from "@/monaco-langs/unrestricted-lambda";
import { Box, Button, Stack, Typography } from "@mui/material";
import { useSuspenseQuery } from "@tanstack/react-query";
import { editor } from "monaco-editor";
import { useCallback, useEffect, useMemo, useState } from "react";
import type { EditorDidMount } from "react-monaco-editor";

type System = typeof import("@/lib/systems/system-f-omega");

const editorOptions: editor.IStandaloneEditorConstructionOptions = {
  renderWhitespace: "boundary",
  tabSize: 2,
};
const editorOptionsReadonly: editor.IStandaloneEditorConstructionOptions = {
  ...editorOptions,
  readOnly: true,
};

const useTermEval = (system: System, code: string) => {
  const cursored = useMemo(() => {
    return new system.UntypedArithTermCursored(
      // TODO: memory leak
      system.TermEvaluator.parse(code),
    );
  }, [code, system]);

  const control = useCursoredControlCustom({
    cursored: cursored,
    smallStepMemoQueueSize: 50,
  });

  const formattedCode = useMemo(() => {
    return control.valWrapped.val.formattedCode();
  }, [control.valWrapped]);

  const parseResultDiagnosticsString = useMemo(() => {
    return control.valWrapped.val.getParseResultDiagnosticsString();
  }, [control.valWrapped]);

  const isParseOk = useMemo(() => {
    return control.valWrapped.val.isParseOk();
  }, [control.valWrapped]);

  return {
    control,
    cursored,
    formattedCode,
    parseResultDiagnosticsString,
    isParseOk,
  };
};

const useTypeEval = (
  system: System,
  termEval: ReturnType<typeof useTermEval>,
) => {
  const cursored = useMemo(() => {
    return new system.UntypedArithTypeCursored(
      // TODO: memory leak
      system.TypeEvaluator.fromTermEvaluator(
        termEval.control.valWrapped.val.clone(),
      ),
    );
  }, [system, termEval.control.valWrapped]);

  const control = useCursoredControlCustom({
    cursored: cursored,
    smallStepMemoQueueSize: 50,
  });

  const formattedType = useMemo(() => {
    return control.valWrapped.val.formattedType();
  }, [control.valWrapped]);

  const isTypingOk = useMemo(() => {
    return control.valWrapped.val.isTypingOk();
  }, [control.valWrapped]);

  return {
    control,
    cursored,
    formattedType,
    isTypingOk,
  };
};

type TermEvalProps = {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
  readonly termEval: ReturnType<typeof useTermEval>;
};
function TermEval({
  monacoVim,
  vimMode,
  termEval: {
    control: {
      next,
      next100,
      nextDisabled,
      prev,
      prev100,
      prevDisabled,
      reset,
      resetDisabled,
      step,
    },
    formattedCode,
    isParseOk,
  },
}: TermEvalProps) {
  return (
    <Box>
      <Typography variant="h3" my={2}>
        項の評価
      </Typography>
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
      <CodeEditor
        monacoVim={monacoVim}
        vimMode={vimMode}
        height="400px"
        language={idUnrestrictedLambda}
        options={editorOptionsReadonly}
        theme="vs-dark"
        value={formattedCode}
      />
    </Box>
  );
}

type TypeEvalProps = {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
  readonly typeEval: ReturnType<typeof useTypeEval>;
};
function TypeEval({
  monacoVim,
  vimMode,
  typeEval: {
    control: {
      next,
      next100,
      nextDisabled,
      prev,
      prev100,
      prevDisabled,
      reset,
      resetDisabled,
      step,
    },
    formattedType,
    isTypingOk,
  },
}: TypeEvalProps) {
  return (
    <Box>
      <Typography variant="h3" my={2}>
        型付けとその評価
      </Typography>
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
        {isTypingOk === false && (
          <Typography variant="caption" my={2} color="error">
            Typing Error
          </Typography>
        )}
      </Stack>
      <Box>STEP: {step.toString()}</Box>
      <CodeEditor
        monacoVim={monacoVim}
        vimMode={vimMode}
        height="400px"
        language={idUnrestrictedLambda}
        options={editorOptionsReadonly}
        theme="vs-dark"
        value={formattedType}
      />
    </Box>
  );
}

type Props = {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
};
export default function Main({ monacoVim, vimMode }: Props) {
  const system = useSuspenseQuery({
    queryKey: ["import:@/lib/systems/system-f-omega"],
    queryFn: () => import("@/lib/systems/system-f-omega"),
  }).data;
  const examples = useSuspenseQuery({
    queryKey: ["import:@/app/system-f-omega/examples"],
    queryFn: () => import("@/app/system-f-omega/examples"),
  }).data;

  const defaultCode = examples.defaultExample.code;
  const [code, setCode] = useState(defaultCode);
  const [codeToUpdate, setCodeToUpdate] = useState(defaultCode);
  const [shouldUpdate, setShouldUpdate] = useState(false);

  const [examplesOpen, setExamplesOpen] = useState(false);
  const handleOpenExample = (newCode: string) => {
    setExamplesOpen(false);
    setCode(newCode);
    setCodeToUpdate(code);
    setShouldUpdate(true);
  };

  useEffect(() => {
    if (!shouldUpdate) return;
    setCodeToUpdate(code);
    setShouldUpdate(false);
  }, [shouldUpdate, code]);

  const handleDidMount: EditorDidMount = useCallback((_editor, monaco) => {
    registerUnrestrictedLambda(monaco);
  }, []);

  const handleCodeChange = useCallback((newValue: string | undefined) => {
    setCode(newValue ?? "");
  }, []);

  const termEval = useTermEval(system, code);
  const typeEval = useTypeEval(system, termEval);

  return (
    <>
      <ExampleSelectModal
        open={examplesOpen}
        examples={examples?.examples ?? []}
        onOpenExample={handleOpenExample}
        onClose={() => setExamplesOpen(false)}
      />
      <Button
        onClick={() => setExamplesOpen(true)}
        disabled={typeof window === "undefined" || examples === undefined}
      >
        例を選択
      </Button>
      <Box>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          value={codeToUpdate}
          height="400px"
          options={editorOptions}
          language={idUnrestrictedLambda}
          onMount={handleDidMount}
          onChange={handleCodeChange}
          theme="vs-dark"
        />
      </Box>
      <TermEval monacoVim={monacoVim} vimMode={vimMode} termEval={termEval} />
      <TypeEval monacoVim={monacoVim} vimMode={vimMode} typeEval={typeEval} />
      <Box>
        <Typography variant="h3" my={2}>
          パース時の警告・エラーなど
        </Typography>
        <Box
          sx={{
            mh: 200,
            overflowX: "hidden",
          }}
        >
          <Box sx={{ overflowX: "scroll", width: "100%" }} component="pre">
            {termEval.parseResultDiagnosticsString === ""
              ? "なし"
              : termEval.parseResultDiagnosticsString}
          </Box>
        </Box>
      </Box>
    </>
  );
}
