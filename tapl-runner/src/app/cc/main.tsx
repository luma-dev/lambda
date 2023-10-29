"use client";
import CodeEditor from "@/components/code-editor/CodeEditor";
import ExampleSelectModal from "@/components/example-select/ExampleSelectModal";
import {
  idUnrestrictedLambda,
  registerUnrestrictedLambda,
} from "@/monaco-langs/unrestricted-lambda";
import { Box, Button, Stack, Typography } from "@mui/material";
import { useSuspenseQuery } from "@tanstack/react-query";
import { editor } from "monaco-editor";
import { useCallback, useEffect, useState } from "react";
import type { EditorDidMount } from "react-monaco-editor";
import { P, match } from "ts-pattern";

type System = typeof import("@/lib/systems/cc");

const editorOptions: editor.IStandaloneEditorConstructionOptions = {
  renderWhitespace: "boundary",
  tabSize: 2,
};
const editorOptionsReadonly: editor.IStandaloneEditorConstructionOptions = {
  ...editorOptions,
  readOnly: true,
};

const useCalc = (system: System, code: string) => {
  const [isEvaluated, setIsEvaluated] = useState(false);
  const evaluateDisabled = isEvaluated;
  const [isTyped, setIsTyped] = useState(false);
  const typingDisabled = isTyped;
  const [evaluate, setEvaluate] = useState(() => () => {});
  const [typing, setTyping] = useState(() => () => {});
  const [formattedCode, setFormattedCode] = useState("");
  const [evaluatedFormattedCode, setEvaluatedFormattedCode] = useState("");
  const [typeFormattedCode, setTypeFormattedCode] = useState("");
  const [parseResultDiagnosticsSummary, setParseResultDiagnosticsString] =
    useState("");
  const [typingDiagnosticSummary, setTypingDiagnosticString] = useState("");
  const [evalDiagnosticSummary, setEvalDiagnosticString] = useState("");
  const [isParsed, setIsParsed] = useState(false);
  const [isParseOk, setIsParseOk] = useState(false);

  useEffect(() => {
    setIsEvaluated(false);
    setIsTyped(false);
    setEvaluatedFormattedCode("");
    setTypingDiagnosticString("");
    setEvalDiagnosticString("");
    // TODO: memory leak
    const v = new system.LambdaCc(code);
    setIsParseOk(v.isParseOk());
    setIsParsed(true);
    setEvaluate(() => () => {
      setIsEvaluated(true);
      v.evaluate();
      setEvaluatedFormattedCode(v.evaluatedCode());
      setEvalDiagnosticString(v.getEvalDiagnosticsSummary());
    });
    setTyping(() => () => {
      setIsTyped(true);
      v.typing();
      setTypeFormattedCode(v.typedCode());
      setTypingDiagnosticString(v.getTypingDiagnosticsSummary());
    });
    setFormattedCode(v.toCode());
    setParseResultDiagnosticsString(v.getParseResultDiagnosticsSummary());

    return () => {
      v.free();
    };
  }, [code, system]);

  return {
    evaluate,
    evaluateDisabled,
    evaluatedFormattedCode,
    formattedCode,
    isEvaluated,
    isTyped,
    typeFormattedCode,
    typing,
    typingDisabled,
    isParsed,
    isParseOk,
    parseResultDiagnosticsSummary,
    typingDiagnosticSummary,
    evalDiagnosticSummary,
  };
};

type Props = {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
};
export default function Main({ monacoVim, vimMode }: Props) {
  const system = useSuspenseQuery({
    queryKey: ["import:@/lib/systems/cc"],
    queryFn: () => import("@/lib/systems/cc"),
  }).data;
  const examples = useSuspenseQuery({
    queryKey: ["import:@/app/cc/examples"],
    queryFn: () => import("@/app/cc/examples").then((m) => m),
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

  const {
    evaluate,
    evaluateDisabled,
    evaluatedFormattedCode,
    formattedCode,
    isEvaluated,
    isTyped,
    typeFormattedCode,
    typing,
    typingDisabled,
    isParsed,
    isParseOk,
    parseResultDiagnosticsSummary,
    typingDiagnosticSummary,
    evalDiagnosticSummary,
  } = useCalc(system, code);

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
          height="600px"
          options={editorOptions}
          language={idUnrestrictedLambda}
          onMount={handleDidMount}
          onChange={handleCodeChange}
        />
      </Box>
      <Box m={4}>
        <Stack direction="row" gap={3}>
          <Button
            disabled={typingDisabled || !isParseOk}
            onClick={typing}
            variant="outlined"
          >
            Type
          </Button>
          <Button
            disabled={evaluateDisabled || !isParseOk}
            onClick={evaluate}
            variant="outlined"
          >
            Evaluate
          </Button>
          {!isParseOk && isParsed && (
            <Typography variant="caption" my={2} color="error">
              Parse Error
            </Typography>
          )}
        </Stack>
      </Box>
      <Box>
        <Typography variant="h3" my={2}>
          型付け結果
        </Typography>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          height="400px"
          language={idUnrestrictedLambda}
          onMount={handleDidMount}
          options={editorOptionsReadonly}
          value={match([isTyped, typeFormattedCode])
            .with([true, P.not("")], () => typeFormattedCode)
            .with([true, P._], () => `<error>\n${typingDiagnosticSummary}`)
            .otherwise(() => "<Not yet typed>")}
        />
      </Box>
      <Box>
        <Typography variant="h3" my={2}>
          評価の結果
        </Typography>
        <Typography variant="caption" my={2} color="gray">
          評価はこのシステムの主目的ではないため、あくまでも参考程度にしてください。
        </Typography>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          height="400px"
          language={idUnrestrictedLambda}
          onMount={handleDidMount}
          options={editorOptionsReadonly}
          value={match([isEvaluated, evaluatedFormattedCode])
            .with([true, P.not("")], () => evaluatedFormattedCode)
            .with([true, P._], () => `<error>\n${evalDiagnosticSummary}`)
            .otherwise(() => "<Not yet evaluated>")}
        />
      </Box>
      <Box>
        <Typography variant="h3" my={2}>
          パースの結果
        </Typography>
        <CodeEditor
          monacoVim={monacoVim}
          vimMode={vimMode}
          height="400px"
          language={idUnrestrictedLambda}
          onMount={handleDidMount}
          options={editorOptionsReadonly}
          value={formattedCode}
        />
      </Box>
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
            {parseResultDiagnosticsSummary === ""
              ? "なし"
              : parseResultDiagnosticsSummary}
          </Box>
        </Box>
      </Box>
    </>
  );
}
