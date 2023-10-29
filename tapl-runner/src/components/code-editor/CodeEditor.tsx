"use client";
import Editor, { OnMount } from "@monaco-editor/react";
import type { editor as monacoEditor } from "monaco-editor";
import React, { useCallback, useEffect, useState } from "react";

const MemoizedEditor = React.memo(Editor);

const monkeyPatch = (editor: monacoEditor.IStandaloneCodeEditor) => {
  const origGetOption = editor.getOption.bind(editor);
  editor.getOption = (id: number) => {
    if (id > 16) {
      id -= 1;
    }
    // eslint-disable-next-line @typescript-eslint/no-unsafe-return
    return origGetOption(id);
  };
};

export type CodeEditorProps = React.ComponentProps<typeof Editor> & {
  readonly monacoVim?: typeof import("monaco-vim") | null;
  readonly vimMode?: boolean;
};
function CodeEditorInternal({
  monacoVim,
  vimMode = false,
  onMount,
  ...params
}: CodeEditorProps) {
  const [editor, setEditor] =
    useState<monacoEditor.IStandaloneCodeEditor | null>(null);

  useEffect(() => {
    if (
      monacoVim !== undefined &&
      monacoVim !== null &&
      editor !== null &&
      vimMode
    ) {
      const { initVimMode } = monacoVim;
      const vimMode = initVimMode(editor);
      return () => {
        vimMode.dispose();
      };
    }
  }, [monacoVim, editor, vimMode]);

  const handleDidMount: OnMount = useCallback(
    (editor, monaco) => {
      monkeyPatch(editor);
      setEditor(editor);
      onMount?.(editor, monaco);
    },
    [onMount],
  );

  return (
    <MemoizedEditor
      key=""
      theme="vs-dark"
      {...params}
      onMount={handleDidMount}
    />
  );
}

const CodeEditor = React.memo(CodeEditorInternal);
export default CodeEditor;
