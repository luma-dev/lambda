declare module "*.txt" {
  const content: string;
  export default content;
}

declare module "monaco-vim" {
  import { editor } from "monaco-editor";
  class VimMode {
    dispose(): void;
  }
  export function initVimMode(
    editor: editor.IStandaloneCodeEditor,
    statusbarNode?: HTMLElement,
  ): VimMode;
}
