type CodeExampleGeneric<T> = {
  readonly title: string;
  readonly description: string | React.FC;
  readonly code: T;
};
export type CodeExampleString = CodeExampleGeneric<string>;
export type CodeExample = CodeExampleGeneric<
  string | (() => Promise<string> | string)
>;
