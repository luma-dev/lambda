import { CodeExample, CodeExampleString } from "@/components/example-select";
import { dedent } from "@qnighy/dedent";
import introBool from "./examples/intro_bool.txt";

export const examples = [
  {
    title: "Bool: 基本の演算",
    description: dedent`
      Bool
      値: true / false
      基本の演算: if <cond> { <then> } else { <else> }
    `,
    code: introBool,
  },
  {
    title: "Nat: 5以下かを判定",
    description: dedent`\
      Nat
      値: 0, 1, ...
      基本の演算:
        succ, pred 後者・前者関数
        is_zero ゼロかを判定
    `,
    code: async () =>
      await import("./examples/intro_nat.txt").then((m) => m.default),
  },
  {
    title: "Unit",
    description: dedent`\
      Unit
      値: unit
      基本の演算: (x1; x2)
    `,
    code: async () =>
      await import("./examples/intro_unit.txt").then((m) => m.default),
  },
  {
    title: "自然数の足し算",
    description: "fix演算子を利用して再帰的な定義を持つ関数の定義をする",
    code: async () =>
      await import("./examples/nat_add.txt").then((m) => m.default),
  },
  {
    title: "チャーチ数上の指数関数",
    description: dedent`\
      高階型システムにより、単純型システムでは不可能だった指数関数が定義可能
    `,
    code: async () =>
      await import("./examples/church_exp.txt").then((m) => m.default),
  },
  {
    title: "関数の合成の抽象化",
    description:
      "項の型による抽象（パラメータ多層）によって、一般化した関数の合成を定義",
    code: async () =>
      await import("./examples/parametric_composition.txt").then(
        (m) => m.default,
      ),
  },
  {
    title: "発散1",
    description: dedent`\
      [注意: 非停止の項]
      fix演算子の導入は(ある意味での)健全性を失います。停止しない項を記述できてしまい、また、任意の型に対する項を定義できてしまいます
    `,
    code: async () =>
      await import("./examples/diverge1.txt").then((m) => m.default),
  },
  {
    title: "導入: 全称型",
    description: dedent`\
      「型による項の抽象」を表す型
    `,
    code: async () =>
      await import("./examples/intro_forall.txt").then((m) => m.default),
  },
  {
    title: "型の計算が必要な例",
    description: dedent`\
      型付けによって得られた型は、評価を進められる場合がある
    `,
    code: async () =>
      await import("./examples/type_calculation.txt").then((m) => m.default),
  },
  {
    title: "型の計算が指数爆発",
    description: () => (
      <pre>
        型の計算が型のサイズに対して指数関数的に増加するケースがある
        {"\n"}
        これはSystem F<sub>3</sub> の時点で起きてしまう
        {"\n"}
        (System F<sub>2</sub> については未調査)
      </pre>
    ),
    code: async () =>
      await import("./examples/type_calculation_exponential.txt").then(
        (m) => m.default,
      ),
  },
  {
    title: "カリー＝ハワード同型対応",
    description: () => (
      <pre>
        System F は直観主義の命題論理に対応する。
        {"\n"}
        なお、ボトム型相当を定義・マッチさせる方法がないため、⊥ → φ
        などは証明できない。 論理和・積を記述する方法もない。
        {"\n"}
        この例では、型付けが正常に行われていれば各項が実際に証明になっているかが確認できる。
      </pre>
    ),
    code: async () =>
      await import("./examples/curry_howard.txt").then((m) => m.default),
  },
] as const satisfies readonly CodeExample[];
export const defaultExample = examples[0] satisfies CodeExampleString;
