import { CodeExample, CodeExampleString } from "@/components/example-select";
import { dedent } from "@qnighy/dedent";
import { Lazy } from "lazy-val";
import INTRO from "./examples/intro.txt";

const natTemplate = new Lazy(() =>
  import("./examples/nat_template.txt").then((m) => m.default),
);

const natIndTemplate = new Lazy(() =>
  import("./examples/nat_ind_template.txt").then((m) => m.default),
);

const eqTemplate = new Lazy(() =>
  import("./examples/eq_template.txt").then((m) => m.default),
);

export const examples = [
  {
    title: "導入: PropとPrf",
    description: () => (
      <pre>
        CCはPropという「型の型(宇宙)」を表す型と、Propの項を型の世界へ持ってくるPrfという２つの基本的な型を持ちます。
        {"\n"}
        また、項として、通常のラムダ式に加えラムダの部分をallに変えたPropに属する項を記述することができます。
        {"\n"}
        たったこれだけの能力で、その表現力はSystem F<sub>ω</sub>
        を自然に内包し、加えて構成的に様々な構造体を追加の構文を必要とせず実現できます。
        （当然いくつかの糖衣構文は欲しくなるでしょう）
        {"\n"}
        Prfのルールも非常に単純で、(Prf all x: T. t) を (pi x: T. Prf t)
        にするというのを繰り返すのみです。
        {"\n"}
        piは依存型で、関数型 T → T の一般化にあたります。実際、 {
          "T1 -> T2"
        }{" "}
        はこの言語では pi _: T1. T2 の糖衣構文になります。
        {"\n"}
        ぜひ、他の例も通して、この圧倒的な表現力を体感してください。
      </pre>
    ),
    code: INTRO,
  },
  {
    title: "構成的な自然数",
    description: dedent`\
      CCでは自然数を始めとした構造体を構成的に定義可能です。
      そして、その定義はRustのstruct宣言のように読み替えられるような読み方ができます。
      最初はコツがいるかも知れません。
      さらに面白いことに，zeroとsuccを組み合わせる方法による自然数の定義は、Church Numeralsの定義と同じものになるのです。
      逆に言えば、Church Numeralsの定義に対し、CCを通して、自然な解釈を得られたとも捉えられます。
    `,
    code: async () =>
      [
        await natTemplate.value,
        await import("./examples/intro_nat.txt").then((m) => m.default),
      ].join("\n"),
  },
  {
    title: "挑戦: Vec",
    description: dedent`\
      読者の課題とする。
      (あとで整備します)
    `,
    code: async () =>
      [
        await natTemplate.value,
        await import("./examples/intro_vec.txt").then((m) => m.default),
      ].join("\n"),
  },
  {
    title: "1 + 1 = 2 の証明",
    description: dedent`\
      カリー＝ハワード同型対応により、CCは高階述語論理を展開できる。
      Leibnizによる等価性の定義を利用することにより、先程の自然数の定義と組み合わせて 1 + 1 = 2 が表現できる。
      この命題（型）に対応する証明（項）を見つけられる。型付けが成功することが確認できれば、すなわちその証明の正しさがこのシステムによって検証されたということになる。
      なお、この証明が言っていることは、足し算の定義に従い計算したら同じものになる、というだけである。
      より興味深い例は、 n + 0 = n などを参照。
    `,
    code: async () =>
      [
        await natTemplate.value,
        await eqTemplate.value,
        await import("./examples/proof_add_1_1_eq_2.txt").then(
          (m) => m.default,
        ),
        "nat_add_1_1_eq_2_proof",
      ].join("\n"),
  },
  {
    title: "∀n ∈ ℕ, 0 + n = n の証明",
    description: dedent`\
      量化を含む定理は pi (Π) 型で表現される。
      足し算は左オペランドをマッチさせ0であれば右オペランドを返すように定義した。
      なので、 0 + n は n に簡約され、結局この証明は 1 + 1 = 2 の場合と同様である。
    `,
    code: async () =>
      [
        await natTemplate.value,
        await eqTemplate.value,
        await import("./examples/proof_add_0_n_eq_n.txt").then(
          (m) => m.default,
        ),
        "nat_add_0_n_eq_n_proof",
      ].join("\n"),
  },
  {
    title: "x = y ならば y = x",
    description: dedent`\
      この当たり前のように思える反射律も証明できる。
      Leibnizの等価性とは、 x = y は、 x に関する任意の命題の証明について、そのなかの x を y に置き換えたものを作れる、ということである。
      そして、その作り方を提示することが等価性を証明したことになる。
      x = y の前提のもとで、反射律は x = x の右側の x を y に置き換えることによって証明できる。
      ところでどのように「右側の x を置き換えたい」というのを記述するのだろうか。
      それは、置き換えたい箇所が引数となったラムダ式となる。
    `,
    code: async () =>
      [
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        "eq_sym_proof",
      ].join("\n"),
  },
  {
    title: "帰納法: n + 0 = n の証明",
    description: dedent`\
      0 + n = 0 と対比して、 n + 0 = n は評価するだけでは証明できない。
      足し算は左オペランドにマッチして分岐するが、この場合左オペランドについてまだ何も知らないから評価が進まない。
      この例は数学的帰納法が必要になる。
      なお、 nat_ind という帰納法を動かすための公理を追加しているが、通常はシステムが自動的にこれをいくつかの簡約規則とともに導入するのが通常である。(この機能はあとで追加予定）
    `,
    code: async () =>
      [
        await natTemplate.value,
        await natIndTemplate.value,
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        await import("./examples/proof_add_n_0_eq_n.txt").then(
          (m) => m.default,
        ),
        "nat_add_n_0_eq_n_proof",
      ].join("\n"),
  },
  {
    title: "挑戦: a + (b + c) = (a + b) + c の証明",
    description: dedent`\
      帰納法を利用すると、自然数の加算の結合律が証明できる。
      Adv. TaPL のエクササイズ 2.6.4 にもなっている。
    `,
    code: async () =>
      [
        await natTemplate.value,
        await natIndTemplate.value,
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        await import("./examples/proof_add_n_0_eq_n.txt").then(
          (m) => m.default,
        ),
        await import("./examples/challenge_proof_nat_associative.txt").then(
          (m) => m.default,
        ),
        "nat_add_associative_proof",
      ].join("\n"),
  },
  {
    title: "解答例: a + (b + c) = (a + b) + c の証明",
    description: dedent`\
    `,
    code: async () =>
      [
        await natTemplate.value,
        await natIndTemplate.value,
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        await import("./examples/proof_add_n_0_eq_n.txt").then(
          (m) => m.default,
        ),
        await import("./examples/proof_nat_associative.txt").then(
          (m) => m.default,
        ),
        "nat_add_associative_proof",
      ].join("\n"),
  },
  {
    title: "挑戦: n + m = m + n の証明",
    description: dedent`\
      帰納法を使うことで自然数の加算の可換律が証明できる。少し難しい。
      ヒント: 帰納法を二回使う。
    `,
    code: async () =>
      [
        await natTemplate.value,
        await natIndTemplate.value,
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        await import("./examples/proof_add_n_0_eq_n.txt").then(
          (m) => m.default,
        ),
        await import("./examples/challenge_proof_nat_commutative.txt").then(
          (m) => m.default,
        ),
        "nat_add_commutative_proof",
      ].join("\n"),
  },
  {
    title: "解答例: n + m = m + n の証明",
    description: dedent`\
    `,
    code: async () =>
      [
        await natTemplate.value,
        await natIndTemplate.value,
        await eqTemplate.value,
        await import("./examples/eq_sym_proof.txt").then((m) => m.default),
        await import("./examples/proof_add_n_0_eq_n.txt").then(
          (m) => m.default,
        ),
        await import("./examples/proof_nat_commutative.txt").then(
          (m) => m.default,
        ),
        "nat_add_commutative_proof",
      ].join("\n"),
  },
  {
    title: "挑戦: 命題論理",
    description: dedent`\
      これらはすべてProp上でなされるので、CC上の述語論理と組み合わせることができます。
      純粋には直観主義論理のみを展開できますが、axiomを利用して古典論理を展開することもできます。
    `,
    code: async () =>
      import("./examples/challenge_proposition_logic.txt").then(
        (m) => m.default,
      ),
  },
  {
    title: "解答例: 命題論理",
    description: dedent`\
    `,
    code: async () =>
      import("./examples/proposition_logic.txt").then((m) => m.default),
  },
] as const satisfies readonly CodeExample[];
export const defaultExample = examples[0] satisfies CodeExampleString;
