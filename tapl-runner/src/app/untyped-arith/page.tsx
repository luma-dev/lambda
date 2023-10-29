import { Metadata } from "next";
import MainContainer from "./container";

export const metadata: Metadata = {
  title: "Untyped Arith",
};

async function getStaticDescription() {
  const katex = await import("katex").then((m) => m.default);
  const META_T = String.raw`\textcolor{#A4D0A4}{\mathrm{\bold t}}`;
  const SYNTAX_HTML = katex.renderToString(
    String.raw`
    \begin{align*}
      ${META_T} \Coloneqq & \\
      & \mathrm{true} \\
      & \mathrm{false} \\
      & \mathrm{if}
          \medspace ${META_T} \medspace
        \mathrm{then}
          \medspace ${META_T} \medspace
        \mathrm{else}
          \medspace ${META_T} \\
      & \mathrm{0} \\
      & \mathrm{succ} \space ${META_T} \\
      & \mathrm{pred} \space ${META_T} \\
      & \mathrm{iszero} \space ${META_T} \\
    \end{align*}
  `,
    { displayMode: true },
  );

  return { SYNTAX_HTML };
}

export default async function Page() {
  const { SYNTAX_HTML } = await getStaticDescription();
  return <MainContainer SYNTAX_HTML={SYNTAX_HTML} />;
}
