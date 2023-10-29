import "@fontsource/roboto/300.css";
import "@fontsource/roboto/400.css";
import "@fontsource/roboto/500.css";
import "@fontsource/roboto/700.css";
import "katex/dist/katex.css";
import ClientLayout from "./ClientLayout";

export const metadata = {
  title: "Lambda Systems (lambda.luma.dev)",
  description:
    "Run your favorite lambda calculus based systems in your browser quickly",
  viewport: "initial-scale=1, width=device-width",
};

export default function RootLayout({
  children,
}: {
  children: React.ReactNode;
}) {
  return (
    <html lang="ja">
      <body>
        <ClientLayout>{children}</ClientLayout>
      </body>
    </html>
  );
}
