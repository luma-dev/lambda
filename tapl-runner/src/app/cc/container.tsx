"use client";
import { useVimMode } from "@/lib/vim-mode";
import {
  Box,
  Checkbox,
  Container,
  FormControlLabel,
  Link,
  Skeleton,
  Stack,
  Typography,
} from "@mui/material";
import { Suspense } from "react";
import Main from "./main";

export default function MainContainer() {
  const { monacoVim, vimMode, setVimMode } = useVimMode();
  return (
    <Container>
      <Box pt={4} pb={32}>
        <Stack direction="row">
          <Link my={2} href="/">
            Back
          </Link>
          <Box flexGrow={1} />
          <FormControlLabel
            control={
              <Checkbox
                inputProps={{ "aria-label": "Vim mode" }}
                onChange={(e) => setVimMode(e.target.checked)}
                checked={vimMode}
              />
            }
            label="Vim mode"
          />
        </Stack>
        <Typography variant="h2">Calculus of Construction（CC）</Typography>
        <Box>
          <ul>
            <li>
              <Typography variant="body1">TODO(ここに説明が来る)</Typography>
            </li>
            <li>
              <Typography variant="body1">
                保存機能等が現時点ではありませんので気をつけください
              </Typography>
            </li>
            <li>
              <Typography variant="body1">
                パースエラーは現状画面下部にスクロールすることで確認できます
              </Typography>
            </li>
            <li>
              <Typography variant="body1">
                パースは停止する確証があります。型付けと評価は停止するはずではありますが、無限ループっぽい挙動やパターンを見つけましたら教えていただけますと幸いです。
              </Typography>
            </li>
            <li>
              <Typography variant="body1">
                現時点で、 <code>nat_ind</code>
                のような帰納法を導入するために等価性の公理と共に導入する方法がありません
              </Typography>
            </li>
          </ul>
        </Box>
        <Suspense fallback={<Skeleton />}>
          <Main monacoVim={monacoVim} vimMode={vimMode} />
        </Suspense>
        <Box>
          <Typography variant="h3">参考</Typography>
          <ul>
            <li>Types and Programming Languages B. Pierce. MIT Press, 2002.</li>
            <li>
              Advanced Topics in Types and Programming Languages B. Pierce. MIT
              Press, 2005.
            </li>
            <li>
              The Calculus of Constructions T. Coquand and G. Huet. In
              Information and Computation, 76(2/3):95–120, 1988.
              <ul>
                <li>
                  型部分のみを評価するよう限定した場合の停止性の解析方法が紹介されている。
                  これを項も評価するようにした場合に応用する方法が自分では分からなかった。
                  誰かわかる方いらっしゃいましたらご教示くださいますと幸いです。
                </li>
              </ul>
            </li>
            <li>
              <Link href="https://en.wikipedia.org/wiki/Calculus_of_constructions">
                Calculus of constructions - Wikipedia
              </Link>
            </li>
          </ul>
        </Box>
      </Box>
    </Container>
  );
}
