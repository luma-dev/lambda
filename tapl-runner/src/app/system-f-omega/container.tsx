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
        <Typography variant="h2">System Fω</Typography>
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
                <code>fix</code>{" "}
                演算子を使うと停止しないコードになる可能性があります。
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
              <Link href="https://en.wikipedia.org/wiki/System_F#System_F.CF.89">
                System F<sub>ω</sub> - Wikipedia
              </Link>
            </li>
          </ul>
        </Box>
      </Box>
    </Container>
  );
}
