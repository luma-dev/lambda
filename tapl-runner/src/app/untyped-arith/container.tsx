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

type MainContainerProp = {
  readonly SYNTAX_HTML: string;
};
export default function MainContainer({ SYNTAX_HTML }: MainContainerProp) {
  const { monacoVim, vimMode, setVimMode } = useVimMode();
  return (
    <Container>
      <Box py={4}>
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
        <Typography variant="h2">型なし計算（NatBool）</Typography>
        <Box>
          <ul>
            <li>
              <Typography variant="body1">
                構文に曖昧性がないため、括弧表記などはありません。
              </Typography>
            </li>
          </ul>
          <ul>
            <li>
              <Typography variant="body1">
                ループや再帰を実現する方法がなく、チューリング完全でない言語の例と言えます。電卓のようなものです。
              </Typography>
            </li>
          </ul>
        </Box>
        <Box>
          <Typography variant="h3">構文</Typography>
          <div dangerouslySetInnerHTML={{ __html: SYNTAX_HTML }} />
        </Box>
        <Suspense fallback={<Skeleton />}>
          <Main monacoVim={monacoVim} vimMode={vimMode} />
        </Suspense>
      </Box>
    </Container>
  );
}
