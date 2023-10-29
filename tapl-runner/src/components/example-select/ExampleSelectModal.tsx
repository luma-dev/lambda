import {
  Box,
  Button,
  Container,
  Modal,
  Stack,
  Typography,
} from "@mui/material";
import Grid from "@mui/material/Unstable_Grid2";
import { useEffect, useState } from "react";

import Paper from "@mui/material/Paper";
import { styled, useTheme } from "@mui/material/styles";
import { P, match } from "ts-pattern";
import { CodeExample } from ".";

const MyPaper = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === "dark" ? "#2A4047" : "#fff",
  ...theme.typography.body2,
  padding: theme.spacing(1),
  color: theme.palette.text.secondary,
}));

type ItemProps = {
  readonly example: CodeExample;
  readonly loading?: boolean;
  readonly onOpenExample?: (code: string) => void;
  readonly onStartLoading?: () => void;
};
const Item = function Item({
  example,
  loading,
  onOpenExample,
  onStartLoading,
}: ItemProps) {
  const theme = useTheme();

  const description = match(example.description)
    .with(P.string, (d) => d)
    .otherwise((Description) => <Description />);

  const handleOpen = () => {
    if (typeof example.code === "string") {
      onOpenExample?.(example.code);
    } else {
      onStartLoading?.();
      Promise.resolve(example.code())
        .then((code) => {
          onOpenExample?.(code);
        })
        .catch((e) => {
          console.error(e);
        });
    }
  };

  return (
    <MyPaper>
      <Typography
        variant="subtitle1"
        sx={{
          textAlign: "center",
          fontWeight: "bold",
        }}
      >
        {example.title}
      </Typography>
      <Typography
        variant="body2"
        sx={{
          whiteSpace: "pre-wrap",
          "& pre": {
            whiteSpace: "pre-wrap",
          },
        }}
      >
        {description}
      </Typography>
      <Stack direction="row-reverse">
        <Button
          disabled={loading}
          size="small"
          variant="contained"
          // in sm, widen the button
          sx={{
            mt: 1,
            [theme.breakpoints.down("sm")]: {
              width: "100%",
            },
          }}
          onClick={handleOpen}
        >
          開く
        </Button>
      </Stack>
    </MyPaper>
  );
};

export type ExampleSelectModalProps = {
  open: boolean;
  examples: readonly CodeExample[];
  onClose?: () => void;
  onOpenExample?: (code: string) => void;
};
export default function ExampleSelectModal({
  open,
  examples,
  onClose,
  onOpenExample,
}: ExampleSelectModalProps) {
  const theme = useTheme();

  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (!open) {
      setLoading(false);
    }
  }, [open]);

  const handleStartLoading = () => {
    setLoading(true);
  };

  const handleOpenExample = (code: string) => {
    onOpenExample?.(code);
  };

  return (
    <>
      <Modal
        open={open}
        onClose={onClose}
        aria-labelledby="parent-modal-title"
        aria-describedby="parent-modal-description"
      >
        <Stack
          alignItems="center"
          justifyContent="center"
          sx={{
            height: "100%",
            width: "100%",
          }}
        >
          <Box
            sx={{
              overflow: "hidden",
              height: "92%",
              width: "92%",
              bgcolor: theme.palette.mode === "dark" ? "#2A4047" : "#fff",
              [theme.breakpoints.down("sm")]: {
                height: "100%",
                width: "100%",
              },
            }}
          >
            <Stack sx={{ overflow: "hidden", height: "100%" }}>
              <Stack
                px={2}
                direction="row"
                justifyContent="space-between"
                alignItems="center"
              >
                <Typography
                  m={2}
                  variant="h5"
                  id="parent-modal-title"
                  component="h2"
                >
                  例を選択
                </Typography>
                <Box>
                  <Button
                    variant="contained"
                    onClick={onClose}
                    disabled={loading}
                  >
                    閉じる
                  </Button>
                </Box>
              </Stack>

              <Container
                sx={{
                  overflowY: "auto",
                  height: "100%",
                  pb: 14,
                }}
              >
                <Box sx={{ flexGrow: 1 }}>
                  <Grid container spacing={2}>
                    {examples.map((example) => (
                      <Grid key={example.title} xs={12} sm={6} md={4}>
                        <Item
                          example={example}
                          loading={loading}
                          onOpenExample={handleOpenExample}
                          onStartLoading={handleStartLoading}
                        />
                      </Grid>
                    ))}
                  </Grid>
                </Box>
              </Container>
            </Stack>
          </Box>
        </Stack>
      </Modal>
    </>
  );
}
