"use client";
import { CssBaseline, ThemeProvider, createTheme } from "@mui/material";
import { QueryClient, QueryClientProvider } from "@tanstack/react-query";

import NextLink from "next/link";

const darkTheme = createTheme({
  palette: {
    mode: "dark",
  },
  components: {
    MuiLink: {
      defaultProps: {
        component: (props: React.ComponentProps<typeof NextLink>) => (
          <NextLink {...props} />
        ),
      },
    },
    MuiButtonBase: {
      defaultProps: {
        LinkComponent: (props: React.ComponentProps<typeof NextLink>) => (
          <NextLink {...props} />
        ),
      },
    },
  },
});

interface ClientLayoutProps {
  children: React.ReactNode;
}
const queryClient = new QueryClient();
export default function ClientLayout({ children }: ClientLayoutProps) {
  return (
    <QueryClientProvider client={queryClient}>
      <ThemeProvider theme={darkTheme}>
        <CssBaseline />
        {children}
      </ThemeProvider>
    </QueryClientProvider>
  );
}
