import { useQuery } from "@tanstack/react-query";
import { useEffect, useState } from "react";

const key = "vimMode";
export const useVimMode = () => {
  const [vimMode, setVimMode] = useState(false);
  const monacoVim = useQuery({
    queryKey: [vimMode, "import:monaco-vim"],
    queryFn: () => (vimMode ? import("monaco-vim") : null),
  }).data;

  useEffect(() => {
    const item = localStorage.getItem(key);
    if (item !== null) {
      setVimMode(item === "true");
    }
  }, []);

  const setVimModeWrapped = (value: boolean) => {
    setVimMode(value);
    localStorage.setItem(key, value ? "true" : "false");
  };

  return { vimMode, setVimMode: setVimModeWrapped, monacoVim };
};
