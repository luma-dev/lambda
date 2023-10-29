import { useCallback } from "react";
import { useCursoredControl } from ".";

export const useCursoredControlCustom = <T>(
  ...args: Parameters<typeof useCursoredControl<T>>
) => {
  const { prev, next, ...rest } = useCursoredControl<T>(...args);

  const prev100 = useCallback(() => {
    for (let i = 0; i < 100; i++) {
      prev();
    }
  }, [prev]);
  const next100 = useCallback(() => {
    for (let i = 0; i < 100; i++) {
      next();
    }
  }, [next]);

  return {
    ...rest,
    prev,
    next,
    prev100,
    next100,
  };
};
