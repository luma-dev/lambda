import assert from "assert";
import { useCallback, useEffect, useMemo, useState } from "react";

export class MemoStack<T> {
  #arr1: T[] = [];
  #arr2: T[] = [];
  readonly #guaranteedLength: number;
  readonly #disposer: ((v: T) => void) | undefined = undefined;
  #nextIndex: bigint = 0n;
  constructor(guaranteedLength: number, disposer?: (v: T) => void) {
    this.#guaranteedLength = guaranteedLength;
    this.#disposer = disposer;
  }

  clear() {
    if (typeof this.#disposer !== "undefined") {
      for (const v of this.#arr1) {
        this.#disposer(v);
      }
      for (const v of this.#arr2) {
        this.#disposer(v);
      }
    }
    this.#arr1 = [];
    this.#arr2 = [];
    this.#nextIndex = 0n;
  }

  has(n: bigint): boolean {
    return (
      this.#nextIndex - BigInt(this.#arr1.length + this.#arr2.length) <= n &&
      n < this.#nextIndex
    );
  }

  nth(n: bigint): T | undefined {
    // debugger;
    if (this.has(n)) {
      const i1 = Number(n - this.#nextIndex) + this.#arr1.length;
      if (i1 >= 0) {
        assert(i1 < this.#arr1.length);
        return this.#arr1[i1];
      } else {
        assert(i1 + this.#arr2.length >= 0);
        return this.#arr2[i1 + this.#arr2.length];
      }
    }
    return undefined;
  }

  set(n: bigint, v: T) {
    if (this.has(n)) {
      return;
    }
    if (n > this.#nextIndex) {
      throw new Error(
        `Trying to set ${n}-th element but expected the value that is less than or equal to virtual length ${
          this.#nextIndex
        }`,
      );
    }
    if (n !== this.#nextIndex) {
      this.clear();
      this.#nextIndex = n;
    }

    if (this.#arr1.length > this.#guaranteedLength) {
      if (typeof this.#disposer !== "undefined") {
        for (const v of this.#arr2) {
          this.#disposer(v);
        }
      }
      this.#arr2 = this.#arr1;
      this.#arr1 = [];
    }
    this.#arr1.push(v);
    this.#nextIndex += 1n;
  }

  get virtualLength(): bigint {
    return this.#nextIndex;
  }

  get length(): number {
    return this.#arr1.length + this.#arr2.length;
  }
}

const recalculate = <T>(
  smallStepMemoQueue: MemoStack<T>,
  cursored: CursoredValue<T>,
  step: bigint,
  smallStepMemoQueueSize: number,
) => {
  let s = 0n;
  let c = cursored.createStart();
  const ssmqSizeBigint = BigInt(smallStepMemoQueueSize);
  while (s < step - ssmqSizeBigint) {
    s++;
    const old = c;
    c = cursored.next(old);
    cursored.drop(old);
  }
  while (s < step) {
    s++;
    const old = c;
    c = cursored.next(old);
    smallStepMemoQueue.set(s, c);
  }
  return c;
};

export interface CursoredValue<T> {
  createStart(): T;
  /** 非破壊 */
  next(v: T): T;
  /** 破壊 */
  drop(v: T): void;
  /** 非破壊 */
  clone(v: T): T;
  /** 非破壊 */
  isLast(v: T): boolean;
}

export type UseCursoredControlParams<T> = {
  cursored: CursoredValue<T>;
  smallStepMemoQueueSize: number;
  // largeStepMemoInterval: number;
  // largeStepMemoQueueSize: number;
};
export const useCursoredControl = <T>({
  cursored,
  smallStepMemoQueueSize,
}: UseCursoredControlParams<T>) => {
  const [states, setStates] = useState(() => {
    const smallStepMemoQueue = new MemoStack<T>(smallStepMemoQueueSize, (v) =>
      cursored.drop(v),
    );
    const cur = cursored.createStart();
    smallStepMemoQueue.set(0n, cur);
    return {
      smallStepMemoQueue,
      step: 0n,
      cur,
    };
  });

  useEffect(() => {
    setStates(({ smallStepMemoQueue }) => {
      const cur = cursored.createStart();
      smallStepMemoQueue.clear();
      smallStepMemoQueue.set(0n, cur);
      return {
        smallStepMemoQueue,
        step: 0n,
        cur,
      };
    });
  }, [cursored]);

  const next = useCallback(() => {
    setStates((states) => {
      if (cursored.isLast(states.cur)) {
        return states;
      }
      const { smallStepMemoQueue, step, cur } = states;
      const c = smallStepMemoQueue.nth(step + 1n);
      if (c === undefined) {
        const c = cursored.next(cur);
        smallStepMemoQueue.set(step + 1n, c);
        return {
          smallStepMemoQueue,
          cur: c,
          step: step + 1n,
        };
      } else {
        return {
          smallStepMemoQueue,
          cur: cursored.clone(c),
          step: step + 1n,
        };
      }
    });
  }, [cursored]);
  const isLast = useMemo(() => cursored.isLast(states.cur), [cursored, states]);
  const nextDisabled = isLast;
  const reset = useCallback(() => {
    setStates(({ smallStepMemoQueue }) => {
      return {
        smallStepMemoQueue,
        cur: cursored.createStart(),
        step: 0n,
      };
    });
  }, [cursored]);
  const resetDisabled = states.step === 0n;
  const prev = useCallback(() => {
    setStates((states) => {
      if (states.step === 0n) {
        return states;
      }
      const { smallStepMemoQueue, step } = states;
      const c = smallStepMemoQueue.nth(step - 1n);
      if (c === undefined) {
        const c = recalculate(
          smallStepMemoQueue,
          cursored,
          step - 1n,
          smallStepMemoQueueSize,
        );
        return {
          smallStepMemoQueue,
          cur: c,
          step: step - 1n,
        };
      } else {
        return {
          smallStepMemoQueue,
          cur: c,
          step: step - 1n,
        };
      }
    });
  }, [cursored, smallStepMemoQueueSize]);
  const prevDisabled = resetDisabled;

  const valWrapped = useMemo(() => {
    return { val: states.cur };
  }, [states]);

  return {
    step: states.step,
    valWrapped,
    isLast,
    next,
    nextDisabled,
    reset,
    resetDisabled,
    prev,
    prevDisabled,
  };
};
