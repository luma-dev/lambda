import { CursoredValue } from "@/lib/cursored";
import { UntypedArithCalc } from "@lumax/tapl/tapl_systems";

export class Evaluator {
  #inner: UntypedArithCalc;
  constructor(inner: UntypedArithCalc) {
    this.#inner = inner;
  }
  static parse(s: string): Evaluator {
    return new Evaluator(UntypedArithCalc.parse(s));
  }
  next(): Evaluator {
    this.#inner = this.#inner.next();
    return this;
  }
  clone(): Evaluator {
    return new Evaluator(this.#inner.wasm_clone());
  }
  isNormalForm(): boolean {
    return this.#inner.is_normal_form();
  }
  free(): void {
    this.#inner.free();
  }
  formattedLatex(): string {
    return this.#inner.formatted_latex();
  }
  formattedCode(): string {
    return this.#inner.formatted_code();
  }
  isParseOk(): boolean {
    return this.#inner.is_parse_ok();
  }
}

export class UntypedArithCursored implements CursoredValue<Evaluator> {
  #initial: Evaluator;
  constructor(initial: Evaluator) {
    this.#initial = initial;
  }
  createStart(): Evaluator {
    return this.#initial.clone();
  }
  next(v: Evaluator): Evaluator {
    const v2 = v.clone();
    return v2.next();
  }
  drop(v: Evaluator): void {
    v.free();
  }
  clone(v: Evaluator): Evaluator {
    return v.clone();
  }
  isLast(v: Evaluator): boolean {
    return v.isNormalForm();
  }
}
