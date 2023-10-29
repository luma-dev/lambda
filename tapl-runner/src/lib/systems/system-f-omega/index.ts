import { CursoredValue } from "@/lib/cursored";
import { UnrestrictedArithCalc } from "@lumax/tapl/tapl_systems";

const TAKE_INNER = Symbol("TAKE_INNER");

export class TermEvaluator {
  #inner: UnrestrictedArithCalc;
  constructor(inner: UnrestrictedArithCalc) {
    this.#inner = inner;
  }
  static parse(s: string): TermEvaluator {
    return new TermEvaluator(UnrestrictedArithCalc.parse(s));
  }
  next(): TermEvaluator {
    this.#inner.eval_next();
    return this;
  }
  clone(): TermEvaluator {
    return new TermEvaluator(this.#inner.wasm_clone());
  }
  isNormalForm(): boolean {
    if (!this.isParseOk()) {
      return true;
    }
    return this.#inner.is_normal_form();
  }
  free(): void {
    this.#inner.free();
  }
  formattedCode(): string {
    if (!this.isParseOk()) {
      return "<parse error>";
    }
    return this.#inner.formatted_code();
  }
  getParseResultDiagnosticsString(): string {
    const ds = this.#inner.parse_result_diagnostics();
    const s = ds.to_summary();
    ds.free();
    return s;
  }
  isParseOk(): boolean {
    return this.#inner.is_parse_ok();
  }
  [TAKE_INNER](): UnrestrictedArithCalc {
    const inner = this.#inner;
    this.#inner = null!;
    return inner;
  }
}

export class UntypedArithTermCursored implements CursoredValue<TermEvaluator> {
  #initial: TermEvaluator;
  constructor(initial: TermEvaluator) {
    this.#initial = initial;
  }
  createStart(): TermEvaluator {
    return this.#initial.clone();
  }
  next(v: TermEvaluator): TermEvaluator {
    const v2 = v.clone();
    return v2.next();
  }
  drop(v: TermEvaluator): void {
    v.free();
  }
  clone(v: TermEvaluator): TermEvaluator {
    return v.clone();
  }
  isLast(v: TermEvaluator): boolean {
    return v.isNormalForm();
  }
}

export class TypeEvaluator {
  #inner: UnrestrictedArithCalc;
  constructor(inner: UnrestrictedArithCalc) {
    this.#inner = inner;
  }
  static fromTermEvaluator(termEvaluator: TermEvaluator): TypeEvaluator {
    const inner = termEvaluator[TAKE_INNER]();
    inner.typing();
    return new TypeEvaluator(inner);
  }
  next(): TypeEvaluator {
    this.#inner.eval_type_next();
    return this;
  }
  clone(): TypeEvaluator {
    return new TypeEvaluator(this.#inner.wasm_clone());
  }
  isNormalForm(): boolean {
    if (!this.isTypingOk()) {
      return true;
    }
    return this.#inner.type_is_normal_form();
  }
  free(): void {
    this.#inner.free();
  }
  isTyped(): boolean {
    return this.#inner.is_typed();
  }
  isTypingOk(): boolean {
    return this.#inner.is_typing_ok();
  }
  typingDiagnosticSummary(): string {
    const ds = this.#inner.typing_diagnostics();
    const s = ds.to_summary();
    ds.free();
    return s;
  }
  formattedType(): string {
    if (!this.isTyped()) {
      return "<not typed>";
    }
    if (!this.isTypingOk()) {
      return `<error>\n` + this.typingDiagnosticSummary();
    }
    return this.#inner.formatted_type();
  }
}

export class UntypedArithTypeCursored implements CursoredValue<TypeEvaluator> {
  #initial: TypeEvaluator;
  constructor(initial: TypeEvaluator) {
    this.#initial = initial;
  }
  createStart(): TypeEvaluator {
    return this.#initial.clone();
  }
  next(v: TypeEvaluator): TypeEvaluator {
    const v2 = v.clone();
    return v2.next();
  }
  drop(v: TypeEvaluator): void {
    v.free();
  }
  clone(v: TypeEvaluator): TypeEvaluator {
    return v.clone();
  }
  isLast(v: TypeEvaluator): boolean {
    return v.isNormalForm();
  }
}
