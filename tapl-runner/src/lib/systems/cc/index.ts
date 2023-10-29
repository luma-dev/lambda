import { LambdaCcCalc } from "@lumax/tapl/tapl_systems";

export class LambdaCc {
  #inner: LambdaCcCalc;
  constructor(code: string) {
    this.#inner = LambdaCcCalc.parse(code);
  }
  get grammaticalSize(): number {
    return this.#inner.grammatical_size();
  }
  toCode() {
    return this.#inner.to_code();
  }
  evaluate() {
    return this.#inner.eval();
  }
  typing() {
    return this.#inner.typing();
  }
  isEvaluated() {
    return this.#inner.is_evaled();
  }
  is() {
    return this.#inner.is_typed();
  }
  typedCode() {
    return this.#inner.typed_to_code();
  }
  evaluatedCode() {
    return this.#inner.evaled_to_code();
  }
  isParseOk() {
    return this.#inner.is_parse_ok();
  }
  getParseResultDiagnosticsSummary() {
    const ds = this.#inner.parse_result_diagnostics();
    const s = ds.to_summary();
    ds.free();
    return s;
  }
  getTypingDiagnosticsSummary() {
    const ds = this.#inner.typing_diagnostics();
    const s = ds.to_summary();
    ds.free();
    return s;
  }
  getEvalDiagnosticsSummary() {
    const ds = this.#inner.eval_diagnostics();
    const s = ds.to_summary();
    ds.free();
    return s;
  }

  free() {
    this.#inner.free();
  }
}
