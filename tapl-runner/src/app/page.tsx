import { Container, Link, Typography } from "@mui/material";

export default function Home() {
  return (
    <Container>
      <Typography variant="h1">Lambda Systems</Typography>
      <ul>
        <li>
          型なし
          <ul>
            <li>
              <Link href="/untyped-arith">型なし計算（NatBool）</Link>
            </li>
            <li>λ: 型なしラムダ計算 (COMING SOON)</li>
          </ul>
        </li>
        <li>
          型あり
          <ul>
            <li>
              Unrestricted
              <ul>
                <li>
                  λ<sub>→</sub>: 単純型付きラムダ計算 (NatBool) (COMING SOON:
                  System F<sub>ω</sub>で代替してください)
                </li>
                <li>
                  System F<sub>n</sub>: n階命題型システム (NatBool) (COMING
                  SOON: 同上)
                </li>
                <li>
                  <Link href="/system-f-omega">
                    System F<sub>ω</sub>: 高階命題型システム (NatBool)
                  </Link>
                </li>
                <li>
                  <Link href="/cc">
                    λ<sub>CC</sub>: Calculus of Construction（CC）
                  </Link>
                </li>
              </ul>
            </li>
          </ul>
          <ul>
            <li>
              Substructural
              <ul>
                <li>Linear Types (TODO)</li>
                <li>Affine Types (TODO)</li>
                <li>Ordered Types (TODO)</li>
              </ul>
            </li>
          </ul>
        </li>
        <li>Pure Type System (TODO)</li>
      </ul>
    </Container>
  );
}
