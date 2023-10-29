import { Metadata } from "next";
import MainContainer from "./container";

export const metadata: Metadata = {
  title: "Calculus of Construction (CC)",
};

export default function Home() {
  return <MainContainer />;
}
