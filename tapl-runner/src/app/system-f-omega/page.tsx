import { Metadata } from "next";
import MainContainer from "./container";

export const metadata: Metadata = {
  title: "System Fω",
};

export default function Home() {
  return <MainContainer />;
}
