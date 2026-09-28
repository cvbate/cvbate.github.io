import type { Route } from "./+types/home";
import { Welcome } from "../welcome/welcome";

export function meta({}: Route.MetaArgs) {
  return [
    { title: "Clio Bate's resume website." },
    { name: "description", content: "Welcome to my interactive resume app!" },
  ];
}

export default function Home() {
  return <Welcome />;
}
